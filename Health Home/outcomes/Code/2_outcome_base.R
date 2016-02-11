modify <- new.env(parent = .GlobalEnv)
saved <- new.env(parent = .GlobalEnv)

output <- copy(sql$output)
wellness <- copy(sql$wellness)
setnames(output, names(output), tolower(names(output)))
setnames(wellness, names(wellness), tolower(names(wellness)))

# OUTPUT ----------------------------------------------------------------------
output[is.na(hh_team), hh_team := "N"]
output[is.na(samhsa_staff), samhsa_staff := "none assigned"]

# exclude all children
output[, dob  := date_convert(dob)]
output[, vt_date := date_convert(vt_date)]
output[, age := as.int(floor((vt_date-dob)/365.25))]
output <- output[age >= 18] # must be 18+ on every vt_date
# # only want the last age... for now
# output[, age := max(age), by = "case_no"] # NAs should not be possible
output[, team := cmh_recode(team)]
modify$bmi_dt <- copy(output[, unique(.SD),
  .SDcols = c("case_no", "team", "hh_team", "samhsa_staff",
              "stafftype", "vt_date", "bmi", "weight", "height_feet",
              "height_inches")])
modify$bp_dt <-  copy(output[, unique(.SD),
  .SDcols = c("case_no", "team", "hh_team", "samhsa_staff",
  "stafftype", "vt_date", "diastolic", "systolic")])
# modify$bmi_dt: BMI ----------------------------------------------------------
# combine all existing heights
modify$bmi_dt[, height_feet := as.num(height_feet)]
modify$bmi_dt[, height_inches := as.num(height_inches)]
modify$bmi_dt[, total_inches := psum(height_feet*12, height_inches)]
# modify$bmi_dt: BMI: for error checking/reporting ----------------------------
# If abs(mean(total_inches)-total_inches) > 1 for any consumer,
# then that consumers record is put on the *BAD* list and removed
# until fixed, per Brandie Hagaman on 11/30/2015.
saved$note_height_issues <-
"If abs(mean(total_inches)-total_inches) > 1 for any consumer,
then that consumers record is put on the *BAD* list and removed
until fixed, per Brandie Hagaman on 11/30/2015."
modify$bmi_dt[, avg_inches := mean(total_inches, na.rm = TRUE),
       by = list(case_no)]
modify$bmi_dt[, abs_inch_diff := abs(avg_inches - total_inches)]
modify$bmi_dt[, avg_inches := round(avg_inches, 1)]
modify$cases_height_errror <-
  modify$bmi_dt[abs_inch_diff > 1, unique(case_no)]
saved$records_height_error <-
  modify$bmi_dt[case_no %in% modify$cases_height_errror &
           !is.na(height_feet),
         unique(.SD), .SDcols = c(
           "case_no",
           "hh_team",
           "vt_date",
           "height_feet",
           "height_inches",
           "total_inches",
           "avg_inches"
         ), keyby = list(case_no, vt_date)]
modify$bmi_dt[, c("height_feet", "height_inches",
                  "avg_inches", "abs_inch_diff") := NULL]
saved$summary_height_errors <- modify$bmi_dt[case_no %in%
                                               modify$cases_height_errror,
       list(num_consumers_height_errors = length(unique(case_no))),
       by = hh_team]
modify$bmi_dt <- modify$bmi_dt[case_no %nin% modify$cases_height_errror]
# calculate BMI manually
modify$bmi_dt[is.na(bmi), bmi := round(weight*703/total_inches^2, 1)]
# filter out people with most recent BPS under 18.5
modify$bmi_dt[, max_vt_date := max(vt_date, na.rm = TRUE), by = case_no]
saved$bmi_too_low_summary <-
  modify$bmi_dt[bmi < 18.5 & max_vt_date == vt_date,
         list(con_bmi_too_low = length(unique(case_no))), by = hh_team]
saved$bmi_too_low_records <-
  modify$bmi_dt[case_no %in% modify$bmi_dt[bmi < 18.5 & max_vt_date == vt_date,
                             unique(case_no)],
         unique(.SD), .SDcols = c("case_no", "hh_team", "vt_date",
                                  "bmi", "total_inches"),
         keyby = c("team", "case_no")]

modify$bmi_dt <- modify$bmi_dt[case_no %nin%
  modify$bmi_dt[bmi < 18.5 & max_vt_date == vt_date, unique(case_no)]]
# people missing weight - 9 right now - removing
modify$bmi_dt[, missing_weight :=
         aux$all_na(weight), by = case_no]
if (nrow(modify$bmi_dt[is.na(missing_weight)]) > 0) {
saved$cases_missing_weight <-
  modify$bmi_dt[is.na(missing_weight)]
modify$bmi_dt <- modify$bmi_dt[!is.na(missing_weight)]
}
modify$bmi_dt[, c("missing_weight", "max_vt_date") := NULL]

# possibly data entry errors
saved$bad_weight <- modify$bmi_dt[bmi < 18.5 & weight < 70, unique(.SD),
  .SDc = c("case_no", "team", "hh_team", "vt_date", "bmi", "weight",
           "total_inches")]
modify$bmi_dt <-setkey(modify$bmi_dt, case_no, vt_date)[
  !saved$bad_weight[, list(case_no, vt_date)]]

# first and last available BMI
modify$bmi_dt[!is.na(bmi), min_vt_date :=
  min(vt_date, na.rm = TRUE), by = case_no]
modify$bmi_dt[!is.na(bmi), max_vt_date :=
  max(vt_date, na.rm = TRUE), by = case_no]
modify$bmi_dt[, bmi_date_diff :=
  as.numeric(as.Date(max_vt_date)- as.Date(min_vt_date))]
# greater than or equal 30 day requirement, per Brandie H 11/23/2015
saved$bmi_not_30_days <- modify$bmi_dt[bmi_date_diff < 30]
modify$bmi_dt <- modify$bmi_dt[bmi_date_diff >= 30]

# we can go further back to get more heights
saved$note_height_idea1 <-
  "we can go back further than one year to get more heights"

modify$bmi_dt <- unique(modify$bmi_dt)
modify$bmi_dt[, num_bmi := length(unique(vt_date)) , by = case_no]

modify$output$bmi <- mmerge( l = list(
  modify$bmi_dt[min_vt_date==vt_date, list(case_no, first_bmi = bmi)],
  modify$bmi_dt[max_vt_date==vt_date, list(case_no, last_bmi = bmi)],
  modify$bmi_dt[, unique(.SD),
    .SDcols = c("case_no", "team", "hh_team", "samhsa_staff",
                "stafftype")]), all = TRUE, by = "case_no")
# pre-analysis summmary to determine the appropriateness of using data --------
modify$pre$weight_output <-
  copy(modify$bmi_dt[!is.na(weight), unique(.SD),
         .SDc = c("case_no", "hh_team", "weight", "vt_date")])
modify$pre$weight_output[, num_weight := length(unique(vt_date)),
              by = list(case_no)]
modify$pre$weight_output  <-
  modify$pre$weight_output[, unique(.SD),
                           .SDc = c("case_no", "hh_team", "num_weight")]
# BMI - consumers by category hh_team
modify$pre$bmi1 <-
  output[, list(total_consumers = length(unique(case_no))),
       by = hh_team]
# BMI at least one
modify$pre$bmi2 <-
  modify$bmi_dt[num_bmi >= 1,
           list(at_least_1_BMI = length(unique(case_no))), by = hh_team]
# BMI at least 2
modify$pre$bmi3 <-
  modify$bmi_dt[num_bmi >= 2,
               list(at_least_2_BMI = length(unique(case_no))), by = hh_team]
# at least 2 weights
modify$pre$bmi4 <-
  modify$pre$weight_output[num_weight >= 2,
               list(at_least_2_weights = length(unique(case_no))),
               by = hh_team]
# Overall health --------------------------------------------------------------
modify$wellness <- copy(wellness)
modify$wellness[overall_health_rating=="No Response",
         overall_health_rating := NA]
modify$wellness[!is.na(overall_health_rating),
         max_wn_date := max(wellnessnote_date, na.rm = TRUE), by = case_no]
modify$wellness[!is.na(overall_health_rating),
         min_wn_date := min(wellnessnote_date, na.rm = TRUE), by = case_no]
modify$pre$w1 <-
  modify$wellness[, list(total_consumers = length(unique(case_no))),
                  by = hh_team]
# wellness[max_wn_date!=min_wn_date, length(unique(case_no)), by = hh_team]
modify$wellness[!is.na(overall_health_rating), num_wn :=
                  length(unique(wellnessnote_date)), by = case_no]
modify$wellness[is.na(num_wn), num_wn := 0]
modify$pre$w2 <-
  modify$wellness[num_wn == 0, list(missing_wn = length(unique(case_no))),
                  by = hh_team]
modify$pre$w3 <-
  modify$wellness[num_wn >= 1, list(at_least_1_wn = length(unique(case_no))),
                  by = hh_team]
modify$pre$w4 <-
  modify$wellness[num_wn >= 2, list(at_least_2_wn = length(unique(case_no))),
                  by = hh_team]

# blood pressure --------------------------------------------------------------
modify$bp_dt <-
  output[, unique(.SD),
   .SDcols = c("case_no", "hh_team", "vt_date", "diastolic", "systolic", "age",
               "samhsa_staff", "stafftype")]
# systolic needs to be higher than diastolic
modify$bp_dt <- modify$bp_dt[systolic >= diastolic]

# we want blood pressure with both the upper and lower numbers
modify$bp_dt <- modify$bp_dt[!is.na(diastolic) & !is.na(systolic)]
modify$bp_dt[, max_vt_date := max(vt_date, na.rm = TRUE), by = case_no]
         modify$bp_dt[, min_vt_date := min(vt_date, na.rm = TRUE), by = case_no]
# fix records with multiple blood pressures in one day
modify$bp_dt[, diastolic := as.num(diastolic)]
modify$bp_dt[, systolic := as.num(systolic)]
modify$bp_dt[, c("diastolic", "systolic") :=
  list(round(mean(diastolic)), round(mean(systolic))),
  by = list(case_no, vt_date)]
modify$bp_dt <- unique(modify$bp_dt)

# jama categories
modify$bp_dt[, c("sys_jama", "dia_jama") :=
  list(aux$sys_jama(age, systolic),
       aux$dia_jama(age, diastolic))]

# combining first/last bp dates to one row = one consumer
modify$output$bp <- mmerge(l = list(
  unique(modify$bp_dt[min_vt_date == vt_date,
                   list(case_no, sys_jama1 = sys_jama, dia_jama1 = dia_jama)]),
  unique(modify$bp_dt[max_vt_date == vt_date,
                      list(case_no, sys_jama2 = sys_jama,
                           dia_jama2 = dia_jama)]),
  modify$bp_dt[, unique(.SD),
                .SDcols = c("case_no", "hh_team", "samhsa_staff",
                            "stafftype")]),
  all = TRUE, by = "case_no")
modify$output$bp <- unique(modify$output$bp)

# improvement -----------------------------------------------------------------
modify$output$bp[, c("jama_sys_status", "jama_dia_status") :=
                   list(aux$jama_eval(sys_jama1, sys_jama2),
                        aux$jama_eval(dia_jama1, dia_jama2))]

### create information about the file to share with end-users ###
about_file <- data.table(data_source = "E2 report 2281: E2_HH_vs_non_HH_Vital",
                         date_range = "7/1/14 to date_ran",
                         date_ran = input$run_date,
                         last_updated = input$run_date,
                         project_location = project_wd$project)

