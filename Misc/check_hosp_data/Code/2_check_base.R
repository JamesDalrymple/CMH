# start_date will always be one year prior to end date
# start_date <- date_convert(end_date) - 365

modify <- new.env(parent = .GlobalEnv)
saved <- new.env(parent = .GlobalEnv)

output <- copy(sql$output)
wellness <- copy(sql$wellness)
setnames(output, names(output), tolower(names(output)))
setnames(wellness, names(wellness), tolower(names(wellness)))

# OUTPUT ----------------------------------------------------------------------
output[is.na(hh_team), hh_team := "N"]
# exclude all children
output[, dob  := date_convert(dob)]
output[, vt_date := date_convert(vt_date)]
output[, age := as.int(floor((vt_date-dob)/365.25))]
output <- output[age >= 18] # must be 18+ on every vt_date
output[, team := cmh_recode(team)]
bmi_dt <- copy(output[, unique(.SD),
  .SDcols = c("case_no", "team", "age", "hh_team", "samhsa_staff",
              "stafftype", "vt_date", "bmi", "weight", "height_feet",
              "height_inches")])
bp_dt <-  copy(output[, unique(.SD),
  .SDcols = c("case_no", "team", "age", "hh_team", "samhsa_staff",
  "stafftype", "vt_date", "diastolic", "systolic")])

# bmi_dt: BMI ------------------------------------------------------------------
# combine all existing heights
bmi_dt[, height_feet := as.num(height_feet)]
bmi_dt[, height_inches := as.num(height_inches)]
bmi_dt[, total_inches := psum(height_feet*12, height_inches)]
# bmi_dt: BMI: for error checking/reporting ----------------------------------
# If abs(mean(total_inches)-total_inches) > 1 for any consumer,
# then that consumers record is put on the *BAD* list and removed
# until fixed, per Brandie Hagaman on 11/30/2015.
saved$note_height_issues <-
"If abs(mean(total_inches)-total_inches) > 1 for any consumer,
then that consumers record is put on the *BAD* list and removed
until fixed, per Brandie Hagaman on 11/30/2015."
bmi_dt[, avg_inches := mean(total_inches, na.rm = TRUE),
       by = list(case_no)]
bmi_dt[, abs_inch_diff := abs(avg_inches - total_inches)]
bmi_dt[, avg_inches := round(avg_inches, 1)]
modify$cases_height_errror <-
  bmi_dt[abs_inch_diff > 1, unique(case_no)]
saved$records_height_error <-
  bmi_dt[case_no %in% modify$cases_height_errror &
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
bmi_dt[, c("height_feet", "height_inches", "avg_inches", "abs_inch_diff")
       := NULL]
saved$summary_height_errors <- bmi_dt[case_no %in% modify$cases_height_errror,
       list(num_consumers_height_errors = length(unique(case_no))),
       by = hh_team]
bmi_dt <- bmi_dt[case_no %nin% modify$cases_height_errror]
# calculate BMI manually
bmi_dt[is.na(bmi), bmi := round(weight*703/total_inches^2, 1)]
# filter out people with most recent BPS under 18.5
bmi_dt[, max_vt_date := max(vt_date, na.rm = TRUE), by = case_no]
saved$bmi_too_low_summary <-
  bmi_dt[bmi < 18.5 & max_vt_date == vt_date,
         list(con_bmi_too_low = length(unique(case_no))), by = hh_team]
saved$bmi_too_low_records <-
  bmi_dt[case_no %in% bmi_dt[bmi < 18.5 & max_vt_date == vt_date,
                             unique(case_no)],
         unique(.SD), .SDcols = c("case_no", "hh_team", "vt_date",
                                  "bmi", "total_inches"),
         keyby = c("team", "case_no")]
bmi_dt[, max_vt_date := NULL]
bmi_dt <- bmi_dt[case_no %nin% bmi_dt[bmi < 18.5 & max_vt_date == vt_date,
                           unique(case_no)]]
# people missing weight - 9 right now - removing
bmi_dt[, missing_weight :=
         aux$all_na(weight), by = case_no]
if (nrow(bmi_dt[is.na(missing_weight)]) > 0) {
saved$cases_missing_weight <-
  bmi_dt[is.na(missing_weight)]
bmi_dt <- bmi_dt[!is.na(missing_weight)]
bmi_dt[, missing_weight := NULL]
}

# possibly data entry errors
saved$bad_weight <- bmi_dt[bmi < 18.5 & weight < 70, unique(.SD),
  .SDc = c("case_no", "team", "hh_team", "vt_date", "bmi", "weight",
           "age", "total_inches")]
bmi_dt <-setkey(bmi_dt, case_no, vt_date)[
  !saved$bad_weight[, list(case_no, vt_date)]]

# first and last available BMI
bmi_dt[!is.na(bmi), min_vt_date := min(vt_date, na.rm = TRUE), by = case_no]
bmi_dt[!is.na(bmi), max_vt_date := max(vt_date, na.rm = TRUE), by = case_no]
bmi_dt[, bmi_date_diff :=
  as.numeric(as.Date(max_vt_date)- as.Date(min_vt_date))]
# greater than or equal 30 day requirement, per Brandie H 11/23/2015
saved$bmi_not_30_days <- bmi_dt[bmi_date_diff < 30]
bmi_dt <- bmi_dt[bmi_date_diff >= 30]

# we can go further back to get more heights
saved$note_height_idea1 <-
  "we can go back further than one year to get more heights"

bmi_dt <- unique(bmi_dt)
bmi_dt[, num_bmi := length(unique(vt_date)) , by = case_no]


######## LEFT OFF HERE 11/30/2015


# # create bmi output ---
# bmi_output <-
#   copy(output[!is.na(bmi) & bmi_date_diff >= 30, .SD,
#          .SDc = c("case_no", "hh_team", "bmi", "vt_date")])

# bmi_output <- unique(bmi_output)
# bmi_output[, num_bmi := length(unique(vt_date)) , by = case_no]
# bmi_output <-
#   unique(bmi_output[, .SD, .SDc = c("case_no", "hh_team", "num_bmi")])

# create weight output ---
weight_output <-
  copy(bmi_dt[!is.na(weight), .SD,
         .SDc = c("case_no", "hh_team", "weight", "vt_date")])
weight_output <- unique(weight_output)
weight_output[, num_weight := length(unique(vt_date)),
              by = list(case_no)]
weight_output <-
  unique(weight_output[, .SD, .SDc = c("case_no", "hh_team", "num_weight")])

# BMI - consumers by category hh_team
bmi1 <- output[, list(total_consumers = length(unique(case_no))),
       by = hh_team]
# BMI at least one
bmi2 <- bmi_dt[num_bmi >= 1,
           list(at_least_1_BMI = length(unique(case_no))), by = hh_team]
# BMI at least 2
bmi3 <- bmi_dt[num_bmi >= 2,
           list(at_least_2_BMI = length(unique(case_no))), by = hh_team]
# at least 2 weights
bmi4 <- weight_output[num_weight >= 2,
              list(at_least_2_weights = length(unique(case_no))),
              by = hh_team]
t(mmerge(bmi1, bmi2, bmi3, bmi4, by = "hh_team"))

# Overall health --------------------------------------------------------------
wellness[overall_health_rating=="No Response",
         overall_health_rating := NA]
wellness[!is.na(overall_health_rating),
         max_wn_date := max(wellnessnote_date, na.rm = TRUE), by = case_no]
wellness[!is.na(overall_health_rating),
         min_wn_date := min(wellnessnote_date, na.rm = TRUE), by = case_no]
w1 <- wellness[, list(total_consumers = length(unique(case_no))),
               by = hh_team]
# wellness[max_wn_date!=min_wn_date, length(unique(case_no)), by = hh_team]
wellness[!is.na(overall_health_rating), num_wn :=
  length(unique(wellnessnote_date)), by = case_no]
wellness[is.na(num_wn), num_wn := 0]
w2 <- wellness[num_wn==0, list(missing_wn = length(unique(case_no))),
                   by = hh_team]
w3 <- wellness[num_wn >= 1, list(at_least_1_wn = length(unique(case_no))),
               by = hh_team]
w4 <- wellness[num_wn >= 2, list(at_least_2_wn = length(unique(case_no))),
               by = hh_team]
t(mmerge(w1, w2, w3, w4, by = "hh_team"))
# wellness[is.na(max_wn_date) &
#          is.na(min_wn_date), length(unique(case_no)), by = hh_team]


### create information about the file to share with end-users ###
about_file <- data.table(data_source = "E2 report 2281: E2_HH_vs_non_HH_Vital",
                         date_range = "7/1/14 to date_ran",
                         date_ran = input$run_date,
                         last_updated = input$run_date,
                         project_location = project_wd$project)

