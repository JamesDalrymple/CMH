# bmi_guide <-
#   list(underweight = "<18.5", healthy = "18.5-24.9",
#        overweight = "25-29.9", obese = ">30")

# hypertension (systolic) guide:
# less_than_60 <-
# list(normal = "<139/89", hypertension = ">140/90", emergency = ">180/90")
# sixty_plus <-
# list(normal = "<149/89", hypertension = ">150/90", emergency = ">180/90")
# http://www.mayoclinic.org/diseases-conditions/low-blood-pressure/basics/causes/con-20032298
# http://www.disabled-world.com/artman/publish/bloodpressurechart.shtml

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
output[, sixty_plus := ifelse(age >= 60, "over 60", "less than 60")]
# # only want the last age.. for now
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
               "sixty_plus", "samhsa_staff", "stafftype")]

# we want blood pressure with both the upper and lower numbers
modify$bp_dt <- modify$bp_dt[!is.na(diastolic) & !is.na(systolic)]
modify$bp_dt[, max_vt_date := max(vt_date, na.rm = TRUE), by = case_no]
modify$bp_dt[, min_vt_date := min(vt_date, na.rm = TRUE), by = case_no]
# fix records with multiple blood pressures in one day
modify$bp_dt[, diastolic := as.num(diastolic)]
modify$bp_dt[, systolic := as.num(systolic)]
modify$bp_dt[min_vt_date == vt_date, diastolic :=
                   round(mean(diastolic)), by = case_no]
modify$bp_dt[max_vt_date == vt_date, diastolic :=
                   round(mean(diastolic)), by = case_no]
modify$bp_dt[min_vt_date == vt_date, systolic :=
                   round(mean(systolic)), by = case_no]
modify$bp_dt[max_vt_date == vt_date, systolic :=
                   round(mean(systolic)), by = case_no]
# detailed categories
modify$bp_dt[, sys_detail_cat := aux$systolic_cat(age, systolic)]
modify$bp_dt[, dia_detail_cat := aux$systolic_cat(age, diastolic)]
# distance from age-based average bp values
modify$bp_dt[, dia_dist := aux$diastolic_distance(age, diastolic)]
modify$bp_dt[, sys_dist := aux$diastolic_distance(age, systolic)]
# combining first/last bp dates to one row = one consumer
modify$output$bp <- mmerge(l = list(
  unique(modify$bp_dt[min_vt_date == vt_date,
                   list(case_no, first_systolic = systolic,
                        first_sys_detail_cat = sys_detail_cat,
                        first_diastolic = diastolic,
                        first_dia_detail_cat = dia_detail_cat,
                        first_sys_dist = sys_dist,
                        first_dia_dist = dia_dist)]),
  unique(modify$bp_dt[max_vt_date == vt_date,
                      list(case_no, last_systolic = systolic,
                           last_sys_detail_cat = sys_detail_cat,
                           last_diastolic = diastolic,
                           last_dia_detail_cat = dia_detail_cat,
                           last_sys_dist = sys_dist,
                           last_dia_dist = dia_dist)]),
  modify$bp_dt[, unique(.SD),
                .SDcols = c("case_no", "hh_team", "samhsa_staff",
                            "stafftype")]),
  all = TRUE, by = "case_no")
modify$output$bp <- unique(modify$output$bp)

# improvement -----------------------------------------------------------------
# improvement based on detail categorization
modify$output$bp[, detail_sys_status :=
  aux$detail_cat_eval(first_sys_detail_cat, last_sys_detail_cat)]
modify$output$bp[, detail_dia_status :=
  aux$detail_cat_eval(first_dia_detail_cat, last_dia_detail_cat)]
# improvement based on distance
modify$output$bp[, sys_dist_status :=
                   aux$dist_eval(first_sys_dist, last_sys_dist)]
modify$output$bp[, dia_dist_status :=
                   aux$dist_eval(first_dia_dist, last_dia_dist)]
# # look for lower bp unless it is too low
# # I just cant see this working accurately... James Dalrymple, 12/9/2015
# modify$output$bp[as.numeric(first_sys_detail_cat) %in% c(1:3) &
#   last_systolic-first_systolic > 0, status_numeric := "improved"]
# modify$output$bp[last_systolic-first_systolic == 0,
#                  status_numeric := "maintained"]
# modify$output$bp[as.numeric(first_sys_detail_cat) %in% c(1:3) &
#   last_systolic-first_systolic < 0, status_numeric := "decreased"]
# modify$output$bp[as.numeric(first_sys_detail_cat) %nin% c(1:3) &
#   last_systolic-first_systolic < 0, status_numeric := "decreased"]





# modify$bp_dt[sixty_plus == "less than 60", systolic_cat :=
#                cut(
#                  systolic,
#                  breaks = c(0, 50, 90, 140, 180, Inf),
#                  right = FALSE,
#                  labels = c("emergency:low", "hypotension", "normal",
#                             "hypertension", "emergency:high"))]
# modify$bp_dt[sixty_plus == "over 60", systolic_cat :=
#                cut(
#                  systolic,
#                  breaks = c(0, 50, 90, 150, 180, Inf),
#                  right = FALSE,
#                  labels = c("emergency:low", "hypotension", "normal",
#                             "hypertension", "emergency:high"))]
# modify$bp_dt[, diastolic_cat :=
#                cut(
#                  diastolic,
#                  breaks = c(0, 33, 60, 90, 110, Inf),
#                  right = FALSE,
#                  labels = c("emergency:low", "hypotension", "normal",
#                             "hypertension", "emergency:high"))]

# modify$bp_dt[systolic < 90 & systolic_cat == "normal"]

# modify$bp_dt[(systolic < 90 | diastolic < 60) &  systolic_cat == "normal",
#              systolic_cat := "hypotension"]



# modify$bp_max <-
#   modify$bp_dt[vt_date == max_vt_date, unique(.SD),
#   .SDc = c("case_no", "hh_team", "diastolic", "systolic", "systolic_cat",
#            "sixty_plus", "samhsa_staff", "stafftype")]
# modify$bp_min <-
#   modify$bp_dt[vt_date == min_vt_date, unique(.SD),
#   .SDc = c("case_no", "hh_team", "diastolic", "systolic", "systolic_cat",
#            "sixty_plus", "samhsa_staff", "stafftype")]
# modify$bp_change <- c("diastolic", "systolic", "systolic_cat", "sixty_plus")
# setnames(modify$bp_min,
#          old = modify$bp_change,
#          new = paste("first", modify$bp_change, sep = "_"))
# setnames(modify$bp_max,
#          old = modify$bp_change,
#          new = paste("last", modify$bp_change, sep = "_"))
# modify$output$bp <- merge(modify$bp_min,
#       modify$bp_max,
#       all = TRUE,
#       by = c("case_no", "hh_team", "samhsa_staff", "stafftype")
# )

# systolic >= 90
# modify$output$bp[last_systolic >= 90 &
#                  last_systolic - first_systolic < 0,
#                  systolic_cond := "improved"]
# modify$output$bp[last_systolic >= 90 &
#                    last_systolic - first_systolic == 0,
#                  systolic_cond := "maintained"]
# modify$output$bp[last_systolic >= 90 &
#                    last_systolic - first_systolic > 0,
#                  systolic_cond := "decreased"]
# # systolic < 90
# modify$output$bp[first_systolic < 90 &
#                  last_systolic < 140 & last_sixty_plus == "less than 60" &
#                    last_systolic - first_systolic > 0,
#                  systolic_cond := "improved"]
# modify$output$bp[first_systolic < 90 &
#                    last_systolic < 150 & last_sixty_plus == "over 60" &
#                    last_systolic - first_systolic > 0,
#                  systolic_cond := "improved"]
# modify$output$bp[first_systolic < 90 &
#                    last_systolic < 140 & last_sixty_plus == "less than 60" &
#                    last_systolic - first_systolic == 0,
#                  systolic_cond := "maintained"]
# modify$output$bp[first_systolic < 90 &
#                    last_systolic < 150 & last_sixty_plus == "over 60" &
#                    last_systolic - first_systolic == 0,
#                  systolic_cond := "maintained"]
# modify$output$bp[first_systolic < 90 &
#                    last_systolic < 140 & last_sixty_plus == "less than 60" &
#                    last_systolic - first_systolic < 0,
#                  systolic_cond := "decreased"]
# modify$output$bp[first_systolic < 90 &
#                    last_systolic < 150 & last_sixty_plus == "over 60" &
#                    last_systolic - first_systolic < 0,
#                  systolic_cond := "decreased"]
# # unusual cases
# modify$output$bp[first_systolic < 90 & last_systolic > 140 &
#                  last_sixty_plus == "less than 60",
#                  systolic_cond := "decreased"]
# modify$output$bp[first_systolic < 90 & last_systolic > 150 &
#                    last_sixty_plus == "over 60",
#                  systolic_cond := "decreased"]
#
# modify$output$bp[last_systolic < 90 &
#                  first_systolic >= 90 & first_systolic < 140 &
#                  first_sixty_plus == "less than 60" &
#                  last_systolic - first_systolic < 0,
#                  systolic_cond := "decreased"]
# modify$output$bp[last_systolic < 90 &
#                    first_systolic >= 90 & first_systolic < 150 &
#                    first_sixty_plus == "over 60" &
#                    last_systolic - first_systolic < 0,
#                  systolic_cond := "decreased"]
# modify$output$bp[last_systolic < 90 &
#                  first_systolic >= 140 &
#                  last_sixty_plus == "less than 60",
#                  systolic_cond := "improved"]
# modify$output$bp[last_systolic < 90 &
#                    first_systolic >= 180,
#                  systolic_cond := "improved"]
#
# modify$output$bp[]
#
# modify$output$bp[is.na(systolic_cond)]

# t(mmerge(modify$pre$w1, modify$pre$w2, modify$pre$w3, modify$pre$w4,
#          by = "hh_team"))
# t(mmerge(modify$pre$bmi1, modify$pre$bmi2, modify$pre$bmi3, modify$pre$bmi4,
#        by = "hh_team"))

### create information about the file to share with end-users ###
about_file <- data.table(data_source = "E2 report 2281: E2_HH_vs_non_HH_Vital",
                         date_range = "7/1/14 to date_ran",
                         date_ran = input$run_date,
                         last_updated = input$run_date,
                         project_location = project_wd$project)

