# start_date will always be one year prior to end date
# start_date <- dateConvert(end_date) - 365

modify <- new.env(parent = .GlobalEnv)

output <- copy(sql$output)
wellness <- copy(sql$wellness)
setnames(output, names(output), tolower(names(output)))
setnames(wellness, names(wellness), tolower(names(wellness)))

# BMI -------------------------------------------------------------------------
output[, height_feet := as.num(height_feet)]
output[, height_inches := as.num(height_inches)]
output[, height_feet := mean(height_feet, na.rm = TRUE), by = case_no]
output[, height_inches := mean(height_inches, na.rm = TRUE), by = case_no]

# initial_status <- output[!is.na(bmi),
#        list(cases_initially_with_bmh = length(unique(case_no)),
#             inital_bmi_total_records = length(case_no))]

output[is.na(hh_team), hh_team := "N"]
# calculate BMI manually
output[is.na(bmi), bmi := round((weight*703)/(
  psum(height_feet*12, height_inches, na.rm = TRUE))^2, 2)]
# second_status <-
#   output[!is.na(bmi),
#          list(
#            cases_initially_with_bmh = length(unique(case_no)),
#            inital_bmi_total_records = length(case_no)
#          )]
# total_status <-
#   output[, list(total_consumers = length(unique(case_no)),
#                 total_records = length(case_no))]

# people missing weight - 9 right now - removing
bad_weight <- output[weight < 70, .SD,
  .SDc = c("case_no", "team", "hh_team", "bmi")]
output <- output[weight < 70, weight := NA]
output <- output[weight < 70, bmi := NA]

# first and last available BMI
output[!is.na(bmi), min_dt_date := min(vt_date, na.rm = TRUE), by = case_no]
output[!is.na(bmi), max_dt_date := max(vt_date, na.rm = TRUE), by = case_no]
# output[max_dt_date == min_dt_date, note := "only 1 BMI provided"]
# output[max_dt_date != min_dt_date, note := "bmi ok to use"]
# output[max_dt_date != min_dt_date, length(unique(case_no)), by = hh_team]
# output[, length(unique(case_no)), by = hh_team]

# most possible improvement if we go back in time for height: 598
output[is.na(bmi) & !is.na(weight)]
# create bmi output ---
bmi_output <-
  copy(output[!is.na(bmi), .SD,
         .SDc = c("case_no", "hh_team", "bmi", "vt_date")])

bmi_output <- unique(bmi_output)
bmi_output[, num_bmi := length(unique(vt_date)) , by = case_no]
bmi_output <-
  unique(bmi_output[, .SD, .SDc = c("case_no", "hh_team", "num_bmi")])

# create weight output ---
weight_output <-
  copy(output[!is.na(weight), .SD,
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
bmi2 <- bmi_output[num_bmi >= 1,
           list(at_least_1_BMI = length(unique(case_no))), by = hh_team]
# BMI at least 2
bmi3 <- bmi_output[num_bmi >= 2,
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
wellness[, num_wn :=
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

