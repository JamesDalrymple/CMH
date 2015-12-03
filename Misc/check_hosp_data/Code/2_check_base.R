# start_date will always be one year prior to end date
# start_date <- date_convert(end_date) - 365

modify <- new.env(parent = .GlobalEnv)
saved <- new.env(parent = .GlobalEnv)

hosp1 <- copy(dat$hosp1)
hosp2 <- copy(dat$hosp2)

setnames(hosp1, names(hosp1), tolower(names(hosp1)))
setnames(hosp2, names(hosp2), tolower(names(hosp2)))
setnames(hosp1, names(hosp1),
         gsub(x = names(hosp1), pattern = "[0-9]", replace = "") )
setnames(hosp2, names(hosp2),
         gsub(x = names(hosp2), pattern = "[0-9]", replace = "") )
hosp1[, auth_eff := date_convert(auth_eff)]
hosp2[, auth_eff := date_convert(auth_eff)]

# create team_short
hosp1[, team_short := cmh_recode(team_at_admit)]
hosp2[, team_short := cmh_recode(team_at_admit)]

# create fy
hosp1[, fy := my_fy(auth_eff)]
hosp2[, fy := my_fy(auth_eff)]
# create auth_id
hosp1[, auth_id := .GRP, by = list(case_no, auth_eff)]
hosp2[, auth_id := .GRP, by = list(case_no, auth_eff)]


hosp1_short <-
  hosp1[, unique(.SD), .SDcols = c("case_no", "auth_eff", "team_at_admit")]
hosp2_short <-
  hosp2[, unique(.SD), .SDcols = c("case_no", "auth_eff", "team_at_admit")]
setnames(hosp1_short, "team_at_admit", "Aug_team_at_admit")

hosp1_short[hosp2_short,
            team_at_admit := team_at_admit,
            on = c("case_no", "auth_eff")]
hosp_differences <-
  hosp1_short[team_at_admit != Aug_team_at_admit]
hosp_differences[, fy := my_fy(auth_eff)]

# hosp1[fy=="2011", length(unique(case_no)), by = team_short]
# hosp2[fy=="2011", length(unique(case_no)), by = team_short]
# diff_cases <- setdiff(
# hosp1[fy=="2011" & team_short=="MI", unique(case_no)],
# hosp2[fy=="2011" & team_short=="MI", unique(case_no)]
# )
#
# hosp1[case_no %in% diff_cases]
# hosp2[case_no %in% diff_cases]

### create information about the file to share with end-users ###
about_file <- data.table(hosp_file1 = dat$hosp_files[1],
                         hosp_file2 = dat$hosp_files[2],
                         date_range = "varies per report",
                         date_ran = input$run_date,
                         last_updated = input$run_date,
                         project_location = project_wd$project)

