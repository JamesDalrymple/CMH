# BASE HOSPITAL - PI SUMMARY
modify <- new.env(parent = .GlobalEnv)

# hospital served -------------------------------------------------------------
hosp <- copy(sql$output$hosp_query)
adm <- copy(sql$output$adm_query)
served <- copy(sql$output$served)
# sql$output$served <- copy(served)

# set date columns to date class
date_cols <- c("auth_eff", "auth_exp", "hosp_disc")
for (j in date_cols)
  set(hosp,
      j = j,
      value = date_convert(hosp[[j]]))
date_cols <- c("team_eff", "team_exp")
for (j in date_cols)
  set(adm,
      j = j,
      value = date_convert(adm[[j]]))
date_cols <- c("span_start", "span_end ")
rm(j, date_cols)
served[, span_end := as.Date(span_end)]
served[, span_start := as.Date(span_start)]
adm[, team := cmh_recode(team)]
adm[, cmh_status := aux$is_cmh(team)]


# # data checking
# hosp[, fy := my_fy(auth_eff)]
# hosp[, list(num_cases = length(unique(case_no)),
#             num_hosp_adms = length(case_no)),
#      keyby = list(team_at_admit, fy)]



# admissions by team ----------------------------------------------------------
# data.table doesnt make full cartesian products too easy... but here is 1 way
adm[, j_key := 1]
sql$hosp_dates[, j_key := 1]
modify$adm_full <- adm[sql$hosp_dates, on = "j_key", allow.cartesian = TRUE]
adm[, j_key := NULL]
modify$adm_full[, j_key := NULL]
# making computations easier :)
modify$adm_full[is.na(team_exp), team_exp := pmin(team_exp, span_end, na.rm = TRUE)]
# filter out dates that shouldnt be there
modify$adm_full <-
  modify$adm_full[team_eff <= span_end & team_exp >= span_start]
# admission: team summary by fiscal year and fiscal quarter
modify$team_adm_summary <-
  modify$adm_full[, list(adm_caseload = length(unique(case_no))),
                  by = list(team, span_label, span_type)]
# admission: cmh summary by fiscal year and fiscal quarter---------------------
modify$cmh_adm_summary <-
  modify$adm_full[, list(adm_caseload = length(unique(case_no))),
                  by = list(cmh_status, span_label, span_type)]
# hospitalizations by team ----------------------------------------------------
# fix bad discharge days (around 20)
hosp[is.na(hosp_disc), hosp_disc := auth_exp+1]
hosp[, team_at_admit := cmh_recode(team_at_admit)]
hosp[, cmh_status_at_admit := aux$is_cmh(team_at_admit)]
# cartesian join to make all summaries at once
hosp[, j_key := 1]
modify$hosp_full <- hosp[sql$hosp_dates, on = "j_key", allow.cartesian = TRUE]
modify$hosp_full[, j_key := NULL]
sql$hosp_dates[, j_key := NULL]
# filter out dates that shouldnt be there
modify$hosp_full <-
  modify$hosp_full[between(auth_eff, span_start, span_end)]
# hospital: team summary by quarter and fiscal year
team_hosp_summary <-
  modify$hosp_full[, list(people_hospitalized = length(unique(case_no)),
                 total_auth_days = sum(auth_days, na.rm = TRUE),
                 num_hosp_adm = length(case_no)),
          by = list(team_at_admit, span_type, span_label)]
team_hosp_summary[, multiple_adms := num_hosp_adm-people_hospitalized]

# hospitalizations by cmh vs non-cmh ------------------------------------------
cmh_hosp_summary <-
  modify$hosp_full[, list(people_hospitalized = length(unique(case_no)),
                   total_auth_days = sum(auth_days, na.rm = TRUE),
                   num_hosp_adm = length(case_no)),
            by = list(cmh_status_at_admit, span_type, span_label)]
cmh_hosp_summary[, multiple_adms := num_hosp_adm-people_hospitalized]

# consumers served by team ----------------------------------------------------
served[, team := cmh_recode(team)]
# add missing columns from hosp_dates
served[sql$hosp_dates,
       c("span_type", "span_label") :=
         list(span_type, span_label),
       on = c(span_start = "span_start", span_end = "span_end")]
modify$svc_team_summary <-
  served[, list(con_served = length(unique(case_no))),
       by = list(team, span_type, span_label)]

served[, cmh_status := aux$is_cmh(team)]

# consumers served by cmh -----------------------------------------------------
modify$svc_cmh_summary <-
  served[, list(con_served = length(unique(case_no))),
         by = list(cmh_status, span_label, span_type)]

# combine results -------------------------------------------------------------
# cmh vs non-cmh results ---
cmh_hosp_summary[modify$cmh_adm_summary,
                 adm_caseload := i.adm_caseload,
                 on = c(cmh_status_at_admit = "cmh_status",
                        span_label = "span_label",
                        span_type = "span_type")]
cmh_hosp_summary[modify$svc_cmh_summary,
                 con_served := i.con_served,
                 on = c(cmh_status_at_admit = "cmh_status",
                        span_label = "span_label",
                        span_type = "span_type")]
cmh_hosp_summary[, pct_served_hosp :=
                   round(people_hospitalized/con_served*100, 1)]
# team results ---
team_hosp_summary[modify$team_adm_summary,
                 adm_caseload := i.adm_caseload,
                 on = c(team_at_admit = "team",
                        span_label = "span_label",
                        span_type = "span_type")]
team_hosp_summary[modify$svc_team_summary,
                  con_served := i.con_served,
                  on = c(team_at_admit = "team",
                         span_label = "span_label",
                         span_type = "span_type")]
team_hosp_summary[, pct_served_hosp :=
                   round(people_hospitalized/con_served*100, 1)]
# ACCESS details --------------------------------------------------------------
# long-term Access (more than 30 days)

modify$adm2 <- copy(sql$output$adm_query)
modify$adm2[, team_fixed := cmh_recode(team)]
modify$access_adm <- modify$adm2[team_fixed == "Access"]
setorder(modify$access_adm, case_no, team_eff, team_exp, team)
modify$access_adm[is.na(team_exp), team_exp := input$end_date]
modify$access_adm[, days_access :=
  as.num(date_convert(team_exp)-date_convert(team_eff))]
modify$access_adm[team_exp == input$end_date, team_exp := NA]
modify$access_adm[, time_status :=
                    cut(days_access,
                      breaks = c(0, 30, 60, 90, Inf),
                      labels = c("0-29", "30-59", "60-89", "90+"),
                      include.lowest = TRUE,
                      right = FALSE)]

modify$long_access_summary <-
  modify$access_adm[, list(num_cases = length(unique(case_no))),
                  by = list(team, time_status)]

modify$wide_access_summary <-
  dcast(modify$long_access_summary,
      team ~ time_status, value.var = "num_cases")
setnames(modify$wide_access_summary, "team", "days assigned to Access team")