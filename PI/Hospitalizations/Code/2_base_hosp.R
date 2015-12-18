# BASE HOSPITAL - PI SUMMARY
modify <- new.env(parent = .GlobalEnv)

# hospital served -------------------------------------------------------------
hosp <- copy(sql$output$hosp)
adm <- copy(sql$output$adm)
served <- copy(sql$output$served)

saveRDS(hosp, file.path(project_wd$data, "hosp.RDS"))
saveRDS(adm, file.path(project_wd$data, "adm.RDS"))
saveRDS(served, file.path(project_wd$data, "served.RDS"))

# served[, service_date := as.Date(service_date)]
# served[, fy := my_fy(service_date)]
# served[, length(unique(case_no)), by = "fy"]

# set date columns to date class
date_cols <- c("auth_eff", "auth_exp", "hosp_disc")
for (j in date_cols)
  set(hosp, j = j, value = date_convert(hosp[[j]]))
date_cols <- c("team_eff", "team_exp")
for (j in date_cols)
  set(adm, j = j, value = date_convert(adm[[j]]))
rm(j, date_cols)
served[, service_date := as.Date(service_date)]
adm[, team := cmh_recode(team)]
adm[, cmh_status := aux$is_cmh(team)]

# admissions by team and cmh_status--------------------------------------------
# foverlaps is the BEST possible way to do this.. this is what is was made for.
adm[is.na(team_exp), team_exp := date_convert(input$end_date)+1]
setkey(sql$span_dates, span_start, span_end)
setkey(adm, team_eff, team_exp)
modify$adm$span <-
  foverlaps(x = sql$span_dates,
          y = adm,
          by.x = c("span_start", "span_end"),
          by.y = c("team_eff", "team_exp"),
          nomatch = 0)
setkey(modify$adm$span, NULL)
# the basis for all summaries
modify$adm$case_detail <-
  modify$adm$span[, unique(.SD),
  .SDcols = c("case_no", "team", "cmh_status", "span_label", "span_type")]
# admission: team summary by fiscal year and fiscal quarter
modify$team_adm_summary <-
  modify$adm$case_detail[, list(adm_caseload = length(unique(case_no))),
                         by = list(team, span_label, span_type)]
# admission: cmh summary by fiscal year and fiscal quarter---------------------
modify$cmh_adm_summary <-
  modify$adm$case_detail[, list(adm_caseload = length(unique(case_no))),
                         by = list(cmh_status, span_label, span_type)]
# hospitalizations by team ----------------------------------------------------
# fix bad discharge days (around 20)
hosp[is.na(hosp_disc), hosp_disc := auth_exp+1]

# hosp[, idx := .bincode(date, c(modify$adm$full[, team_eff], Inf), right = FALSE)]
# hosp[, idx := .bincode(auth_eff,
#   c(modify$adm$full[, span_start],
#     modify$adm$full[, span_end]), right = FALSE)]
# data2[, idx := .I]
# merge(data1, data2, by = 'idx', all.x = TRUE)[, idx := NULL]

tmp_adm <- modify$adm$span
hosp <-
  sqldf("select distinct
    adm.team, hosp.case_no, hosp.auth_eff, hosp.auth_exp, hosp.hosp_disc,
    hosp.auth_days
    from hosp
    left join tmp_adm as adm on hosp.case_no = adm.case_no and
      hosp.auth_eff between adm.team_eff and adm.team_exp")
rm(tmp_adm)
hosp <- data.table(hosp)
hosp[, team := cmh_recode(team)]
hosp[, cmh_status := aux$is_cmh(team)]
hosp[, hosp_id := .GRP, by = list(case_no, auth_eff)]
hosp[cmh_priority_dt, priority := i.priority, on = "team"]
hosp[, min_priority := min(priority), by = hosp_id]
hosp <- hosp[min_priority == priority]
hosp[, c("min_priority", "priority") := NULL]
hosp[, c("auth_exp", "auth_days", "hosp_disc") :=
       list(max(auth_exp), max(auth_days), max(hosp_disc)), by = hosp_id]
hosp <- unique(hosp)

# cartesian join to make all summaries at once
hosp[, j_key := 1]
sql$span_dates[, j_key := 1]
modify$hosp_full <-
  hosp[sql$span_dates, on = "j_key", allow.cartesian = TRUE]
modify$hosp_full[, j_key := NULL]
sql$span_dates[, j_key := NULL]
# filter out dates that shouldnt be there
modify$hosp_full <-
  modify$hosp_full[between(auth_eff, span_start, span_end)]
# served ----------------------------------------------------------------------
# add cmh_team at service date based on priority

modify$svc$qtr <- served[, list(span_type = "qtr",
                                span_label = my_qtr(service_date),
                                served = 1), by = case_no]
modify$svc$qtr <- unique(modify$svc$qtr)

modify$svc$fy <- served[, list(span_type = "fy",
                               span_label = my_fy(service_date),
                               served = 1), by = case_no]
modify$svc$fy <- unique(modify$svc$fy)
# combine fy and qtr service data
modify$svc$all <-
  rbindlist(list(modify$svc$qtr, modify$svc$fy), use.names = TRUE)
# full outer join admissions and services
modify$adm_svc <-
  merge(modify$adm$case_detail, modify$svc$all, all = TRUE,
      by = c("case_no", "span_type", "span_label"))

modify$adm_svc[is.na(cmh_status), cmh_status := "non-CMH"]
modify$adm_svc[is.na(team), team := "non-CMH"]
modify$adm_svc[is.na(served), served := 0]

# ACCESS details --------------------------------------------------------------
# long-term Access (more than 30 days)
modify$adm2 <- copy(sql$output$adm)
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