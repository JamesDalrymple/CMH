analyze <- new.env(parent = .GlobalEnv)

# admission and service aggregation -------------------------------------------
analyze$adm_svc$team <-
  modify$adm_svc[, list(adm_caseload = length(unique(case_no)),
                  adm_served = sum(served)),
                by = list(span_type, span_label, team)]
analyze$adm_svc$cmh <-
modify$adm_svc[, list(adm_caseload = length(unique(case_no)),
                      adm_served = sum(served)),
               by = list(span_type, span_label, cmh_status)]
# hospital: team summary by quarter and fiscal year ---------------------------
team_hosp_summary <-
  modify$hosp_full[, list(people_hospitalized = length(unique(case_no)),
                          total_auth_days = sum(auth_days, na.rm = TRUE),
                          num_hosp_adm = length(case_no)),
                   by = list(team, span_type, span_label)]
team_hosp_summary[, multiple_adms := num_hosp_adm-people_hospitalized]

team_hosp_summary <- merge(team_hosp_summary,
      modify$adm_svc[, list(caseload_adm = length(unique(case_no)),
                            caseload_served = sum(served)),
                     by = list(team, span_type, span_label)],
      by = c("span_type", "span_label", "team"),
      all = TRUE)
team_hosp_summary[, `:=`
  (`% hosp/served` = round(people_hospitalized/caseload_served*100, 1),
   `% served/admitted` = round(caseload_served/caseload_adm*100, 1))]
team_hosp_summary[is.na(team_hosp_summary)] <- 0
# hospitalizations by cmh vs non-cmh ------------------------------------------
cmh_hosp_summary <-
  modify$hosp_full[, list(people_hospitalized = length(unique(case_no)),
                          total_auth_days = sum(auth_days, na.rm = TRUE),
                          num_hosp_adm = length(case_no)),
                   by = list(cmh_status, span_type, span_label)]
cmh_hosp_summary[, multiple_adms := num_hosp_adm-people_hospitalized]

cmh_hosp_summary <- merge(cmh_hosp_summary,
  modify$adm_svc[, list(caseload_adm = length(unique(case_no)),
                        caseload_served = sum(served)),
                     by = list(cmh_status, span_type, span_label)],
                     by = c("span_type", "span_label", "cmh_status"),
                     all = TRUE)
cmh_hosp_summary[, `:=`
  (`% hosp/served` = round(people_hospitalized/caseload_served*100, 1),
   `% served/admitted` = round(caseload_served/caseload_adm*100, 1))]
cmh_hosp_summary[is.na(cmh_hosp_summary)] <- 0