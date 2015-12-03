# BASE HOSPITAL - PI SUMMARY
modify <- new.env(parent = .GlobalEnv)

# hospital served -------------------------------------------------------------
hosp_served <-
  hosp_served[, list(
    num_served_consumers = sum(num_served_consumers),
    num_hosp_consumers = sum(num_hosp_consumers),
    num_hosp_adms = sum(num_hosp_adms),
    num_hosp_adms_w_disc = sum(num_hosp_adms_w_disc),
    total_hosp_days = sum(total_hosp_days),
    max_los = max(max_los),
    num_readmit_30_days = sum(num_readmit_30_days),
    consumers_readmit_30_days = sum(consumers_readmit_30_days),
    num_readmit_90_days = sum(num_readmit_90_days),
    consumers_readmit_90_days = sum(consumers_readmit_90_days)
  ),
  by = list(team, span_label, span_type)]
hosp_served[, avg_los := total_hosp_days / num_hosp_consumers]
hosp_served[, pct_hosp_consumers := num_hosp_consumers / num_served_consumers]

hosp_served[, pct_hosp_consumers := round(pct_hosp_consumers * 100, 1)]
hosp_served[, avg_los := round(avg_los, 1)]
hosp_fy <- hosp_served[span_type == "fy"]
hosp_qtr <- hosp_served[span_type == "qtr"]

# admission: consumer counts on team if there at least 1 day; overlap allowed
admit[, team := cmh_recode(team)]
admit[, team := cmh_teams_f(team)]
admit <- admit[!is.na(team)]

admit_cl_summary <- NULL
for (i in seq_len(nrow(sql$hosp_dates))) {
admit_tmp <- copy(admit)
admit_tmp <- cbind(admit_tmp, sql$hosp_dates[i,])
admit_tmp <- admit_tmp[(team_expdt >= span_start | is.na(team_expdt)) &
          team_effdt <= span_end]
admit_tmp[is.na(team_expdt), team_expdt := span_end]

admit_tmp_cl <-
  admit_tmp[, list(admission_case_load = length(unique(case_no))),
          by = list(team, span_label, span_type)]
admit_cl_summary <- rbindlist(list(admit_tmp_cl, admit_cl_summary))
}
rm(list = c("i", ls(pattern = "tmp")))

full_summary <- hosp_served[admit_cl_summary,
            admission_case_load := i.admission_case_load,
            on = c("span_label", "span_type", "team")]

# cmh vs non-cmh admission summary
admit[, iscmh := is_cmh(team)]

admit_iscmh_cl <- NULL
for (i in seq_len(nrow(sql$hosp_dates))) {
  admit_iscmh_tmp <- copy(admit)
  admit_iscmh_tmp <- cbind(admit_iscmh_tmp, sql$hosp_dates[i, ])
  admit_iscmh_tmp <-
    admit_iscmh_tmp[(team_expdt >= span_start | is.na(team_expdt)) &
                      team_effdt <= span_end]
  admit_iscmh_tmp[is.na(team_expdt), team_expdt := span_end]
  admit_iscmh_tmp_cl <-
    admit_iscmh_tmp[, list(admission_case_load = length(unique(case_no))),
                    by = list(iscmh, span_label, span_type)]
  admit_iscmh_cl <-
    rbindlist(list(admit_iscmh_tmp_cl, admit_iscmh_cl))
}

