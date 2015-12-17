modify <- new.env(parent = .GlobalEnv)

# make id's for safe aggregating
sql$output$hosp[, adm_id := .GRP, by = list(case_no, auth_eff)]
sql$output$prescreen2[, ps_id := .GRP, by = list(case_no, doc_date)]

hosp <- copy(sql$output$hosp)
prescreen2 <- copy(sql$output$prescreen2)
hosp[, auth_eff := date_convert(auth_eff)]
prescreen2[, doc_date := date_convert(doc_date)]

hosp_summary <-
  hosp[, list(unique_hosp_adms = length(unique(adm_id)),
       unique_con = length(unique(case_no)))]
prescreen_summary <-
  prescreen2[, list(unique_prescreen_adm = length(unique(ps_id)),
            unique_con = length(unique(case_no)))]

setkey(hosp, case_no, auth_eff)
hosp[prescreen2[, list(case_no, doc_date)], match := "prescreen"]
# setkey(hosp, case_no, auth_eff)
hosp[prescreen2[, list(case_no, doc_date-1)], match := "prescreen"]
hosp[prescreen2[, list(case_no, doc_date+1)], match := "prescreen"]

hosp[match == "prescreen",
  list(unique_prescreen_adm = length(unique(adm_id)),
       unique_con = length(unique(case_no)))]

# write.csv(hosp, "hosp_matching_prescreens_fy_2015.csv", row.names = FALSE)

hosp[is.na(match)]
prescreen[case_no == 11613]

  prescreen[, list(case_no, doc_date+1)]
