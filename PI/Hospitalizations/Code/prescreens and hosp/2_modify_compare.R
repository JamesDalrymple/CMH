modify <- new.env(parent = .GlobalEnv)

# make id's for safe aggregating
sql$output$hosp[, adm_id := .GRP, by = list(case_no, auth_eff)]
sql$output$prescreen[, ps_id := .GRP, by = list(case_no, doc_date)]
# date class conversion
sql$output$hosp[, auth_eff := date_convert(auth_eff)]
sql$output$hosp[, auth_exp := date_convert(auth_exp)]
sql$output$hosp[, hosp_disc := date_convert(hosp_disc)]
sql$output$prescreen[, doc_date := date_convert(doc_date)]
# copy data so we dont need to rerun everything
hosp <- copy(sql$output$hosp)
prescreen <- copy(sql$output$prescreen)
full_prescreen <- copy(sql$output$prescreen3)
# summary data ----------------------------------------------------------------
hosp_summary <-
  hosp[, list(unique_hosp_adms = length(unique(adm_id)),
       unique_con = length(unique(case_no)))]
prescreen_summary <-
  prescreen[, list(unique_prescreen_adm = length(unique(ps_id)),
            unique_con = length(unique(case_no)))]

setkey(hosp, case_no, auth_eff)
hosp[prescreen[, list(case_no, doc_date)], match := "prescreen same day"]
hosp[prescreen[, list(case_no, doc_date-1)], match := "prescreen day after"]
hosp[prescreen[, list(case_no, doc_date-2)], match := "prescreen 2 days after"]
hosp[prescreen[, list(case_no, doc_date-3)], match := "prescreen 3 days after"]
hosp[prescreen[, list(case_no, doc_date-4)], match := "prescreen 4 days after"]
hosp[prescreen[, list(case_no, doc_date+1)], match := "prescreen day before"]
hosp[prescreen[, list(case_no, doc_date+2)], match := "prescreen 2 days before"]
hosp[prescreen[, list(case_no, doc_date+3)], match := "prescreen 2 days before"]
hosp[prescreen[, list(case_no, doc_date+4)], match := "prescreen 2 days before"]

# missing records: hospital stay with no prescreen within +/- 1 day
hosp[is.na(match), match := "no match found"]
modify$nomatch_hosp_summary <- hosp[,
  list(hosp_adms = length(unique(adm_id)),
       hosp_consumers = length(unique(case_no))), by = match]
# missing records: prescreen indicate hosp. stay w/o hosp. stay +/- 1 day
setkey(prescreen, case_no, doc_date)
prescreen[hosp[, list(case_no, auth_eff)], match := "hosp same day"]
prescreen[hosp[, list(case_no, auth_eff-1)], match := "hosp day after"]
prescreen[hosp[, list(case_no, auth_eff-2)], match := "hosp 2 days after"]
prescreen[hosp[, list(case_no, auth_eff-3)], match := "hosp 3 days after"]
prescreen[hosp[, list(case_no, auth_eff-4)], match := "hosp 4 days after"]
prescreen[hosp[, list(case_no, auth_eff+1)], match := "hosp day before"]
prescreen[hosp[, list(case_no, auth_eff+2)], match := "hosp 2 days before"]
prescreen[hosp[, list(case_no, auth_eff+3)], match := "hosp 3 days before"]
prescreen[hosp[, list(case_no, auth_eff+4)], match := "hosp 4 days before"]

prescreen[is.na(match), match := "no match found"]
modify$nomatch_prescreen_summary <- prescreen[,
          list(prescreen_adms = length(unique(ps_id)),
               prescreen_consumers = length(unique(case_no))),
          by = match]

full_prescreen[, doc_date := date_convert(doc_date)]
setkey(full_prescreen, case_no, doc_date)
full_prescreen[prescreen[match=="no match found",
                         list(case_no, doc_date)],
               no_match := 1]