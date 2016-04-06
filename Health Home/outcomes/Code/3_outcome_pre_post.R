pp <- new.env(parent = .GlobalEnv) # pre-post environment

# Eligibility Summary ---------------------------------------------------------
# health home vs non-health home ---
saved$eligible$hh <- modify$eligibility[, .(adm_records = length(case_no),
  consumers = length(unique(case_no))), by = .(e_status, hh_status)]
# non-health home vs health home L1/L2 vs health home L3 ---
saved$eligible$hh_lev <- modify$eligibility[, .(adm_records = length(case_no),
  consumers = length(unique(case_no))), by = .(e_status, hh_lev_status)]

# health home vs non-health home, by team ---
saved$eligible$hh_cmh <- modify$eligibility[, .(adm_records = length(case_no),
  consumers = length(unique(case_no))), by = .(e_status, cmh_team, hh_status)]
# non-health home vs health home L1/L2 vs health home L3, by team ---
saved$eligible$hh_lev_cmh <-
  modify$eligibility[, .(adm_records = length(case_no),
                         consumers = length(unique(case_no))),
                     by = .(e_status, hh_lev_status, cmh_team)]

# PRE/POST BMI ----------------------------------------------------------------
# health home vs non-health home ---
pp$bmi$hh <- modify$bmi[, .(pre_dt  = min(vt_date),
               post_dt = max(vt_date),
               n_recs = .N),
           by = .(case_no, adm_pk, hh_pk)]
pp$bmi$hh[post_dt - pre_dt < input$record_dist_req,
          error := paste("rec. dist. <", input$record_dist_req)]
pp$bmi$hh[modify$bmi, pre_bmi := calc_bmi,
          on = c(case_no = "case_no", pre_dt = "vt_date")]
pp$bmi$hh[modify$bmi, post_bmi := calc_bmi,
          on = c(case_no = "case_no", post_dt = "vt_date")]
saved$pp$bmi$hh  <- copy(pp$bmi$hh)
pp$bmi$hh <- pp$bmi$hh[is.na(error)]
pp$bmi$hh[, status := aux$bmi_cat(pre_bmi, post_bmi)]
pp$bmi$hh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$bmi$hh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3 ---
pp$bmi$hh_lev <- modify$bmi[, .(pre_dt  = min(vt_date),
                               post_dt = max(vt_date),
                               n_recs = .N),
           by = .(case_no, adm_pk, hh_pk, L3_pk)]
pp$bmi$hh_lev[post_dt - pre_dt < input$record_dist_req,
              error := paste("rec. dist. <", input$record_dist_req)]
pp$bmi$hh_lev[modify$bmi, pre_bmi := calc_bmi,
          on = c(case_no = "case_no", pre_dt = "vt_date")]
pp$bmi$hh_lev[modify$bmi, post_bmi := calc_bmi,
          on = c(case_no = "case_no", post_dt = "vt_date")]
saved$pp$bmi$hh_lev  <- copy(pp$bmi$hh_lev)
pp$bmi$hh_lev <- pp$bmi$hh_lev[is.na(error)]
pp$bmi$hh_lev[, error := NULL]
pp$bmi$hh_lev[, status := aux$bmi_cat(pre_bmi, post_bmi)]
pp$bmi$hh_lev[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$bmi$hh_lev[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$bmi$hh_lev[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$bmi$hh_lev[, Cs(adm_pk, hh_pk, L3_pk, pre_dt, post_dt) := NULL]
# health home vs non-health home, by team ---
pp$bmi$hh_cmh <- modify$bmi[, .(pre_dt  = min(vt_date),
                                post_dt = max(vt_date),
                                n_recs = .N),
           by = .(case_no, adm_pk, hh_pk, cmh_team)]
pp$bmi$hh_cmh[post_dt - pre_dt < input$record_dist_req,
              error := paste("rec. dist. <", input$record_dist_req)]
pp$bmi$hh_cmh[modify$bmi, pre_bmi := calc_bmi,
              on = c(case_no = "case_no", pre_dt = "vt_date")]
pp$bmi$hh_cmh[modify$bmi, post_bmi := calc_bmi,
              on = c(case_no = "case_no", post_dt = "vt_date")]
saved$pp$bmi$hh_cmh  <- copy(pp$bmi$hh_cmh)
pp$bmi$hh_cmh <- pp$bmi$hh_cmh[is.na(error)]
pp$bmi$hh_cmh[, error := NULL]
pp$bmi$hh_cmh[, status := aux$bmi_cat(pre_bmi, post_bmi)]
pp$bmi$hh_cmh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$bmi$hh_cmh[, Cs(adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3, by team ---
pp$bmi$hh_lev_cmh <- modify$bmi[, .(pre_dt  = min(vt_date),
                                    post_dt = max(vt_date),
                                    n_recs = .N),
           by = .(case_no, adm_pk, hh_pk, L3_pk, cmh_team)]
pp$bmi$hh_lev_cmh[post_dt - pre_dt < input$record_dist_req,
                  error := paste("rec. dist. <", input$record_dist_req)]
pp$bmi$hh_lev_cmh[modify$bmi, pre_bmi := calc_bmi,
              on = c(case_no = "case_no", pre_dt = "vt_date")]
pp$bmi$hh_lev_cmh[modify$bmi, post_bmi := calc_bmi,
              on = c(case_no = "case_no", post_dt = "vt_date")]
saved$pp$bmi$hh_lev_cmh  <- copy(pp$bmi$hh_lev_cmh)
pp$bmi$hh_lev_cmh <- pp$bmi$hh_lev_cmh[is.na(error)]
pp$bmi$hh_lev_cmh[, error := NULL]
pp$bmi$hh_lev_cmh[, status := aux$bmi_cat(pre_bmi, post_bmi)]
pp$bmi$hh_lev_cmh[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$bmi$hh_lev_cmh[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$bmi$hh_lev_cmh[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$bmi$hh_lev_cmh[, Cs(adm_pk, hh_pk, L3_pk, pre_dt, post_dt) := NULL]

# PRE/POST Wellness note overall health ---------------------------------------
# health home vs non-health home ---
pp$wn$oh$hh <- modify$wn_oh[, .(pre_dt  = min(wn_date),
                            post_dt = max(wn_date),
                            n_recs = .N),
                        by = .(case_no, adm_pk, hh_pk)]
pp$wn$oh$hh[post_dt - pre_dt < input$record_dist_req,
         error := paste("rec. dist. <", input$record_dist_req)]
pp$wn$oh$hh[modify$wn_oh, pre_oh := ovr_health,
          on = c(case_no = "case_no", pre_dt = "wn_date")]
pp$wn$oh$hh[modify$wn_oh, post_oh := ovr_health,
          on = c(case_no = "case_no", post_dt = "wn_date")]
saved$pp$wn$oh$hh  <- copy(pp$wn$oh$hh)
pp$wn$oh$hh <- pp$wn$oh$hh[is.na(error)]
pp$wn$oh$hh[, status := aux$wn_oh_cat(pre_oh, post_oh)]
pp$wn$oh$hh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$wn$oh$hh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]

# 4/6/2016 4:34

# non-health home vs health home L1/L2 vs health home L3 ---
# health home vs non-health home, by team ---
# non-health home vs health home L1/L2 vs health home L3, by team ---

# PRE/POST WN: OVR HEALTH -----------------------------------------------------



# PRE/POST blood pressure
# health home vs non-health home ---
# non-health home vs health home L1/L2 vs health home L3 ---
# health home vs non-health home, by team ---
# non-health home vs health home L1/L2 vs health home L3, by team ---
modify$bp_pp <-
  bp[, list(min_vt = min(vt_date, na.rm = TRUE),
            max_vt = max(vt_date, na.rm = TRUE)),
     by = case_no]
# move out cases with min/max too close
modify$bp_cases <-
  modify$bp_pp[max_vt - min_vt < input$record_dist_req, unique(case_no)]
saved$bp[case_no %in% modify$bp_cases, error :=
           aux$cat_error(error, paste("min/max diff <", input$record_dist_req))]
modify$bp_pp <- modify$bp_pp[case_no %nin% modify$bp_cases]
modify$bp_pp[bp, Cs(min_sys, min_dia) := list(i.systolic, i.diastolic),
             on = c(case_no = "case_no", min_vt = "vt_date")]
modify$bp_pp[bp, Cs(max_sys, max_dia) := list(i.systolic, i.diastolic),
             on = c(case_no = "case_no", max_vt = "vt_date")]

modify$bp_pp

# improvement ---
modify$output$bp[, c("jama_sys_status", "jama_dia_status") :=
                   list(aux$jama_eval(sys_jama1, sys_jama2),
                        aux$jama_eval(dia_jama1, dia_jama2))]




# PRE/POST Lab Values: Cholesterol --------------------------------------------
# health home vs non-health home ---
pp$labs$chol<- modify$labs$chol[, .(pre_dt = min(lab_date),
                                    post_dt = max(lab_date)),
                                by = .(case_no, lab_name, adm_pk, hh_pk)]
pp$labs$chol[modify$labs$chol, pre_value := lab_value,
             on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$chol[modify$labs$chol, post_value := lab_value,
             on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$chol[post_dt-pre_dt < input$record_dist_req,
             error := paste("rec. dist. <", input$record_dist_req)]
pp$labs$chol <- modify$labs$chol[is.na(error)][, error := NULL]


# non-health home vs health home L1/L2 vs health home L3 ---
# health home vs non-health home, by team ---
# non-health home vs health home L1/L2 vs health home L3, by team ---

# PRE/POST ER visits ----------------------------------------------------------
# health home vs non-health home ---
# non-health home vs health home L1/L2 vs health home L3 ---
# health home vs non-health home, by team ---
# non-health home vs health home L1/L2 vs health home L3, by team ---
