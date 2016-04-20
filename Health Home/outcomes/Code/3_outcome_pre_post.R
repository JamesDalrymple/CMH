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

# PRE/POST Wellness note: overall health ---------------------------------------
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
# non-health home vs health home L1/L2 vs health home L3 ---
pp$wn$oh$hh_lev <- modify$wn_oh[, .(pre_dt = min(wn_date),
                                  post_dt = max(wn_date),
                                  n_recs = .N),
                              by = .(case_no, adm_pk, hh_pk, L3_pk)]
pp$wn$oh$hh_lev[post_dt - pre_dt < input$record_dist_req,
              error := paste("rec. dist. <", input$record_dist_req)]
pp$wn$oh$hh_lev[modify$wn_oh, pre_oh := ovr_health,
              on = c(case_no = "case_no", pre_dt = "wn_date")]
pp$wn$oh$hh_lev[modify$wn_oh, post_oh := ovr_health,
              on = c(case_no = "case_no", post_dt = "wn_date")]
saved$pp$wn$oh$hh_lev <- copy(pp$bmi$hh_lev)
pp$wn$oh$hh_lev <- pp$wn$oh$hh_lev[is.na(error)]
pp$wn$oh$hh_lev[, error := NULL]
pp$wn$oh$hh_lev[, status := aux$wn_oh_cat(pre_oh, post_oh)]
pp$wn$oh$hh_lev[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$wn$oh$hh_lev[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$wn$oh$hh_lev[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$wn$oh$hh_lev[, Cs(adm_pk, hh_pk, L3_pk, pre_dt, post_dt) := NULL]
# health home vs non-health home, by team ---
pp$wn$oh$hh_cmh <- modify$wn_oh[, .(pre_dt = min(wn_date),
                                    post_dt = max(wn_date),
                                    n_recs = .N),
                                by = .(case_no, adm_pk, hh_pk, cmh_team)]
pp$wn$oh$hh_cmh[post_dt - pre_dt < input$record_dist_req,
                error := paste("rec. dist. <", input$record_dist_req)]
pp$wn$oh$hh_cmh[modify$wn_oh, pre_oh := ovr_health,
                on = c(case_no = "case_no", pre_dt = "wn_date")]
pp$wn$oh$hh_cmh[modify$wn_oh, post_oh := ovr_health,
                on = c(case_no = "case_no", post_dt = "wn_date")]
saved$pp$wn$oh$hh_lev <- copy(pp$bmi$hh_lev)
pp$wn$oh$hh_cmh <- pp$wn$oh$hh_cmh[is.na(error)]
pp$wn$oh$hh_cmh[, error := NULL]
pp$wn$oh$hh_cmh[, status := aux$wn_oh_cat(pre_oh, post_oh)]
pp$wn$oh$hh_cmh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$wn$oh$hh_cmh[, Cs(adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3, by team ---
pp$wn$oh$hh_lev_cmh <- modify$wn_oh[, .(pre_dt = min(wn_date),
                                    post_dt = max(wn_date),
                                    n_recs = .N),
                                by = .(case_no, adm_pk, hh_pk, L3_pk, cmh_team)]
pp$wn$oh$hh_lev_cmh[post_dt - pre_dt < input$record_dist_req,
                error := paste("rec. dist. <", input$record_dist_req)]
pp$wn$oh$hh_lev_cmh[modify$wn_oh, pre_oh := ovr_health,
                on = c(case_no = "case_no", pre_dt = "wn_date")]
pp$wn$oh$hh_lev_cmh[modify$wn_oh, post_oh := ovr_health,
                on = c(case_no = "case_no", post_dt = "wn_date")]
saved$pp$wn$oh$hh_lev_cmh <- copy(pp$bmi$hh_lev_cmh)
pp$wn$oh$hh_lev_cmh <- pp$wn$oh$hh_lev_cmh[is.na(error)]
pp$wn$oh$hh_lev_cmh[, error := NULL]
pp$wn$oh$hh_lev_cmh[, status := aux$wn_oh_cat(pre_oh, post_oh)]
pp$wn$oh$hh_lev_cmh[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$wn$oh$hh_lev_cmh[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$wn$oh$hh_lev_cmh[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$wn$oh$hh_lev_cmh[, Cs(adm_pk, hh_pk, L3_pk, pre_dt, post_dt) := NULL]

# PRE/POST Wellness note: pain ------------------------------------------------
# health home vs non-health home ---
pp$wn$pain$hh <- modify$wn_pain[, .(pre_dt  = min(wn_date),
                                post_dt = max(wn_date),
                                n_recs = .N),
                            by = .(case_no, adm_pk, hh_pk)]
pp$wn$pain$hh[post_dt - pre_dt < input$record_dist_req,
            error := paste("rec. dist. <", input$record_dist_req)]
pp$wn$pain$hh[modify$wn_pain, pre_pain := pain,
            on = c(case_no = "case_no", pre_dt = "wn_date")]
pp$wn$pain$hh[modify$wn_pain, post_pain := pain,
            on = c(case_no = "case_no", post_dt = "wn_date")]
saved$pp$wn$pain$hh  <- copy(pp$wn$pain$hh)
pp$wn$pain$hh <- pp$wn$pain$hh[is.na(error)]
pp$wn$pain$hh[, status := aux$wn_pain_cat(pre_pain, post_pain)]
pp$wn$pain$hh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$wn$pain$hh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3 ---
pp$wn$pain$hh_lev <- modify$wn_pain[, .(pre_dt = min(wn_date),
                                    post_dt = max(wn_date),
                                    n_recs = .N),
                                by = .(case_no, adm_pk, hh_pk, L3_pk)]
pp$wn$pain$hh_lev[post_dt - pre_dt < input$record_dist_req,
                error := paste("rec. dist. <", input$record_dist_req)]
pp$wn$pain$hh_lev[modify$wn_pain, pre_pain := pain,
                on = c(case_no = "case_no", pre_dt = "wn_date")]
pp$wn$pain$hh_lev[modify$wn_pain, post_pain := pain,
                on = c(case_no = "case_no", post_dt = "wn_date")]
saved$pp$wn$pain$hh_lev <- copy(pp$bmi$hh_lev)
pp$wn$pain$hh_lev <- pp$wn$pain$hh_lev[is.na(error)]
pp$wn$pain$hh_lev[, error := NULL]
pp$wn$pain$hh_lev[, status := aux$wn_pain_cat(pre_pain, post_pain)]
pp$wn$pain$hh_lev[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$wn$pain$hh_lev[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$wn$pain$hh_lev[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$wn$pain$hh_lev[, Cs(adm_pk, hh_pk, L3_pk, pre_dt, post_dt) := NULL]
# health home vs non-health home, by team ---
pp$wn$pain$hh_cmh <- modify$wn_pain[, .(pre_dt = min(wn_date),
                                    post_dt = max(wn_date),
                                    n_recs = .N),
                                by = .(case_no, adm_pk, hh_pk, cmh_team)]
pp$wn$pain$hh_cmh[post_dt - pre_dt < input$record_dist_req,
                error := paste("rec. dist. <", input$record_dist_req)]
pp$wn$pain$hh_cmh[modify$wn_pain, pre_pain := pain,
                on = c(case_no = "case_no", pre_dt = "wn_date")]
pp$wn$pain$hh_cmh[modify$wn_pain, post_pain := pain,
                on = c(case_no = "case_no", post_dt = "wn_date")]
saved$pp$wn$pain$hh_lev <- copy(pp$bmi$hh_lev)
pp$wn$pain$hh_cmh <- pp$wn$pain$hh_cmh[is.na(error)]
pp$wn$pain$hh_cmh[, error := NULL]
pp$wn$pain$hh_cmh[, status := aux$wn_pain_cat(pre_pain, post_pain)]
pp$wn$pain$hh_cmh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$wn$pain$hh_cmh[, Cs(adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3, by team ---
pp$wn$pain$hh_lev_cmh <- modify$wn_pain[, .(pre_dt = min(wn_date),
                                        post_dt = max(wn_date),
                                        n_recs = .N),
                                    by = .(case_no, adm_pk, hh_pk, L3_pk, cmh_team)]
pp$wn$pain$hh_lev_cmh[post_dt - pre_dt < input$record_dist_req,
                    error := paste("rec. dist. <", input$record_dist_req)]
pp$wn$pain$hh_lev_cmh[modify$wn_pain, pre_pain := pain,
                    on = c(case_no = "case_no", pre_dt = "wn_date")]
pp$wn$pain$hh_lev_cmh[modify$wn_pain, post_pain := pain,
                    on = c(case_no = "case_no", post_dt = "wn_date")]
saved$pp$wn$pain$hh_lev_cmh <- copy(pp$bmi$hh_lev_cmh)
pp$wn$pain$hh_lev_cmh <- pp$wn$pain$hh_lev_cmh[is.na(error)]
pp$wn$pain$hh_lev_cmh[, error := NULL]
pp$wn$pain$hh_lev_cmh[, status := aux$wn_pain_cat(pre_pain, post_pain)]
pp$wn$pain$hh_lev_cmh[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$wn$pain$hh_lev_cmh[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$wn$pain$hh_lev_cmh[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$wn$pain$hh_lev_cmh[, Cs(adm_pk, hh_pk, L3_pk, pre_dt, post_dt) := NULL]

# PRE/POST blood pressure: diastolic ------------------------------------------
# health home vs non-health home ---
pp$bp$hh <- modify$bp[!is.na(diastolic), .(pre_dt  = min(vt_date),
                                    post_dt = max(vt_date),
                                    n_recs = .N),
                                by = .(case_no, adm_pk, hh_pk)]
pp$bp$hh[post_dt - pre_dt < input$record_dist_req,
         error := paste("rec. dist. <", input$record_dist_req)]
pp$bp$hh[modify$bp, `:=`(
  pre_dia = i.diastolic, pre_sys = i.systolic,
  pre_dia_jama = i.dia_jama, pre_sys_jama = i.sys_jama),
  on = c(case_no = "case_no", pre_dt = "vt_date")]
pp$bp$hh[modify$bp, `:=`(
  post_dia = diastolic, post_sys = systolic,
  post_dia_jama = dia_jama, post_sys_jama = sys_jama),
  on = c(case_no = "case_no", post_dt = "vt_date")]
saved$pp$bp$hh  <- copy(pp$bp$hh)
pp$bp$hh <- pp$bp$hh[is.na(error)]
pp$bp$hh[, `:=`(sys_status = aux$bp_cat(pre_sys_jama, post_sys_jama),
                dia_status = aux$bp_cat(pre_dia_jama, post_dia_jama))]
pp$bp$hh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$bp$hh[, grep("pre|post|pk|error",
                names(pp$bp$hh_lev), value = TRUE) := NULL]
# non-health home vs health home L1/L2 vs health home L3 ---
pp$bp$hh_lev <- modify$bp[!is.na(diastolic), .(pre_dt  = min(vt_date),
                                           post_dt = max(vt_date),
                                           n_recs = .N),
                      by = .(case_no, adm_pk, hh_pk, L3_pk)]
pp$bp$hh_lev[post_dt - pre_dt < input$record_dist_req,
         error := paste("rec. dist. <", input$record_dist_req)]
pp$bp$hh_lev[modify$bp, `:=`(
  pre_dia = i.diastolic, pre_sys = i.systolic,
  pre_dia_jama = i.dia_jama, pre_sys_jama = i.sys_jama),
  on = c(case_no = "case_no", pre_dt = "vt_date")]
pp$bp$hh_lev[modify$bp, `:=`(
  post_dia = diastolic, post_sys = systolic,
  post_dia_jama = dia_jama, post_sys_jama = sys_jama),
  on = c(case_no = "case_no", post_dt = "vt_date")]
saved$pp$bp$hh_lev  <- copy(pp$bp$hh_lev)
pp$bp$hh_lev <- pp$bp$hh_lev[is.na(error)]
pp$bp$hh_lev[, `:=`(sys_status = aux$bp_cat(pre_sys_jama, post_sys_jama),
                    dia_status = aux$bp_cat(pre_dia_jama, post_dia_jama))]
pp$bp$hh_lev[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$bp$hh_lev[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$bp$hh_lev[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$bp$hh_lev[, grep("pre|post|pk|error",
  names(pp$bp$hh_lev), value = TRUE) := NULL]
# health home vs non-health home, by team ---
pp$bp$hh_cmh <- modify$bp[!is.na(diastolic), .(pre_dt  = min(vt_date),
                                               post_dt = max(vt_date),
                                               n_recs = .N),
                          by = .(case_no, adm_pk, hh_pk, cmh_team)]
pp$bp$hh_cmh[post_dt - pre_dt < input$record_dist_req,
             error := paste("rec. dist. <", input$record_dist_req)]
pp$bp$hh_cmh[modify$bp, `:=`(
  pre_dia = i.diastolic, pre_sys = i.systolic,
  pre_dia_jama = i.dia_jama, pre_sys_jama = i.sys_jama),
  on = c(case_no = "case_no", pre_dt = "vt_date")]
pp$bp$hh_cmh[modify$bp, `:=`(
  post_dia = diastolic, post_sys = systolic,
  post_dia_jama = dia_jama, post_sys_jama = sys_jama),
  on = c(case_no = "case_no", post_dt = "vt_date")]
saved$pp$bp$hh_cmh  <- copy(pp$bp$hh_cmh)
pp$bp$hh_cmh <- pp$bp$hh_cmh[is.na(error)]
pp$bp$hh_cmh[, `:=`(sys_status = aux$bp_cat(pre_sys_jama, post_sys_jama),
                    dia_status = aux$bp_cat(pre_dia_jama, post_dia_jama))]
pp$bp$hh_cmh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$bp$hh_cmh[, grep("pre|post|pk|error",
                    names(pp$bp$hh_cmh), value = TRUE) := NULL]
# non-health home vs health home L1/L2 vs health home L3, by team ---
pp$bp$hh_lev_cmh <- modify$bp[!is.na(diastolic), .(pre_dt  = min(vt_date),
                                               post_dt = max(vt_date),
                                               n_recs = .N),
                          by = .(case_no, adm_pk, hh_pk, L3_pk, cmh_team)]
pp$bp$hh_lev_cmh[post_dt - pre_dt < input$record_dist_req,
             error := paste("rec. dist. <", input$record_dist_req)]
pp$bp$hh_lev_cmh[modify$bp, `:=`(
  pre_dia = i.diastolic, pre_sys = i.systolic,
  pre_dia_jama = i.dia_jama, pre_sys_jama = i.sys_jama),
  on = c(case_no = "case_no", pre_dt = "vt_date")]
pp$bp$hh_lev_cmh[modify$bp, `:=`(
  post_dia = diastolic, post_sys = systolic,
  post_dia_jama = dia_jama, post_sys_jama = sys_jama),
  on = c(case_no = "case_no", post_dt = "vt_date")]
saved$pp$bp$hh_lev_cmh  <- copy(pp$bp$hh_lev_cmh)
pp$bp$hh_lev_cmh <- pp$bp$hh_lev_cmh[is.na(error)]
pp$bp$hh_lev_cmh[, `:=`(sys_status = aux$bp_cat(pre_sys_jama, post_sys_jama),
                        dia_status = aux$bp_cat(pre_dia_jama, post_dia_jama))]
pp$bp$hh_lev_cmh[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$bp$hh_lev_cmh[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$bp$hh_lev_cmh[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$bp$hh_lev_cmh[, grep("pre|post|pk|error",
                        names(pp$bp$hh_lev_cmh), value = TRUE) := NULL]

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
pp$labs$chol <- pp$labs$chol[is.na(error)][, error := NULL]
pp$labs$chol[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$chol, j = Cs(pre_cat, post_cat),
  value = interval_cut, ., guide_dt = aux$chol_guide,
  closure = aux$chol_guide[, as.matrix(.SD), .SDcols = Cs(b_init, b_end)],
  category = "cat", interval_names = c("init", "end"), type = "R")
pp$labs$chol[, status :=
  aux$chol_change(pre_cat, post_cat)]

# non-health home vs health home L1/L2 vs health home L3 ---
# health home vs non-health home, by team ---
# non-health home vs health home L1/L2 vs health home L3, by team ---

# PRE/POST ER visits ----------------------------------------------------------
# health home vs non-health home ---
# non-health home vs health home L1/L2 vs health home L3 ---
# health home vs non-health home, by team ---
# non-health home vs health home L1/L2 vs health home L3, by team ---
