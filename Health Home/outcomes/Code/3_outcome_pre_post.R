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
                names(pp$bp$hh), value = TRUE) := NULL]
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
pp$labs$chol$hh <- modify$labs$chol[, .(pre_dt = min(lab_date),
                                    post_dt = max(lab_date),
                                    n_recs = .N),
                                by = .(case_no, lab_name, adm_pk, hh_pk)]
pp$labs$chol$hh[modify$labs$chol, pre_value := lab_value,
             on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$chol$hh[modify$labs$chol, post_value := lab_value,
             on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$chol$hh[post_dt-pre_dt < input$record_dist_req,
  error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$chol$hh  <- copy(pp$labs$chol$hh)
pp$labs$chol$hh <- pp$labs$chol$hh[is.na(error)]
pp$labs$chol$hh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$chol$hh, j = Cs(pre_cat, post_cat),
  value = aux$chol_cut)
pp$labs$chol$hh[, status :=
  aux$chol_change(pre_cat, post_cat)]
pp$labs$chol$hh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$chol$hh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3 ---
pp$labs$chol$hh_lev <- modify$labs$chol[, .(pre_dt = min(lab_date),
                                    post_dt = max(lab_date),
                                    n_recs = .N),
  by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk)]
pp$labs$chol$hh_lev[modify$labs$chol, pre_value := lab_value,
             on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$chol$hh_lev[modify$labs$chol, post_value := lab_value,
             on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$chol$hh_lev[post_dt-pre_dt < input$record_dist_req,
             error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$chol$hh_lev  <- copy(pp$labs$chol$hh_lev)
pp$labs$chol$hh_lev <- pp$labs$chol$hh_lev[is.na(error)]
pp$labs$chol$hh_lev[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$chol$hh_lev, j = Cs(pre_cat, post_cat),
     value = aux$chol_cut)
pp$labs$chol$hh_lev[, status :=
               aux$chol_change(pre_cat, post_cat)]
pp$labs$chol$hh_lev[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$chol$hh_lev[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$chol$hh_lev[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$chol$hh_lev[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, L3_pk) := NULL]
# health home vs non-health home, by team ---
pp$labs$chol$hh_cmh <- modify$labs$chol[, .(pre_dt = min(lab_date),
                                            post_dt = max(lab_date),
                                            n_recs = .N),
  by = .(case_no, lab_name, adm_pk, hh_pk, cmh_team)]
pp$labs$chol$hh_cmh[modify$labs$chol, pre_value := lab_value,
                    on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$chol$hh_cmh[modify$labs$chol, post_value := lab_value,
                    on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$chol$hh_cmh[post_dt-pre_dt < input$record_dist_req,
                    error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$chol$hh_cmh  <- copy(pp$labs$chol$hh_cmh)
pp$labs$chol$hh_cmh <- pp$labs$chol$hh_cmh[is.na(error)]
pp$labs$chol$hh_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$chol$hh_cmh, j = Cs(pre_cat, post_cat),
     value = aux$chol_cut)
pp$labs$chol$hh_cmh[, status :=
                      aux$chol_change(pre_cat, post_cat)]
pp$labs$chol$hh_cmh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$chol$hh_cmh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3, by team ---
pp$labs$chol$hh_lev_cmh <- modify$labs$chol[, .(pre_dt = min(lab_date),
                                            post_dt = max(lab_date),
                                            n_recs = .N),
  by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk, cmh_team)]
pp$labs$chol$hh_lev_cmh[modify$labs$chol, pre_value := lab_value,
                    on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$chol$hh_lev_cmh[modify$labs$chol, post_value := lab_value,
                    on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$chol$hh_lev_cmh[post_dt-pre_dt < input$record_dist_req,
                    error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$chol$hh_lev_cmh  <- copy(pp$labs$chol$hh_lev_cmh)
pp$labs$chol$hh_lev_cmh <- pp$labs$chol$hh_lev_cmh[is.na(error)]
pp$labs$chol$hh_lev_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$chol$hh_lev_cmh, j = Cs(pre_cat, post_cat),
     value = aux$chol_cut)
pp$labs$chol$hh_lev_cmh[, status :=
                          aux$chol_change(pre_cat, post_cat)]
pp$labs$chol$hh_lev_cmh[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$chol$hh_lev_cmh[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$chol$hh_lev_cmh[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$chol$hh_lev_cmh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, L3_pk) := NULL]

# PRE/POST Lab Values: Glucose ------------------------------------------------
# health home vs non-health home ---
pp$labs$gluc$hh <- modify$labs$gluc[, .(pre_dt = min(lab_date),
                                        post_dt = max(lab_date),
                                        n_recs = .N),
                                    by = .(case_no, lab_name, adm_pk, hh_pk)]
pp$labs$gluc$hh[modify$labs$gluc, pre_value := lab_value,
                on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$gluc$hh[modify$labs$gluc, post_value := lab_value,
                on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$gluc$hh[post_dt-pre_dt < input$record_dist_req,
                error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$gluc$hh  <- copy(pp$labs$gluc$hh)
pp$labs$gluc$hh <- pp$labs$gluc$hh[is.na(error)]
pp$labs$gluc$hh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$gluc$hh, j = Cs(pre_cat, post_cat),
     value = function(x) as.character(aux$gluc_cut(x)))
pp$labs$gluc$hh[, status :=
                  as.integer(factor(post_cat, levels = aux$gluc_levels)) -
                  as.integer(factor(pre_cat, levels = aux$gluc_levels))]
pp$labs$gluc$hh[, status := aux$status(status)]
pp$labs$gluc$hh[, jump := 0]
setkey(pp$labs$gluc$hh, pre_cat, post_cat)
pp$labs$gluc$hh[J("risky-", c("risky+", "very risky+")), jump := 1]
pp$labs$gluc$hh[J(c("risky+", "very risky+"), "risky-"), jump := 1]
pp$labs$gluc$hh[jump == 1, `:=`(abs_pre = abs(pre_value - 85))]
pp$labs$gluc$hh[jump == 1, `:=`(abs_post = abs(post_value - 85))]
pp$labs$gluc$hh[jump == 1 & abs_pre == abs_post, status := "maintained"]
pp$labs$gluc$hh[jump == 1 & abs_pre < abs_post, status := "regressed"]
pp$labs$gluc$hh[jump == 1 & abs_pre > abs_post, status := "improved"]
pp$labs$gluc$hh[, Cs(jump, abs_post, abs_pre) := NULL]
pp$labs$gluc$hh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$gluc$hh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3 ---
pp$labs$gluc$hh_lev <- modify$labs$gluc[, .(pre_dt = min(lab_date),
                                            post_dt = max(lab_date),
                                            n_recs = .N),
  by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk)]
pp$labs$gluc$hh_lev[modify$labs$gluc, pre_value := lab_value,
                    on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$gluc$hh_lev[modify$labs$gluc, post_value := lab_value,
                    on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$gluc$hh_lev[post_dt-pre_dt < input$record_dist_req,
                    error := paste("rec. dist. <", input$record_dist_req)]
pp$labs$gluc$hh_lev <- pp$labs$gluc$hh_lev[is.na(error)]
saved$pp$labs$gluc$hh_lev  <- copy(pp$labs$gluc$hh_lev)
pp$labs$gluc$hh_lev[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$gluc$hh_lev, j = Cs(pre_cat, post_cat),
     value = function(x) as.character(aux$gluc_cut(x)))
pp$labs$gluc$hh_lev[, status :=
                  as.integer(factor(post_cat, levels = aux$gluc_levels)) -
                  as.integer(factor(pre_cat, levels = aux$gluc_levels))]
pp$labs$gluc$hh_lev[, status := aux$status(status)]
pp$labs$gluc$hh_lev[, jump := 0]
setkey(pp$labs$gluc$hh_lev, pre_cat, post_cat)
pp$labs$gluc$hh_lev[J("risky-", c("risky+", "very risky+")), jump := 1]
pp$labs$gluc$hh_lev[J(c("risky+", "very risky+"), "risky-"), jump := 1]
pp$labs$gluc$hh_lev[jump == 1, `:=`(abs_pre = abs(pre_value - 85))]
pp$labs$gluc$hh_lev[jump == 1, `:=`(abs_post = abs(post_value - 85))]
pp$labs$gluc$hh_lev[jump == 1 & abs_pre == abs_post, status := "maintained"]
pp$labs$gluc$hh_lev[jump == 1 & abs_pre < abs_post, status := "regressed"]
pp$labs$gluc$hh_lev[jump == 1 & abs_pre > abs_post, status := "improved"]
pp$labs$gluc$hh_lev[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$gluc$hh_lev[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$gluc$hh_lev[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$gluc$hh_lev[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, jump,
                     abs_post, abs_pre) := NULL]
# health home vs non-health home, by team ---
pp$labs$gluc$hh_cmh <- modify$labs$gluc[, .(pre_dt = min(lab_date),
                                            post_dt = max(lab_date),
                                            n_recs = .N),
                                        by = .(case_no, lab_name, adm_pk, hh_pk, cmh_team)]
pp$labs$gluc$hh_cmh[modify$labs$gluc, pre_value := lab_value,
                    on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$gluc$hh_cmh[modify$labs$gluc, post_value := lab_value,
                    on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$gluc$hh_cmh[post_dt-pre_dt < input$record_dist_req,
                    error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$gluc$hh_cmh  <- copy(pp$labs$gluc$hh_cmh)
pp$labs$gluc$hh_cmh <- pp$labs$gluc$hh_cmh[is.na(error)]
pp$labs$gluc$hh_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$gluc$hh_cmh, j = Cs(pre_cat, post_cat),
     value = function(x) as.character(aux$gluc_cut(x)))
pp$labs$gluc$hh_cmh[, status :=
                  as.integer(factor(post_cat, levels = aux$gluc_levels)) -
                  as.integer(factor(pre_cat, levels = aux$gluc_levels))]
pp$labs$gluc$hh_cmh[, status := aux$status(status)]
pp$labs$gluc$hh_cmh[, jump := 0]
setkey(pp$labs$gluc$hh_cmh, pre_cat, post_cat)
pp$labs$gluc$hh_cmh[J("risky-", c("risky+", "very risky+")), jump := 1]
pp$labs$gluc$hh_cmh[J(c("risky+", "very risky+"), "risky-"), jump := 1]
pp$labs$gluc$hh_cmh[jump == 1, `:=`(abs_pre = abs(pre_value - 85))]
pp$labs$gluc$hh_cmh[jump == 1, `:=`(abs_post = abs(post_value - 85))]
pp$labs$gluc$hh_cmh[jump == 1 & abs_pre == abs_post, status := "maintained"]
pp$labs$gluc$hh_cmh[jump == 1 & abs_pre < abs_post, status := "regressed"]
pp$labs$gluc$hh_cmh[jump == 1 & abs_pre > abs_post, status := "improved"]
pp$labs$gluc$hh_cmh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$gluc$hh_cmh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, jump,
                         abs_post, abs_pre) := NULL]
# non-health home vs health home L1/L2 vs health home L3, by team ---
pp$labs$gluc$hh_lev_cmh <- modify$labs$gluc[, .(pre_dt = min(lab_date),
                                                post_dt = max(lab_date),
                                                n_recs = .N),
  by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk, cmh_team)]
pp$labs$gluc$hh_lev_cmh[modify$labs$gluc, pre_value := lab_value,
                        on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$gluc$hh_lev_cmh[modify$labs$gluc, post_value := lab_value,
                        on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$gluc$hh_lev_cmh[post_dt-pre_dt < input$record_dist_req,
                        error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$gluc$hh_lev_cmh  <- copy(pp$labs$gluc$hh_lev_cmh)
pp$labs$gluc$hh_lev_cmh <- pp$labs$gluc$hh_lev_cmh[is.na(error)]
pp$labs$gluc$hh_lev_cmh <- pp$labs$gluc$hh_lev_cmh[is.na(error)]
pp$labs$gluc$hh_lev_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$gluc$hh_lev_cmh, j = Cs(pre_cat, post_cat),
     value = function(x) as.character(aux$gluc_cut(x)))
pp$labs$gluc$hh_lev_cmh[, status :=
                      as.integer(factor(post_cat, levels = aux$gluc_levels)) -
                      as.integer(factor(pre_cat, levels = aux$gluc_levels))]
pp$labs$gluc$hh_lev_cmh[, status := aux$status(status)]
pp$labs$gluc$hh_lev_cmh[, jump := 0]
setkey(pp$labs$gluc$hh_lev_cmh, pre_cat, post_cat)
pp$labs$gluc$hh_lev_cmh[J("risky-", c("risky+", "very risky+")), jump := 1]
pp$labs$gluc$hh_lev_cmh[J(c("risky+", "very risky+"), "risky-"), jump := 1]
pp$labs$gluc$hh_lev_cmh[jump == 1, `:=`(abs_pre = abs(pre_value - 85))]
pp$labs$gluc$hh_lev_cmh[jump == 1, `:=`(abs_post = abs(post_value - 85))]
pp$labs$gluc$hh_lev_cmh[jump == 1 & abs_pre == abs_post, status := "maintained"]
pp$labs$gluc$hh_lev_cmh[jump == 1 & abs_pre < abs_post, status := "regressed"]
pp$labs$gluc$hh_lev_cmh[jump == 1 & abs_pre > abs_post, status := "improved"]
pp$labs$gluc$hh_lev_cmh[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$gluc$hh_lev_cmh[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$gluc$hh_lev_cmh[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$gluc$hh_lev_cmh[, Cs(error, adm_pk, hh_pk, L3_pk, pre_dt, post_dt, jump,
                         abs_post, abs_pre) := NULL]
# PRE/POST Lab Values: triglycerides ------------------------------------------
# health home vs non-health home ---
pp$labs$trig$hh <- modify$labs$trig[, .(pre_dt = min(lab_date),
                                        post_dt = max(lab_date),
                                        n_recs = .N),
                                    by = .(case_no, lab_name, adm_pk, hh_pk)]
pp$labs$trig$hh[modify$labs$trig, pre_value := lab_value,
                on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$trig$hh[modify$labs$trig, post_value := lab_value,
                on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$trig$hh[post_dt-pre_dt < input$record_dist_req,
                error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$trig$hh  <- copy(pp$labs$trig$hh)
pp$labs$trig$hh <- pp$labs$trig$hh[is.na(error)]
pp$labs$trig$hh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$trig$hh, j = Cs(pre_cat, post_cat),
     value = aux$trig_cut)
pp$labs$trig$hh[, status :=
                  aux$trig_change(pre_cat, post_cat)]
pp$labs$trig$hh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$trig$hh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3 ---
pp$labs$trig$hh_lev <- modify$labs$trig[, .(pre_dt = min(lab_date),
                                            post_dt = max(lab_date),
                                            n_recs = .N),
                                        by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk)]
pp$labs$trig$hh_lev[modify$labs$trig, pre_value := lab_value,
                    on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$trig$hh_lev[modify$labs$trig, post_value := lab_value,
                    on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$trig$hh_lev[post_dt-pre_dt < input$record_dist_req,
                    error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$trig$hh_lev  <- copy(pp$labs$trig$hh_lev)
pp$labs$trig$hh_lev <- pp$labs$trig$hh_lev[is.na(error)]
pp$labs$trig$hh_lev[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$trig$hh_lev, j = Cs(pre_cat, post_cat),
     value = aux$trig_cut)
pp$labs$trig$hh_lev[, status :=
                      aux$trig_change(pre_cat, post_cat)]
pp$labs$trig$hh_lev[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$trig$hh_lev[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$trig$hh_lev[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$trig$hh_lev[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, L3_pk) := NULL]
# health home vs non-health home, by team ---
pp$labs$trig$hh_cmh <- modify$labs$trig[, .(pre_dt = min(lab_date),
                                            post_dt = max(lab_date),
                                            n_recs = .N),
                                        by = .(case_no, lab_name, adm_pk, hh_pk, cmh_team)]
pp$labs$trig$hh_cmh[modify$labs$trig, pre_value := lab_value,
                    on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$trig$hh_cmh[modify$labs$trig, post_value := lab_value,
                    on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$trig$hh_cmh[post_dt-pre_dt < input$record_dist_req,
                    error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$trig$hh_cmh  <- copy(pp$labs$trig$hh_cmh)
pp$labs$trig$hh_cmh <- pp$labs$trig$hh_cmh[is.na(error)]
pp$labs$trig$hh_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$trig$hh_cmh, j = Cs(pre_cat, post_cat),
     value = aux$trig_cut)
pp$labs$trig$hh_cmh[, status :=
                      aux$trig_change(pre_cat, post_cat)]
pp$labs$trig$hh_cmh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$trig$hh_cmh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3, by team ---
pp$labs$trig$hh_lev_cmh <- modify$labs$trig[, .(pre_dt = min(lab_date),
                                                post_dt = max(lab_date),
                                                n_recs = .N),
  by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk, cmh_team)]
pp$labs$trig$hh_lev_cmh[modify$labs$trig, pre_value := lab_value,
                        on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$trig$hh_lev_cmh[modify$labs$trig, post_value := lab_value,
                        on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$trig$hh_lev_cmh[post_dt-pre_dt < input$record_dist_req,
                        error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$trig$hh_lev_cmh  <- copy(pp$labs$trig$hh_lev_cmh)
pp$labs$trig$hh_lev_cmh <- pp$labs$trig$hh_lev_cmh[is.na(error)]
pp$labs$trig$hh_lev_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$trig$hh_lev_cmh, j = Cs(pre_cat, post_cat),
     value = aux$trig_cut)
pp$labs$trig$hh_lev_cmh[, status :=
                          aux$trig_change(pre_cat, post_cat)]
pp$labs$trig$hh_lev_cmh[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$trig$hh_lev_cmh[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$trig$hh_lev_cmh[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$trig$hh_lev_cmh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, L3_pk) := NULL]

# PRE/POST Lab Values: a1c --------------------------------------------
# health home vs non-health home ---
pp$labs$a1c$hh <- modify$labs$a1c[, .(pre_dt = min(lab_date),
                                      post_dt = max(lab_date),
                                      n_recs = .N),
                                    by = .(case_no, lab_name, adm_pk, hh_pk)]
pp$labs$a1c$hh[modify$labs$a1c, pre_value := lab_value,
                on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$a1c$hh[modify$labs$a1c, post_value := lab_value,
                on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$a1c$hh[post_dt-pre_dt < input$record_dist_req,
                error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$a1c$hh  <- copy(pp$labs$a1c$hh)
pp$labs$a1c$hh <- pp$labs$a1c$hh[is.na(error)]
pp$labs$a1c$hh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$a1c$hh, j = Cs(pre_cat, post_cat),
     value = aux$a1c_cut)
pp$labs$a1c$hh[, status :=
                  aux$a1c_change(pre_cat, post_cat)]
pp$labs$a1c$hh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$a1c$hh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3 ---
pp$labs$a1c$hh_lev <- modify$labs$a1c[, .(pre_dt = min(lab_date),
                                          post_dt = max(lab_date),
                                          n_recs = .N),
                                        by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk)]
pp$labs$a1c$hh_lev[modify$labs$a1c, pre_value := lab_value,
                    on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$a1c$hh_lev[modify$labs$a1c, post_value := lab_value,
                    on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$a1c$hh_lev[post_dt-pre_dt < input$record_dist_req,
                    error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$a1c$hh_lev  <- copy(pp$labs$a1c$hh_lev)
pp$labs$a1c$hh_lev <- pp$labs$a1c$hh_lev[is.na(error)]
pp$labs$a1c$hh_lev[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$a1c$hh_lev, j = Cs(pre_cat, post_cat),
     value = aux$a1c_cut)
pp$labs$a1c$hh_lev[, status :=
                      aux$a1c_change(pre_cat, post_cat)]
pp$labs$a1c$hh_lev[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$a1c$hh_lev[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$a1c$hh_lev[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$a1c$hh_lev[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, L3_pk) := NULL]
# health home vs non-health home, by team ---
pp$labs$a1c$hh_cmh <- modify$labs$a1c[, .(pre_dt = min(lab_date),
                                          post_dt = max(lab_date),
                                          n_recs = .N),
                                        by = .(case_no, lab_name, adm_pk, hh_pk, cmh_team)]
pp$labs$a1c$hh_cmh[modify$labs$a1c, pre_value := lab_value,
                    on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$a1c$hh_cmh[modify$labs$a1c, post_value := lab_value,
                    on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$a1c$hh_cmh[post_dt-pre_dt < input$record_dist_req,
                    error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$a1c$hh_cmh  <- copy(pp$labs$a1c$hh_cmh)
pp$labs$a1c$hh_cmh <- pp$labs$a1c$hh_cmh[is.na(error)]
pp$labs$a1c$hh_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$a1c$hh_cmh, j = Cs(pre_cat, post_cat),
     value = aux$a1c_cut)
pp$labs$a1c$hh_cmh[, status :=
                      aux$a1c_change(pre_cat, post_cat)]
pp$labs$a1c$hh_cmh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$a1c$hh_cmh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3, by team ---
pp$labs$a1c$hh_lev_cmh <- modify$labs$a1c[, .(pre_dt = min(lab_date),
                                              post_dt = max(lab_date),
                                              n_recs = .N),
                                            by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk, cmh_team)]
pp$labs$a1c$hh_lev_cmh[modify$labs$a1c, pre_value := lab_value,
                        on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$a1c$hh_lev_cmh[modify$labs$a1c, post_value := lab_value,
                        on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$a1c$hh_lev_cmh[post_dt-pre_dt < input$record_dist_req,
                        error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$a1c$hh_lev_cmh  <- copy(pp$labs$a1c$hh_lev_cmh)
pp$labs$a1c$hh_lev_cmh <- pp$labs$a1c$hh_lev_cmh[is.na(error)]
pp$labs$a1c$hh_lev_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$a1c$hh_lev_cmh, j = Cs(pre_cat, post_cat),
     value = aux$a1c_cut)
pp$labs$a1c$hh_lev_cmh[, status :=
                          aux$a1c_change(pre_cat, post_cat)]
pp$labs$a1c$hh_lev_cmh[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$a1c$hh_lev_cmh[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$a1c$hh_lev_cmh[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$a1c$hh_lev_cmh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, L3_pk) := NULL]
# PRE/POST Lab Values: hdl ----------------------------------------------------
# health home vs non-health home ---
pp$labs$hdl$hh <- modify$labs$hdl[, .(pre_dt = min(lab_date),
                                      post_dt = max(lab_date),
                                      n_recs = .N),
                                  by = .(case_no, lab_name, adm_pk, hh_pk)]
pp$labs$hdl$hh[modify$labs$hdl, pre_value := lab_value,
               on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$hdl$hh[modify$labs$hdl, post_value := lab_value,
               on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$hdl$hh[post_dt-pre_dt < input$record_dist_req,
               error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$hdl$hh  <- copy(pp$labs$hdl$hh)
pp$labs$hdl$hh <- pp$labs$hdl$hh[is.na(error)]
pp$labs$hdl$hh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$hdl$hh, j = Cs(pre_cat, post_cat),
     value = aux$hdl_cut)
pp$labs$hdl$hh[, status :=
                 aux$hdl_change(pre_cat, post_cat)]
pp$labs$hdl$hh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$hdl$hh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3 ---
pp$labs$hdl$hh_lev <- modify$labs$hdl[, .(pre_dt = min(lab_date),
                                          post_dt = max(lab_date),
                                          n_recs = .N),
                                      by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk)]
pp$labs$hdl$hh_lev[modify$labs$hdl, pre_value := lab_value,
                   on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$hdl$hh_lev[modify$labs$hdl, post_value := lab_value,
                   on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$hdl$hh_lev[post_dt-pre_dt < input$record_dist_req,
                   error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$hdl$hh_lev  <- copy(pp$labs$hdl$hh_lev)
pp$labs$hdl$hh_lev <- pp$labs$hdl$hh_lev[is.na(error)]
pp$labs$hdl$hh_lev[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$hdl$hh_lev, j = Cs(pre_cat, post_cat),
     value = aux$hdl_cut)
pp$labs$hdl$hh_lev[, status :=
                     aux$hdl_change(pre_cat, post_cat)]
pp$labs$hdl$hh_lev[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$hdl$hh_lev[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$hdl$hh_lev[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$hdl$hh_lev[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, L3_pk) := NULL]
# health home vs non-health home, by team ---
pp$labs$hdl$hh_cmh <- modify$labs$hdl[, .(pre_dt = min(lab_date),
                                          post_dt = max(lab_date),
                                          n_recs = .N),
                                      by = .(case_no, lab_name, adm_pk, hh_pk, cmh_team)]
pp$labs$hdl$hh_cmh[modify$labs$hdl, pre_value := lab_value,
                   on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$hdl$hh_cmh[modify$labs$hdl, post_value := lab_value,
                   on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$hdl$hh_cmh[post_dt-pre_dt < input$record_dist_req,
                   error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$hdl$hh_cmh  <- copy(pp$labs$hdl$hh_cmh)
pp$labs$hdl$hh_cmh <- pp$labs$hdl$hh_cmh[is.na(error)]
pp$labs$hdl$hh_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$hdl$hh_cmh, j = Cs(pre_cat, post_cat),
     value = aux$hdl_cut)
pp$labs$hdl$hh_cmh[, status :=
                     aux$hdl_change(pre_cat, post_cat)]
pp$labs$hdl$hh_cmh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$hdl$hh_cmh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3, by team ---
pp$labs$hdl$hh_lev_cmh <- modify$labs$hdl[, .(pre_dt = min(lab_date),
                                              post_dt = max(lab_date),
                                              n_recs = .N),
                                          by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk, cmh_team)]
pp$labs$hdl$hh_lev_cmh[modify$labs$hdl, pre_value := lab_value,
                       on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$hdl$hh_lev_cmh[modify$labs$hdl, post_value := lab_value,
                       on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$hdl$hh_lev_cmh[post_dt-pre_dt < input$record_dist_req,
                       error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$hdl$hh_lev_cmh  <- copy(pp$labs$hdl$hh_lev_cmh)
pp$labs$hdl$hh_lev_cmh <- pp$labs$hdl$hh_lev_cmh[is.na(error)]
pp$labs$hdl$hh_lev_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$hdl$hh_lev_cmh, j = Cs(pre_cat, post_cat),
     value = aux$hdl_cut)
pp$labs$hdl$hh_lev_cmh[, status :=
                         aux$hdl_change(pre_cat, post_cat)]
pp$labs$hdl$hh_lev_cmh[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$hdl$hh_lev_cmh[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$hdl$hh_lev_cmh[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$hdl$hh_lev_cmh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, L3_pk) := NULL]
# PRE/POST Lab Values: ldl ----------------------------------------------------
# health home vs non-health home ---
pp$labs$ldl$hh <- modify$labs$ldl[, .(pre_dt = min(lab_date),
                                      post_dt = max(lab_date),
                                      n_recs = .N),
                                  by = .(case_no, lab_name, adm_pk, hh_pk)]
pp$labs$ldl$hh[modify$labs$ldl, pre_value := lab_value,
               on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$ldl$hh[modify$labs$ldl, post_value := lab_value,
               on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$ldl$hh[post_dt-pre_dt < input$record_dist_req,
               error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$ldl$hh  <- copy(pp$labs$ldl$hh)
pp$labs$ldl$hh <- pp$labs$ldl$hh[is.na(error)]
pp$labs$ldl$hh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$ldl$hh, j = Cs(pre_cat, post_cat),
     value = aux$ldl_cut)
pp$labs$ldl$hh[, status :=
                 aux$ldl_change(pre_cat, post_cat)]
pp$labs$ldl$hh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$ldl$hh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3 ---
pp$labs$ldl$hh_lev <- modify$labs$ldl[, .(pre_dt = min(lab_date),
                                          post_dt = max(lab_date),
                                          n_recs = .N),
                                      by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk)]
pp$labs$ldl$hh_lev[modify$labs$ldl, pre_value := lab_value,
                   on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$ldl$hh_lev[modify$labs$ldl, post_value := lab_value,
                   on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$ldl$hh_lev[post_dt-pre_dt < input$record_dist_req,
                   error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$ldl$hh_lev  <- copy(pp$labs$ldl$hh_lev)
pp$labs$ldl$hh_lev <- pp$labs$ldl$hh_lev[is.na(error)]
pp$labs$ldl$hh_lev[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$ldl$hh_lev, j = Cs(pre_cat, post_cat),
     value = aux$ldl_cut)
pp$labs$ldl$hh_lev[, status :=
                     aux$ldl_change(pre_cat, post_cat)]
pp$labs$ldl$hh_lev[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$ldl$hh_lev[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$ldl$hh_lev[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$ldl$hh_lev[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, L3_pk) := NULL]
# health home vs non-health home, by team ---
pp$labs$ldl$hh_cmh <- modify$labs$ldl[, .(pre_dt = min(lab_date),
                                          post_dt = max(lab_date),
                                          n_recs = .N),
                                      by = .(case_no, lab_name, adm_pk, hh_pk, cmh_team)]
pp$labs$ldl$hh_cmh[modify$labs$ldl, pre_value := lab_value,
                   on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$ldl$hh_cmh[modify$labs$ldl, post_value := lab_value,
                   on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$ldl$hh_cmh[post_dt-pre_dt < input$record_dist_req,
                   error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$ldl$hh_cmh  <- copy(pp$labs$ldl$hh_cmh)
pp$labs$ldl$hh_cmh <- pp$labs$ldl$hh_cmh[is.na(error)]
pp$labs$ldl$hh_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$ldl$hh_cmh, j = Cs(pre_cat, post_cat),
     value = aux$ldl_cut)
pp$labs$ldl$hh_cmh[, status :=
                     aux$ldl_change(pre_cat, post_cat)]
pp$labs$ldl$hh_cmh[, hh_cat := ifelse(is.na(hh_pk), "CMH only", "HH")]
pp$labs$ldl$hh_cmh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt) := NULL]
# non-health home vs health home L1/L2 vs health home L3, by team ---
pp$labs$ldl$hh_lev_cmh <- modify$labs$ldl[, .(pre_dt = min(lab_date),
                                              post_dt = max(lab_date),
                                              n_recs = .N),
                                          by = .(case_no, lab_name, adm_pk, hh_pk, L3_pk, cmh_team)]
pp$labs$ldl$hh_lev_cmh[modify$labs$ldl, pre_value := lab_value,
                       on = c(case_no = "case_no", pre_dt = "lab_date")]
pp$labs$ldl$hh_lev_cmh[modify$labs$ldl, post_value := lab_value,
                       on = c(case_no = "case_no", post_dt = "lab_date")]
pp$labs$ldl$hh_lev_cmh[post_dt-pre_dt < input$record_dist_req,
                       error := paste("rec. dist. <", input$record_dist_req)]
saved$pp$labs$ldl$hh_lev_cmh  <- copy(pp$labs$ldl$hh_lev_cmh)
pp$labs$ldl$hh_lev_cmh <- pp$labs$ldl$hh_lev_cmh[is.na(error)]
pp$labs$ldl$hh_lev_cmh[, Cs(pre_cat, post_cat) := list(pre_value, post_value)]
setf(pp$labs$ldl$hh_lev_cmh, j = Cs(pre_cat, post_cat),
     value = aux$ldl_cut)
pp$labs$ldl$hh_lev_cmh[, status :=
                         aux$ldl_change(pre_cat, post_cat)]
pp$labs$ldl$hh_lev_cmh[is.na(hh_pk) & is.na(L3_pk), hh_cat := "CMH only"]
pp$labs$ldl$hh_lev_cmh[is.na(hh_cat) & is.na(L3_pk), hh_cat := "HH no nurse"]
pp$labs$ldl$hh_lev_cmh[is.na(hh_cat) & !is.na(L3_pk), hh_cat := "HH Nurse"]
pp$labs$ldl$hh_lev_cmh[, Cs(error, adm_pk, hh_pk, pre_dt, post_dt, L3_pk) := NULL]

# PRE/POST ER visits ----------------------------------------------------------
# health home vs non-health home ---
# non-health home vs health home L1/L2 vs health home L3 ---
# health home vs non-health home, by team ---
# non-health home vs health home L1/L2 vs health home L3, by team ---
