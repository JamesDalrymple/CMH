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
# non-health home vs health home L1/L2 vs health home L3 ---
# health home vs non-health home, by team ---
# non-health home vs health home L1/L2 vs health home L3, by team ---




modify$bmi_pre_post <-
  rbindlist(list(
    modify$bmi[!is.na(hh_effdt),
               list(group = "HH",
                    min_vt = min(vt_date, na.rm = TRUE),
                    max_vt = max(vt_date, na.rm = TRUE)),
               by = case_no],
    modify$bmi[is.na(hh_effdt),
               list(group = "non-HH",
                    min_vt = min(vt_date, na.rm = TRUE),
                    max_vt = max(vt_date, na.rm = TRUE)),
               by = case_no]), use.names = TRUE)

modify$bmi_pre_post[modify$bmi, Cs(pre_bmi) := list(i.calc_bmi),
                    on = c(case_no = "case_no", min_vt = "vt_date")]
modify$bmi_pre_post[modify$bmi,
                    Cs(post_bmi, last_cmh_team) := list(i.calc_bmi, i.last_cmh_team),
                    on = c(case_no = "case_no", max_vt = "vt_date")]
modify$bmi_cases$dist_req_fail <-
  modify$bmi_pre_post[max_vt - min_vt < input$record_dist_req, unique(case_no)]
modify$bmi_pre_post <-
  modify$bmi_pre_post[max_vt - min_vt >= input$record_dist_req]
saved$bmi[case_no %in% modify$bmi_cases$dist_req_fail,
          Cs(error, num_bmi_no_error) := list(aux$cat_error(error,
                                                            paste("bmi pre/post <", input$record_dist_req)),
                                              as.integer(pmax(num_bmi_no_error-1, 0)))]

# IMPROVEMENT #1: post_bmi > pre_bmi
modify$bmi_pre_post[post_bmi-pre_bmi>0 & pre_bmi >= 18.5, imp1 := "worsened"]
modify$bmi_pre_post[post_bmi-pre_bmi>0 & pre_bmi < 18.5, imp1 := "improved"]
modify$bmi_pre_post[post_bmi-pre_bmi==0, imp1 := "maintained"]
modify$bmi_pre_post[post_bmi-pre_bmi<0 & pre_bmi >= 18.5, imp1 := "improved"]
modify$bmi_pre_post[post_bmi-pre_bmi<0 & pre_bmi < 18.5, imp1 := "worsened"]
# IMPROVEMENT #2: less than 2% diff. = maintain
modify$bmi_pre_post[abs((post_bmi - pre_bmi)/pre_bmi) <= 0.02,
                    imp2 := "maintained"]
modify$bmi_pre_post[((post_bmi - pre_bmi)/pre_bmi) > 0.02 & pre_bmi >= 18.5,
                    imp2 := "worsened"]
modify$bmi_pre_post[((post_bmi - pre_bmi)/pre_bmi) < -0.02 & pre_bmi >= 18.5,
                    imp2 := "improved"]
modify$bmi_pre_post[((post_bmi - pre_bmi)/pre_bmi) > 0.02 & pre_bmi < 18.5,
                    imp2 := "improved"]
modify$bmi_pre_post[((post_bmi - pre_bmi)/pre_bmi) < -0.02 & pre_bmi < 18.5,
                    imp2 := "worsened"]
# IMPROVEMENT #3: less than 0.5 BMI diff. = maintain
modify$bmi_pre_post[abs(post_bmi - pre_bmi) <= 0.5,
                    imp3 := "maintained"]
modify$bmi_pre_post[post_bmi - pre_bmi > 0.5 & pre_bmi >= 18.5,
                    imp3 := "worsened"]
modify$bmi_pre_post[post_bmi - pre_bmi < -0.5 & pre_bmi >= 18.5,
                    imp3 := "improved"]
modify$bmi_pre_post[post_bmi - pre_bmi > 0.5 & pre_bmi < 18.5,
                    imp3 := "improved"]
modify$bmi_pre_post[post_bmi - pre_bmi < -0.5 & pre_bmi < 18.5,
                    imp3 := "worsened"]

modify$bmi_imp1 <- rbindlist(list(
  modify$bmi_pre_post[,
                      list(cases_imp1 = length(unique(case_no))), keyby = list(group, imp1)],
  data.table(group = "HH", imp1 = "hh_eligible", cases_imp1 = modify$hh$eligible)),
  use.names = TRUE)
modify$bmi_imp2 <- rbindlist(list(
  modify$bmi_pre_post[,
                      list(cases_imp2 = length(unique(case_no))), keyby = list(group, imp2)],
  data.table(group = "HH", imp2 = "hh_eligible", cases_imp2 = modify$hh$eligible)),
  use.names = TRUE)
modify$bmi_imp3 <- rbindlist(list(
  modify$bmi_pre_post[,
                      list(cases_imp3 = length(unique(case_no))), keyby = list(group, imp3)],
  data.table(group = "HH", imp3 = "hh_eligible", cases_imp3 = modify$hh$eligible)),
  use.names = TRUE)
setnames(modify$bmi_imp1, "imp1", "status")
setnames(modify$bmi_imp2, "imp2", "status")
setnames(modify$bmi_imp3, "imp3", "status")
saved$bmi_imp <-
  mmerge(modify$bmi_imp1, modify$bmi_imp2, modify$bmi_imp3, by = c("group", "status"), all = TRUE)



# PRE/POST Wellness note overall health ---------------------------------------
# health home vs non-health home ---
# non-health home vs health home L1/L2 vs health home L3 ---
# health home vs non-health home, by team ---
# non-health home vs health home L1/L2 vs health home L3, by team ---

# PRE/POST WN: OVR HEALTH -----------------------------------------------------
modify$wn_pre_post_ovr <-
  rbindlist(list(
    modify$wn_ovr[!is.na(hh_effdt),
                  list(group = "HH",
                       min_wn_ovr = min(wn_date, na.rm = TRUE),
                       max_wn_ovr = max(wn_date, na.rm = TRUE)),
                  by = case_no],
    modify$wn_ovr[is.na(hh_effdt),
                  list(group = "non-HH",
                       min_wn_ovr = min(wn_date, na.rm = TRUE),
                       max_wn_ovr = max(wn_date, na.rm = TRUE)),
                  by = case_no]), use.names = TRUE)
modify$wn_pre_post_pain <-
  rbindlist(list(
    modify$wn_pain[!is.na(hh_effdt),
                   list(group = "HH",
                        min_wn_pain = min(wn_date, na.rm = TRUE),
                        max_wn_pain = max(wn_date, na.rm = TRUE)),
                   by = case_no],
    modify$wn_pain[is.na(hh_effdt),
                   list(group = "non-HH",
                        min_wn_pain = min(wn_date, na.rm = TRUE),
                        max_wn_pain = max(wn_date, na.rm = TRUE)),
                   by = case_no]), use.names = TRUE)
modify$wn_pre_post <- merge(modify$wn_pre_post_ovr, modify$wn_pre_post_pain,
                            all = TRUE, by = c("case_no", "group"))

modify$wn_pre_post[wn, pre_ovr := i.ovr_health,
                   on = c(case_no = "case_no", min_wn_ovr = "wn_date")]
modify$wn_pre_post[wn, post_ovr := i.ovr_health,
                   on = c(case_no = "case_no", max_wn_ovr = "wn_date")]
modify$wn_pre_post[wn, pre_pain := i.pain,
                   on = c(case_no = "case_no", min_wn_pain = "wn_date")]
modify$wn_pre_post[wn, post_pain := i.pain,
                   on = c(case_no = "case_no", max_wn_pain = "wn_date")]

modify$wn_ovr_cases <-
  modify$wn_pre_post[max_wn_ovr - min_wn_ovr < input$record_dist_req,
                     unique(case_no)]
modify$wn_pain_cases <-
  modify$wn_pre_post[max_wn_pain - min_wn_pain < input$record_dist_req,
                     unique(case_no)]
saved$wn[case_no %in% modify$wn_ovr_cases, ovr_error :=
           aux$cat_error(ovr_error, paste("wn pre/post <", input$record_dist_req))]
saved$wn[case_no %in% modify$wn_pain_cases, pain_error :=
           aux$cat_error(pain_error, paste("wn pre/post <", input$record_dist_req))]

modify$wn_pre_post[max_wn_ovr - min_wn_ovr < input$record_dist_req,
                   Cs(pre_ovr, post_ovr) := list(NA, NA)]
modify$wn_pre_post[max_wn_pain - min_wn_pain < input$record_dist_req,
                   Cs(pre_pain, post_pain) := list(NA, NA)]
modify$wn_pre_post <- modify$wn_pre_post[!is.na(pre_ovr) | !is.na(post_ovr) |
                                           !is.na(pre_pain) | !is.na(post_pain)]
modify$wn_pre_post[, change_ovr := aux$health_compare(pre_ovr, post_ovr)]
modify$wn_pre_post[, change_pain := aux$pain_compare(pre_pain, post_pain)]

modify$health_imp <-
  modify$wn_pre_post[!is.na(change_ovr), list(cases_ovr = length(unique(case_no))),
                     by = list(change_ovr, group)]
modify$pain_imp <-
  modify$wn_pre_post[!is.na(change_pain), list(cases_pain = length(unique(case_no))),
                     by = list(change_pain, group)]
setnames(modify$health_imp, "change_ovr", "status")
setnames(modify$pain_imp, "change_pain", "status")
saved$health_imp <- merge(modify$health_imp, modify$pain_imp,
                          by = c("status", "group"), all = TRUE)


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





# PRE/POST Lab Values ---------------------------------------------------------
# health home vs non-health home ---
# non-health home vs health home L1/L2 vs health home L3 ---
# health home vs non-health home, by team ---
# non-health home vs health home L1/L2 vs health home L3, by team ---

# PRE/POST ER visits ----------------------------------------------------------
# health home vs non-health home ---
# non-health home vs health home L1/L2 vs health home L3 ---
# health home vs non-health home, by team ---
# non-health home vs health home L1/L2 vs health home L3, by team ---
