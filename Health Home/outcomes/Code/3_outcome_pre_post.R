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
