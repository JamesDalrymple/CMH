modify <- new.env(parent = .GlobalEnv)
saved <- new.env(parent = .GlobalEnv)

cmh_adm <- copy(sql$output$cmh_adm)
wn <- copy(sql$output$wn)
vitals <- copy(sql$output$vitals)
labs <- copy(sql$output$labs)

# vitals ----------------------------------------------------------------------
vitals[, vt_date := as.Date(vt_date)]

# CMH admissions --------------------------------------------------------------
cmh_adm[, cmh_team := cmh_recode(team)]
cmh_adm[like(team, "Health Home"), cmh_team := "Health Home"]
cmh_adm[, dob := as.Date(dob)]
# # create secondary team for possibly comparison with Jessica's work
# cmh_adm[like(team, "PBHCI"), secondary_team := "SAMHSA PBHCI"]
# cmh_adm[like(team, "Health Home"), secondary_team := "Health Home"]
# cmh_adm[like(team, "Pilot Disease"), secondary_team := "Disease Mgmt"]

# identifying Health Home staff (nurse and non-nurse too)
modify$hh_staff <- cmh_adm[current_sup == "Hagaman, Brandie" &
  staff_type == "SAMHSA Staff", unique(assigned_staff)]
modify$hh_nurse <- cmh_adm[current_sup == "Hagaman, Brandie" &
  assigned_staff %in% modify$hh_staff & staff_type == "Registered Nurse",
  unique(assigned_staff)]
cmh_adm[, team := NULL]

modify$date_cols <-
  Cs(cmh_effdt, cmh_expdt, team_effdt, team_expdt, staff_eff, staff_exp)
for (j in modify$date_cols) {
  set(cmh_adm, j = j, value = as.Date(cmh_adm[[j]]))
}

# cmh core teams only ---------------------------------------------------------
modify$cmh_core <-
  cmh_adm[cmh_team %in% Cs(Child, DD, Access, ACT, MI, "Child HB"),
    unique(.SD), .SDcols = Cs(case_no, cmh_effdt, cmh_expdt,
  team_effdt, team_expdt, cmh_team, dob)]
# blanks in expdt cause problems
modify$cmh_core[is.na(cmh_expdt), cmh_expdt := Sys.Date() + 999]
modify$cmh_core[is.na(team_expdt), team_expdt := Sys.Date() + 999]

# remove 11 very odd records: overlap_fix_dt requires start to be before end
modify$cmh_core <- modify$cmh_core[cmh_effdt <= cmh_expdt]
modify$cmh_core <- modify$cmh_core[team_effdt <= team_expdt]

a1 <- proc.time()
modify$cmh_core[cmh_priority_dt, priority := i.priority, on = c(cmh_team = "team")]
modify$cmh_core <-
  priority_overlap(data = modify$cmh_core,
                group_cols = c("case_no", "cmh_effdt"), priority_col = "cmh_team",
                start_col = "team_effdt", end_col = "team_expdt",
                overlap_int = 1L, analysis_date = Sys.Date()+999,
                priority_value = "priority")
a2 <- proc.time()
a2 - a1 # 17.5-19 secs
rm(a1, a2)

# remove known bad records (temporarily) ---
if (Sys.Date() == "2016-4-4") {
modify$cmh_core <-
modify$cmh_core[!(case_no==220766 & cmh_effdt == as.Date("2012-11-17"))]
modify$cmh_core <-
  modify$cmh_core[!(case_no==1144969 & cmh_effdt == as.Date("2015-03-13"))]
}

# find any bad records, fix temporarily and then on front end permanently
setorder(modify$cmh_core, case_no, cmh_effdt, team_effdt)
modify$cmh_core[, shift_exp := shift(team_expdt), by = case_no]
modify$adm_error <- modify$cmh_core[between(shift_exp, team_effdt, team_expdt)]
if (nrow(modify$adm_error) > 0) {
  p_stop("modify$adm_error show these consumers have admission record
         inconsistencies which need to be fixed on the front end:",
         modify$adm_error)
}
modify$cmh_core[, shift_exp := NULL]
modify$adm_error <- NULL

modify$cmh_core[, days_cmh :=
  as.integer(pmin(cmh_expdt, input$end_date) - cmh_effdt)]
modify$cmh_core[, cmh_error := NA_character_]
modify$cmh_core[days_cmh < input$days_req_cmh,
  cmh_error := aux$cat_error(old = cmh_error,
    new = paste("days_cmh_req <", input$days_req_cmh))]

# Health Home admission processing --------------------------------------------
modify$hh_teams <-
  cmh_adm[cmh_team %in% c("Health Home"), unique(.SD),
  .SDcols = Cs(case_no, cmh_effdt, cmh_expdt, team_effdt,
               team_expdt, cmh_team, dob)]
# blanks in expdt cause problems
modify$hh_teams[is.na(cmh_expdt), cmh_expdt := Sys.Date() + 999]
modify$hh_teams[is.na(team_expdt), team_expdt := Sys.Date() + 999]

# remove 11 very odd records: overlap_fix_dt requires start to be before end
modify$hh_teams <- modify$hh_teams[cmh_effdt <= cmh_expdt]
modify$hh_teams <- modify$hh_teams[team_effdt <= team_expdt]

modify$hh_teams <-
  overlap_combine(data = modify$hh_teams,
                   group_cols = c("case_no", "cmh_effdt"),
                   start_col = "team_effdt", end_col = "team_expdt",
                   overlap_int = 1L, analysis_date = Sys.Date()+999)

# find any bad records, fix temporarily and then on front end permanently
setorder(modify$hh_teams, case_no, start_date, end_date)
modify$hh_teams[, shift_exp := shift(end_date), by = case_no]
modify$adm_error <- modify$hh_teams[between(shift_exp, start_date, end_date)]
if (nrow(modify$adm_error) > 0) {
  p_stop("modify$adm_error show these consumers have admission record
         inconsistencies which need to be fixed on the front end:",
         modify$adm_error)
}
modify$hh_teams[, shift_exp := NULL]
modify$adm_error <- NULL

modify$hh_teams[, days_cmh :=
  as.integer(pmin(cmh_expdt, input$end_date) - cmh_effdt)]
modify$hh_teams[, days_hh :=
  as.integer(pmin(end_date, input$end_date) - start_date)]
# requiring people to be X days open CMH/HH
modify$hh_teams[, error := NA_character_]
modify$hh_teams[days_cmh < input$days_req_cmh,
  error := aux$cat_error(old = error,
  new = paste("days_cmh_req <", input$days_req_cmh))]
modify$hh_teams[days_hh < input$days_req_hh, error :=
  aux$cat_error(old = error, new = paste("hh_days <", input$days_req_hh))]
modify$hh_teams[, cur_age := floor(as.integer(input$end_date-dob)/365)]
modify$hh_teams[cur_age < 18, error := aux$cat_error(error, "age < 18")]
modify$hh$eligible <- modify$hh_teams[is.na(error), length(unique(case_no))]
modify$hh$ineligible <- modify$hh_teams[!is.na(error), length(unique(case_no))]
saved$hh_summary <-
  data.table(hh_eliglible = modify$hh$eligible,
             hh_ineligible = modify$hh$ineligible)
modify$hh_teams[, Cs(cur_age, dob, end_col, days_cmh) := NULL]
setnames(modify$hh_teams,
         Cs(cmh_team, start_date, end_date, error),
         Cs(hh_team, hh_start, hh_end, hh_error))
modify$hh_nurse
# Health Home Levels ----------------------------------------------------------
modify$hh_levels <-
  cmh_adm[cmh_team %in% c("Health Home") & assigned_staff %in%
      modify$hh_nurse & staff_type == "SAMHSA Staff", unique(.SD),
    .SDcols = Cs(case_no, staff_eff, staff_exp)]
setnames(modify$hh_levels, Cs(staff_eff, staff_exp), Cs(L3_start, L3_end))

modify$hh_levels <-
  overlap_combine(data = modify$hh_levels, group_cols = "case_no",
  start_col = "L3_start", end_col = "L3_end", analysis_date = Sys.Date() + 999)
setnames(modify$hh_levels, Cs(start_date, end_date), Cs(L3_start, L3_end))
modify$hh_levels[is.na(L3_end), L3_end := end_col]

# Eligible CMH/HH table -------------------------------------------------------
modify$eligibility <- copy(modify$cmh_core)
modify$eligibility <- modify$eligibility[cmh_effdt >= input$cmh_exp_after |
                                           cmh_expdt >= input$cmh_exp_after]
modify$eligibility[, cur_age := floor(as.integer(input$end_date-dob)/365)]
modify$eligibility[cur_age < 18, cmh_error := aux$cat_error(cmh_error, "cur_age < 18")]
setkey(modify$hh_teams, case_no, hh_start, hh_end)
modify$eligibility <- foverlaps(modify$eligibility,
          modify$hh_teams[, .SD, .SDcols = Cs(case_no, hh_start, hh_end, hh_error)],
          by.x = Cs(case_no, team_effdt, team_expdt),
          by.y = Cs(case_no, hh_start, hh_end))
setkey(modify$hh_levels, case_no, L3_start, L3_end)
modify$eligibility <- foverlaps(modify$eligibility,
  modify$hh_levels[, .SD, .SDcols = Cs(case_no, L3_start, L3_end)],
  by.x = Cs(case_no, team_effdt, team_expdt),
  by.y = Cs(case_no, L3_start, L3_end))
modify$eligibility[, e_status :=
  ifelse(is.na(cmh_error) & is.na(hh_error), "eligible", "ineligible")]
modify$eligibility[, hh_status := ifelse(is.na(hh_start), "CMH only", "HH")]
modify$eligibility[, hh_lev_status := "CMH only"]
modify$eligibility[!is.na(hh_start) & is.na(L3_start),
                   hh_lev_status := "HH no nurse"]
modify$eligibility[!is.na(hh_start) & !is.na(L3_start),
                   hh_lev_status := "HH nurse"]
saved$eligibility <- copy(modify$eligibility)
modify$eligibility[, Cs(L3_start, L3_end, hh_start, hh_end, dob, priority,
  days_cmh, cmh_error, hh_error, cur_age) := NULL]

# BMI -------------------------------------------------------------------------
modify$bmi <- vitals[!is.na(weight) | !is.na(height_feet) |
  !is.na(height_inches), unique(.SD),
  .SDcols = Cs(case_no, vt_date, weight, height_feet, height_inches)]
modify$bmi[cmh_adm,
           vt_age := floor(as.integer(vt_date - i.dob)/365.25), on = "case_no"]
# remove blanks without losing information
# i.e. weight/height were recorded same day, separately
modify$bmi[, Cs(weight, height_feet, height_inches) :=
             list(as.integer(median(weight, na.rm = TRUE)),
                  as.integer(median(height_feet, na.rm = TRUE)),
                  as.integer(median(height_inches, na.rm = TRUE))),
           by = list(case_no, vt_date)]
modify$bmi <- unique(modify$bmi)
# add cmh_core columns
setkey(modify$cmh_core, case_no, team_effdt, team_expdt)
modify$bmi[, vt_date2 := vt_date]
modify$bmi <-
  foverlaps(modify$bmi,
    modify$cmh_core[, unique(.SD),
      .SDc = Cs(case_no, cmh_effdt, cmh_expdt, cmh_team, team_effdt,
                team_expdt, days_cmh, cmh_error)],
    by.x = c("case_no", "vt_date", "vt_date2"),
    by.y = c("case_no", "team_effdt", "team_expdt"))
setkey(modify$hh_teams, case_no, hh_start, hh_end)
# add hh_team columns
modify$bmi <-
  foverlaps(modify$bmi,
    modify$hh_teams[, unique(.SD),
      .SDc = Cs(case_no, hh_team, hh_start, hh_end, days_hh, hh_error)],
    by.x = c("case_no", "vt_date", "vt_date2"),
    by.y = c("case_no", "hh_start", "hh_end"))
# add hh_level (nurse vs no nurse)
setkey(modify$hh_levels, case_no, L3_start, L3_end)
modify$bmi <-
  foverlaps(modify$bmi,
    modify$hh_levels[, unique(.SD),
      .SDc = Cs(case_no, L3_start, L3_end)],
    by.x = c("case_no", "vt_date", "vt_date2"),
    by.y = c("case_no", "L3_start", "L3_end"))
modify$bmi[, vt_date2 := NULL]

# combine errors ---
modify$bmi[, bmi_error := NA_character_]
modify$bmi[!is.na(cmh_error),
           bmi_error := aux$cat_error(old = bmi_error, new = cmh_error)]
modify$bmi[!is.na(hh_error),
           bmi_error := aux$cat_error(old = bmi_error, new = hh_error)]
modify$bmi[vt_age < 18 & is.na(bmi_error),
  bmi_error := aux$cat_error(bmi_error, "vital age < 18")]

# combine inchest/feet
modify$bmi[, med_in :=
             as.integer(median(height_inches, na.rm = TRUE)), by = list(case_no)]
modify$bmi[, med_ft:= as.integer(median(height_feet, na.rm = TRUE)), by = case_no]

# one possible way to deal with bad heights
modify$bmi[med_ft > 8, med_ft := NA]
modify$bmi[med_ft < 3, med_ft := NA]
modify$bmi[med_in > 11, med_in := NA]
# bmi calculation
modify$bmi[, med_ht := med_ft*12+med_in]
modify$bmi[, Cs(med_ft, med_in) := NULL]
# Mike H says we will use standard BMI calc. for now.
modify$bmi[, calc_bmi := aux$calc_bmi(lb = weight, inches = med_ht)]
# how many days have passed between vt_dates?
setorder(modify$bmi, case_no, vt_date)
modify$bmi[, days_since_vt_date :=
  as.integer(vt_date-shift(vt_date)), by = list(case_no)]
# for data verification later
modify$bmi[is.na(calc_bmi), bmi_error :=
           aux$cat_error(bmi_error, "bmi not calculable")]
modify$bmi[cmh_expdt < input$cmh_exp_after,
  bmi_error := aux$cat_error(bmi_error, paste("cmh_exp prior to ", input$cmh_exp_after))]
modify$bmi[calc_bmi < 10, bmi_error := aux$cat_error(bmi_error,
  "BMI suspiciously low due to weight or height error")]
# modify$bmi[calc_bmi > 95]
modify$bmi[, Cs(med_ht, weight, height_feet, height_inches, vt_age) := NULL]
modify$bmi[, num_bmi := .N, by = case_no]
modify$bmi[is.na(bmi_error), num_bmi_no_error := .N, by = case_no]
modify$bmi[is.na(num_bmi_no_error), num_bmi_no_error := 0]
modify$bmi[num_bmi_no_error < 2,
           bmi_error := aux$cat_error(bmi_error, "BMI's error-free < 2")]
# saving copy of pre-analysis data for review
saved$bmi <- copy(modify$bmi)
# remove all errors; at least 2 BMI's required
modify$bmi <- modify$bmi[num_bmi_no_error >= 2]
modify$bmi[, Cs(num_bmi, num_bmi_no_error, cmh_effdt, cmh_expdt, hh_error,
                cmh_error, bmi_error) := NULL]
modify$bmi <- modify$bmi[!is.na(cmh_team)]

# wellness note: overall health -----------------------------------------------
modify$wn <- copy(wn)
modify$wn <- modify$wn[!is.na(ovr_health) | !is.na(pain)]
modify$wn[, wn_date2 := wn_date]
# add cmh_core columns
setkey(modify$cmh_core, case_no, team_effdt, team_expdt)
modify$wn <-
  foverlaps(modify$wn,
            modify$cmh_core[, unique(.SD),
                            .SDc = Cs(case_no, cmh_effdt, cmh_expdt, cmh_team, team_effdt,
                                      team_expdt, days_cmh, cmh_error)],
            by.x = c("case_no", "wn_date", "wn_date2"),
            by.y = c("case_no", "team_effdt", "team_expdt"))
setkey(modify$hh_teams, case_no, hh_start, hh_end)
# add hh_team columns
modify$wn <-
  foverlaps(modify$wn,
            modify$hh_teams[, unique(.SD),
                            .SDc = Cs(case_no, hh_team, hh_start, hh_end, days_hh, hh_error)],
            by.x = c("case_no", "wn_date", "wn_date2"),
            by.y = c("case_no", "hh_start", "hh_end"))
# add hh_level (nurse vs no nurse)
setkey(modify$hh_levels, case_no, L3_start, L3_end)
modify$wn <-
  foverlaps(modify$wn,
            modify$hh_levels[, unique(.SD),
                             .SDc = Cs(case_no, L3_start, L3_end)],
            by.x = c("case_no", "wn_date", "wn_date2"),
            by.y = c("case_no", "L3_start", "L3_end"))
modify$wn[, wn_date2 := NULL]

# error handling ---

modify$wn_oh <- copy(modify$wn[!is.na(ovr_health)])[, pain := NULL]
modify$wn_pain <- copy(modify$wn[!is.na(pain)])[, ovr_health := NULL]

# LEFT OFF HERE ----- james 4/4/2016 6:15pm
modify$wn_oh
modify$wn_pain



wn[ovr_health=="No Response" & is.na(pain)]
wn[, unique(.SD), .SDc = Cs(ovr_health, pain)]



wn[modify$last_core, Cs(cmh_team, cmh_effdt, cmh_expdt, team_effdt,
  team_expdt, days_cmh) := list(i.cmh_team, i.cmh_effdt,
  i.cmh_expdt, i.team_effdt, i.team_expdt, i.days_cmh), on = "case_no"]
wn[cmh_adm, cur_age := i.cur_age, on = "case_no"]
wn[, error := NA_character_]
wn[days_cmh < input$days_req_cmh, error :=
  aux$cat_error(error, paste("days_cmh < ", input$days_req_cmh))]
wn[, wn_date := as.Date(wn_date)]
wn[, wn_date2 := wn_date]
setkey(modify$hh_teams, case_no, hh_effdt, hh_expdt)
wn <- foverlaps(wn,
          modify$hh_teams[, unique(.SD),
                          .SDc = Cs(case_no, hh_effdt, hh_expdt, days_hh)],
          by.x = c("case_no", "wn_date", "wn_date2"),
          by.y = c("case_no", "hh_effdt", "hh_expdt"))
wn[, wn_date2 := NULL]
wn[days_hh < input$days_req_hh, error :=
  aux$cat_error(error, paste("days_hh < ", input$days_req_hh))]
wn[, wn_date := as.Date(wn_date)]
wn[, age_at_wn := cur_age - as.integer(Sys.Date()-wn_date)/365.25]
wn[age_at_wn < 18, error := aux$cat_error(error, "age_at_wn < 18")]
wn[wn_date < input$cmh_exp_after, error :=
  aux$cat_error(error, paste("wn_date <", input$cmh_exp_after))]
wn[ovr_health == "No Response", ovr_health := NA_character_]
wn[!is.na(ovr_health), ovr_N := .N, by = case_no]
wn[!is.na(pain), pain_N := .N, by = case_no]
wn[, ovr_error := NA_character_]
wn[, pain_error := NA_character_]
wn[ovr_N < 2 | is.na(ovr_N),
   ovr_error := aux$cat_error(ovr_error, "less than 2 ovr_health")]
wn[pain_N < 2 | is.na(pain_N),
   pain_error := aux$cat_error(pain_error, "less than 2 ovr_health")]
# save copy for data validation/verification
saved$wn <- copy(wn)
wn <- wn[is.na(error)]
wn <- wn[is.na(pain_error) | is.na(ovr_error)]
wn[, setdiff(names(wn), Cs(case_no, wn_date, ovr_health,
  pain, cmh_team, hh_effdt, hh_expdt)) := NULL]
# wn[case_no %in% modify$hh_teams[, unique(case_no)], hh_ever := "Y"]
modify$wn_ovr <- wn[!is.na(ovr_health)]
modify$wn_pain <- wn[!is.na(pain)]

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
# blood pressure --------------------------------------------------------------
bp <- copy(vitals[, unique(.SD),
                  .SDcols = Cs(case_no, vt_date, diastolic, systolic)])
bp <- bp[vt_date >= input$cmh_exp_after]
bp[, error := NA_character_]
bp[!is.na(diastolic) & is.na(systolic),
   error := aux$cat_error(error, "missing systolic with known diastolic")]
bp[is.na(diastolic) & !is.na(systolic),
   error := aux$cat_error(error, "missing diastolic with known systolic")]

bp[is.na(diastolic) & is.na(systolic),
   error := aux$cat_error(error, "missing both vp values")]
bp[!is.na(diastolic) & !is.na(systolic), N_bp := .N, by = case_no]
bp[N_bp < 2, error := aux$cat_error(error, "less than 2 BP vt_dates")]
bp[, N_bp := NULL]

bp[, vt_date2 := vt_date]
setkey(modify$hh_teams, case_no, hh_effdt, hh_expdt)
bp <- foverlaps(bp,
                modify$hh_teams[, unique(.SD),
                                .SDc = Cs(case_no, hh_effdt, hh_expdt, days_hh)],
                by.x = c("case_no", "vt_date", "vt_date2"),
                by.y = c("case_no", "hh_effdt", "hh_expdt"))
bp[, vt_date2 := NULL]

bp[cmh_adm, vt_age :=
               round(as.integer(vt_date-i.dob)/365.25, 4), on = "case_no"]
bp[modify$last_core, Cs(cmh_team, days_cmh) :=
               list(i.cmh_team, i.days_cmh), on = "case_no"]
bp[days_cmh < input$days_req_cmh, error :=
  aux$cat_error(error, paste("days_cmh <", input$days_req_cmh))]
bp[days_cmh < input$days_req_cmh][days_hh >= input$days_req_hh]



saved$bp <- copy(bp)
bp <- bp[is.na(error)]

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


# pulse ----------------_v-------------------------------------------------------
pulse <- copy(vitals[, unique(.SD), .SDcols = Cs(case_no, vt_date, pulse)])
# respiration -----------------------------------------------------------------
resp <- copy(vitals[, unique(.SD),
                    .SDcols = Cs(case_no, vt_date, respiration_rate)])




## old code ##

modify$bp_dt <-
  output[, unique(.SD),
   .SDcols = c("case_no", "hh_team", "vt_date", "diastolic", "systolic", "age",
               "samhsa_staff", "stafftype")]
# systolic needs to be higher than diastolic
modify$bp_dt <- modify$bp_dt[systolic >= diastolic]

# we want blood pressure with both the upper and lower numbers
modify$bp_dt <- modify$bp_dt[!is.na(diastolic) & !is.na(systolic)]
modify$bp_dt[, max_vt_date := max(vt_date, na.rm = TRUE), by = case_no]
         modify$bp_dt[, min_vt_date := min(vt_date, na.rm = TRUE), by = case_no]
# fix records with multiple blood pressures in one day
modify$bp_dt[, diastolic := as.num(diastolic)]
modify$bp_dt[, systolic := as.num(systolic)]
modify$bp_dt[, c("diastolic", "systolic") :=
  list(round(mean(diastolic)), round(mean(systolic))),
  by = list(case_no, vt_date)]
modify$bp_dt <- unique(modify$bp_dt)

# jama categories
modify$bp_dt[, c("sys_jama", "dia_jama") :=
  list(aux$sys_jama(age, systolic),
       aux$dia_jama(age, diastolic))]

# combining first/last bp dates to one row = one consumer
modify$output$bp <- mmerge(l = list(
  unique(modify$bp_dt[min_vt_date == vt_date,
                   list(case_no, sys_jama1 = sys_jama, dia_jama1 = dia_jama)]),
  unique(modify$bp_dt[max_vt_date == vt_date,
                      list(case_no, sys_jama2 = sys_jama,
                           dia_jama2 = dia_jama)]),
  modify$bp_dt[, unique(.SD),
                .SDcols = c("case_no", "hh_team", "samhsa_staff",
                            "stafftype")]),
  all = TRUE, by = "case_no")
modify$output$bp <- unique(modify$output$bp)

# improvement -----------------------------------------------------------------
modify$output$bp[, c("jama_sys_status", "jama_dia_status") :=
                   list(aux$jama_eval(sys_jama1, sys_jama2),
                        aux$jama_eval(dia_jama1, dia_jama2))]

### create information about the file to share with end-users ###
about_file <- data.table(data_source = "E2 report 2281: E2_HH_vs_non_HH_Vital",
                         date_range = "7/1/14 to date_ran",
                         date_ran = input$run_date,
                         last_updated = input$run_date,
                         project_location = project_wd$project)

