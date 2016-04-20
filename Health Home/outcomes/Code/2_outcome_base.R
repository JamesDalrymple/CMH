modify <- new.env(parent = .GlobalEnv)
saved <- new.env(parent = .GlobalEnv)

cmh_adm <- copy(sql$output$cmh_adm)
wn <- copy(sql$output$wn)
vitals <- copy(sql$output$vitals)
# labs <- copy(sql$output$labs)

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
# if (Sys.Date() == "2016-4-4") {
# modify$cmh_core <-
#   modify$cmh_core[!(case_no==12 & cmh_effdt == as.Date("2012-11-17"))]
# modify$cmh_core <-
#   modify$cmh_core[!(case_no==13 & cmh_effdt == as.Date("2015-03-13"))]
# }

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
modify$cmh_core[, adm_pk := .GRP, by = .(case_no, cmh_effdt)]

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
modify$hh_teams[, hh_pk := .GRP, by = .(case_no, start_date)]
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
# Health Home Levels ----------------------------------------------------------
modify$hh_levels <-
  cmh_adm[cmh_team %in% c("Health Home") & assigned_staff %in%
      modify$hh_nurse & staff_type == "SAMHSA Staff", unique(.SD),
    .SDcols = Cs(case_no, staff_eff, staff_exp, team_effdt, team_expdt)]
setnames(modify$hh_levels, Cs(staff_eff, staff_exp), Cs(L3_start, L3_end))
modify$hh_levels <- modify$hh_levels[L3_end >= team_effdt | is.na(L3_end)]
modify$hh_levels[L3_end < team_effdt]
modify$hh_levels[L3_start < team_effdt, L3_start := team_effdt]
modify$hh_levels[, Cs(team_effdt, team_expdt) := NULL]
modify$hh_levels <-
  overlap_combine(data = modify$hh_levels, group_cols = "case_no",
  overlap_int = 1L,  start_col = "L3_start", end_col = "L3_end",
  analysis_date = Sys.Date() + 999)
setnames(modify$hh_levels, Cs(start_date, end_date), Cs(L3_start, L3_end))
modify$hh_levels[is.na(L3_end), L3_end := end_col]
modify$hh_levels[, L3_pk := .GRP, by = .(case_no, L3_start)]

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
                team_expdt, days_cmh, cmh_error, adm_pk)],
    by.x = c("case_no", "vt_date", "vt_date2"),
    by.y = c("case_no", "team_effdt", "team_expdt"))
setkey(modify$hh_teams, case_no, hh_start, hh_end)
# add hh_team columns
modify$bmi <-
  foverlaps(modify$bmi,
    modify$hh_teams[, unique(.SD),
      .SDc = Cs(case_no, hh_team, hh_start, hh_end, days_hh, hh_error, hh_pk)],
    by.x = c("case_no", "vt_date", "vt_date2"),
    by.y = c("case_no", "hh_start", "hh_end"))
# add hh_level (nurse vs no nurse)
setkey(modify$hh_levels, case_no, L3_start, L3_end)
modify$bmi <-
  foverlaps(modify$bmi,
    modify$hh_levels[, unique(.SD),
      .SDc = Cs(case_no, L3_start, L3_end, L3_pk)],
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
# deal with multiple weights on one day
modify$bmi[, weight := as.numeric(weight)]
modify$bmi[, weight := mean(weight), by = list(case_no, vt_date)]
# bmi calculation
modify$bmi[, med_ht := med_ft*12+med_in]
modify$bmi[, Cs(med_ft, med_in) := NULL]
# Mike H says we will use standard BMI calc. for now.
modify$bmi[, calc_bmi := aux$calc_bmi(lb = weight, inches = med_ht)]
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
# modify$bmi[, hh_status := ifelse(is.na(hh_team), "CMH only", "HH")]
modify$bmi[, Cs(num_bmi, num_bmi_no_error, cmh_effdt, cmh_expdt, hh_error,
            cmh_error, bmi_error, hh_start, hh_end, L3_start, L3_end) := NULL]
modify$bmi <- modify$bmi[!is.na(cmh_team)]

# wellness note: overall health -----------------------------------------------
modify$wn <- copy(wn)
modify$wn[, wn_date := as.Date(wn_date)]
modify$wn <- modify$wn[!is.na(ovr_health) | !is.na(pain)]
modify$wn[, wn_date2 := wn_date]
# add cmh_core columns
setkeyv(modify$cmh_core, c("case_no", "team_effdt", "team_expdt"))
modify$wn <-
  foverlaps(modify$wn,
            modify$cmh_core[, unique(.SD),
                            .SDc = Cs(case_no, cmh_effdt, cmh_expdt, cmh_team, team_effdt,
                                      team_expdt, days_cmh, cmh_error, dob, adm_pk)],
            by.x = c("case_no", "wn_date", "wn_date2"),
            by.y = c("case_no", "team_effdt", "team_expdt"))
# add hh_team columns
setkeyv(modify$hh_teams, c("case_no", "hh_start", "hh_end"))
modify$wn <-
  foverlaps(modify$wn,
    modify$hh_teams[, unique(.SD),
    .SDc = Cs(case_no, hh_team, hh_start, hh_end, days_hh, hh_error, hh_pk)],
            by.x = c("case_no", "wn_date", "wn_date2"),
            by.y = c("case_no", "hh_start", "hh_end"))
# add hh_level (nurse vs no nurse)
setkey(modify$hh_levels, case_no, L3_start, L3_end)
modify$wn <-
  foverlaps(modify$wn,
            modify$hh_levels[, unique(.SD),
              .SDc = Cs(case_no, L3_start, L3_end, L3_pk)],
            by.x = c("case_no", "wn_date", "wn_date2"),
            by.y = c("case_no", "L3_start", "L3_end"))
modify$wn[, wn_date2 := NULL]
modify$wn[, wn_age := floor(as.integer(wn_date - dob)/365.25)]
# oh = overall health
modify$wn_oh <- copy(modify$wn[!is.na(ovr_health)])[, pain := NULL]
# pain
modify$wn_pain <- copy(modify$wn[!is.na(pain)])[, ovr_health := NULL]

# error handling ---
# overall health error handling
modify$wn_oh[, oh_error := NA_character_]
modify$wn_oh[!is.na(cmh_error), oh_error := aux$cat_error(oh_error, cmh_error)]
modify$wn_oh[!is.na(hh_error), oh_error := aux$cat_error(oh_error, hh_error)]
modify$wn_oh[ovr_health == "No Response",
             oh_error := aux$cat_error(oh_error, "ovr_health 'no response'")]
modify$wn_oh[wn_age < 18, oh_error := aux$cat_error(oh_error, "wn_age < 18")]
modify$wn_oh[wn_date < input$cmh_exp_after, oh_error :=
               aux$cat_error(oh_error, paste("wn_date <", input$cmh_exp_after))]
modify$wn_oh[!is.na(ovr_health), ovr_N := .N, by = case_no]
modify$wn_oh[ovr_N < 2 | is.na(ovr_N),
  oh_error := aux$cat_error(oh_error, "less than 2 ovr_health")]
saved$wn_oh <- copy(modify$wn_oh)
modify$wn_oh[, unique(oh_error)]
modify$wn_oh <- modify$wn_oh[is.na(oh_error)]
modify$wn_oh[, Cs(cmh_error, dob, oh_error, cmh_effdt, cmh_expdt, team_effdt,
  team_expdt, hh_start, hh_end, L3_start, L3_end):= NULL]
modify$wn_oh <- modify$wn_oh[!is.na(cmh_team)]
modify$wn_oh[, ovr_health :=
  aux$ovr_health_reduce(ovr_health), by = .(case_no, wn_date)]
modify$wn_oh <- unique(modify$wn_oh)
modify$wn_oh[, days_idx := .GRP, by = list(case_no, wn_date)]
if (nrow(modify$wn_oh[duplicated(days_idx)]) > 0) {
  p_stop("modify$wn_oh has duplicates", modify$wn_oh[duplicated(days_idx)])
} ; modify$wn_oh[, days_idx := NULL]
# pain error handling
modify$wn_pain[, pain_error := NA_character_]
modify$wn_pain[!is.na(cmh_error), pain_error := aux$cat_error(pain_error, cmh_error)]
modify$wn_pain[!is.na(hh_error), pain_error := aux$cat_error(pain_error, hh_error)]
modify$wn_pain[wn_age < 18, pain_error := aux$cat_error(pain_error, "wn__age < 18")]
modify$wn_pain[wn_date < input$cmh_exp_after, pain_error :=
               aux$cat_error(pain_error, paste("wn_date <", input$cmh_exp_after))]
modify$wn_pain[!is.na(pain), pain_N := .N, by = case_no]
modify$wn_pain[pain_N < 2 | is.na(pain_N),
  pain_error := aux$cat_error(pain_error, "less than 2 ovr_health")]
saved$wn_pain <- copy(modify$wn_pain)
modify$wn_pain <- modify$wn_pain[is.na(pain_error)]
modify$wn_pain[, Cs(cmh_error, dob, pain_error, cmh_effdt, cmh_expdt,
  team_effdt, team_expdt, hh_start, hh_end, L3_start, L3_end) := NULL]
modify$wn_pain <- modify$wn_pain[!is.na(cmh_team)]
modify$wn_pain[, days_idx := .GRP, by = list(case_no, wn_date)]
if (nrow(modify$wn_pain[duplicated(days_idx)]) > 0) {
  p_stop("modify$wn_pain has duplicates", modify$wn_pain[duplicated(days_idx)])
} ; modify$wn_pain[, days_idx := NULL]

# blood pressure --------------------------------------------------------------
modify$bp <- copy(vitals[, unique(.SD),
                  .SDcols = Cs(case_no, vt_date, diastolic, systolic)])
modify$bp <- modify$bp[vt_date >= input$cmh_exp_after]
modify$bp[!is.na(diastolic) | !is.na(systolic),
  `:=`(diastolic = as.integer(round(mean(diastolic, na.rm = TRUE))),
       systolic  = as.integer(round(mean(systolic,  na.rm = TRUE)))),
  by = .(case_no, vt_date)]
modify$bp <- unique(modify$bp)
modify$bp[, bp_error := NA_character_]
modify$bp[is.na(diastolic) & is.na(systolic),
  bp_error := aux$cat_error(bp_error, "missing both vp values")]
modify$bp[diastolic == 0 & systolic == 0,
          bp_error := aux$cat_error(bp_error, "missing both vp values")]
modify$bp[!is.na(diastolic) & is.na(systolic),
  bp_error := aux$cat_error(bp_error, "missing systolic with known diastolic")]
modify$bp[is.na(diastolic) & !is.na(systolic),
  bp_error := aux$cat_error(bp_error, "missing diastolic with known systolic")]
modify$bp[!is.na(diastolic) & !is.na(systolic), N_bp := .N, by = case_no]
modify$bp[N_bp < 2, bp_error :=
            aux$cat_error(bp_error, "less than 2 BP vt_dates")]
# systolic needs to be higher than diastolic
modify$bp[systolic <= diastolic, aux$cat_error(bp_error, "sys <= dia")]
modify$bp[systolic - diastolic < 20, aux$cat_error(bp_error, "sys-dia < 20")]
# fix records with multiple blood pressures on same day
modify$bp[, `:=`(diastolic = as.integer(round(mean(diastolic))),
   systolic = as.integer(round(mean(systolic)))), by = .(case_no, vt_date)]
modify$bp <- unique(modify$bp)
modify$bp[, vt_date2 := vt_date]

modify$bp[case_no == 265717 & systolic == 128 ]
modify$bp[case_no == 263649 & systolic == 120]
modify$bp[case_no == 263649 & vt_date == as.Date("2015-03-18")]

# add cmh_core cols
setkeyv(modify$cmh_core, c("case_no", "team_effdt", "team_expdt"))
modify$bp <- foverlaps(modify$bp,
        modify$cmh_core[, unique(.SD),
        .SDc = Cs(case_no, cmh_effdt, cmh_expdt, cmh_team, team_effdt,
                  team_expdt, days_cmh, cmh_error, dob, adm_pk)],
        by.x = c("case_no", "vt_date", "vt_date2"),
        by.y = c("case_no", "team_effdt", "team_expdt"))
# add hh_teams columns
setkey(modify$hh_teams, case_no, hh_start, hh_end)
modify$bp <- foverlaps(modify$bp,
  modify$hh_teams[, unique(.SD),
  .SDc = Cs(case_no, hh_team, hh_start, hh_end, days_hh, hh_error, hh_pk)],
  by.x = c("case_no", "vt_date", "vt_date2"),
  by.y = c("case_no", "hh_start", "hh_end"))
# add hh_level (nurse vs no nurse)
setkey(modify$hh_levels, case_no, L3_start, L3_end)
modify$bp <-
  foverlaps(modify$bp,
            modify$hh_levels[, unique(.SD),
                             .SDc = Cs(case_no, L3_start, L3_end, L3_pk)],
            by.x = c("case_no", "vt_date", "vt_date2"),
            by.y = c("case_no", "L3_start", "L3_end"))
modify$bp[, vt_date2 := NULL]
modify$bp[, vt_age :=
  floor(as.integer(vt_date-dob)/365.25), on = "case_no"]
modify$bp[vt_age < 18, bp_error := aux$cat_error(bp_error, "vt_age < 18")]
modify$bp[!is.na(cmh_error), bp_error := aux$cat_error(bp_error, cmh_error)]
modify$bp[!is.na(hh_error), bp_error := aux$cat_error(bp_error, hh_error)]
saved$bp <- copy(modify$bp)
modify$bp <- modify$bp[is.na(bp_error)]
# modify$bp[, hh_status := ifelse(is.na(hh_team), "CMH only", "HH")]
modify$bp[, Cs(cmh_effdt, cmh_expdt, hh_start, hh_end, cmh_error,
               hh_error, dob, L3_start, L3_end) := NULL]
modify$bp <- modify$bp[!is.na(cmh_team)]
# jama categories
modify$bp[, `:=`(sys_jama = aux$sys_jama(vt_age, systolic),
                 dia_jama = aux$dia_jama(vt_age, diastolic))]

# pulse -----------------------------------------------------------------------
modify$pulse <- copy(vitals[, unique(.SD), .SDcols = Cs(case_no, vt_date, pulse)])
modify$pulse <- modify$pulse[!is.na(pulse)]
modify$pulse[, pulse_error := NA_character_]
modify$pulse[, N_pulse := .N, by = case_no]
modify$pulse[N_pulse < 2, pulse_error :=
            aux$cat_error(pulse_error, "less than 2 BP vt_dates")]
# fix multiple pulses on same day
modify$pulse[, pulse := as.numeric(pulse)]
modify$pulse[, pulse := mean(pulse), by = list(case_no, vt_date)]
modify$pulse[, vt_date2 := vt_date]
# add cmh_core cols
setkeyv(modify$cmh_core, c("case_no", "team_effdt", "team_expdt"))
modify$pulse <- foverlaps(modify$pulse,
  modify$cmh_core[, unique(.SD),
  .SDc = Cs(case_no, cmh_effdt, cmh_expdt, cmh_team, team_effdt,
  team_expdt, days_cmh, cmh_error, dob, adm_pk)],
  by.x = c("case_no", "vt_date", "vt_date2"),
  by.y = c("case_no", "team_effdt", "team_expdt"))
# add hh_teams columns
setkey(modify$hh_teams, case_no, hh_start, hh_end)
modify$pulse <- foverlaps(modify$pulse,
  modify$hh_teams[, unique(.SD),
  .SDc = Cs(case_no, hh_team, hh_start, hh_end, days_hh, hh_error, hh_pk)],
                       by.x = c("case_no", "vt_date", "vt_date2"),
                       by.y = c("case_no", "hh_start", "hh_end"))
# add hh_level (nurse vs no nurse)
setkey(modify$hh_levels, case_no, L3_start, L3_end)
modify$pulse <-
  foverlaps(modify$pulse,
    modify$hh_levels[, unique(.SD),
    .SDc = Cs(case_no, L3_start, L3_end, L3_pk)],
    by.x = c("case_no", "vt_date", "vt_date2"),
    by.y = c("case_no", "L3_start", "L3_end"))
modify$pulse[, vt_date2 := NULL]
modify$pulse[, vt_age :=
            floor(as.integer(vt_date-dob)/365.25), on = "case_no"]
modify$pulse[vt_age < 18, pulse_error := aux$cat_error(pulse_error, "vt_age < 18")]
modify$pulse[!is.na(cmh_error),
             pulse_error := aux$cat_error(pulse_error, cmh_error)]
modify$pulse[!is.na(hh_error),
             pulse_error := aux$cat_error(pulse_error, hh_error)]
saved$pulse <- copy(modify$pulse)
modify$pulse <- modify$pulse[is.na(pulse_error)]
# modify$pulse[, hh_status := ifelse(is.na(hh_team), "CMH only", "HH")]
modify$pulse[, Cs(cmh_effdt, cmh_expdt, hh_start, hh_end, cmh_error,
               hh_error, dob, pulse_error, L3_start, L3_end) := NULL]
modify$pulse <- modify$pulse[!is.na(cmh_team)]

# respiration -----------------------------------------------------------------
modify$resp <- copy(vitals[, unique(.SD),
                    .SDcols = Cs(case_no, vt_date, respiration_rate)])
modify$resp <- modify$resp[!is.na(respiration_rate)]
modify$resp[, resp_error := NA_character_]
modify$resp[, N_resp := .N, by = case_no]
modify$resp[N_resp < 2, resp_error :=
               aux$cat_error(resp_error, "less than 2 BP vt_dates")]
modify$resp[, vt_date2 := vt_date]
# add cmh_core cols
setkeyv(modify$cmh_core, c("case_no", "team_effdt", "team_expdt"))
modify$resp <- foverlaps(modify$resp,
  modify$cmh_core[, unique(.SD),
  .SDc = Cs(case_no, cmh_effdt, cmh_expdt, cmh_team, team_effdt,
  team_expdt, days_cmh, cmh_error, dob, adm_pk)],
  by.x = c("case_no", "vt_date", "vt_date2"),
  by.y = c("case_no", "team_effdt", "team_expdt"))
# add hh_teams columns
setkey(modify$hh_teams, case_no, hh_start, hh_end)
modify$resp <- foverlaps(modify$resp,
  modify$hh_teams[, unique(.SD),
  .SDc = Cs(case_no, hh_team, hh_start, hh_end, days_hh, hh_error, hh_pk)],
  by.x = c("case_no", "vt_date", "vt_date2"),
  by.y = c("case_no", "hh_start", "hh_end"))
# add hh_level (nurse vs no nurse)
setkey(modify$hh_levels, case_no, L3_start, L3_end)
modify$resp <-
  foverlaps(modify$resp,
    modify$hh_levels[, unique(.SD),
   .SDc = Cs(case_no, L3_start, L3_end, L3_pk)],
    by.x = c("case_no", "vt_date", "vt_date2"),
    by.y = c("case_no", "L3_start", "L3_end"))
modify$resp[, vt_date2 := NULL]
modify$resp[, vt_age :=
               floor(as.integer(vt_date-dob)/365.25), on = "case_no"]
modify$resp[vt_age < 18, resp_error := aux$cat_error(resp_error, "vt_age < 18")]
modify$resp[!is.na(cmh_error), resp_error := aux$cat_error(resp_error, cmh_error)]
modify$resp[!is.na(hh_error), resp_error := aux$cat_error(resp_error, hh_error)]
saved$resp <- copy(modify$resp)
modify$resp <- modify$resp[is.na(resp_error)]
# modify$resp[, hh_status := ifelse(is.na(hh_team), "CMH only", "HH")]
modify$resp[, Cs(cmh_effdt, cmh_expdt, hh_start, hh_end, cmh_error,
                  hh_error, dob, resp_error, L3_start, L3_end) := NULL]
modify$resp <- modify$resp[!is.na(cmh_team)]

# lab values ------------------------------------------------------------------
labs <- copy(sql$output$labs)
labs[, lab_value := mean(lab_value), by = .(case_no, lab_name, lab_date)]
labs <- unique(labs)
labs[, lab_error := NA_character_]
labs[, N_resp := .N, by = case_no]
labs[N_resp < 2, lab_error :=
              aux$cat_error(lab_error, "less than 2 lab_dates")]
labs[, lab_date := as.Date(lab_date)]
labs[, lab_date2 := lab_date]
# add cmh_core cols
setkeyv(modify$cmh_core, c("case_no", "team_effdt", "team_expdt"))
labs <- foverlaps(labs,
  modify$cmh_core[, unique(.SD),
  .SDc = Cs(case_no, cmh_effdt, cmh_expdt, cmh_team, team_effdt,
  team_expdt, days_cmh, cmh_error, dob, adm_pk)],
  by.x = c("case_no", "lab_date", "lab_date2"),
  by.y = c("case_no", "team_effdt", "team_expdt"))
# add hh_teams columns
setkey(modify$hh_teams, case_no, hh_start, hh_end)
labs <- foverlaps(labs,
  modify$hh_teams[, unique(.SD),
  .SDc = Cs(case_no, hh_team, hh_start, hh_end, days_hh, hh_error, hh_pk)],
  by.x = c("case_no", "lab_date", "lab_date2"),
  by.y = c("case_no", "hh_start", "hh_end"))
# add hh_level (nurse vs no nurse)
setkey(modify$hh_levels, case_no, L3_start, L3_end)
labs <- foverlaps(labs,
  modify$hh_levels[, unique(.SD),
  .SDc = Cs(case_no, L3_start, L3_end, L3_pk)],
  by.x = c("case_no", "lab_date", "lab_date2"),
  by.y = c("case_no", "L3_start", "L3_end"))
labs[, lab_date2 := NULL]
labs[, lab_age :=
              floor(as.integer(lab_date-dob)/365.25), on = "case_no"]
labs[lab_age < 18, lab_error := aux$cat_error(lab_error, "lab_age < 18")]
labs[!is.na(cmh_error), lab_error := aux$cat_error(lab_error, cmh_error)]
labs[!is.na(hh_error), lab_error := aux$cat_error(lab_error, hh_error)]


saved$labs <- copy(labs)
labs <- labs[is.na(lab_error)]
labs[, Cs(cmh_effdt, cmh_expdt, hh_start, hh_end, cmh_error,
  hh_error, dob, lab_error, L3_start, L3_end) := NULL]
labs <- labs[!is.na(cmh_team)]
# various lab types
# cholesterol
modify$labs$chol <- labs[lab_name == "CHOL"]
# glucose
modify$labs$glu <- labs[lab_name == "GLU"]
# triglycerides
modify$labs$trig <- labs[lab_name == "TRIG"]
# A1C
modify$labs$a1c <- labs[lab_name == "A1C"]
# HDL
modify$labs$hdl <- labs[lab_name == "HDL"]
# LDL ---
modify$labs$ldl <- labs[lab_name == "LDL"]