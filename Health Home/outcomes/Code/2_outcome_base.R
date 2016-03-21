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
cmh_adm[, dob := as.Date(dob)]
cmh_adm[, cur_age := as.num(round((Sys.Date() - dob)/365.25, 4))]
# create secondary team for possibly comparison with Jessica's work
cmh_adm[like(team, "PBHCI"), secondary_team := "SAMHSA PBHCI"]
cmh_adm[like(team, "Health Home"), secondary_team := "Health Home"]
cmh_adm[like(team, "Pilot Disease"), secondary_team := "Disease Mgmt"]

# identifying Health Home staff (nurse and non-nurse too)
modify$hh_staff <- cmh_adm[current_sup == "Hagaman, Brandie" &
  staff_type == "SAMHSA Staff", unique(assigned_staff)]
modify$hh_nurse <- cmh_adm[current_sup == "Hagaman, Brandie" &
  assigned_staff %in% modify$hh_staff & staff_type == "Registered Nurse",
  unique(assigned_staff)]
cmh_adm[, team := NULL]
cmh_adm[cmh_team == "OBRA", cmh_team := "non-core CMH"]
cmh_adm[cmh_team == "PORT", cmh_team := "non-core CMH"]

modify$date_cols <-
  Cs(cmh_effdt, cmh_expdt, team_effdt, team_expdt, staff_eff, staff_exp)
for (j in modify$date_cols) {
  set(cmh_adm, j = j, value = as.Date(cmh_adm[[j]]))
}
modify$cmh_core <- cmh_adm[, unique(.SD), .SDcols = Cs(case_no, cmh_effdt,
  cmh_expdt, team_effdt, team_expdt, cmh_team, secondary_team, dob)]
modify$cmh_core[, cur_age := round(as.int(Sys.Date()-as.Date(dob))/365.25, 4)]
# modify$cmh_core[, cn_cmh_eff := .GRP, by = list(case_no, cmh_effdt)]
# overlap_fix_dt requires that NO blanks exist in end dates
modify$cmh_core[is.na(cmh_expdt), cmh_expdt := Sys.Date() + 999]
modify$cmh_core[is.na(team_expdt), team_expdt := Sys.Date() + 999]

# remove 11 very odd records: overlap_fix_dt requires start to be before end
modify$cmh_core <- modify$cmh_core[cmh_effdt <= cmh_expdt]
modify$cmh_core <- modify$cmh_core[team_effdt <= team_expdt]


# fixes MOST but not all records
overlap_fix_dt(overlap_dt = modify$cmh_core,
               id_cols = c("case_no", "cmh_effdt", "cmh_team"),
               start_col = "team_effdt", expire_col = "team_expdt",
               disc_col = "team_expdt",
               overlap_int = 1L, range_class = "Date")
modify$cmh_core <-
  modify$cmh_core[, list(cmh_effdt = min(cmh_effdt),
                           cmh_expdt = max(cmh_expdt),
                           team_effdt = min(team_effdt),
                           team_expdt = max(team_expdt)),
                    by = list(case_no, cmh_team, cur_age, overlap_id)]

test <- cmh_adm[case_no == 11660 & cmh_team == "ACT", unique(.SD),
        .SDcols = Cs(case_no, cmh_effdt, cmh_expdt, team_effdt, team_expdt, cmh_team)]
modify$cmh_core[case_no == 11660 & cmh_team == "ACT"]
modify$cmh_core2[case_no == 11660 & cmh_team == "ACT"]


modify$cmh_core2 <-
  modify$cmh_core2[cmh_team %nin% c("unknown", "non-core CMH", "non-CMH")]
modify$cmh_core2[cmh_priority_dt,
  priority := i.priority, on = c(cmh_team = "team")]
modify$cmh_core2[, min_priority := as.int(min(priority, na.rm = TRUE)),
  by = list(case_no, cmh_effdt, overlap_id)]
modify$cmh_core2 <- modify$cmh_core2[min_priority == priority]
modify$cmh_core2[, Cs(priority, min_priority) := NULL]
modify$cmh_core2[, overlap_id := NULL]

# case_no 10869 is an example of someone who has an ACT adm inside of MI Adult
# admission. We would need to rewrite the admission record to be able to join
# to other datasets via data.table::foverlaps

# find most recent CMH core team ----------------------------------------------
setorder(modify$cmh_core2, case_no, -cmh_effdt, -team_effdt)
modify$cmh_core2[, team_order := seq.int(.N), by = list(case_no, cmh_effdt)]
modify$cmh_core2[, adm_order := as.integer(as.factor(.GRP)),
                    by = list(case_no, cmh_effdt)]
modify$cmh_core2[, case_grp := seq(.N), by = case_no]

modify$last_core <-
  copy(modify$cmh_core2[team_order == 1 & adm_order == 1 & case_grp == 1])
modify$last_core[, Cs(team_order, adm_order) := NULL]
modify$last_core <-
  modify$last_core[cmh_team %in%
  c("DD", "MI", "Child", "ACT", "Child HB", "Access")]
modify$last_core[, days_core_team :=
  as.int(pmin(team_expdt, input$end_date)-team_effdt)]
modify$last_core[,
  days_cmh := as.int(pmin(cmh_expdt, input$end_date)-cmh_effdt)]

# find HH consumers open for a year or more -----------------------------------
modify$hh_teams <-
  modify$cmh_core[secondary_team == "Health Home",
  list(cmh_effdt = min(cmh_effdt),
       cmh_expdt = max(cmh_expdt),
       hh_effdt = min(team_effdt),
       hh_expdt = max(team_expdt)),
  by = list(case_no, cur_age, overlap_id)]
# there are 5 cases that admitted, discharged, then readmitted to CMH & HH
# NOTE: I add 999 to NA discharges to fix ovrlp; future expdt are due to that
# Q: Should we add total HH days/person if disc+readmit CMH/HH (i.e. 5 mon gap)

# setorder(modify$hh_teams, case_no, -cmh_effdt)
# modify$hh_teams[, adm_grp := seq(.N), by = list(case_no)]
modify$hh_teams[, days_cmh :=
  as.int(pmin(cmh_expdt, input$end_date) - cmh_effdt)]
modify$hh_teams[, days_hh :=
  as.int(pmin(hh_expdt, input$end_date) - hh_effdt)]
# requiring people to be X days open CMH/HH
modify$hh_teams[, error := NA_character_]
modify$hh_teams[days_cmh < input$days_req_cmh,
  error := aux$cat_error(old = error, new = "cmh_days_req not met")]
modify$hh_teams[days_hh < input$days_req_hh, error :=
  aux$cat_error(old = error, new = "hh_days_req not met")]
modify$hh_teams[, Cs(overlap_id) := NULL]
modify$hh_teams[cur_age < 18, error := aux$cat_error(error, "age < 18")]
modify$hh$eligible <- modify$hh_teams[is.na(error), length(unique(case_no))]
modify$hh$ineligible <- modify$hh_teams[!is.na(error), length(unique(case_no))]
saved$hh_summary <-
  data.table(hh_eliglible = modify$hh$eligible,
             hh_ineligible = modify$hh$ineligible)

# BMI -------------------------------------------------------------------------
modify$bmi <- vitals[!is.na(weight) | !is.na(height_feet) |
  !is.na(height_inches), unique(.SD),
  .SDcols = Cs(case_no, vt_date, weight, height_feet, height_inches)]
modify$bmi[cmh_adm,
           vt_age := floor(as.int(vt_date - i.dob)/365.25), on = "case_no"]
# remove blanks without losing information
# i.e. weight/height were recorded same day, separately
modify$bmi[, Cs(weight, height_feet, height_inches) :=
             list(as.int(median(weight, na.rm = TRUE)),
                  as.int(median(height_feet, na.rm = TRUE)),
                  as.int(median(height_inches, na.rm = TRUE))),
           by = list(case_no, vt_date)]
modify$bmi <- unique(modify$bmi)

setkey(modify$cmh_core2, case_no, team_effdt, team_expdt)
modify$bmi[, vt_date2 := vt_date]
modify$bmi <-
  foverlaps(modify$bmi,
            modify$cmh_core2[, unique(.SD),
                            .SDc = Cs(case_no, cmh_team, team_effdt, team_expdt)],
            by.x = c("case_no", "vt_date", "vt_date2"),
            by.y = c("case_no", "team_effdt", "team_expdt"))
modify$bmi[, vt_date2 := NULL]

modify$bmi[cmh_priority_dt, cmh_priority := i.priority, on = c(cmh_team = "team")]
modify$bmi[, min_priority := as.int(min(cmh_priority, na.rm = TRUE)), by = list(case_no, vt_date)]
modify$bmi <- modify$bmi[min_priority == cmh_priority | is.na(min_priority)]

modify$bmi[, rec_id := .GRP, by = list(case_no, vt_date)]
modify$bmi[rec_id == 4299]
modify$bmi[duplicated(rec_id)]
modify$bmi[, unique(.SD), .SDcols = Cs(case_no, vt_date)]

cmh_adm[case_no == 11660, unique(.SD), .SDcols =
  Cs(case_no, cmh_effdt, cmh_expdt, team_effdt, team_expdt, cmh_team)]

modify$cmh_core2[case_no == 11660]


modify$bmi[cmh_priority != min_priority]
modify$bmi[case_no == 11091 & vt_date == as.Date("2015-12-8")]

modify$bmi[modify$last_core,
  Cs(last_cmh_team, days_core_team, days_cmh, cmh_effdt, cmh_expdt) :=
  list(i.cmh_team, i.days_core_team, i.days_cmh, i.cmh_effdt,
       i.cmh_expdt), on = c("case_no")]

modify$bmi[, error := NA_character_]
modify$bmi[age_at_vt < 18 | is.na(age_at_vt), error :=
  aux$cat_error(error, "age_at_vt < 18")]
# combine inchest/feet
modify$bmi[, med_in :=
             as.int(median(height_inches, na.rm = TRUE)), by = list(case_no)]
modify$bmi[, med_ft:= as.int(median(height_feet, na.rm = TRUE)), by = case_no]

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
  as.int(vt_date-shift(vt_date)), by = list(case_no)]
# for data verification later
modify$bmi[is.na(calc_bmi), error :=
           aux$cat_error(error, "bmi not calculable")]
modify$bmi[cmh_expdt < input$cmh_exp_after,
  error := aux$cat_error(error, paste("bmi <", input$cmh_exp_after))]
modify$bmi[days_cmh < input$days_req_cmh,
  error := aux$cat_error(error, paste("days_cmh_req <", input$cmh_exp_after))]
modify$bmi[is.na(last_cmh_team), last_cmh_team := "never core CMH"]
modify$bmi[, vt_date2 := vt_date]
setkey(modify$bmi, case_no, vt_date, vt_date2)
setkey(modify$hh_teams, case_no, hh_effdt, hh_expdt)

modify$bmi <-
  foverlaps(modify$bmi,
            modify$hh_teams[, unique(.SD),
              .SDc = Cs(case_no, hh_effdt, hh_expdt, days_hh)],
          by.x = c("case_no", "vt_date", "vt_date2"),
          by.y = c("case_no", "hh_effdt", "hh_expdt"))
modify$bmi[, vt_date2 := NULL]
modify$bmi[calc_bmi < 10, error := aux$cat_error(error,
  "BMI suspiciously low due to weight or height error")]
modify$bmi[calc_bmi > 95]
modify$bmi[, Cs(med_ht, weight, height_feet, height_inches,
                cur_age, age_at_vt) := NULL]
modify$bmi[, num_bmi := .N, by = case_no]
modify$bmi[is.na(error), num_bmi_no_error := .N, by = case_no]
# saving copy of pre-analysis data for review
saved$bmi <- copy(modify$bmi)
# remove all errors
modify$bmi <- modify$bmi[is.na(error)]
# at least 2 BMI's required
modify$bmi <- modify$bmi[num_bmi_no_error > 1]
modify$bmi[, Cs(num_bmi, num_bmi_no_error, days_hh, cmh_effdt, cmh_expdt, error) := NULL]
modify$bmi[is.na(hh_effdt) & hh_ever == "Y"]
modify$bmi[, hh_diff_vt := as.int(vt_date - hh_effdt)]
modify$bmi[, hh_min_diff := min(hh_diff_vt, na.rm = TRUE)]
modify$bmi[hh_min_diff == hh_diff_vt, hh_min_vt := vt_date]
modify$bmi[, Cs(hh_diff_vt, hh_min_diff) := NULL]
if (length(modify$bmi[!is.na(hh_effdt), unique(.SD), .SDcols = Cs(case_no, hh_effdt)][
  duplicated(case_no)][, length(case_no)]) > 10 ){
  p_warn("FYI: there are now 11+ cases that were HH and disc. then readmitted.")
}
# PRE/POST BMI ----------------------------------------------------------------
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
  as.int(pmax(num_bmi_no_error-1, 0)))]

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

# wellness note: overall health -----------------------------------------------
# wn <- copy(sql$output$wn)
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
wn[, age_at_wn := cur_age - as.int(Sys.Date()-wn_date)/365.25]
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
               round(as.int(vt_date-i.dob)/365.25, 4), on = "case_no"]
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

