analyze <- new.env(parent = .GlobalEnv)

analyze$bmi_dt <- copy(modify$output$bmi)
analyze$bp_dt <- copy(modify$output$bp)

# bmi -------------------------------------------------------------------------
# we would expect a decrease in BMI to be a good thing,
# except for special cases.
analyze$bmi_dt[first_bmi >= 18.5 & last_bmi >= 18.5,
       outcome_num := as.chr(factor(sign(last_bmi-first_bmi),
              levels = c("-1", "0", "1"),
              labels = c("improved", "maintained", "decreased")))]
analyze$bmi_dt[, outcome_num := as.chr(outcome_num)]
analyze$bmi_dt[first_bmi >= 18.5 & last_bmi < 18.5,
       outcome_num := "decreased"]
analyze$bmi_dt[first_bmi < 18.5, outcome_num :=
         as.chr(factor(sign(last_bmi-first_bmi),
                levels = c("1", "0", "-1"),
                labels = c("improved", "maintained", "decreased")))]
# results: bmi by team --------------------------------------------------------
analyze$bmi_long_team <-
  analyze$bmi_dt[, list(num_cases = length(unique(case_no))),
                 by = list(team, outcome_num)]
saved$print$bmi_wide_team <-
  merge(dcast(analyze$bmi_long_team, fill= 0, drop = FALSE,
      team ~ outcome_num, value.var = "num_cases"),
      output[, list(eligible_cases = length(unique(case_no))),
             by = team], all.x = TRUE, by = "team")
saved$print$bmi_wide_team[, rate_improve :=
  round(improved/psum(decreased, improved, maintained), 3)]
saved$print$bmi_wide_team[, `rate_imp/maint` :=
  round(psum(improved, maintained) / psum(decreased, improved, maintained), 3)]

saved$print$bmi_wide_team[, rate_data :=
  round(psum(decreased, improved, maintained)/eligible_cases, 3)]
# results: bmi by hh_team -----------------------------------------------------
analyze$bmi_hh <- merge(dcast(analyze$bmi_dt, hh_team ~ outcome_num,
      fun.aggregate = length, value.var = "case_no"),
      output[, list(eligible_cases = length(unique(case_no))),
             by = hh_team], all.x = TRUE, by = "hh_team")
analyze$bmi_hh[, rate_improve :=
  round(improved/psum(decreased, improved, maintained), 3)]
analyze$bmi_hh[, `rate_imp/maint` :=
  round(psum(improved, maintained) / psum(decreased, improved, maintained), 3)]
analyze$bmi_hh[, rate_data :=
  round(psum(decreased, improved, maintained)/eligible_cases, 3)]
# results: bmi by individual staff member -------------------------------------
analyze$bmi_hh_staff_ind <-
  merge(dcast(analyze$bmi_dt, hh_team + samhsa_staff ~ outcome_num,
        subset = .(hh_team == "Y"),
        fun.aggregate = length, value.var = "case_no"),
        output[hh_team == "Y", list(eligible_cases = length(unique(case_no))),
               by = list(hh_team, samhsa_staff)], all.x = TRUE,
        by = c("hh_team", "samhsa_staff"))
analyze$bmi_hh_staff_ind[, rate_improve :=
  round(improved/psum(decreased, improved, maintained), 3)]
analyze$bmi_hh_staff_ind[, rate_data :=
  round(psum(decreased, improved, maintained)/eligible_cases, 3)]

# blood pressure --------------------------------------------------------------
analyze$bp_sys_detail <-
  modify$output$bp[, list(num_cases = length(unique(case_no))),
                   keyby = list(hh_team, detail_sys_status)]
analyze$bp_dia_detail <-
  modify$output$bp[, list(num_cases = length(unique(case_no))),
                   keyby = list(hh_team, detail_dia_status)]
analyze$bp_sys_dist <-
  modify$output$bp[, list(num_cases = length(unique(case_no))),
                   keyby = list(hh_team, sys_dist_status)]
analyze$bp_dia_dist <-
  modify$output$bp[, list(num_cases = length(unique(case_no))),
                   keyby = list(hh_team, dia_dist_status)]
analyze$bp_hh_team <-
  output[age >= 18,
    list(eligible_cases = length(unique(case_no))), by = hh_team]

# dcast data for human readability
saved$bp_sys_detail <-
  dcast(analyze$bp_sys_detail,
        hh_team ~ detail_sys_status, value.var = "num_cases")
saved$bp_dia_detail <-
  dcast(analyze$bp_dia_detail,
        hh_team ~ detail_dia_status, value.var = "num_cases")
saved$bp_sys_dist <-
  dcast(analyze$bp_sys_dist,
        hh_team ~ sys_dist_status, value.var = "num_cases")
saved$bp_dia_dist <-
  dcast(analyze$bp_dia_dist,
        hh_team ~ dia_dist_status, value.var = "num_cases")
# add hh_team totals
saved$bp_sys_detail <- merge(saved$bp_sys_detail,
                             analyze$bp_hh_team,
                             by = "hh_team",
                             all = TRUE)
saved$bp_dia_detail <- merge(saved$bp_dia_detail,
                             analyze$bp_hh_team,
                             by = "hh_team",
                             all = TRUE)
saved$bp_sys_dist <- merge(saved$bp_sys_dist,
                             analyze$bp_hh_team,
                             by = "hh_team",
                             all = TRUE)
saved$bp_dia_dist <- merge(saved$bp_dia_dist,
                           analyze$bp_hh_team,
                           by = "hh_team",
                           all = TRUE)
# add rate_improved and rate improved/maintained
saved$bp_sys_detail[, rate_improved :=
  round(improved / psum(decreased, improved, maintained), 3)]
saved$bp_sys_detail[, `rate_imp/maint` :=
  round(psum(improved, maintained) / psum(decreased, improved, maintained), 3)]
saved$bp_dia_detail[, rate_improved :=
  round(improved / psum(decreased, improved, maintained), 3)]
saved$bp_dia_detail[, `rate_imp/maint` :=
  round(psum(improved, maintained) / psum(decreased, improved, maintained), 3)]
saved$bp_sys_dist[, rate_improved :=
  round(improved / psum(decreased, improved, maintained), 3)]
saved$bp_sys_dist[, `rate_imp/maint` :=
  round(psum(improved, maintained) / psum(decreased, improved, maintained), 3)]
saved$bp_dia_dist[, rate_improved :=
  round(improved / psum(decreased, improved, maintained), 3)]
saved$bp_dia_dist[, `rate_imp/maint` :=
  round(psum(improved, maintained) / psum(decreased, improved, maintained), 3)]

# add rate_data
saved$bp_sys_detail[,
  rate_data := round(psum(decreased, improved, maintained)/eligible_cases, 3)]
saved$bp_dia_detail[,
  rate_data := round(psum(decreased, improved, maintained)/eligible_cases, 3)]
saved$bp_sys_dist[,
  rate_data := round(psum(decreased, improved, maintained)/eligible_cases, 3)]
saved$bp_dia_dist[,
  rate_data := round(psum(decreased, improved, maintained)/eligible_cases, 3)]

# wellness note ---------------------------------------------------------------



