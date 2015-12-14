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
saved$bmi_team <-
  merge(dcast(analyze$bmi_long_team, fill= 0, drop = FALSE,
      team ~ outcome_num, value.var = "num_cases"),
      output[, list(`eligible cases` = length(unique(case_no))),
             by = team], all.x = TRUE, by = "team")
saved$bmi_team[, `rate improved` :=
  round(improved/psum(decreased, improved, maintained), 3)]
saved$bmi_team[, `rate_imp/maint` :=
  round(psum(improved, maintained) / psum(decreased, improved, maintained), 3)]

saved$bmi_team[, `rate data` :=
  round(psum(decreased, improved, maintained)/`eligible cases`, 3)]
# results: bmi by hh_team -----------------------------------------------------
analyze$bmi_hh <- merge(dcast(analyze$bmi_dt, hh_team ~ outcome_num,
      fun.aggregate = length, value.var = "case_no"),
      output[, list(`eligible cases` = length(unique(case_no))),
             by = hh_team], all.x = TRUE, by = "hh_team")
analyze$bmi_hh[, `rate improved` :=
  round(improved/psum(decreased, improved, maintained), 3)]
analyze$bmi_hh[, `rate_imp/maint` :=
  round(psum(improved, maintained) / psum(decreased, improved, maintained), 3)]
analyze$bmi_hh[, `rate data` :=
  round(psum(decreased, improved, maintained)/`eligible cases`, 3)]
# results: bmi by individual staff member -------------------------------------
analyze$bmi_hh_staff_ind <-
  merge(dcast(analyze$bmi_dt, hh_team + samhsa_staff ~ outcome_num,
        subset = .(hh_team == "Y"),
        fun.aggregate = length, value.var = "case_no"),
        output[hh_team == "Y", list(`eligible cases` = length(unique(case_no))),
               by = list(hh_team, samhsa_staff)], all.x = TRUE,
        by = c("hh_team", "samhsa_staff"))
analyze$bmi_hh_staff_ind[, `rate improved` :=
  round(improved/psum(decreased, improved, maintained), 3)]
analyze$bmi_hh_staff_ind[, `rate data` :=
  round(psum(decreased, improved, maintained)/`eligible cases`, 3)]

# blood pressure --------------------------------------------------------------
analyze$bp_dw_sys <-
  dcast(modify$output$bp, hh_team ~ dw_sys_status,
      fun.aggregate = function(x) length(unique(x)),
      value.var = "case_no")
analyze$bp_dw_dia <-
  dcast(modify$output$bp, hh_team ~ dw_dia_status,
        fun.aggregate = function(x) length(unique(x)),
        value.var = "case_no")
analyze$bp_jama_sys <-
  dcast(modify$output$bp, hh_team ~ jama_sys_status,
        fun.aggregate = function(x) length(unique(x)),
        value.var = "case_no")
analyze$bp_jama_dia <-
  dcast(modify$output$bp, hh_team ~ jama_dia_status,
        fun.aggregate = function(x) length(unique(x)),
        value.var = "case_no")
analyze$bp_hh_team <- output[age >= 18,
  list(`eligible cases` = length(unique(case_no))), by = hh_team]
# add hh_team totals
analyze$bp_jama_sys <- merge(analyze$bp_jama_sys,
                             analyze$bp_hh_team,
                             by = "hh_team",
                             all = TRUE)
analyze$bp_jama_dia <- merge(analyze$bp_jama_dia,
                             analyze$bp_hh_team,
                             by = "hh_team",
                             all = TRUE)
analyze$bp_dw_sys <- merge(analyze$bp_dw_sys,
                             analyze$bp_hh_team,
                             by = "hh_team",
                             all = TRUE)
analyze$bp_dw_dia <- merge(analyze$bp_dw_dia,
                           analyze$bp_hh_team,
                           by = "hh_team",
                           all = TRUE)
# add rate_improved and rate improved/maintained
# jama sys
analyze$bp_jama_sys[, `rate improved` :=
  round(improved / psum(decreased, improved, maintained), 3)]
analyze$bp_jama_sys[, `rate imp/maint` :=
  round(psum(improved, maintained)/psum(decreased, improved, maintained), 3)]
analyze$bp_jama_sys[, `rate decreased` := 1 - `rate imp/maint`]
# jama dia
analyze$bp_jama_dia[, `rate improved` :=
  round(improved / psum(decreased, improved, maintained), 3)]
analyze$bp_jama_dia[, `rate imp/maint` :=
  round(psum(improved, maintained)/psum(decreased, improved, maintained), 3)]
analyze$bp_jama_dia[, `rate decreased` := 1 - `rate imp/maint`]
# dw sys
analyze$bp_dw_sys[, `rate improved` :=
  round(improved / psum(decreased, improved, maintained), 3)]
analyze$bp_dw_sys[, `rate imp/maint` :=
  round(psum(improved, maintained)/psum(decreased, improved, maintained), 3)]
analyze$bp_dw_sys[, `rate decreased` := 1 - `rate imp/maint`]
# dw dia
analyze$bp_dw_dia[, `rate improved` :=
                    round(improved / psum(decreased, improved, maintained), 3)]
analyze$bp_dw_dia[, `rate imp/maint` :=
  round(psum(improved, maintained)/psum(decreased, improved, maintained), 3)]
analyze$bp_dw_dia[, `rate decreased` := 1 - `rate imp/maint`]

# add rate_data
analyze$bp_dw_sys[,
  `rate data` := round(psum(decreased, improved, maintained)/`eligible cases`, 3)]
analyze$bp_dw_dia[,
  `rate data` := round(psum(decreased, improved, maintained)/`eligible cases`, 3)]
analyze$bp_jama_sys[,
  `rate data` := round(psum(decreased, improved, maintained)/`eligible cases`, 3)]
analyze$bp_jama_dia[,
  `rate data` := round(psum(decreased, improved, maintained)/`eligible cases`, 3)]

# wellness note ---------------------------------------------------------------



