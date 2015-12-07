

analyze <- new.env(parent = .GlobalEnv)

analyze$bmi_dt <- copy(modify$output$bmi)
analyze$bp_dt <- copy(modify$output$bp)

# bmi -------------------------------------------------------------------------
# we would expect a decrease in BMI to be a good thing,
# except for special cases.
analyze$bmi_dt[first_bmi >= 18.5 & last_bmi >= 18.5,
       outcome_num := as.chr(factor(sign(last_bmi-first_bmi),
              levels = c("-1", "0", "1"),
              labels = c("improve", "no change", "decrease")))]
analyze$bmi_dt[, outcome_num := as.chr(outcome_num)]
analyze$bmi_dt[first_bmi >= 18.5 & last_bmi < 18.5,
       outcome_num := "decrease"]
analyze$bmi_dt[first_bmi < 18.5, outcome_num :=
         as.chr(factor(sign(last_bmi-first_bmi),
                levels = c("1", "0", "-1"),
                labels = c("improve", "no change", "decrease")))]
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
  round(improve/psum(decrease, improve, `no change`), 3)]
saved$print$bmi_wide_team[, rate_data :=
  round(psum(decrease, improve, `no change`)/eligible_cases, 3)]
# results: bmi by hh_team -----------------------------------------------------
analyze$bmi_hh <- merge(dcast(analyze$bmi_dt, hh_team ~ outcome_num,
      fun.aggregate = length, value.var = "case_no"),
      output[, list(eligible_cases = length(unique(case_no))),
             by = hh_team], all.x = TRUE, by = "hh_team")
analyze$bmi_hh[, rate_improve :=
  round(improve/psum(decrease, improve, `no change`), 3)]
analyze$bmi_hh[, rate_data :=
  round(psum(decrease, improve, `no change`)/eligible_cases, 3)]
# results: bmi by individual staff member -------------------------------------
analyze$bmi_hh_staff_ind <-
  merge(dcast(analyze$bmi_dt, hh_team + samhsa_staff ~ outcome_num,
        subset = .(hh_team == "Y"),
        fun.aggregate = length, value.var = "case_no"),
        output[hh_team == "Y", list(eligible_cases = length(unique(case_no))),
               by = list(hh_team, samhsa_staff)], all.x = TRUE,
        by = c("hh_team", "samhsa_staff"))
analyze$bmi_hh_staff_ind[, rate_improve :=
  round(improve/psum(decrease, improve, `no change`), 3)]
analyze$bmi_hh_staff_ind[, rate_data :=
  round(psum(decrease, improve, `no change`)/eligible_cases, 3)]

# blood pressure --------------------------------------------------------------

# left off here 12/7/2015
analyze$bp_dt[]

# wellness note ---------------------------------------------------------------



