modify <- new.env()


setf(sql$output$hh_detail, j = Cs(staff_eff, staff_exp), as.Date)
modify$hh_detail <- overlap_combine(data = sql$output$hh_detail,
  group_cols = Cs(case_no, team, team_effdt, team_expdt, dob, gender, hh_nurse),
  start_col = "staff_eff", end_col = "staff_exp")

modify$hh_services <- copy(sql$output$hh_services)
modify$cc360_main <- copy(sql$output$cc360_main)
modify$cc360_med <- copy(sql$output$cc360_med)
modify$hh_bucket <- copy(sql$output$hh_bucket)



