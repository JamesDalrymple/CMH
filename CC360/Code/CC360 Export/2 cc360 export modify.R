modify <- new.env()
# save all data to modify env ----
modify$hh_detail <- copy(sql$output$hh_detail)
modify$hh_services <- copy(sql$output$hh_services)
modify$cc360_main <- copy(sql$output$cc360_main)
modify$cc360_med <- copy(sql$output$cc360_med)
modify$hh_bucket <- copy(sql$output$hh_bucket)
modify$tiers <- copy(sql$tier_dt)
# miscellaneous ---
modify$adv_date <- Sys.Date() + 999

# hh_detail -------------------------------------------------------------------
setf(modify$hh_detail , j = Cs(staff_eff, staff_exp), as.Date)
modify$hh_detail[hh_nurse == "N", Cs(staff_eff, staff_exp) :=
                       list(modify$adv_date, modify$adv_date)]
modify$hh_detail[staff_eff > staff_exp, Cs(staff_eff, staff_exp) :=
                       list(modify$adv_date, modify$adv_date)]
modify$hh_detail <- overlap_combine(data = modify$hh_detail ,
  group_cols = Cs(case_no, team, team_effdt, team_expdt, dob, gender,
                  hh_nurse),
  start_col = "staff_eff", end_col = "staff_exp")



