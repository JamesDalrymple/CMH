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

# cc360_main
modify$cc360_main[, Consumer_Unique_ID := NULL]

# hh_detail -------------------------------------------------------------------
setf(modify$hh_detail , j = Cs(staff_eff, staff_exp), as.Date)
modify$hh_detail[hh_nurse == "N", Cs(staff_eff, staff_exp) :=
                       list(modify$adv_date, modify$adv_date)]
modify$hh_detail <- unique(modify$hh_detail)
modify$hh_detail[staff_eff > staff_exp, Cs(staff_eff, staff_exp) :=
                       list(modify$adv_date, modify$adv_date)]
modify$hh_detail <- overlap_combine(data = modify$hh_detail ,
  group_cols = Cs(case_no, team, team_effdt, team_expdt, dob, gender,
                  hh_nurse),
  start_col = "staff_eff", end_col = "staff_exp")
setnames(modify$hh_detail, Cs(start_date, end_date),
         Cs(nurse_start, nurse_end))
modify$hh_detail[, end_col := NULL]
modify$hh_detail[hh_nurse == "N", Cs(nurse_start, nurse_end) :=
                   list(as.Date(NA), as.Date(NA))]
setf(modify$hh_detail, j = Cs(MI, DD), stringi::stri_trim)
setf(modify$hh_detail, j = Cs(nurse_start, nurse_end), as.character)

modify$hh_detail[DD == "Not evaluated", DD := NA_character_]
modify$hh_detail[DD != "Yes" | is.na(DD), MI := "Yes"]
# hh_services
setf(modify$hh_services, j = Cs(doc_date), as.character)