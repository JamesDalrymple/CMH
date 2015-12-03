

output[bmi_date_diff >= 30 & min_dt_date == vt_date,
       list(first_bmi = bmi), by = list(case_no)]
output[bmi_date_diff >= 30 & max_dt_date == vt_date,
       list(last_bmi = bmi), by = list(case_no)]

output[case_no==10040]
# , by = c(hh_team)]
