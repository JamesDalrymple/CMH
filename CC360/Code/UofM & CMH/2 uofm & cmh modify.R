modify <- new.env()

# cc360 prescritions ----------------------------------------------------------
modify$cc360_med <- copy(sql$output$cc360_med)
setf(modify$cc360_med, j = Cs(Service_From_Date, Service_To_Date), as.Date)
modify$cc360_med[, uofm := grepl(x = Billing_Provider_Name,
  pattern = "University of Michigan", ignore.case = TRUE)]

# cc360_main ------------------------------------------------------------------
modify$cc360_main <- copy(sql$output$cc360_main)
modify$cc360_main[, uofm := grepl(x = Billing_Provider_Name,
  pattern = "University of Michigan", ignore.case = TRUE)]
modify$cc360_main[is.na(Place_of_Service), Place_of_Service := Facility_Type]
modify$cc360_main[, group := .GRP,
  by = list(case_no, Procedure_Code, Revenue_Code,
  Place_of_Service, Billing_Provider_Name,Service_From_Date, Service_To_Date)]

# diagnoses -------------------------------------------------------------------
modify$diagnoses <- copy(sql$output$diagnoses)
setf(modify$diagnoses , j = Cs(icd9_code, icd9_desc),
     value = gsub, pattern = "\\?-", replacement = "")
setf(modify$diagnoses, j = Cs(icd9_code, icd9_desc),
     value = stri_trim)
setnames(modify$diagnoses, names(modify$diagnoses),
         tolower(names(modify$diagnoses)))