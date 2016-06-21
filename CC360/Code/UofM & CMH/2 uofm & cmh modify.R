modify <- new.env()

# # cc360 prescriptions ----------------------------------------------------------
# modify$cc360_med <- copy(sql$output$cc360_med)
# setf(modify$cc360_med, j = Cs(Service_From_Date, Service_To_Date), as.Date)
# modify$cc360_med[, uofm := grepl(x = Billing_Provider_Name,
#   pattern = "University of Michigan", ignore.case = TRUE)]

# cc360_main ------------------------------------------------------------------
modify$cc360_main <- copy(sql$output$cc360_main)
modify$cc360_main[, uofm := grepl(x = Billing_Provider_Name,
  pattern = "University of Michigan", ignore.case = TRUE)]
modify$cc360_main[is.na(Place_of_Service), Place_of_Service := Facility_Type]
modify$cc360_main[, group := .GRP,
  by = list(case_no, Procedure_Code, Revenue_Code,
  Place_of_Service, Billing_Provider_Name,Service_From_Date, Service_To_Date)]
# remove our CMH services
modify$cc360_main <-
  modify$cc360_main[Billing_Provider_Name != "COUNTY OF WASHTENAW"]

# diagnoses -------------------------------------------------------------------
modify$dx_past <- copy(sql$output$dx_past)
modify$dx_present <- copy(sql$output$dx_present)
modify$dx_dt <- copy(sql$output$dx_dt)

# dx_dt
setf(modify$dx_dt , j = Cs(icd9_code, icd9_desc),
     value = gsub, pattern = "\\?-", replacement = "")
setf(modify$dx_dt, j = Cs(icd9_code, icd9_desc),
     value = stri_trim)
setnames(modify$dx_dt, names(modify$dx_dt),
         tolower(names(modify$dx_dt)))
setf(modify$dx_dt, j = "status_dt", as.Date)

modify$dx_dt[, idx := .GRP, by = list(case_no, status_dt) ]

# fix duplicated data ---------------------------------------------------------
# modify$dx_dt[duplicated(idx)]
# manually removing duplicate record
modify$dx_dt <-
  modify$dx_dt[!(idx == 50729 & icd9_code == 311 & status_dt == "2011-10-27")]
modify$dx_dt[, idx := NULL]

# dx_past
setnames(modify$dx_past, names(modify$dx_past),
         tolower(names(modify$dx_past)))
setf(modify$dx_past, j = "max_status_dt", as.Date)

modify$dx_past[, uofm := grepl(x = billing_provider_name,
  pattern = "University of Michigan", ignore.case = TRUE)]
modify$dx_past <-
  modify$dx_past[, unique(.SD),
    .SDcols = Cs(case_no, service_from_date, max_status_dt)]

modify$missing_dx <- data.table(
  total_dx_records = nrow(modify$dx_past),
  missing_dx_records = nrow(modify$dx_past[is.na(max_status_dt)]),
  num_cases_missing_dx =
    modify$dx_past[is.na(max_status_dt), length(unique(case_no))])

# dx_present
modify$dx_present
setnames(modify$dx_present, names(modify$dx_present),
         tolower(names(modify$dx_present)))
setf(modify$dx_present, j = "max_status_dt", as.Date)
setf(modify$dx_present , j = Cs(icd_code, icd_desc),
     value = gsub, pattern = "\\?-", replacement = "")
setf(modify$dx_present, j = Cs(icd_code, icd_desc),
     value = stri_trim)

modify$dx_present[, idx := .GRP, by = list(case_no, max_status_dt)]
setorder(modify$dx_present, case_no, max_status_dt, -icd9_or_10)
modify$dx_present[, keep := seq_along(case_no), by = idx]
modify$dx_present <- modify$dx_present[keep == 1]

# adding in dx_dt to dx_past ---
modify$dx_dt <- modify$dx_dt[, unique(.SD),
  .SDcols = Cs(case_no, icd9_code, icd9_desc, status_dt)]
# modify$dx_dt[, idx := .GRP, by = list(case_no, status_dt)]
# modify$dx_past[, idx := .GRP, by = list(case_no, max_status_dt)]
modify$dx_past[modify$dx_dt, Cs(icd9_code, icd9_desc) :=
  list(i.icd9_code, i.icd9_desc),
  on = c("max_status_dt" = "status_dt", "case_no" = "case_no")]

# modify$dx_past[icd9_desc == "missing icd history"]
# icd_code, icd9_or_10, icd_desc

modify$dx_past[modify$dx_present,
  Cs(cur_icd_code, cur_icd9_or_10, cur_icd_desc) :=
  list(i.icd_code, i.icd9_or_10, i.icd_desc), on = c("case_no" = "case_no")]
modify$dx_past[is.na(max_status_dt), Cs(icd9_code,
  icd9_desc) := list(cur_icd_code, cur_icd_desc)]
modify$dx_past[is.na(icd9_desc), icd9_desc := "missing icd history"]