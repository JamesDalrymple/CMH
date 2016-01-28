prepare <- new.env(parent = .GlobalEnv)

# community hospitalization ---------------------------------------------------
comm_hosp <- sql$output$comm_hosp
prepare$date_cols <- c("auth_eff", "auth_exp", "hosp_disc")
for (j in prepare$date_cols) {
  set(comm_hosp, j = j, value = date_convert((comm_hosp[[j]])))
}
# test for bad behavior: start needs to be less than auth_exp & hosp_disc
prepare$comm_hosp$bad_start <-
  comm_hosp[auth_eff > auth_exp | auth_eff > hosp_disc]
if (nrow(prepare$comm_hosp$bad_start) > 0) {
  p_stop("these records need fixing manually:", data.table(1:10))
         # prepare$comm_hosp$bad_start)
}




lapply(comm_hosp, class)






rm(j)