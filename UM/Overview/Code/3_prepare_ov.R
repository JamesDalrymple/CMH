prepare <- new.env(parent = .GlobalEnv)

# community hospitalization ---------------------------------------------------
comm_hosp <- copy(sql$output$comm_hosp)
prepare$date_cols <- c("auth_eff", "auth_exp", "hosp_disc")
for (j in prepare$date_cols) {
  set(comm_hosp, j = j, value = date_convert((comm_hosp[[j]])))
}
# manually fixing cases ---
comm_hosp[case_no == 1157442 & auth_eff == "2015-12-28",
          c("auth_exp", "auth_days") := list(as.Date("2015-12-31"), 4)]
comm_hosp[case_no == 1136667 & auth_eff == "2013-10-26",
          c("auth_exp", "auth_days") := list(as.Date("2013-10-27"), 2)]
# data integrity check ---
comm_hosp[auth_eff > auth_exp | auth_eff > hosp_disc,
          check := "auth_eff is not first chronologically"]
comm_hosp[auth_exp - 1 > hosp_disc, # condition may need rethinking 2/2/16
          check := "hosp_disc comes before auth_exp"]
comm_hosp[auth_days >= 90, check := "auth_days 90 or more"]
comm_hosp[auth_days > Sys.Date() | hosp_disc > Sys.Date(),
          check := "future dates in expiration or discharge"]
# verification that a particular case is not an error ---
comm_hosp[case_no == 14312 & auth_eff == "2014-05-09", check := NA]

# do all of the checking in one fell swoop
prepare$check <- comm_hosp[!is.na(check)]
if (nrow(prepare$check) > 0) {
  p_note("Review the following community hospital data for data errors
         (see check)", prepare$check)
  p_msg("Choose an option to proceed:\newline
         1. Review successful - ignore all checks.\newline
         2. Save potentially incorrect data to file for later review.
            Proceed with running code.\newline
         3. Halt R, revisions and further checks are necessary.\newline")
  prepare$continue <- aux$ask()
  switch(as.chr(prepare$continue),
         "1" = {p_msg("you declared comm_hosp checks as unconcerning.")},
         "2" = {
           p_msg("saving comm_hosp checks data for later review.")
           if (!dir.exists(file.path(project_wd$results, "Checks"))) {
               dir.create(file.path(project_wd$results, "Checks"))
           }
           write.csv(prepare$check, row.names = FALSE, file = file.path(
             project_wd$results, "Checks", paste("comm_hosp checks",
             format(Sys.Date(), "%b-%d-%Y"))))
         },
         "3" = p_stop("the user asked to stop the current operation to fully
                      investigate/fix prepare$check"),
         p_stop("this should not have happened, error in aux$ask likely."))
}

## LEFT OFF HERE 2/2/16
comm_hosp[]






lapply(comm_hosp, class)






rm(j)