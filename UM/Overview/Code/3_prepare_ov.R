# check data for potential integrity issues
prepare <- new.env(parent = .GlobalEnv)

# community hospitalization ---------------------------------------------------
comm_hosp <- copy(sql$output$comm_hosp)
prepare$date_cols <- c("auth_eff", "auth_exp", "hosp_disc", "dob")
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
comm_hosp[auth_eff > Sys.Date() | hosp_disc > Sys.Date(),
          check := "future dates in expiration or discharge"]
# verification that a particular case is not an error ---
comm_hosp[case_no == 14312 & auth_eff == "2014-05-09", check := NA]
if (length(intersect(names(comm_hosp), "check")) == 1) {
  prepare$check <- comm_hosp[!is.na(check)]
}
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
if (length(intersect(names(comm_hosp), "check")) == 1) {
  comm_hosp[, check := NULL]
} else {prepare$check = data.table()}
# state hospitalizations ------------------------------------------------------
state_hosp <- copy(sql$output$state_hosp)
prepare$date_cols <- c("auth_eff", "auth_exp", "hosp_disc", "dob")
for (j in prepare$date_cols) {
  set(state_hosp, j = j, value = date_convert((state_hosp[[j]])))
}
state_hosp[auth_eff > auth_exp | auth_eff > hosp_disc,
           check := "auth_eff is not first chronologically"]
state_hosp[auth_exp - 1 > hosp_disc, # condition may need rethinking 2/2/16
           check := "hosp_disc comes before auth_exp"]
state_hosp[auth_eff > Sys.Date() | hosp_disc > Sys.Date(),
           check := "future dates in expiration or discharge"]
if (length(intersect(names(state_hosp), "check")) == 1) {
  prepare$check <- state_hosp[!is.na(check)]
} else {prepare$check = data.table()}
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
         "1" = {p_msg("you declared state_hosp checks as unconcerning.")},
         "2" = {
           p_msg("saving state_hosp checks data for later review.")
           if (!dir.exists(file.path(project_wd$results, "Checks"))) {
             dir.create(file.path(project_wd$results, "Checks"))
           }
           write.csv(prepare$check, row.names = FALSE, file = file.path(
             project_wd$results, "Checks", paste("state_hosp checks",
                                                 format(Sys.Date(), "%b-%d-%Y"))))
         },
         "3" = p_stop("the user asked to stop the current operation to fully
                      investigate/fix prepare$check"),
         p_stop("this should not have happened, error in aux$ask likely."))
}
if (length(intersect(names(state_hosp), "check")) == 1) {
  state_hosp[, check := NULL]
}
# admissions ------------------------------------------------------------------
adm <- copy(sql$output$adm)
prepare$date_cols <- c("team_eff", "staff_eff", "staff_exp")
for (j in prepare$date_cols) {
  set(adm, j = j, value = as.Date((adm[[j]])))
}
adm[, team := cmh_recode(team)]
# these 22 records are mistakenly mixed up on start/end dates
adm[staff_eff > staff_exp & staff %in%
    c("Donnelly, Kevin", "Mitchell, Holly", "Gavin, Ellen", "LeBlanc, Belinda",
      "Shaffer (Kniceley), Ashley") & staff_eff < date_convert("10/1/14"),
    c("staff_eff", "staff_exp") := list(staff_exp, staff_eff)]
adm[staff_eff > staff_exp, check := "staff_eff after staff_exp"]
adm[is.na(team), team := "NA inside team"]
adm[team_eff > team_exp, check := "team_eff after team_exp"]

if (length(intersect(names(adm), "check")) == 1) {
  prepare$check <- adm[!is.na(check)]
} else {prepare$check = data.table()}
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
         "1" = {p_msg("you declared adm checks as unconcerning.")},
         "2" = {
           p_msg("saving adm checks data for later review.")
           if (!dir.exists(file.path(project_wd$results, "Checks"))) {
             dir.create(file.path(project_wd$results, "Checks"))
           }
           write.csv(prepare$check, row.names = FALSE, file = file.path(
             project_wd$results, "Checks", paste("adm checks",
                                                 format(Sys.Date(), "%b-%d-%Y"))))
         },
         "3" = p_stop("the user asked to stop the current operation to fully
                      investigate/fix prepare$check"),
         p_stop("this should not have happened, error in aux$ask likely."))
}
if (length(intersect(names(adm), "check")) == 1) {
  adm[, check := NULL]
}
# served ----------------------------------------------------------------------
served <- copy(sql$output$served)
served[, service_date := as.Date(service_date)]
# no need to check served at this time 2/3/16
rm(j)