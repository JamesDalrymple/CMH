modify <- new.env(parent = .GlobalEnv)
# admissions ------------------------------------------------------------------
adm <- copy(sql$output$adm)
setf(adm, j = Cs(team_eff, team_exp, staff_eff, staff_exp,
                 adm_effdt, adm_expdt), as.Date)
adm[, team := cmh_recode(team)]

# these 22 records are mistakenly mixed up on start/end dates
adm[staff_eff > staff_exp & staff %in%
      c("Donnelly, Kevin", "Mitchell, Holly", "Gavin, Ellen", "LeBlanc, Belinda",
        "Shaffer (Kniceley), Ashley") & staff_eff <= date_convert("10/1/14"),
    c("staff_eff", "staff_exp") := list(staff_exp, staff_eff)]
adm[staff_eff == as.Date("2012-04-09") & staff_exp == "2012-04-08" &
    case_no == 76413, Cs(staff_eff, staff_exp) :=
    list(rep(as.Date("2012-04-09"),2))]
adm[case_no == 95395 & staff_eff == as.Date("2011-08-19") &
    staff_exp == as.Date("2011-08-18"), Cs(staff_eff, staff_exp) :=
      list(rep(as.Date("2011-08-18"),2))]


adm[staff_eff > staff_exp, check := "staff_eff after staff_exp"]
adm[is.na(team), team := "NA inside team"]
adm[team_eff > team_exp, check := "team_eff after team_exp"]

if (length(intersect(names(adm), "check")) == 1) {
  modify$check <- adm[!is.na(check)]
} else {modify$check = data.table()}
if (nrow(modify$check) > 0) {
  p_note("Review the following adm data.table for data errors
         (see check)", modify$check)
  p_msg("Choose an option to proceed:\newline
        1. Review successful - ignore all checks.\newline
        2. Save potentially incorrect data to file for later review.
        Proceed with running code.\newline
        3. Halt R, revisions and further checks are necessary.\newline")
  modify$continue <- aux$ask()
  switch(as.chr(modify$continue),
         "1" = {p_msg("you declared adm checks as unconcerning.")},
         "2" = {
           p_msg("saving adm checks data for later review.")
           if (!dir.exists(file.path(project_wd$results, "Checks"))) {
             dir.create(file.path(project_wd$results, "Checks"))
           }
           write.csv(modify$check, row.names = FALSE, file = file.path(
             project_wd$results, "Checks", paste("adm checks",
                                                 format(Sys.Date(), "%b-%d-%Y"))))
         },
         "3" = p_stop("the user asked to stop the current operation to fully
                      investigate/fix modify$check"),
         p_stop("this should not have happened, error in aux$ask likely."))
}
if (length(intersect(names(adm), "check")) == 1) {
  adm[, check := NULL]
}
adm[, Cs(staff_eff, staff_exp, staff) := NULL]
adm <- unique(adm)

adm[is.na(adm_expdt), adm_expdt := Sys.Date() + 999]

adm[, program := recode_team_prog(team)]
# adm[, adm_grp := seq(.N), by = list(case_no, adm_effdt)]
