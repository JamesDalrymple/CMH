

# check data for potential integrity issues
prepare <- new.env(parent = .GlobalEnv)

# locus -----------------------------------------------------------------------
locus <- copy(sql$output$locus)
setf(locus, j = Cs(init_disp, ovr_disp), stringi::stri_trim)

locus[, init_disp := wccmh::locus_word2num(init_disp)]
locus[, ovr_disp := wccmh::locus_word2num(ovr_disp)]
locus[, comb_disp := ifelse(is.na(ovr_disp), init_disp, ovr_disp)]
locus[, locus_date := as.Date(locus_date)]

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
# services --------------------------------------------------------------------
services <- copy(sql$output$services)
services[1]
# served[, service_date := as.Date(service_date)]
# no need to check served at this time 2/3/16