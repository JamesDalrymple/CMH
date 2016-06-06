modify <- new.env(parent = .GlobalEnv)

adm <- copy(sql$output$adm)
staff <- copy(sql$output$staff)

setf(adm, j = Cs(cmh_effdt, team_effdt), as.Date)
adm[, team_short := cmh_recode(team)]

adm
grep(pattern = "[[:alpha:+], :alpha:{0}]", replacement = "", x = "last, first", perl = FALSE)
gsub(pattern = "[[:alpha:+], :alpha:{0}]", replacement = "", x = "last, first", perl = FALSE)

aux$initials <- function(x) {
  first_i <- str_match(string = x, pattern = "[:alpha:]+, ([:alpha:]{1})")[, 2]
  last_i = substr(x, 1, 1)
  toupper(paste0(first_i, last_i))
}
setf(adm, j = "client_name", initials)
adm[, active_teams := .N, by = case_no]
adm <- adm[active_teams > 1]

adm[staff[primary_staff_or_not == "Y"],
    Cs(prim_staff, supervisor) := list(i.assigned_staff, i.supervisor),
    on = "case_no"]