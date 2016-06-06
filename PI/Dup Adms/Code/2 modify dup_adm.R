modify <- new.env(parent = .GlobalEnv)

adm <- copy(sql$output$adm)
staff <- copy(sql$output$staff)

setf(adm, j = Cs(cmh_effdt, team_effdt), as.Date)
adm[, team_short := cmh_recode(team)]

adm[, active_teams := .N, by = case_no]


adm <- adm[active_teams > 1]

adm[staff[primary_staff_or_not == "Y"],
    Cs(prim_staff, supervisor) := list(i.assigned_staff, i.supervisor),
    on = "case_no"]

