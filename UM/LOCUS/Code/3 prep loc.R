prep <- new.env(parent = .GlobalEnv)
agg <- new.env(parent = .GlobalEnv)

# new cmh admissions, monthly -------------------------------------------------
prep$adm <- copy(adm[, unique(.SD),
  .SDcols = Cs(case_no, team_eff, team, adm_grp)])
prep$adm[, team_eff2 := team_eff]
setkey(prep$adm, team_eff, team_eff2)
# setkey(aux$mon_dt, span_start, span_end)
prep$adm <- foverlaps(x=aux$mon_dt, y=prep$adm,
          by.x = Cs(span_start, span_end),
          by.y = Cs(team_eff, team_eff2))

agg$new_adm <-
  prep$adm[adm_grp==1, list(num_adm = length(unique(case_no))), by = list(span_label)]
write.csv(agg$new_adm, file = file.path(project_wd$results,
  "new core cmh admissions.csv"), row.names = FALSE)

# CMH monthly admissions, discharges, total admitted ---------------------------
# WITHOUT people closed <= 30 days
prep$adm_new <- copy(adm[, unique(.SD), .SDcols = Cs(case_no, team_eff, team_exp, team)])
prep$adm_new[is.na(team_exp), team_exp := date_convert(input$end_dt)]
prep$adm_new[, day_diff := as.numeric(team_exp - team_eff + 1)]
prep$adm_new <- prep$adm_new[day_diff >= 30]

setkey(prep$adm_new, team_eff, team_exp)
prep$adm_new <- foverlaps(aux$mon_dt, prep$adm_new,
          by.x = Cs(span_start, span_end),
          by.y = Cs(team_eff, team_exp))
prep$adm_new[between(team_eff, span_start, span_end), adm := 1]
prep$adm_new[between(team_exp, span_start, span_end), disc := 1]

agg$adm_status <-
  prep$adm_new[, list(adm = sum(adm, na.rm = TRUE),
                      disc = sum(disc, na.rm = TRUE),
                      active = length(unique(case_no))),
               by = list(span_label)]
write.csv(agg$adm_status, file = file.path(project_wd$results,
  "monthly over 30 days admissions.csv"), row.names = FALSE)