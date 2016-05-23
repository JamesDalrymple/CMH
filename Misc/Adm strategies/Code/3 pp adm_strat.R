# pre-processing admisision records
pp <- new.env(parent = .GlobalEnv)
stopifnot(is.null(key(adm)))
adm[is.na(team_exp), team_exp := Sys.Date() + 999]


# number 1: primary team with duplicate approach
pp$adm1 <- copy(adm[prim == "Y"])
setkey(pp$adm1, team_eff, team_exp)
pp$adm1 <- foverlaps(aux$time_dt,
                     pp$adm1,
                     by.x = Cs(span_start, span_end),
                     by.y = Cs(team_eff, team_exp))

# number 2: using primary team and a min/max approach
pp$adm2 <- copy(adm[prim == "Y"])
setkey(pp$adm2, team_eff, team_exp)
pp$adm2 <- foverlaps(aux$time_dt,
          pp$adm2,
          by.x = Cs(span_start, span_end),
          by.y = Cs(team_eff, team_exp))
setkey(pp$adm2, NULL)
pp$adm2[cmh_priority_dt, priority := i.priority, on = "team"]
pp$adm2[, min_priority := min(priority, na.rm = TRUE),
       by = list(case_no, span_type, span_start)]
pp$adm2 <- pp$adm2[min_priority == priority]

pp$adm2[, list(cases = length(unique(case_no)),
               approach = "prim_team & min"),
       by = list(program, span_label, span_type)]
# number 3: using a duplicates allowed 'as is' approach
pp$adm3 <- copy(adm)
pp$adm3[, prim := NULL]
setkey(pp$adm3, team_eff, team_exp)
pp$adm3 <- foverlaps(aux$time_dt,
                     pp$adm3,
                     by.x = Cs(span_start, span_end),
                     by.y = Cs(team_eff, team_exp))
pp$adm3[, list(cases = length(unique(case_no)),
               approach = "adm w/ duplicates"),
        by = list(program, span_label, span_type)]

# number 4: using priority_overlap (not yet finished)
pp$adm4 <- copy(adm)
pp$adm4[, prim := NULL]

# eventually...
# pp$adm4[, prim := NULL]
# pp$adm4[cmh_priority_dt, priority := i.priority, on = "team"]
# pp$adm4 <- priority_overlap(data = pp$adm4,
#                 group_cols = Cs(case_no, adm_effdt, adm_expdt, program, team),
#                 start_col = "team_eff", end_col = "team_exp")

setkey(pp$adm4, team_eff, team_exp)
pp$adm4 <- foverlaps(aux$time_dt,
                     pp$adm4,
                     by.x = Cs(span_start, span_end),
                     by.y = Cs(team_eff, team_exp))
setkey(pp$adm4, NULL)
pp$adm4[cmh_priority_dt, priority := i.priority, on = "team"]
pp$adm4[, min_priority := min(priority, na.rm = TRUE),
        by = list(case_no, span_type, span_start)]
pp$adm4 <- pp$adm4[min_priority == priority]

pp$adm4[, list(cases = length(unique(case_no)),
               approach = "priority_overlap"),
        by = list(program, span_label, span_type)]

# number 5: using majority team days approach
pp$adm5 <- copy(adm)
pp$adm5[, prim := NULL]
setkey(pp$adm5, team_eff, team_exp)
pp$adm5 <- foverlaps(aux$time_dt,
                     pp$adm5,
                     by.x = Cs(span_start, span_end),
                     by.y = Cs(team_eff, team_exp))
setkey(pp$adm5, NULL)

pp$adm5[, team_days := pmin(span_end, team_exp) - pmax(span_start, team_eff)]
pp$adm5[, team_days := as.numeric(team_days)]

pp$adm5[, max_td := max(team_days), by = list(case_no, span_type, span_start)]
pp$adm5 <- pp$adm5[max_td == team_days]

pp$adm5[, list(cases = length(unique(case_no)),
               approach = "majority team days"),
        by = list(program, span_label, span_type)]





