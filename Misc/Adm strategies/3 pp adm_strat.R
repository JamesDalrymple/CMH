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
pp$adm1[, list(cases = length(unique(case_no)),
               approach = "prim_team duplicates"),
        by = list(program, span_label, span_type)]
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
setkey(pp$adm3, team_eff, team_exp)
pp$adm3 <- foverlaps(aux$time_dt,
                     pp$adm3,
                     by.x = Cs(span_start, span_end),
                     by.y = Cs(team_eff, team_exp))
pp$adm3[, list(cases = length(unique(case_no)),
               approach = "prim_team duplicates"),
        by = list(program, span_label, span_type)]

# number 4: using priority_overlap
pp$adm4 <- copy(adm)
pp$adm4[cmh_priority_dt, priority := i.priority, on = "team"]
pp$adm4[, prim := NULL]

priority_overlap(data = pp$adm4,
                 group_cols = Cs(case_no, adm_effdt, adm_expdt, program, adm),
                 start_col = "team_eff", end_col = "team_exp")

# number 5: using majority team days approach