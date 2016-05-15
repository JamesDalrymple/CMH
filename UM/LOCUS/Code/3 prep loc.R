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
prep$adm <- prep$adm[adm_grp==1]
# number of new consumer admissions for CMH core collectively
agg$new_adm <-
  prep$adm[, list(num_adm = length(unique(case_no))),
           by = list(span_label)]
write.csv(agg$new_adm, file = file.path(project_wd$results,
  "new core cmh admissions.csv"), row.names = FALSE)

# locus prep ---
locus[, locus_date2 := locus_date]
setkey(locus, case_no, locus_date, locus_date2)

locus[, length(unique(case_no))]

setkeyv(adm, Cs(case_no, adm_effdt, adm_expdt))
locus <- foverlaps(y = adm[, unique(.SD), .SDcols = Cs(case_no, adm_effdt, adm_expdt)],
          x =locus,
          by.y = Cs(case_no, adm_effdt, adm_expdt),
          by.x = Cs(case_no, locus_date, locus_date2),
          nomatch = NA)
locus[is.na(adm_effdt)]


locus[, pre_l_dt := locus_date - input$locus_range]
locus[, post_l_dt := locus_date + input$locus_range]



prep$adm[, pk_adm := .I]
setkey(prep$adm, case_no, team_eff, team_eff2)
setkey(locus, pre_l_dt, post_l_dt)
prep$adm_loc <- foverlaps(locus[, unique(.SD), .SDcols = Cs(case_no, locus_date,
  pre_l_dt, post_l_dt, comb_disp)], prep$adm,
          by.x = Cs(case_no, pre_l_dt, post_l_dt),
          by.y = Cs(case_no, team_eff, team_eff2),
  nomatch = 0) # mult = "first"

prep$re_add_pk <- setdiff(prep$adm[, unique(pk_adm)],
        prep$adm_loc[, unique(pk_adm)])

prep$adm_loc <- rbindlist(list(
  prep$adm_loc,
  prep$adm[pk_adm %in% prep$re_add_pk]
), fill = TRUE, use.names = TRUE)

prep$adm_loc[is.na(locus_date), table(span_label)]


locus[case_no %in% prep$adm_loc[span_label == "Jan 2016" & is.na(locus_date), case_no]]
prep$adm_loc[case_no == 1126666]


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