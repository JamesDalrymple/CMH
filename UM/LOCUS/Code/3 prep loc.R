prep <- new.env(parent = .GlobalEnv)
agg <- new.env(parent = .GlobalEnv)

# Q1. new cmh admissions, monthly, new care segment only ----------------------
# (program transitions do not count, first core CMH in E2 admission range)
prep$adm <- copy(adm[, unique(.SD),
  .SDcols = Cs(case_no, team_eff, team, adm_grp)])
prep$adm[, team_eff2 := team_eff]
setkey(prep$adm, team_eff, team_eff2)
# setkey(aux$mon_dt, span_start, span_end)
prep$adm <- foverlaps(x=aux$mon_dt, y=prep$adm,
          by.x = Cs(span_start, span_end),
          by.y = Cs(team_eff, team_eff2))
prep$adm <- prep$adm[adm_grp == 1]
# number of new consumer admissions for CMH core collectively
agg$new_adm <-
  prep$adm[, list(num_adm = length(unique(case_no))),
           by = list(span_label)]
write.csv(agg$new_adm, file = file.path(project_wd$results,
  "new core cmh admissions.csv"), row.names = FALSE)

# locus prep ---
# locus[, locus_date2 := locus_date]
# setkey(locus, case_no, locus_date, locus_date2)
locus <- unique(locus)
locus[, pre_l_dt := locus_date - input$locus_range]
locus[, post_l_dt := locus_date + input$locus_range]
setkey(locus, case_no, pre_l_dt, post_l_dt)

# locus[, length(unique(case_no))]

setkeyv(adm, Cs(case_no, adm_effdt, adm_expdt))
locus <- foverlaps(
  x = locus,
  y = adm[, unique(.SD), .SDcols = Cs(case_no, adm_effdt, adm_expdt)],
  by.x = Cs(case_no, pre_l_dt, post_l_dt),
  by.y = Cs(case_no, adm_effdt, adm_expdt),
  nomatch = NA)
# locus[is.na(adm_effdt)]
locus_no_adm <- copy(locus[is.na(adm_effdt)])
locus <- locus[!is.na(adm_effdt)]
# this could potentially be done in the SQL code
locus <-
  locus[between(adm_effdt,
                date_convert(input$start_dt) - input$locus_range,
                date_convert(input$end_dt) + input$locus_range)]

locus[, adm_pk := .GRP, by = list(case_no, adm_effdt)]
locus[, abs_diff := as.integer(abs(locus_date - adm_effdt)), by = list(adm_pk)]

locus[, min_abs_diff := min(abs_diff), by = list(adm_pk)]
locus <- locus[min_abs_diff == abs_diff]
if (nrow(locus[duplicated(adm_pk)]) > 0) stop("locus adm_pk duplicates exist")
locus[, Cs(pre_l_dt, post_l_dt, adm_pk, abs_diff, min_abs_diff) := NULL]

agg$locus_levels <-
  locus[, list(cases = length(unique(case_no))), keyby = list(comb_disp)]

prep$loc_low_lev <- locus[comb_disp <= 2, unique(.SD),
                          .SDcols = Cs(case_no, adm_effdt, comb_disp)]

prep$loc_low_lev <- merge(prep$loc_low_lev, services,
                          by = "case_no", all.x = TRUE)

prep$loc_low_lev[aux$um_desc,
  um_desc := i.um_desc, on = c("cpt_code" = "cpt_cd")]
prep$loc_low_lev <- prep$loc_low_lev[service_date >= adm_effdt]


prep$loc_low_lev[, three_mon_end := adm_effdt + 92]
prep$loc_low_lev[, six_mon_end := adm_effdt + 183]

agg$loc_3mon <-
  prep$loc_low_lev[between(service_date, adm_effdt, three_mon_end),
                   list(total_units = sum(units),
                        service_count = length(cpt_code)), by = um_desc]
write.csv(agg$loc_3mon,
  file = file.path(project_wd$results,
  "monthly over 30 days admissions.csv"), row.names = FALSE)

agg$loc_6mon <-
  prep$loc_low_lev[between(service_date, adm_effdt, six_mon_end),
                   list(total_units = sum(units),
                        service_count = length(cpt_code)), by = um_desc]
write.csv(agg$loc_6mon,
  file = file.path(project_wd$results,
  "monthly over 30 days admissions.csv"), row.names = FALSE)

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