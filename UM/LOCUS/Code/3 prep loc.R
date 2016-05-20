prep <- new.env(parent = .GlobalEnv)

# Q1. new cmh admissions, monthly, new care segment only ----------------------
# (program transitions do not count, first core CMH in E2 admission range)
prep$adm <- copy(adm[, unique(.SD),
  .SDcols = Cs(case_no, team_eff, program, adm_grp)])
prep$adm[, team_eff2 := team_eff]
setkey(prep$adm, team_eff, team_eff2)
# setkey(aux$mon_dt, span_start, span_end)
prep$adm <- foverlaps(x=aux$mon_dt, y=prep$adm,
          by.x = Cs(span_start, span_end),
          by.y = Cs(team_eff, team_eff2))
prep$adm <- prep$adm[adm_grp == 1]
setkey(prep$adm, NULL)
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
  y = adm[, unique(.SD), .SDcols = Cs(case_no, adm_effdt, adm_expdt, program)],
  by.x = Cs(case_no, pre_l_dt, post_l_dt),
  by.y = Cs(case_no, adm_effdt, adm_expdt),
  nomatch = NA)
setkey(adm, NULL)
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

prep$loc_low_lev <- locus[comb_disp <= 2, unique(.SD),
                          .SDcols = Cs(case_no, adm_effdt, program, comb_disp)]

prep$loc_low_lev <- merge(prep$loc_low_lev, services,
                          by = "case_no", all.x = TRUE)

prep$loc_low_lev[aux$um_desc,
  um_desc := i.um_desc, on = c("cpt_code" = "cpt_cd")]

prep$loc_low_lev <- prep$loc_low_lev[service_date >= adm_effdt]
prep$loc_low_lev[, three_mon_end := adm_effdt + 92]
prep$loc_low_lev[, six_mon_end := adm_effdt + 183]
prep$loc_low_lev[, svc_fy := my_fy(service_date)]
prep$loc_low_lev <- prep$loc_low_lev[program == "MI"]

prep$loc_low_lev[is.na(um_desc), um_desc := cpt_code]
if ( nrow(prep$loc_low_lev[is.na(um_desc)]) > 0 ) stop("um_desc unassigned")

# CMH monthly admissions, discharges, total admitted ---------------------------
# WITHOUT people closed <= 30 days
prep$adm_new <- copy(adm[, unique(.SD),
                         .SDcols = Cs(case_no, team_eff, team_exp, program)])
prep$adm_new[!is.na(team_exp),
             day_diff := as.numeric(team_exp - team_eff + 1)]
prep$adm_new[is.na(team_exp), day_diff :=
  as.numeric(date_convert(input$end_dt) - team_eff + 1)]
prep$adm_new[is.na(team_exp), team_exp := date_convert(input$end_dt) + 999]

prep$adm_new <- prep$adm_new[day_diff >= 30]

setkey(prep$adm_new, team_eff, team_exp)
prep$adm_new <- foverlaps(aux$mon_dt, prep$adm_new,
          by.x = Cs(span_start, span_end),
          by.y = Cs(team_eff, team_exp))
setkey(prep$adm_new, NULL)
prep$adm_new[between(team_eff, span_start, span_end), adm := 1]
prep$adm_new[between(team_exp, span_start, span_end), disc := 1]