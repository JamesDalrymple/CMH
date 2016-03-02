modify <- new.env(parent = .GlobalEnv)

er <- copy(sql$output$er)
adm <- copy(sql$output$adm)
hh_util <- copy(sql$hh_util)

modify$date_cols <- Cs(er_start_date)
for (j in modify$date_cols) {
  set(er, j = j, value = as.Date(er[[j]]))
}

# hh util ---
hh_util[, mon := as.yearmon(mon, "%b%y")]
hh_util[is.na(util), util := 0L]
hh_util[, util_cat := aux$util_cat(util)]
hh_util[, c("mon_start", "mon_end") :=
          list(as.Date(mon), as.Date(mon, frac = 1))]


# admissions ------------------------------------------------------------------
modify$date_cols <- Cs(team_eff, team_exp, staff_eff, staff_exp)
for (j in modify$date_cols) {
  set(adm, j = j, value = as.Date(adm[[j]]))
}
adm[, team := recode_string(x = team,
  recode_key = list("Health Home" = c("WSH - Health Home", "Health Home")))]
adm[is.na(team_exp), team_exp := Sys.Date()+999]
adm[like(staff_type, "Nurse"), hh_nurse := 1]
adm[, Cs(staff, staff_type) := NULL]
# combine hh_util and admissions ---
setkey(adm, case_no, team_eff, team_exp)
hh_util <- foverlaps(hh_util, adm,
          by.x = Cs(case_no, mon_start, mon_end),
          by.y = Cs(case_no, team_eff, team_exp),
          type = "any", nomatch = NA)
hh_util[is.na(team), team := "non-HH"]
hh_util[is.na(hh_nurse), hh_nurse := 0]
hh_util <- hh_util[, list(hh_nurse = max(hh_nurse, na.rm = TRUE)),
        by = list(case_no, team, util, mon, util_cat)]
hh_util[, year_ago := mon - 1]

# ER visits -------------------------------------------------------------------
er[, mon := as.yearmon(er_start_date)]
modify$er_visits <-
  er[, list(er_visits = length(er_start_date)), by = list(case_no, mon)]

# current fy data
modify$cur_fy$er_visits <-
  modify$er_visits[hh_util, on = Cs(case_no, mon), nomatch = NA]
modify$cur_fy$er_visits[is.na(er_visits), er_visits := 0]
# previous fy data
modify$prev_fy$er_visits <-
  modify$er_visits[hh_util,
  on = c(case_no = "case_no", mon = "year_ago"), nomatch = NA]
modify$prev_fy$er_visits[is.na(er_visits), er_visits := 0]
# combine current and previous fy data ---
modify$cur_fy$er_visits[, year_ago := NULL]
modify$prev_fy$er_visits[, i.mon := NULL]
modify$fy_er <-
  rbindlist(list(modify$cur_fy$er_visits,
                 modify$prev_fy$er_visits), use.names = TRUE)
# GLM confidence intervals
modify$fy_er[, fy := my_fy(as.Date(mon))]
modify$fy_er[, mon_name := substr(mon, 1, 3)]
# modify$fy_er[util_cat %in% c("L1-L2", "L3"),
#   Cs(ci_lower, ci_upper) := as.list(as.num(
#     confint(glm(er_visits ~ mon, family = poisson), parm = "mon"))),
#   by = list(util_cat, mon_name)]
modify$fy_er[, mann_whit_pvalue :=
  wilcox.test(er_visits ~ mon, alternative = "two.sided")$p.value,
  by = list(util_cat, mon_name)]
# aggregation
modify$er_agg <-
  modify$fy_er[, list(cases = length(unique(case_no)),
                      er_visits = sum(er_visits)),
               keyby = list(mon, team, hh_nurse, util_cat, mann_whit_pvalue),
               nomatch = NA]
setkey(modify$er_agg, mon, util_cat)
modify$er_agg <-
  modify$er_agg[CJ(modify$er_agg[, unique(mon)],
                   modify$er_agg[, unique(util_cat)])]
# replacing NA's with 0 (created by cross join above)
modify$er_agg[is.na(cases), c(Cs(cases, er_visits)) := 0]
modify$er_agg[, fy := my_fy(as.Date(mon))]
modify$er_agg[, mon_abr := substr(mon, 1, 3)]
modify$er_agg[, er_rate := er_visits/cases]
modify$er_agg[is.na(er_rate), er_rate := NA_real_]
modify$mon_levels <- modify$er_agg[order(mon), as.chr(unique(mon_abr))]
modify$er_agg[, mon_abr := factor(mon_abr, levels = modify$mon_levels)]
modify$er_agg[is.na(team), team := "non-HH"]
modify$er_agg[is.na(hh_nurse), hh_nurse := 0]
modify$er_agg[, stat_evidence := aux$evidence_p(mann_whit_pvalue)]
modify$er_agg[, evidence_symbol := aux$evidence_symbol(mann_whit_pvalue)]
modify$er_l0 <- modify$er_agg[util_cat == "L0"]
modify$er_agg <- modify$er_agg[util_cat != "L0"] # per Laura/Brandie 3/1/16
# changes in levels from month to month ---------------------------------------
modify$lvl_change <- copy(hh_util)
modify$lvl_change[, mon_int := as.integer(as.factor(mon))]
modify$lvl_change[, mon_shift := shift(mon_int), by = case_no]
modify$lvl_change[, prev_util := shift(util), by = case_no]
modify$lvl_change[mon_int-mon_shift == 1, consec_mon := 1]

# modify$lvl_change[consec_mon == 1 & case_no == 229047]
modify$lvl_change[consec_mon == 1, level_change :=
                  aux$level_change(util, prev_util)]

modify$lvl_agg <- modify$lvl_change[, list(cases = length(unique(case_no))),
                  by = list(team, level_change, mon)]
modify$mon_levels <- modify$lvl_agg[, as.chr(sort(unique(mon)))]
modify$lvl_agg[, mon := factor(mon, levels = modify$mon_levels)]
modify$lvl_agg <- modify$lvl_agg[!is.na(level_change)]

# modify$lvl_agg[is.na(level_change), level_change := 0]
# setkey(modify$lvl_agg, team, mon, level_change)
# modify$lvl_agg[CJ(modify$lvl_agg[, unique(team)],
#                   modify$lvl_agg[, unique(mon)],
#                   modify$lvl_agg[, unique(level_change)])]
