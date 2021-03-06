# cast, melt, graph
cmg <- new.env(parent = .GlobalEnv)
# Eligibility summary ---------------------------------------------------------
# saved$eligible # may be overkill information

cmg$hh <- rbindlist(list(
  pp$bmi$hh[, data.table(unique(.SD), var = "bmi"),
           .SDcols = Cs(case_no, n_recs, status, hh_cat)],
  pp$bp$hh[, data.table(unique(.SD), var = "sys"),
           .SDcols = Cs(case_no, n_recs, sys_status, hh_cat)],
  pp$bp$hh[, data.table(unique(.SD), var = "dia"),
           .SDcols = Cs(case_no, n_recs, dia_status, hh_cat)],
  pp$wn$oh$hh[, data.table(unique(.SD), var = "oh"),
              .SDcols = Cs(case_no, n_recs, status, hh_cat)],
  pp$wn$pain$hh[, data.table(unique(.SD), var = "pain"),
                .SDcols = Cs(case_no, n_recs, status, hh_cat)],
  pp$labs$chol$hh[, data.table(unique(.SD)),
               .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)],
  pp$labs$gluc$hh[, data.table(unique(.SD)),
                  .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)],
  pp$labs$trig$hh[, data.table(unique(.SD)),
                  .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)],
  pp$labs$a1c$hh[, data.table(unique(.SD)),
                  .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)],
  pp$labs$hdl$hh[, data.table(unique(.SD)),
                 .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)],
  pp$labs$ldl$hh[, data.table(unique(.SD)),
                 .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)]
))

cmg$hh_lev <- rbindlist(list(
  pp$bmi$hh_lev[, data.table(unique(.SD), var = "bmi"),
            .SDcols = Cs(case_no, n_recs, status, hh_cat)],
  pp$bp$hh_lev[, data.table(unique(.SD), var = "sys"),
           .SDcols = Cs(case_no, n_recs, sys_status, hh_cat)],
  pp$bp$hh_lev[, data.table(unique(.SD), var = "dia"),
           .SDcols = Cs(case_no, n_recs, dia_status, hh_cat)],
  pp$wn$oh$hh_lev[, data.table(unique(.SD), var = "oh"),
              .SDcols = Cs(case_no, n_recs, status, hh_cat)],
  pp$wn$pain$hh_lev[, data.table(unique(.SD), var = "pain"),
                .SDcols = Cs(case_no, n_recs, status, hh_cat)],
  pp$labs$chol$hh_lev[, data.table(unique(.SD)),
                  .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)],
  pp$labs$gluc$hh_lev[, data.table(unique(.SD)),
                  .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)],
  pp$labs$trig$hh_lev[, data.table(unique(.SD)),
                  .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)],
  pp$labs$a1c$hh_lev[, data.table(unique(.SD)),
                 .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)],
  pp$labs$hdl$hh_lev[, data.table(unique(.SD)),
                 .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)],
  pp$labs$ldl$hh_lev[, data.table(unique(.SD)),
                 .SDcols = Cs(case_no, n_recs, status, hh_cat, lab_name)]
))

cmg$hh_cmh <- rbindlist(list(
  pp$bmi$hh_cmh[, data.table(unique(.SD), var = "bmi"),
                    .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team)],
  pp$bp$hh_cmh[, data.table(unique(.SD), var = "sys"),
                   .SDcols = Cs(case_no, n_recs, sys_status, hh_cat, cmh_team)],
  pp$bp$hh_cmh[, data.table(unique(.SD), var = "dia"),
                   .SDcols = Cs(case_no, n_recs, dia_status, hh_cat, cmh_team)],
  pp$wn$oh$hh_cmh[, data.table(unique(.SD), var = "oh"),
                  .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team)],
  pp$wn$pain$hh_cmh[, data.table(unique(.SD), var = "pain"),
                    .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team)],
  pp$labs$chol$hh_cmh[, data.table(unique(.SD)),
                          .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)],
  pp$labs$gluc$hh_cmh[, data.table(unique(.SD)),
                          .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)],
  pp$labs$trig$hh_cmh[, data.table(unique(.SD)),
                          .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)],
  pp$labs$a1c$hh_cmh[, data.table(unique(.SD)),
                         .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)],
  pp$labs$hdl$hh_cmh[, data.table(unique(.SD)),
                         .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)],
  pp$labs$ldl$hh_cmh[, data.table(unique(.SD)),
                         .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)]
))

cmg$hh_lev_cmh <- rbindlist(list(
  pp$bmi$hh_lev_cmh[, data.table(unique(.SD), var = "bmi"),
                .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team)],
  pp$bp$hh_lev_cmh[, data.table(unique(.SD), var = "sys"),
               .SDcols = Cs(case_no, n_recs, sys_status, hh_cat, cmh_team)],
  pp$bp$hh_lev_cmh[, data.table(unique(.SD), var = "dia"),
               .SDcols = Cs(case_no, n_recs, dia_status, hh_cat, cmh_team)],
  pp$wn$oh$hh_lev_cmh[, data.table(unique(.SD), var = "oh"),
                  .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team)],
  pp$wn$pain$hh_lev_cmh[, data.table(unique(.SD), var = "pain"),
                    .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team)],
  pp$labs$chol$hh_lev_cmh[, data.table(unique(.SD)),
                      .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)],
  pp$labs$gluc$hh_lev_cmh[, data.table(unique(.SD)),
                      .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)],
  pp$labs$trig$hh_lev_cmh[, data.table(unique(.SD)),
                      .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)],
  pp$labs$a1c$hh_lev_cmh[, data.table(unique(.SD)),
                     .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)],
  pp$labs$hdl$hh_lev_cmh[, data.table(unique(.SD)),
                     .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)],
  pp$labs$ldl$hh_lev_cmh[, data.table(unique(.SD)),
                     .SDcols = Cs(case_no, n_recs, status, hh_cat, cmh_team, lab_name)]
))

cmg$hh[, Cs(cmh_team, cat) := list("CMH", "hh")]
cmg$hh_lev[, Cs(cmh_team, cat) := list("CMH", "hh_lev")]
cmg$hh_lev_cmh[, cat := "hh_lev_cmh"]
cmg$hh_cmh[, cat := "hh_cmh"]
cmg$comb <- rbindlist(list(
  cmg$hh, cmg$hh_lev, cmg$hh_cmh, cmg$hh_lev_cmh), use.names = TRUE)
# dcast
cmg$castc <-
  dcast(cmg$comb, fill = 0, drop = TRUE, fun.aggregate = length,
        var + cat + hh_cat + cmh_team ~ status, value.var = "case_no")
cmg$castc[, rate_imp := improved/psum(improved, regressed, maintained)]
cmg$castc[, rate_maint_imp := psum(maintained, improved)/
            psum(improved, regressed, maintained)]
cmg$castc[, n := psum(improved, regressed, maintained)]

cmg$hh_only <-
  cmg$castc[like(hh_cat, "HH") & cat %in% c("hh", "hh_lev")]
cmg$hh_only[, `pct impr` := round(rate_imp*100,1)]
cmg$hh_only[, `pct impr+maint` := round(rate_maint_imp*100,1)]
cmg$hh_only[, Cs(rate_imp, rate_maint_imp, cmh_team) := NULL]
setnames(cmg$hh_only, Cs(improved, maintained, regressed),
         Cs(impr, maint, regr))
cmg$hh_only[rbindlist(list(saved$eligible$hh[e_status == "eligible"],
                           saved$eligible$hh_lev[e_status == "eligible"])),
            N_eligible := i.consumers, on = c("hh_cat" = "hh_status")]
setorder(cmg$hh_only, var, cat, hh_cat, `pct impr`)
setcolorder(cmg$hh_only, Cs(var, cat, hh_cat, impr, regr, maint, "pct impr",
            "pct impr+maint", n, N_eligible))
# melt
cmg$meltc <-
  melt(data = cmg$castc, id.vars = c("var", "cat", "hh_cat", "cmh_team"),
     measure.vars = Cs(improved, regressed, maintained),
     variable.name = "imp_status", value.name = "cases")
cmg$meltc[, var := recode_string(var,
  recode_key = list(
    "Glucose" = c("Glucose", "GLU"),
    "LDL" = "LDL",
    "A1C" = "A1C",
    "Triglycerides" = "TRIG",
    "Overall Health",
    "Pain",
    "BMI" = c("bmi", "BMI"),
    "Cholesterol" = c("Cholesterol", "CHOL"),
    "HDL" = "HDL",
    "Diastolic BP" = c("Diastolic BP", "dia"),
    "Systolic BP" = c("Systolic BP", "sys")))]
cmg$meltc[, imp_status := factor(imp_status,
  levels = Cs(regressed, maintained, improved), ordered = TRUE)]
cmg$graphs[['hh']] <-
cmg$meltc[cat == "hh", ggplot(data = .SD,
  aes(x = imp_status, y = cases, fill = hh_cat, ymax = 1.2*cases))+
    geom_bar(stat = "identity", position = position_dodge(0.5), width = 0.5,
             color = "black")+
    theme_light()+theme(legend.position = "top")+
    facet_wrap(~ var, as.table = FALSE, ncol = 3)+
    geom_text(aes(x = imp_status, y = cases, fill = hh_cat, label = cases, hjust = 0.5),
              position = position_dodge(0.5), hjust = -0.5, size = 3)+
    coord_flip()+
    scale_fill_manual(name = NULL, values = aux$colors[c(1:4)])+
    labs(x = NULL, y = "number of consumers",
         title = "Health Home vs CMH")]

cmg$graphs[['hh_lev']] <-
  cmg$meltc[cat == "hh_lev", ggplot(data = .SD,
                                aes(x = imp_status, y = cases, fill = hh_cat, ymax = 1.2*cases))+
              geom_bar(stat = "identity", position = position_dodge(0.5), width = 0.5,
                       color = "black")+
              theme_light()+theme(legend.position = "top")+
              facet_wrap(~ var, as.table = FALSE, ncol = 3)+
              geom_text(aes(x = imp_status, y = cases, fill = hh_cat, label = cases, hjust = 0.5),
                        position = position_dodge(0.5), hjust = -0.5, size = 3)+
              coord_flip()+
              scale_fill_manual(name = NULL, values = aux$colors[c(1:4)])+
              labs(x = NULL, y = "number of consumers",
                   title = "Health Home w/ Levels vs CMH")]

aux$cmh_teams <- c("ACT", "Access", "Child", "Child HB", "DD", "MI")
# hh_cmh graphs by team
cmg$graphs[['hh_cmh']] <-
  lapply(aux$cmh_teams,
  function(x) {
    ggplot(data = cmg$meltc[cat == "hh_cmh" & cmh_team == x],
           aes(x = imp_status, y = cases, fill = hh_cat, ymax = 1.2*cases))+
      geom_bar(stat = "identity", position = position_dodge(0.5), width = 0.5,
               color = "black")+
      theme_light()+theme(legend.position = "top")+
      facet_wrap(~ var, as.table = FALSE, ncol = 3)+
      geom_text(aes(x = imp_status, y = cases, fill = hh_cat,
                    label = cases, hjust = 0.5),
                position = position_dodge(0.5), hjust = -0.5, size = 3)+
      coord_flip()+
      scale_fill_manual(name = NULL, values = aux$colors[c(1:4)])+
      labs(x = NULL, y = "number of consumers",
           title = paste0("Health Home w/ Levels vs team ", x))
  })
names(cmg$graphs[['hh_cmh']]) <- aux$cmh_teams
# hh_lev_cmh graphs by team
cmg$graphs[['hh_lev_cmh']] <-
  lapply(aux$cmh_teams,
         function(x) {
           ggplot(data = cmg$meltc[cat == "hh_lev_cmh" & cmh_team == x],
                  aes(x = imp_status, y = cases, fill = hh_cat, ymax = 1.2*cases))+
             geom_bar(stat = "identity", position = position_dodge(0.5), width = 0.5,
                      color = "black")+
             theme_light()+theme(legend.position = "top")+
             facet_wrap(~ var, as.table = FALSE, ncol = 3)+
             geom_text(aes(x = imp_status, y = cases, fill = hh_cat,
                           label = cases, hjust = 0.5),
                       position = position_dodge(0.5), hjust = -0.5, size = 3)+
             coord_flip()+
             scale_fill_manual(name = NULL, values = aux$colors[c(1:4)])+
             labs(x = NULL, y = "number of consumers",
                  title = paste0("Health Home w/ Levels vs team ", x))
         })
names(cmg$graphs[['hh_lev_cmh']]) <- aux$cmh_teams
