agg <- new.env(parent = .GlobalEnv)

agg$locus_levels$all <-
  locus[, list(cases = length(unique(case_no))), keyby = list(comb_disp)]
agg$locus_levels$prog <-
  locus[, list(cases = length(unique(case_no))),
        keyby = list(comb_disp, program)]

agg$loc_3mon$all <-
  prep$loc_low_lev[between(service_date, adm_effdt, three_mon_end),
                   list(consumers = length(unique(case_no)),
                        total_units = sum(units),
                        service_count = length(cpt_code)),
                   by = list(um_desc, svc_fy)]
agg$loc_6mon$all <-
  prep$loc_low_lev[between(service_date, adm_effdt, six_mon_end),
                   list(consumers = length(unique(case_no)),
                        total_units = sum(units),
                        service_count = length(cpt_code)),
                   by = list(um_desc, svc_fy)]
agg$loc_3mon$prog <-
  prep$loc_low_lev[between(service_date, adm_effdt, three_mon_end),
                   list(consumers = length(unique(case_no)),
                        total_units = sum(units),
                        service_count = length(cpt_code)),
                   by = list(um_desc, svc_fy, program)]
agg$loc_6mon$prog <-
  prep$loc_low_lev[between(service_date, adm_effdt, six_mon_end),
                   list(consumers = length(unique(case_no)),
                        total_units = sum(units),
                        service_count = length(cpt_code)),
                   by = list(um_desc, svc_fy, program)]
# levels to use for factoring
agg$loc_levels <-
  prep$loc_low_lev[between(service_date, adm_effdt, three_mon_end),
                   list(consumers = length(unique(case_no)),
                        total_units = sum(units),
                        service_count = length(cpt_code)),
                   by = list(um_desc)][order(total_units), unique(um_desc)]

agg$loc_3mon$all[, um_desc := factor(um_desc, levels = agg$loc_levels)]
agg$loc_6mon$all[, um_desc := factor(um_desc, levels = agg$loc_levels)]
agg$loc_3mon$prog[, um_desc := factor(um_desc, levels = agg$loc_levels)]
agg$loc_6mon$prog[, um_desc := factor(um_desc, levels = agg$loc_levels)]

# admission status ------------------------------------------------------------
agg$adm_status$all <-
  prep$adm_new[, list(adm = sum(adm, na.rm = TRUE),
                      disc = sum(disc, na.rm = TRUE),
                      active = length(unique(case_no))),
               by = list(span_label)]
agg$adm_status$prog <-
  prep$adm_new[, list(adm = sum(adm, na.rm = TRUE),
                      disc = sum(disc, na.rm = TRUE),
                      active = length(unique(case_no))),
               by = list(span_label, program)]
agg$adm_status$all[, span_label := factor(span_label,
  levels = agg$adm_status$all[, unique(span_label)])]
agg$adm_status$prog[, span_label := factor(span_label,
  levels = agg$adm_status$all[, unique(span_label)])]


# number of new consumer admissions for CMH core collectively
agg$new_adm$all <-
  prep$adm[, list(num_adm = length(unique(case_no))),
           by = list(span_label)]
agg$new_adm$prog <-
  prep$adm[, list(num_adm = length(unique(case_no))),
           by = list(span_label, program)]
agg$new_adm$all[, span_label := factor(span_label,
  levels = agg$new_adm$all[, span_label])]
agg$new_adm$prog[, span_label := factor(span_label,
  levels = agg$new_adm$all[, span_label])]

