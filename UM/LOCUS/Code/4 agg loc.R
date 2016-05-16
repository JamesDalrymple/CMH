agg <- new.env(parent = .GlobalEnv)

agg$locus_levels <-
  locus[, list(cases = length(unique(case_no))), keyby = list(comb_disp)]


agg$loc_3mon <-
  prep$loc_low_lev[between(service_date, adm_effdt, three_mon_end),
                   list(consumers = length(unique(case_no)),
                        total_units = sum(units),
                        service_count = length(cpt_code)), by = um_desc]
write.csv(agg$loc_3mon,
          file = file.path(project_wd$results,
          "monthly over 30 days admissions.csv"), row.names = FALSE)

agg$loc_6mon <-
  prep$loc_low_lev[between(service_date, adm_effdt, six_mon_end),
                   list(consumers = length(unique(case_no)),
                        total_units = sum(units),
                        service_count = length(cpt_code)), by = um_desc]
write.csv(agg$loc_6mon,
          file = file.path(project_wd$results,
          "monthly over 30 days admissions.csv"), row.names = FALSE)

agg$loc_3mon[, um_desc := factor(um_desc,
  levels = agg$loc_3mon[order(total_units), um_desc])]
agg$loc_6mon[, um_desc := factor(um_desc,
  levels = agg$loc_6mon[order(total_units), um_desc])]

agg$adm_status <-
  prep$adm_new[, list(adm = sum(adm, na.rm = TRUE),
                      disc = sum(disc, na.rm = TRUE),
                      active = length(unique(case_no))),
               by = list(span_label)]
write.csv(agg$adm_status, file = file.path(project_wd$results,
          "monthly over 30 days admissions.csv"), row.names = FALSE)

# number of new consumer admissions for CMH core collectively
agg$new_adm <-
  prep$adm[, list(num_adm = length(unique(case_no))),
           by = list(span_label)]
write.csv(agg$new_adm, file = file.path(project_wd$results,
          "new core cmh admissions.csv"), row.names = FALSE)