agg <- new.env(parent = .GlobalEnv)

agg$adm_comb <-
rbindlist(list(
  pp$adm1[, list(cases = length(unique(case_no)),
                 approach = "prim_team duplicates"),
          by = list(program, span_label, span_type)],
  pp$adm2[, list(cases = length(unique(case_no)),
                 approach = "prim_team & min"),
          by = list(program, span_label, span_type)],
  pp$adm3[, list(cases = length(unique(case_no)),
                 approach = "adm w/ duplicates"),
          by = list(program, span_label, span_type)],
  pp$adm4[, list(cases = length(unique(case_no)),
                 approach = "priority_overlap"),
          by = list(program, span_label, span_type)],
  pp$adm5[, list(cases = length(unique(case_no)),
                 approach = "majority team days"),
          by = list(program, span_label, span_type)]
))




agg$adm_cast <- dcast(
  agg$adm_comb, program + span_label + span_type ~ approach,
  value.var = "cases")
setorder(agg$adm_cast, span_type, program, span_label)
write.csv(agg$adm_cast, file = file.path(project_wd$results,
  "adm agg by options.csv"), row.names = FALSE)
