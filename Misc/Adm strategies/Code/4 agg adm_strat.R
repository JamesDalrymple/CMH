
adm_comb <-
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

adm_comb[span_label == "Oct 2014" & program == "MI"]
adm_comb[span_label == "2014" & program == "MI"]
