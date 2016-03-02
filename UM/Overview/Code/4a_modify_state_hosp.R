state <- new.env(parent = .GlobalEnv)
# read in state files from data folder ---
state$budget <-
  fread(file.path(project_wd$data, "state hosp budget.csv"))
state$rates <-
  fread(file.path(project_wd$data, "state_hosp_fy_rates.csv"))

if (nrow(state$rates[fy == my_fy(max(input$end_dates))]) < 4) {
  p_stop("Expected at least 4 state facilities in current fiscal year. Please
         check state$rates:",
         file.path(project_wd$data, "state_hosp_fy_rates.csv"))
}
state_hosp <- state_hosp[!like(hosp, "Forensic Center")]
# prepare hosp for joining ---
state_hosp[, hosp := recode_hosp(hosp)]
state$rates[, hosp := recode_hosp(hosp)]
# fix overlap and other IP date issues ---
overlap_fix_dt(state_hosp, id_cols = c("case_no", "hosp"),
  start_col = "auth_eff", expire_col = "auth_exp", disc_col = "hosp_disc",
  overlap_int = 1, range_class = "Date")
state_hosp[, hosp_end := ifelse(is.na(hosp_disc), auth_exp+1, hosp_disc)]
state_hosp[, hosp_end := as.Date(hosp_end)]
state_hosp <-
  state_hosp[, list(auth_eff = min(auth_eff),
                  auth_exp = max(auth_exp),
                  hosp_disc = max(hosp_end)),
           by = list(case_no, dob, hosp, overlap_id)]
state_hosp[, overlap_id := NULL]

# dt_rate_expansion -----------------------------------------------------------
state$dt_expansion <-
  date_expansion(start_date = input$min_start,
                 end_date = input$max_end, types = c("qtr", "mon", "fy"))
setnames(state$rates, "fy", "fy_label")
# state$rates[, span_type := "fy"]
state$rates[,fy_start :=
  as.Date(as.yearmon(paste("Oct", as.num(fy_label)-1)))]
state$rates[,
  fy_end := as.Date(as.yearmon(paste("Sep", as.num(fy_label))), frac = 1)]
setkeyv(state$dt_expansion, c("span_start", "span_end"))
# join based on overlap
state$dt_expansion <- foverlaps(state$rates, state$dt_expansion,
                               by.x = c("fy_start", "fy_end"),
                               by.y = c("span_start", "span_end"),
                               type = "any", nomatch = 0)
state$dt_expansion[, Cs(fy_label, fy_start, fy_end) := NULL]
# omit Forensic Hospitals per Kelly B.
state$dt_expansion <- state$dt_expansion[hosp != "Forensic"]


setkey(state_hosp, hosp, auth_eff, hosp_disc)
state$state_full <-
  foverlaps(state$dt_expansion, state_hosp,
          by.x = c("hosp", "span_start", "span_end"),
          by.y = c("hosp", "auth_eff", "hosp_disc"),
          type = "any")
state$state_full[, Cs(hs_start, hs_end) :=
  list(pmax(auth_eff, span_start), pmin(hosp_disc, span_end))]
state$state_full[, hs_days := as.integer(hs_end-hs_start)]
state$state_full[, hs_cost := hs_days*rate]
state$state_full[between(auth_eff, span_start, span_end), new_adm := 1L]
state$state_full[between(hosp_disc, span_start, span_end), new_disc := 1L]
state$state_full[, age_status := ifelse((span_start-dob)/365.25 < 18, "child", "adult")]
# main ---
state$agg$main <-
  state$state_full[, list(consumers = aux$dist_cases(case_no),
                          cost = sum(hs_cost, na.rm = TRUE),
                          days = sum(hs_days, na.rm = TRUE),
                          admissions = sum(new_adm, na.rm = TRUE),
                          discharges = sum(new_disc, na.rm = TRUE)),
                   by = list(span_label, span_type)]
state$agg$main[, cost_lab := money_add(cost)]
# age ---
state$agg$age <-
  state$state_full[, list(consumers = aux$dist_cases(case_no),
                          cost = sum(hs_cost, na.rm = TRUE),
                          days = sum(hs_days, na.rm = TRUE),
                          admissions = sum(new_adm, na.rm = TRUE),
                          discharges = sum(new_disc, na.rm = TRUE)),
                   by = list(span_label, span_type, age_status)]
state$agg$age[, cost_lab := money_add(cost)]
# hospital name ---
state$agg$h_name <-
  state$state_full[, list(consumers = aux$dist_cases(case_no),
                          cost = sum(hs_cost, na.rm = TRUE),
                          days = sum(hs_days, na.rm = TRUE),
                          admissions = sum(new_adm, na.rm = TRUE),
                          discharges = sum(new_disc, na.rm = TRUE)),
                   by = list(span_label, span_type, hosp)]
state$agg$h_name[, cost_lab := money_add(cost)]