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
# state_hosp[, hosp_id := .GRP, by = list(case_no, auth_eff)]
state$rates[, hosp := recode_hosp(hosp)]

state_hosp[, auth_eff := min(Auth_eff, na.rm=TRUE), by = "auh_id" ]
state_hosp[, auth_exp := max(Auth_Exp, na.rm=TRUE), by = "auh_id" ]
state_hosp[, hosp_disc := max(Hosp_disc, na.rm=TRUE) , by = "auh_id" ]

#### LEFT OFF HERE ####

# state_hosp[duplicated(auh_id)]
# state_hosp[, .N, by=case_no]
test_dt <- state_hosp[case_no==221445, list(start = auth_eff, end = auth_exp)]





setnames(state$rates, "fy", "span_label")
state$rates[, span_type := "fy"]
state$rates[span_type == "fy",
  span_start := as.Date(as.yearmon(paste("Oct", as.num(span_label)-1)))]
state$rates[span_type == "fy",
  span_end := as.Date(as.yearmon(paste("Sep", as.num(span_label))), frac = 1)]
dt_expansion <- date_expansion(start_date = input$min_start,
  end_date = input$max_end, types = c("qtr", "mon", "fy"))
setkeyv(dt_expansion, c("span_start", "span_end"))
# join based on overlap
dt_rate_expansion <- foverlaps(state$rates, dt_expansion,
          by.x = c("span_start", "span_end"),
          by.y = c("span_start", "span_end"),
          type = "any")
dt_rate_expansion <- dt_rate_expansion[!is.na(span_start)]
dt_rate_expansion[, (grep(x = names(dt_rate_expansion),
  pattern = "i.", value = TRUE)) := NULL]
dt_rate_expansion <- dt_rate_expansion[!like(hosp, "Forensic")]
dt_rate_expansion[1]

state_hosp[is.na(hosp_disc), hosp_disc := input$max_end+1]

state_copy <- copy(state_hosp)


state_hosp <-
  sqldf("select distinct
      st.case_no, st.auth_eff, st.auth_exp, st.hosp_disc, st.auth_days,
      st.hosp, st.hosp_id, rate.span_label, rate.span_type, rate.rate,
      rate.span_start, rate.span_end
    from dt_rate_expansion as rate
    left join state_hosp as st on
      (rate.span_start between st.auth_eff and st.hosp_disc OR
      rate.span_end between st.auth_eff and st.hosp_disc) AND
      st.hosp = rate.hosp")
state_hosp <- as.data.table(state_hosp)
state_hosp <- state_hosp[!is.na(case_no)]


state_hosp[case_no==14312 & span_type == "mon", length(unique(span_label))]

### LEFT OFF HERE --- use foverlaps one more time with state_hosp


# join (cartesian) and filter to only what we want ----------------------------
state_hosp <- state$rates[state_hosp, on = c(hosp = "hosp"),
                          allow.cartesian = TRUE]
state_hosp <- state_hosp[between(auth_eff, fy_start, fy_end) |
                         between(auth_exp, fy_start, fy_end) |
                        (is.na(hosp_disc) &
                           between(input$max_end, fy_start, fy_end)) |
                         between(hosp_disc, fy_start, fy_end)]
state_hosp[, days_overlap :=
  aux$count_overlap(fy_start, fy_end, h_start = auth_eff,
                    h_exp = auth_exp, h_end = hosp_disc)]
state_hosp[, cost := rate*days_overlap]
state_hosp[, list(cost = sum(cost, na.rm = TRUE)),
           by =]


state_hosp

# MONTHLY/QUARTERLY/YEARLY ---
# cost (consumers)
# (+ admissions, - discharges) enrolled
# adult vs child: enrolled
# adult vs child: cost (enrolled)
# cost & consumers grouped by state facility: cost (enrolled)
# FY ONLY: costs with budget projection
