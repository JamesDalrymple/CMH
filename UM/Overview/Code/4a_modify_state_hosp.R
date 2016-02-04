state <- new.env(parent = .GlobalEnv)
state$budget <-
  fread(file.path(project_wd$data, "state hosp budget.csv"))
state$rates <-
  fread(file.path(project_wd$data, "state_hosp_fy_rates.csv"))

state_hosp <- state_hosp[!like(hosp, "Forensic Center")]


state$rates[, unique(hosp)]
state_hosp[, unique(hosp)]

# MONTHLY/QUARTERLY/YEARLY ---
# cost (consumers)
# (+ admissions, - discharges) enrolled
# adult vs child: enrolled
# adult vs child: cost (enrolled)
# cost & consumers grouped by state facility: cost (enrolled)
# FY ONLY: costs with budget projection
