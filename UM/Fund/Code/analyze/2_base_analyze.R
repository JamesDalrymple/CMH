modify <- new.env(parent = .GlobalEnv)

#### Load Data ####
### load, save, compress data ###
modify$new_fb_names <- c("case_no", "cpt", "unit_type", "from_date", "thru_date",
                  "units", "cost")
modify$old_fb_names <- c("CASE #", "PRI PROCEDURE CODE", "UNIT TYPE", "FROM DATE",
                         "THRU DATE", "UNITS", "ALLOWED AMOUNT")
setnames(fb_data, old = modify$old_fb_names, new = modify$new_fb_names)
fb_data <- fb_data[, .SD, .SDcols = modify$new_fb_names]
fb_data[, case_no := as.numeric(case_no)]

# admit
admit <- sql$output[['admit']]
setnames(admit, old = "primary_provide_or_not", new = "prim_provider")
# insure - sheet1
insure <- sql$output[['insure']]
setnames(insure,
  old = c("medicaid_related", "medicare_A", "medicare_B", "medicare_A_B",
    "medicare_D"),
  new = c("medicaid", "a", "b", "a_b", "d"))

# insurance detail
ins_detail <- sql$output[['pvt_insure']]
# case load
case_load <- sql$output[['case_load']]
# court order repetition & PRR
court <- sql$output[['court']]

# current state hospital consumers
modify$state_hosp <- sql$output[['state_hosp']]

# demo
# demo <- sql$output[['demo']]
# setnames(demo, names(demo), tolower(names(demo)))
# demo[, primarycarephysician :=
#   gsub(x=primarycarephysician, pattern="\n ", fixed=TRUE, replace="")]
# diagnoses ... download as xls file and keep first tab only
# diagnoses <- sql$output[['diagnoses']]
# locus - run in E.1, download as Excel file
locus <- sql$output[['locus']]
setnames(locus,
         old = c("recommended_disposition", "overidden_disposition",
                 "cmh_adm_date"),
         new = c("recommend", "override", "adm_date"))
#### manipulate data ####
### demographics data - contains primary care doctor ###
# demo <- unique(demo)

### court data ###
# remove dups if exist (probably un-needed)
# court <- unique(court)

court[, cs_order_date := as.Date(cs_order_date)]
modify$levels <- court[, rev(unique(ordertype))]
court[, ordertype := factor(ordertype, levels = modify$levels)]
court <- court[order(case_no, ordertype)]
court[, group := .GRP, by = list(case_no)]
court[, index := .N, by = group]
court <- court[!(index > 1 & ordertype == "Other")]
court[, index := .N, by = group]
court[, max_order_dt := max(cs_order_date), by = list(case_no)]
court <- court[max_order_dt == cs_order_date]
court[, c("group", "index", "max_order_dt") := NULL]
court[, ordertype := as.character(ordertype)]

### insure ###
# make medicare column
insure[is.na(a) & is.na(b) & is.na(a_b) &
       is.na(d), medicare := "N"]
insure[is.na(medicare), medicare := "Y"]
insure[, c("a", "b", "a_b", "d") := NULL]
## remove certain primary and secondary insurance for listed 'insurances' ##
modify$not_ins <-
  c(
    "Assessed for Medicaid Expansion - Eligible",
    "Assessed for Medicaid Expansion - Not Eligible",
    "Autism", "Adoption Subsidy", "WRAPAROUND - LIVINGSTON")
setkey(insure, primary_ins)[J(modify$not_ins), primary_ins := NA]
setkey(insure, secondary_ins)[J(modify$not_ins), secondary_ins := NA]
setkey(insure, other_ins)[J(modify$not_ins), other_ins := NA]
# remove COFR consumers from results per Kelly B 11/4/2015
modify$cofr_cases <-
  unique(insure[grep(x = primary_ins, pattern = "COFR"), case_no],
         insure[grep(x = secondary_ins, pattern = "COFR"), case_no],
         insure[grep(x = other_ins, pattern = "COFR"), case_no])

## Medicare HMO ##
# set medicare="Y" if consumer has HMO Medicare
insure[grep(x = primary_ins, pattern = "medicare", ignore.case = TRUE),
       medicare := "Y"]
insure[grep(x = secondary_ins, pattern = "medicare",
            ignore.case = TRUE), medicare := "Y"]
invisible(insure[grep(x = other_ins, pattern = "medicare",
                      ignore.case = TRUE), medicare := "Y"])
# remove medicare HMO from private insurance
insure[grep(x = primary_ins, pattern = "medicare", ignore.case = TRUE),
       primary_ins := NA]
insure[grep(x = secondary_ins, pattern = "medicare",
            ignore.case = TRUE), secondary_ins := NA]
invisible(insure[grep(x = other_ins, pattern = "medicare", ignore.case =
                        TRUE), other_ins := NA])

## make primary insurance column ##
insure[!is.na(primary_ins) | !is.na(secondary_ins) |
       !is.na(other_ins), private_insurance := "Y"]
insure[is.na(primary_ins) & is.na(secondary_ins) & is.na(other_ins),
       private_insurance := "N"]
# remove duplicates
insure <- unique(insure)
### make fund columns ###
# add MI Child to Medicaid
setkey(insure, medicaid)[J(c("Medicaid", "MI Child")), fund := "Medicaid"]
# spend-down
setkey(insure, medicaid)[J(c("S/D/N", "S/D/Y")), fund := "spend-down"]
invisible(setkey(insure, NULL))
insure[fund == "spend-down" &
         waiver == "HAB Waiver", fund := "HAB spend-down"]

# per Kelly B on 9/25/2015
insure[is.na(fund) & waiver == "HAB Waiver", fund := "Medicaid"]
# HMP
insure[medicaid == "HMP", fund := "HMP"]

# insure[waiver == "HAB Waiver" & is.na(fund)]
# insure[is.na(fund) &
#          (medicare == "Y" | private_insurance == "Y")][waiver=="HAB Waiver"]

# grouping Medicare and private ins. together
insure[is.na(fund) &
         (medicare == "Y" | private_insurance == "Y"),
       fund := "Medicare/TPP"]
# GF consumers because they have no insurance (as far as we know)
insure[is.na(fund), fund := "GF"]
#### unique(insure[, c("waiver", "medicaid", "fund"), with=FALSE]) # all possibilities

# remove medicaid columns
# insure[, c("medicaid", "waiver") := NULL]
# get rid of case_no NAs
insure <- insure[!is.na(case_no)]

### ins_detail ###
# ins_detail <- unique(ins_detail)
ins_detail <-
  setkey(ins_detail, insurance_name)[!J(modify$not_ins)]
ins_detail <-
  ins_detail[!grep(x = insurance_name, pattern = "COFR")]
ins_detail <-
  ins_detail[!grep(x = insurance_name, pattern = "Medicare", ignore.case =
                     TRUE)]
add_private_ins <- merge(insure, ins_detail, all.y = TRUE,
  by = "case_no")[!is.na(private_insurance)][private_insurance == "N"]

setkey(insure, case_no)[J(add_private_ins[,
  unique(case_no)]), private_insurance := "Y"]
rm(add_private_ins, ins_detail)

### admissions ###
# convert dates
date_cols = c("team_effdt", "team_expdt", "cmh_effdt", "cmh_expdt")
for (j in date_cols)
  set(admit, j = j, value = as.Date(admit[[j]]))
rm(date_cols, j)
# keep only the teams in teamCMH
admit[, team := cmh$cmh_recode(team)]
admit[, team := cmh$cmh_teams_f(team)]
admit <- admit[!is.na(team)]
# remove duplicates
admit <- unique(admit)

## last team per consumer ##
admit[is.na(team_expdt), team_expdt := Sys.Date() + 9999]
admit <-
  merge(admit, admit[, list(team_expdt = max(team_expdt)), by = .(case_no)],
        all.y = TRUE, by = c("case_no", "team_expdt"))
admit <- admit[order(case_no, team_expdt)]

modify$last_team <- admit[, list(case_no = unique(case_no)), by = team]
if (modify$last_team[, length(case_no)] != admit[, length(unique(case_no))]) {
  stop("error in last_team, duplicates found, please fix!")
}

# not best approach - mark all cases never marked primary as primary... works for now
cases_not_prim <- setdiff(admit[prim_provider == "N", unique(case_no)],
                          admit[prim_provider == "Y", unique(case_no)])
admit[case_no %in% cases_not_prim, prim_provider := "Y"]
rm(cases_not_prim)

# keep most recent team only (closed consumers are kept this way)
admit <- admit[prim_provider == "Y"]

## find the lowest priority team for each consumer ##
# keep only the team with the lowest priority
admit <- cmh$cmh_priority_dt[admit, on = "team"]

# find lowest priority per consumer
admit[, minPriority := min(priority), by = list(case_no)]

# keep the lowest priority per consumer (for simplicity)
admit <- admit[minPriority == priority]
# remove priority columns
admit[, c("priority", "minPriority") := NULL]
# max cmh - have open cases override all closed cases
admit[, max_cmh_expdt := max(cmh_expdt), by = list(case_no)]
if (nrow(admit[is.na(cmh_expdt) &
               !is.na(max_cmh_expdt)]) > 0) {
  stop("duplicate admission errors, fix now")
}
admit <-
  admit[cmh_expdt == max_cmh_expdt |
          (is.na(cmh_expdt) & is.na(max_cmh_expdt))]
# using only 1 (most recent) team for consumer for simplicity
admit[, c("team_effdt", "team_expdt", "prim_provider", "max_cmh_expdt") := NULL]
# remove duplicates
admit <- unique(admit)
### funding bucket ###
# aggregating data to condense multiple rows that have the same consumer, CPT code, and date into one row
fb_data <- fb_data[, list(cost = sum(cost, na.rm = TRUE),
                          units = sum(units, na.rm = TRUE)),
                   by = list(case_no, cpt, unit_type, from_date)]

## date conversion and date filter ##
fb_data[, from_date := as.Date(from_date)]
fb_data <- fb_data[between(from_date, cmh$date_convert(input$start_date),
                   cmh$date_convert(input$end_date))]
## establish fund based on file input ##
fb_data <- mmerge(l = list(fb_data, insure, admit), all.x = TRUE, by="case_no")
setkey(fb_data, NULL)

# missing team to be labeled as Non-CMH
fb_data[is.na(team), team := "Non-CMH"]
## create Program ##
setkey(fb_data, team)
# MI Adult
fb_data[J(c("MI", "ACT")), program := "MI Adult"]
# DD Adult
fb_data[J("DD"), program := "DD Adult"]
# Youth and Family
fb_data[J(c("Child", "Child HB")), program := "Y&F"]
# Non-CMH
fb_data[J("Non-CMH"), program := "Non-CMH"]
## months, quarters, and years columns ##
# create fiscal year column
fb_data[, fy := cmh$my_fy(from_date)]
# create fiscal quarter column
fb_data[, qtr := cmh$my_qtr(from_date)]
# create month column
fb_data[, month_date := as.yearmon(from_date)]
## set fy/qtr to NA if date/cpt code are NA ##
fb_data[is.na(cpt), fy := NA_character_]
fb_data[is.na(cpt), qtr := NA_character_]
fb_data[, unit_type := trim(unit_type)]

### code descriptions ###
setnames(um_code_desc,
         names(um_code_desc),
         gsub(x = names(um_code_desc), pattern = "[.]", replace = " "))
um_code_desc[, c("State Svc Desc") := NULL]
# change names
setnames(um_code_desc, old = c("UM Desc", "CPT CD"), new = c("UM_desc", "cpt"))
# merge services with um code desc
fb_data <- um_code_desc[fb_data, on="cpt"]
rm(um_code_desc)

# for blank UM description, replace with CPT code
fb_data[is.na(UM_desc), UM_desc := "missing UM Desc"]

### case load dataset ###
# NA case numbers removed
case_load <- case_load[!is.na(case_no)]

### locus ###
# convert dates
date_cols = c("adm_date", "locus_date")
for (j in date_cols)
  set(locus, j = j, value = as.Date(locus[[j]]))
rm(date_cols, j)
# locus disposition level
locus[is.na(override), disp := recommend]
locus[!is.na(override), disp := override]
# find recent admissions (last 6 months)
locus[, days_cmh_open := cmh$date_convert(input$end_date) - adm_date]
locus[between(days_cmh_open,-Inf, 182), open_status := "0 to 6 months"]
locus[between(days_cmh_open, 183, 364), open_status := "6 to 12 months"]
locus[, adm_diff := as.numeric(adm_date - locus_date)]
locus[, min_adm_diff := min(adm_diff), by = list(case_no)]
locus <- locus[min_adm_diff == adm_diff]
# remove unwanted columns
locus[, c("adm_diff", "min_adm_diff", "recommend",
          "override", "adm_date") := NULL]
locus <- locus[!is.na(open_status)]
locus[, disp := trim(disp)]
locus[, disp := sapply(disp, aux$word_to_num)]
# add TCM to locus consumers ... inner join
modify$TCM_by_con <- fb_data[team %in% c("ACT", "MI") & cpt == "T1017",
                      list(TCM = length(cpt)), by = case_no]
locus <- merge(locus, modify$TCM_by_con, all.x = TRUE, by = "case_no")
locus[is.na(TCM), TCM := 0]
### create prediction disposition ###
# consumers open less than 6 months
# locus[, pred_level := NA_integer_]
locus[open_status == "0 to 6 months", pred_level := disp]
locus[open_status == "6 to 12 months",
      pred_TCM := 365 * TCM / as.integer(days_cmh_open)]
locus[open_status == "6 to 12 months",
      pred_TCM_level := aux$levelTCM(pred_TCM)]
locus[open_status == "6 to 12 months",
      pred_model := as.integer(days_cmh_open - 182) / 182 * pred_TCM_level +
        (182 - as.integer(days_cmh_open - 182)) / 182 * disp]
locus[open_status == "6 to 12 months",
      pred_round := pmin(round(pred_model), 3)]
locus[, replace_level := ifelse(!is.na(pred_round), pred_round, pred_level)]
locus[, setdiff(names(locus), c("case_no", "replace_level")) := NULL]

#### aggregate data ####
# services by consumers
service_same_day <-
  fb_data[!(unit_type %in% c("Day", "Encounter")),
          list(records = length(from_date),
               units = sum(units, na.rm = TRUE)),
          by = c("case_no", "cpt", "UM_desc", "unit_type")]
service_range_day <- fb_data[unit_type %in% c("Day", "Encounter"),
                             list(records = sum(units, na.rm = TRUE),
                                  units = sum(units, na.rm = TRUE)),
                             by = c("case_no", "cpt", "UM_desc", "unit_type")]
services <- rbindlist(list(service_same_day, service_range_day))
## create error message if duplicates exist - dont worry about missing UM Desc ##
dup_services <- services[, list(case_no, UM_desc)]
dup_services <- services[duplicated(services), unique(UM_desc)]
if (length(dup_services[dup_services != "missing UM Desc"]) > 0) {
  print(data.frame(rep(
    paste("stop...warning...", dup_services), 10
  )))
  stop("services have duplicates - investigate!")
}
services <-
  services[, list(records = sum(records, na.rm = TRUE),
                  units = sum(units, na.rm = TRUE)),
           by = list(case_no, cpt, UM_desc, unit_type)]
rm(service_same_day, service_range_day, dup_services)

# change names
setnames(services, old = "UM_desc", new = "cpt_desc")

# find typical range
services[, c("typical_record_range", "full_record_range",
             "sd_records", "total_records")
         := list(aux$my_range(records, 0.25, 0.75),
                 aux$my_range(records, 0.0, 1.0),
                 round(sd(records), 2),
                 sum(records, na.rm = TRUE)),
         by=list(cpt, cpt_desc, unit_type)]
services[, outlier := !aux$in_range(x=records, range=typical_record_range),
         by=list(cpt, cpt_desc, unit_type)]

# cost by consumers and cpt code
modify$cost_cpt <- fb_data[, list(cost = sum(cost, na.rm = FALSE)),
                    by = c("case_no", "cpt")]
# join cost to services
services[modify$cost_cpt, c("cost") := cost, on = c("case_no", "cpt") ]
# join team
services[admit, team := team, on = "case_no"]

# remove COFRs per Kely B. 11/4/2015
services <- services[case_no %nin% modify$cofr_cases]
# remove state hospital consumers per Kelly B. 11/4/2015
services <- services[case_no %nin% modify$state_hosp[, case_no]]

# MI Adult Levels -------------------------------------------------------------
# MI Adult - Level 0 - no contacts
mi_consumers <- services[team %in% c("ACT", "MI"), unique(case_no)]
mi_nonzeros <- services[cpt=="T1017" &
                          team %in% c("ACT", "MI"), unique(case_no)]
services[case_no %in%
           setdiff(mi_consumers, mi_nonzeros), level := "L0_No_TCM"]
rm(mi_consumers, mi_nonzeros)
# MI Adult - Level 1 - 1-2 TCM contacts
modify$mi_TCM <- services[team %in% c("ACT", "MI") & cpt=="T1017",
                   list(TCM = sum(records)),
                   by = list(case_no, team)]
modify$mi_TCM[, level := cut(TCM, breaks = c(1, 2, 12, Inf),
                      labels = c("L1", "L2", "L3"),
                      include.lowest = TRUE)]
modify$mi_TCM[, level := as.character(level)]
services[modify$mi_TCM, level := i.level, on = "case_no"]
# locus is adding levels to non-MI Adults
services[locus, level := paste0("L", pmin(replace_level, 3)), on = "case_no"]
services[team %nin% c("ACT", "MI"), level := NA]

# MI Adult - Level 5 - Residential
services[team=="ACT", level := "L4_ACT"]
# MI Adult - Level 5 - Residential
services[case_load, c("supervisor", "primary_staff") :=
           list(supervisor, primary_staff),
         on = "case_no"]
services[supervisor == "Hoener, Katie" |
           primary_staff == "Hoener, Katie", level := "L5_Residential"]
# create program column
services[is.na(team), team := "non-CMH"]
services[, program := cmh$recode_team_prog(x=team)]

# summary ---------------------------------------------------------------------
modify$team_summary <-
  services[, list(num_cases = length(unique(case_no))),
           by = list(program, team)]
modify$mi_level_summary <-
  services[team %in% c("MI", "ACT"),
           list(num_cases = length(unique(case_no))),
           keyby = list(level, program, team)]

svc_summary <-
  services[, list(num_cases = length(unique(case_no)),
                  total_cost = sum(cost, na.rm = TRUE),
                  avg_cost = round(mean(cost, na.rm = TRUE)),
                  sd_cost = round(sd(cost, na.rm = TRUE), 3),
                  total_units = sum(units, na.rm = TRUE),
                  avg_units = round(mean(units, na.rm = TRUE), 2),
                  unit_range_full = aux$my_range(units, 0.0, 1.0),
                  unit_range_typical = aux$my_range(units, 0.25, 0.75),
                  sd_units = round(sd(units, na.rm = TRUE), 2)),
           by = list(program, team, level, cpt, cpt_desc, unit_type,
                     typical_record_range, full_record_range, sd_records)]

modify$outlier_cases <-
  services[isTRUE(outlier),
         list(num_outlier_cases = length(unique(case_no)),
              outlier_cost = sum(cost, na.rm = TRUE),
              avg_outlier_cost = round(mean(cost, na.rm = TRUE), 2),
              sd_outlier_cost = sd(cost, na.rm = TRUE),
              sd_outlier_records = round(sd(records), 2),
              outlier_cost_range = aux$my_money_range(cost),
              outlier_units = sum(units, na.rm = TRUE),
              avg_outlier_units = round(mean(units, na.rm = TRUE), 2),
              sd_outlier_units = round(sd(units, na.rm = TRUE), 2)),
         by = list(program, team, level, cpt, cpt_desc, unit_type,
                   typical_record_range, full_record_range, sd_records)]
modify$typical_cases <-
  services[isFALSE(outlier),
           list(num_typical_cases = length(unique(case_no)),
                typical_cost = sum(cost, na.rm = TRUE),
                avg_typical_cost = round(mean(cost, na.rm = TRUE), 2),
                sd_typical_cost = sd(cost, na.rm = TRUE),
                sd_typical_records = round(sd(records), 2),
                typical_cost_range = aux$my_money_range(cost),
                typical_units = sum(units, na.rm = TRUE),
                avg_typical_units = round(mean(units, na.rm = TRUE), 2),
                sd_typical_units = round(sd(units, na.rm = TRUE), 2)),
           by = list(program, team, level, cpt, cpt_desc, unit_type,
                     typical_record_range, full_record_range, sd_records)]
svc_summary[modify$typical_cases,
            c("cpt", "num_typical_cases", "typical_cost", "avg_typical_cost",
              "sd_typical_cost", "sd_typical_records", "typical_cost_range",
              "typical_units", "avg_typical_units", "sd_typical_units")
            := list(cpt, num_typical_cases, typical_cost, avg_typical_cost,
                    sd_typical_cost, sd_typical_records, typical_cost_range,
                    typical_units, avg_typical_units, sd_typical_units),
            on = c("program", "team", "level", "cpt", "cpt_desc", "unit_type",
                    "typical_record_range", "full_record_range", "sd_records")]
svc_summary[modify$outlier_cases,
            c("cpt", "num_outlier_cases", "outlier_cost", "avg_outlier_cost",
              "sd_outlier_cost", "sd_outlier_records", "outlier_cost_range",
              "outlier_units", "avg_outlier_units", "sd_outlier_units")
            := list(cpt, num_outlier_cases, outlier_cost, avg_outlier_cost,
                    sd_outlier_cost, sd_outlier_records, outlier_cost_range,
                    outlier_units, avg_outlier_units, sd_outlier_units),
            on = c("program", "team", "level", "cpt", "cpt_desc", "unit_type",
                   "typical_record_range", "full_record_range", "sd_records")]

svc_summary[, .SD, .SDc = c("num_cases", "num_typical_cases")]

# column re-ordering for human readability


grep(x=names(svc_summary), pattern="unit", value=TRUE)

setcolorder(svc_summary,
  c("program", "team", "level", "cpt", "cpt_desc", "unit_type",
  "num_cases", "num_typical_cases", "num_outlier_cases",
  "total_cost", "typical_cost", "outlier_cost",
  "avg_cost", "avg_typical_cost", "avg_outlier_cost",
  "sd_cost", "sd_typical_cost", "sd_outlier_cost",
  "full_record_range", "typical_record_range",
  # "outlier_record_range" - not needed unless asked for by Kelly
  "sd_typical_records", "sd_outlier_records", "sd_records",
  "total_units", "avg_units", "unit_range_full", "sd_units",
  "unit_range_typical", "typical_units", "avg_typical_units",
  "sd_typical_units", "outlier_units", "avg_outlier_units",
  "sd_outlier_units", "typical_cost_range", "outlier_cost_range"))
setorder(svc_summary, program, team, cpt)

rm(admit, case_load, court, fb_data, insure, locus, file_list)