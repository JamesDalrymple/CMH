# start_date will always be one year prior to end date
# start_date <- dateConvert(end_date) - 365

modify <- new.env(parent = .GlobalEnv)


# state hospital - current consumers  -----------------------------------------
state_hosp <- copy(sql$output$state_hosp)

# demo ------------------------------------------------------------------------
demo <- copy(sql$output$demo)
setnames(demo, names(demo), tolower(names(demo)))
demo[, primarycarephysician :=
  gsub(x=primarycarephysician, pattern="\n ", fixed=TRUE, replace="")]
demo <- unique(demo)

# diagnoses -------------------------------------------------------------------
diagnoses <- copy(sql$output$diagnoses)
# locus -----------------------------------------------------------------------
locus <- copy(sql$output$locus)
setnames(locus,
         old = c("recommended_disposition", "overidden_disposition",
                 "cmh_adm_date"),
         new = c("recommend", "override", "adm_date"))

# court data -  court order repetition & PRR ----------------------------------
court <- copy(sql$output$court)
court[, cs_order_date := as.Date(cs_order_date)]
levels <- court[, rev(unique(ordertype))]
court[, ordertype := factor(ordertype, levels = levels)]
court <- court[order(case_no, ordertype)]
court[, group := .GRP, by = list(case_no)]
court[, index := .N, by = group]
court <- court[!(index > 1 & ordertype == "Other")]
court[, index := .N, by = group]
court[, max_order_dt := max(cs_order_date), by = list(case_no)]
court <- court[max_order_dt == cs_order_date]
court[, c("group", "index", "max_order_dt") := NULL]
court[, ordertype := as.character(ordertype)]

# insure ----------------------------------------------------------------------
insure <- copy(sql$output$insure)
setnames(insure,
         old = c("medicaid_related", "medicare_A", "medicare_B", "medicare_A_B",
                 "medicare_D"),
         new = c("medicaid", "a", "b", "a_b", "d"))

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
# Medicare HMO - set medicare="Y" if consumer has HMO Medicare ---
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
# make primary insurance column ---
insure[!is.na(primary_ins) | !is.na(secondary_ins) |
       !is.na(other_ins), private_insurance := "Y"]
insure[is.na(primary_ins) & is.na(secondary_ins) & is.na(other_ins),
       private_insurance := "N"]
# remove duplicates
insure <- unique(insure)
# make fund columns ---
setkey(insure, medicaid)[J(c("Medicaid", "MI Child")), fund := "Medicaid"]
setkey(insure, medicaid)[J(c("S/D/N", "S/D/Y")), fund := "spend-down"]
invisible(setkey(insure, NULL))
insure[fund == "spend-down" &
         waiver == "HAB Waiver", fund := "HAB spend-down"]
# HAB Waiver to Medicaid per Kelly B on 9/25/2015
insure[is.na(fund) & waiver == "HAB Waiver", fund := "Medicaid"]
insure[medicaid == "HMP", fund := "HMP"]
insure[is.na(fund) &
         (medicare == "Y" | private_insurance == "Y"),
       fund := "Medicare/TPP"]
insure[is.na(fund), fund := "GF"]
insure <- insure[!is.na(case_no)]

# ins_detail ------------------------------------------------------------------
ins_detail <- copy(sql$output$pvt_insure)
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

# admissions ------------------------------------------------------------------
admit <- copy(sql$output$admit)
# convert dates
date_cols = c("team_effdt", "team_expdt", "cmh_effdt", "cmh_expdt")
setf(admit, j = date_cols, value = as.Date)
rm(date_cols)
# keep only the teams in teamCMH
admit[, team := cmh_recode(team)]
admit[, team := cmh_teams_f(team)]
admit <- admit[!is.na(team)]
# remove duplicates
admit <- unique(admit)
admit[cmh_priority_dt, priority := i.priority, on = "team"]

admit <- overlap_combine(data = admit, group_cols = Cs(case_no, cmh_effdt, cmh_expdt, priority),
  start_col = "team_effdt", end_col = "team_expdt")
admit[, max_priority := max(priority), by = .(case_no, cmh_effdt)]
admit <- admit[priority == max_priority]
admit[, Cs(priority, max_priority) := NULL]

if (nrow(admit[duplicated(case_no)]) > 0 ) {
  stop("admission duplicates found, please fix!")
}
admit[, Cs(start_date, end_date, end_col) := NULL]
admit <- unique(admit)

# Funding bucket --------------------------------------------------------------
modify$new_fb_names <-
  c("case_no", "cpt", "unit_type", "from_date", "thru_date", "units", "cost")
modify$old_fb_names <-
  c("CASE #", "PRI PROCEDURE CODE", "UNIT TYPE", "FROM DATE", "THRU DATE",
    "UNITS", "ALLOWED AMOUNT")
setnames(fb_data, old = modify$old_fb_names, new = modify$new_fb_names)
fb_data <- fb_data[, .SD, .SDcols = modify$new_fb_names]
fb_data[, case_no := as.numeric(case_no)]

# combine/sum units, costs
fb_data <- fb_data[, list(cost = sum(cost, na.rm = TRUE),
                          units = sum(units, na.rm = TRUE)),
                   by = list(case_no, cpt, unit_type, from_date)]

## date conversion and date filter ##
fb_data[, from_date := as.Date(from_date)]
fb_data <- fb_data[between(from_date, date_convert(input$start_date),
                   date_convert(input$end_date))]

## establish fund based on file input ##
fb_data <- mmerge(l = list(fb_data, admit), all = TRUE, by="case_no")
fb_data <- mmerge(l = list(fb_data, insure), all.x = TRUE, by="case_no")
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
fb_data[, fy := my_fy(from_date)]
# create fiscal quarter column
fb_data[, qtr := my_qtr(from_date)]
# create month column
fb_data[, month_date := as.yearmon(from_date)]
## set fy/qtr to NA if date/cpt code are NA ##
fb_data[is.na(cpt), fy := NA_character_]
fb_data[is.na(cpt), qtr := NA_character_]

### code descriptions ###
setnames(um_code_desc,
         names(um_code_desc),
         gsub(
           x = names(um_code_desc),
           pattern = "[.]",
           replacement = " "
         ))
um_code_desc[, c("State Svc Desc") := NULL]
# change names
setnames(um_code_desc, old = c("UM Desc", "CPT CD"), new = c("UM_desc", "cpt"))
# merge services with um code desc
fb_data <- um_code_desc[fb_data, on="cpt"]
rm(um_code_desc)

# for blank UM description, replace with CPT code
fb_data[is.na(UM_desc), UM_desc := "missing UM Desc"]

### case load dataset ###
case_load <- copy(sql$output$case_load)
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
locus[, days_cmh_open := date_convert(input$end_date) - adm_date]
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
TCM_by_con <- fb_data[team %in% c("ACT", "MI") & cpt == "T1017",
                      list(TCM = length(cpt)), by = case_no]
locus <- merge(locus, TCM_by_con, all.x = TRUE, by = "case_no")
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
#### diagnoses ####
diagnoses <- unique(diagnoses)

#### aggregate data ####
# services by consumers
setf(fb_data, j = "unit_type", stri_trim, side = "both")
fb_data[UM_desc == "Wraparound Services" &
          unit_type == "Day" & cpt == "H2022",
        UM_desc := "Wraparound Svc Day"]
service_same_day <-
  fb_data[unit_type %nin% c("Day", "Encounter"),
          list(records = length(from_date)),
          by = c("case_no", "UM_desc")]
service_range_day <- fb_data[unit_type %in% c("Day", "Encounter"),
                             list(records = sum(units, na.rm = TRUE)),
                             by = c("case_no", "UM_desc")]
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
  services[, list(records = sum(records, na.rm = TRUE)), by = list(case_no, UM_desc)]
rm(service_same_day, service_range_day, dup_services)

# change names
setnames(services, old = "UM_desc", new = "cpt")

# cost by consumers
cost <- fb_data[, list(cost = sum(cost, na.rm = FALSE)),
                by = c("case_no", "program", "team", "cmh_effdt", "cmh_expdt")]
# number of TCM units per person
TCMunits <-
  fb_data[cpt == "T1017", list(TCMunits = sum(units)), by = "case_no"]

# services reshaped from long to wide
services <-
  reshape(services, idvar = "case_no", timevar = "cpt", direction = "wide")
services <- data.table(services)

# removing 'records.' from column names
setnames(
  services,
  old = names(services)[grep(x = names(services), pattern =
                                  "records.")],
  new = gsub(
    x = names(services)[grep(x = names(services), pattern = "records.")],
    pattern = "records.", replacement = ""
  )
)
# merge services and costs
setkey(services, case_no); setkey(cost, case_no)
services <- merge(services, cost, all.x = TRUE, by = "case_no")

# merge services and TCM units
setkey(services, case_no); setkey(TCMunits, case_no)
services <- merge(services, TCMunits, all.x = TRUE, by = "case_no")

# add case_load dataset via data.table merge (like SQL join)
setkey(services, case_no); setkey(case_load, case_no)
services <- merge(services, case_load, all.x = TRUE, by = "case_no")

# add demo to services
services <- merge(services, demo, all.x = TRUE, by = "case_no")
# add insurance layer
services <- merge(services, insure, all.x = TRUE, by = "case_no")
# add diagnoses layer
services <- merge(services, diagnoses, all.x = TRUE, by = "case_no")

## add court orders to services ##
court[, case_no := as.numeric(case_no)]
services[, case_no := as.numeric(case_no)]
services <- merge(services, court, all.x = TRUE, by = "case_no")
services[is.na(ordertype), ordertype := "No Court Order"]
services[, cs_order_date := as.character(cs_order_date)]

### prepare file for saving ###
# change column order
setcolorder(services,
            c(
              c(
                "case_no", "age", "ordertype", "cs_order_date", "cmh_effdt", "cmh_expdt",
                "fund", "medicare", "private_insurance", "primary_ins", "secondary_ins",
                "other_ins", "diag1", "diag1_desc", "diag2", "diag2_desc",
                "cost", "team", "program", "primary_staff",
                "supervisor", "primary_staff_type", "primarycarephysician", "primarycareclinic",
                "TCM", "TCMunits"
              ),
              sort(setdiff(
                names(services),
                c(
                  "case_no", "age", "ordertype", "cs_order_date", "cmh_effdt", "cmh_expdt",
                  "fund", "medicare", "private_insurance", "primary_ins", "secondary_ins",
                  "other_ins",  "diag1", "diag1_desc", "diag2", "diag2_desc",
                  "cost", "team", "program", "primary_staff",
                  "supervisor", "primary_staff_type", "primarycarephysician", "primarycareclinic", "TCM", "TCMunits"
                )
              ))
            ))
# establish Programs using teams
setkey(services, team)
# Y&F
services[J(c("Child", "Child HB")), program := "Y&F"]
# MI Adult
services[J(c("MI", "ACT")), program := "MI Adult"]
# DD Adult
services[J("DD"), program := "DD Adult"]
# Non-CMH
services[J("Non-CMH"), program := "Non-CMH"]
invisible(setkey(services, NULL))

# consumers on ACT team should always have team as ACT
services[case_no %in% c(services[team == "ACT", unique(case_no)]), team := "ACT"]
services[case_no %in% c(services[team == "ACT", unique(case_no)]), program := "MI Adult"]
# consumers on DD team should always have team as DD
services[case_no %in%
  c(services[team == "DD", unique(case_no)]), team := "DD"]
services[case_no %in%
  c(services[team == "DD", unique(case_no)]), program := "DD"]

# remove duplicates
services[, cost :=
  sum(cost, na.rm = TRUE), by = "case_no"] # add costs per person
services <- unique(services)

# re-order by cost - highest to lowest
services <- services[order(-cost, program, team)]
services[!is.na(cmh_expdt), fund := "non_CMH"]
services[is.na(fund), fund := "non_CMH"]
services[team=="Non-CMH", fund := "non_CMH"]

# remove COFRs per Kely B. 11/4/2015
services <- services[case_no %nin% modify$cofr_cases]
# remove state hospital consumers per Kelly B. 11/4/2015
services <- services[case_no %nin% state_hosp[, case_no]]

### does anyone have non-ACT team with ACT services? ###
###    services[as.vector(!is.na(services[, "ACT | H0039", with=FALSE]))][team=="MI"]

### MI Adult Services ###
mi_services <- services[team == "MI" | team == "ACT"]
## MI Adult --- Level 0 --- no contacts ##
mi_services[is.na(TCM), level := "L0_No_TCM"]
## MI Adult --- Level 1 –-- 1-2 TCM contacts ##
mi_services[TCM %between% c(1, 2), level := "L1"]
## MI Adult --- Level 2 –-- 3-12 TCM contacts ##
mi_services[TCM %between% c(3, 12), level := "L2"]
## MI Adult --- Level 3 –-- 12 and above TCM contacts ##
mi_services[TCM %between% c(13, Inf), level := "L3"]
mi_services <-
  merge(mi_services, locus[, c("case_no", "replace_level"), with = FALSE],
        all.x = TRUE, by = "case_no")
mi_services[replace_level == 1, level := "L1"]
mi_services[replace_level == 2, level := "L2"]
mi_services[replace_level >= 3, level := "L3"]
mi_services[, replace_level := NULL]
# level 4: ACT
mi_services[team == "ACT", level := "ACT"]
# level 5: Residential
mi_services[supervisor == "Hoener, Katie" |
              primary_staff == "Hoener, Katie", level := "Residential"]
### Y & F ###
yf_services <- services[team %in% c("Child", "Child HB")]

#### save results ####
### create information about the file to share with end-users ###
aboutFile <- data.table(
  Report_Date = input$run_date,
  Last_Updated = as.character(Sys.time()),
  Data_Sources = c(list.files(file.path(project_wd$data),
                  pattern = "[.]"), "sql: Encompass", "funding bucket")
)
