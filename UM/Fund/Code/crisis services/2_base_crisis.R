modify <- new.env(parent = .GlobalEnv)

#### Load Data ####
# current state hospital consumers
modify$state_hosp <- copy(sql$state_hosp)
cmh_crisis <- copy(sql$cmh_crisis)

# ------------- fixing data problem
non_cmh_crisis <- sql$non_cmh_crisis
# fix duplicate teams per consumer issue
modify$non_cmh_priority_dt<-
  data.table(team = c("WSH - PATH/PORT", "WSH - OBRA", "WSH - MH Court",
                      "WSH - Sobriety Court", NA),
             priority = 1:5)
non_cmh_crisis[modify$non_cmh_priority_dt, priority := priority,
               on = c(team2 = "team")]
non_cmh_crisis[, min_priority := min(priority), by = "case_no"]
non_cmh_crisis[modify$non_cmh_priority_dt, team2 := i.team,
               on = c(min_priority = "priority")]
# non_cmh_crisis[case_no == 1137717] # duplicate teams for this consumer
non_cmh_crisis[, c("min_priority", "priority") := NULL]
# ----------------------- data issue fixed #
insure <- sql$insure
# case load
case_load <- sql$case_load
ins_detail <- sql$pvt_insure
locus <- sql$locus
setnames(locus,
         old = c("recommended_disposition", "overidden_disposition",
                 "cmh_adm_date"),
         new = c("recommend", "override", "adm_date"))
# #### manipulate data ####
# ### demographics data - contains primary care doctor ###
# demo <- unique(demo)

# ### court data ###
# court[, cs_order_date := as.Date(cs_order_date)]
# modify$levels <- court[, rev(unique(ordertype))]
# court[, ordertype := factor(ordertype, levels = modify$levels)]
# court <- court[order(case_no, ordertype)]
# court[, group := .GRP, by = list(case_no)]
# court[, index := .N, by = group]
# court <- court[!(index > 1 & ordertype == "Other")]
# court[, index := .N, by = group]
# court[, max_order_dt := max(cs_order_date), by = list(case_no)]
# court <- court[max_order_dt == cs_order_date]
# court[, c("group", "index", "max_order_dt") := NULL]
# court[, ordertype := as.character(ordertype)]

### insure ###
setnames(insure, "medicaid_related", "medicaid")
# make medicare column
insure[is.na(medicare_A) & is.na(medicare_B) & is.na(medicare_A_B) &
       is.na(medicare_D), medicare := "N"]
insure[is.na(medicare), medicare := "Y"]
insure[, grep(x=names(insure), pattern="medicare_", value = TRUE) := NULL]
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
# insure[case_no %in% modify$cofr_cases,
# .SD, .SDc = c("case_no", "primary_ins")]

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
# insure[, table(waiver, medicaid)] #### checking ...
# HMP
insure[medicaid == "HMP", fund := "HMP"]

# grouping Medicare and private ins. together
insure[is.na(fund) &
         (medicare == "Y" | private_insurance == "Y"),
       fund := "Medicare/TPP"]
# GF consumers because they have no insurance (as far as we know)
insure[is.na(fund), fund := "GF"]

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

# cmh crisis ------------------------------------------------------------------
setnames(cmh_crisis, names(cmh_crisis), tolower(names(cmh_crisis)))
setnames(
  cmh_crisis,
  old = c("cpt_code", "medicaid_related"),
  new = c("cpt", "medicaid")
)

# CRS is not a CMH team ... have teRi fix this. Removing this for now.
cmh_crisis <- cmh_crisis[!team=="Crisis Residential Services"]
cmh_crisis[, team := wccmh::cmh_recode(team)]
cmh_crisis[, program := wccmh::recode_team_prog(team)]
cmh_crisis[fb_unit_type, unit_type := unit_type,
           on = c(cpt = "cpt", cpt_modifier = "mod")]
cmh_crisis[fb_unit_type, unit_type := unit_type,
           on = c(cpt = "cpt", cpt_modifier = "mod")]
cmh_crisis[is.na(unit_type), unit_type := "missing"]
cmh_crisis[um_code_desc, cpt_desc := UM.Desc, on = c(cpt = "CPT.CD")]

modify$TCM_cmh <- cmh_crisis[cpt == "T1017" & program =="MI",
           list(TCM = length(cpt)), by = case_no]
locus <- merge(locus, modify$TCM_cmh, all.x = TRUE, by = "case_no")
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
# non_cmh_crisis --------------------------------------------------------------
setnames(
  non_cmh_crisis,
  old = c("cpt_code", "medicaid_related", "team2"),
  new = c("cpt", "medicaid", "team")
)
non_cmh_crisis[fb_unit_type, unit_type := unit_type,
           on = c(cpt = "cpt", cpt_modifier = "mod")]
non_cmh_crisis[is.na(unit_type), unit_type := "missing"]
non_cmh_crisis[um_code_desc, cpt_desc := UM.Desc, on = c(cpt = "CPT.CD")]
#### aggregate data ####

svc_agg$cmh_crisis <- aux$summary_services(cmh_crisis)
svc_agg$non_cmh_crisis <- aux$summary_services(non_cmh_crisis)

# rm(case_load, fb_data, insure, locus, file_list)