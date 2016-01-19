### PI Monthly  Management Reports ###
#### initializing working directory and input parameters ####
rm(list = ls(envir = .GlobalEnv, all.names = TRUE))
# which computer results in correct base working directory
baseWD <- switch(
  Sys.info()["nodename"],
  "WSHSQLGP" = "C:/Users/dalrymplej", # county PC
  p_stop("other nodenames not supported yet")
)

# setup for working directories - data and results
user_input <- list(
  dataWD = file.path(baseWD,
    "Dropbox/PI Projects/Monthly Management/Data/Fiscal Year"),
  resultsWD = file.path(baseWD,
    "Dropbox/PI Projects/Monthly Management/Results/Fiscal Year"),
  codeWD = file.path(basedWD,
    "Dropbox/PI Projects/Monthly Management/R Code"),
  current_month = "October",
  current_fy = "2015",
  run_date = "11_2_2015"
)
#### source files ####
source(file.path(baseWD,
                 "Dropbox/WCCMH/R/global library.r"))
source(file.path(baseWD, codeWD, "pi auxillary v2.r"))
#### user input required ####
# current month

input$report_end <- gsub(run_date, pattern = "_", replace = "/")
# report_end <- as.Date(as.yearmon(paste(current_month, calendar_year)), frac=1) # maybe later, if we can auto
#### creating report parameters and working directories based on user input ####
# calculate calendar year
if (current_month %in% c("October", "November", "December")) {
  calendar_year <- as.character(as.numeric(current_fy) - 1)
} else {
  calendar_year <- current_fy
}

# create directory in data/results folder w/ notification if it does not exist
if (!dir.exists(file.path(baseWD, resultsWD, current_fy, current_month))) {
  dir.create(file.path(baseWD, resultsWD, current_fy, current_month))
  print(paste(
    "created directory:",
    file.path(baseWD, resultsWD, current_fy, current_month)
  ))
}
if (!dir.exists(file.path(baseWD, dataWD, current_fy, current_month))) {
  # load most recent teamStaffLevels.csv
  fy_mons_completed <-
    grep(
      x = list.files(file.path(baseWD, dataWD, current_fy)),
      pattern = ".csv",
      invert = TRUE,
      value =
        TRUE
    )
  recent_mon <-
    (as.yearmon(paste("Oct", as.numeric(calendar_year) - 1)) +
       0:(length(fy_mons_completed) - 1) / 12)[length(fy_mons_completed)]
  dir.create(file.path(baseWD, dataWD, current_fy, current_month))
  print(paste(
    "created directory:",
    file.path(baseWD, dataWD, current_fy, current_month)
  ))
  if (length(fy_mons_completed) > 0) {
    team_staff_levels <-
      fread(list.files(
        list.files(
          file.path(baseWD, dataWD, current_fy),
          pattern = substr(recent_mon, 1, 3),
          full.names = TRUE
        ),
        pattern = "team",
        full.names = TRUE
      ))
  } else {
    team_staff_levels <-
      fread(list.files(
        list.files(
          file.path(baseWD, dataWD, as.numeric(current_fy) - 1),
          pattern = "September",
          full.names = TRUE
        ),
        pattern = "team",
        full.names = TRUE
      ))
  }
  write.csv(
    team_staff_levels,
    file.path(
      baseWD,
      dataWD,
      current_fy,
      current_month,
      "teamStaffLevels.csv"
    ),
    row.names = FALSE
  )
  print(paste(
    "team_staff_levels added:",
    file.path(
      baseWD,
      dataWD,
      current_fy,
      current_month,
      "teamStaffLevels.csv"
    )
  ))
}

currentDay <- as.Date(run_date, format = "%m_%d_%Y")
fy_start <-
  as.Date(as.yearmon(paste("Oct", as.numeric(current_fy) - 1)))
fy_file <- format(fy_start, "%m_%d_%Y")
fy_file <-
  gsub(
    x = fy_file,
    pattern = "_01_",
    replace = "_1_",
    fixed = TRUE
  )
# the number of months that have passed since the start of the fiscal year
num_months <- (as.yearmon(paste(current_month, calendar_year)) -
                 as.yearmon(paste("October", as.numeric(current_fy) - 1))) *
  12 + 1
#### read in sql_data/csv_files ####
channel <- odbcConnect("wshsqlgp")
odbcQuery(channel = channel, query = "use James_CSTS")

### case load (primary staff) data
sql_case_load <- "select distinct
CMH.team, CMH.supervisor, CMH.primary_staff, CMH.staff_type,  CMH.case_no
from encompass.dbo.tblE2_CMH_Open_Consumers_w_OBRA as CMH
where CMH.County = 'Washtenaw'
and CMH.team in ('WSH - Access/Engagement', 'Community Support and Treatment Services - CSTS',
'WSH - ACT' , 'WSH - Children''s Services' ,'WSH - Children''s Services - Home Based',
'WSH - MI - Adult', 'WSH - DD Adult')
order by CMH.team,  CMH.supervisor, CMH.primary_staff"
case_load <-
  sqlQuery(channel = channel,
           query = sql_case_load,
           stringsAsFactors = FALSE)
case_load <- data.table(case_load)
### IPOS
sql_ipos <- "select distinct
CMH.team, CMH.primary_staff,
CMH.case_no, CMH.supervisor,
ip_effdt, ip_expdt, ipos_type
from encompass.dbo.E2_Fn_Consumer_Most_Recent_IPOS_Date('Washtenaw') as CMH
where CMH.team in ('WSH - Access/Engagement', 'Community Support and Treatment Services - CSTS',
'WSH - ACT' , 'WSH - Children''s Services' ,'WSH - Children''s Services - Home Based',
'WSH - MI - Adult', 'WSH - DD Adult')"
ipos <-
  sqlQuery(channel = channel,
           query = sql_ipos,
           stringsAsFactors = FALSE)
ipos <- data.table(ipos)
### last service
sql_last_svc <- "select distinct
CMH.team, CMH.primary_staff, CMH.case_no,
DATEDIFF( dd, MAX( case when Cat = 'SAL' then Claim.service_date else null end), GETDATE()) as num_days_no_service
from encompass.dbo.E2_Fn_CMH_Open_N_IPOS ('Washtenaw') as CMH
left join encompass.dbo.tblE2_SAL_claims_for4 Claim on Claim.county = CMH.County and Claim.Case_No = CMH.Case_No
and Claim.service_date between GETDATE() - 365 and GETDATE()
group by CMH.team, CMH.primary_staff, CMH.case_no"
last_svc <-
  sqlQuery(channel = channel,
           query = sql_last_svc,
           stringsAsFactors = FALSE)
last_svc <- data.table(last_svc)
# demographic errors
sql_demo_errors <- paste0(
  "select distinct
  team, case_no, num_qi_errors, supervisor, primary_staff
  from encompass.dbo.E2_Fn_QI_Demo_Errors2('Washtenaw', '",
  fy_start,
  "' , '",
  report_end,
  "') as qi"
  )
demo_errors <-
  sqlQuery(channel = channel,
           query = sql_demo_errors,
           stringsAsFactors = FALSE)
demo_errors <- data.table(demo_errors)
# health errors
sql_health_errors <- "select distinct
HI.team, HI.primary_staff_supervisor, HI.primary_staff, HI.case_no, HI.data_issue_type, HI.notes
from encompass.dbo.E2_Fn_Data_Issues_Health_Info_Download ('Washtenaw') as HI
join encompass.dbo.tblE2_Health_N_Other_Conditions as C on C.case_no = HI.Case_No"
health_errors <-
  sqlQuery(channel = channel,
           query = sql_health_errors,
           stringsAsFactors = FALSE)
health_errors <- data.table(health_errors)
# wage errors
sql_wage_errors <- paste0(
  "select distinct
  qi.case_no, qi.team, qi.primary_staff, cmh.supervisor,
  qi.EmploymentStatus as employment_status, qi.MinimumWage as minimum_wage, qi.MinimumWage_Error as minimum_wage_error
  from encompass.dbo.E2_Fn_Min_Wage_Missed('Washtenaw', '",
  fy_start,
  "' , '",
  report_end,
  "') as qi
  join encompass.dbo.tblE2_CMH_Open_Consumers_w_OBRA as CMH on CMH.County = 'Washtenaw' and
  CMH.team in ('WSH - Access/Engagement', 'Community Support and Treatment Services - CSTS',
  'WSH - ACT' , 'WSH - Children''s Services' ,'WSH - Children''s Services - Home Based',
  'WSH - MI - Adult', 'WSH - DD Adult') and qi.case_no = cmh.case_no"
  )
wage_errors <-
  sqlQuery(channel = channel,
           query = sql_wage_errors,
           stringsAsFactors = FALSE)
wage_errors <- data.table(wage_errors)
# unsigned drafts by supervisor and author
sql_unsigned_docs <- paste0(
  "select distinct
  doc.DO_TITLE as do_title, doc.doc_status, doc.do_date, doc.DO_RCDID as doc_id,
  doc.case_no, Entered_By as author, Entered_by_Sup as supervisor, team, FaceToFace as f2f
  from encompass.dbo.E2_Fn_Draft_Documents_by_Staff_Date3 ('Washtenaw',  '",
  fy_start,
  "' , '",
  report_end,
  "' ) doc
  group by DO_TITLE,
  doc.Doc_Status, doc.DO_RCDID, doc.DO_date,
  doc.Case_No, doc.Entered_By, doc.Entered_by_Sup, doc.team, doc.entered_by_sup, doc.Primary_Staff,
  doc.FaceToFace"
  )
unsigned_docs <-
  sqlQuery(channel = channel,
           query = sql_unsigned_docs,
           stringsAsFactors = FALSE)
unsigned_docs <- data.table(unsigned_docs)
# self sufficiency matrix
sql_ssm_summary <-
  paste0(
    "select supervisor, at_least2_ssms, case_load
    from dbo.JD_E2_fn_MI_atleast2_SSM('3/1/2014', '",
    report_end,
    "' )
    order by at_least2_ssms desc"
    )
ssm_summary <-
  sqlQuery(channel = channel,
           query = sql_ssm_summary,
           stringsAsFactors = FALSE)
ssm_summary <- data.table(ssm_summary)
sql_smm_case_detail <-
  paste0(
    "select distinct supervisor, primary_staff, case_no, at_least2_ssms
    from dbo.JD_E2_fn_MI_SSM_detail('3/1/2014', '",
    report_end,
    "')
    order by supervisor, Primary_Staff, case_no"
    )
smm_case_detail <-
  sqlQuery(channel = channel,
           query = sql_smm_case_detail,
           stringsAsFactors = FALSE)
smm_case_detail <- data.table(smm_case_detail)
sql_ssm_staff_detail <- paste0(
  "select
  supervisor, primary_staff, count(case_no) as SSM_2_eligible, sum(at_least2_ssms) as SSM_2_obtained,
  round(sum(at_least2_ssms)*100.0/count(case_no)*1.0, 2) as pct_SSM_2
  from dbo.JD_E2_fn_MI_SSM_detail('3/1/2014', '",
  report_end,
  "')
  group by supervisor, primary_staff"
  )
ssm_staff_detail <-
  sqlQuery(channel = channel,
           query = sql_ssm_staff_detail,
           stringsAsFactors = FALSE)
ssm_staff_detail <- data.table(ssm_staff_detail)
teamLevels <-
  fread(file.path(
    baseWD,
    dataWD,
    current_fy,
    current_month,
    "teamStaffLevels.csv"
  ))

#### team levels ####
teamLevels[, team := cmh_recode(team)]
setnames(teamLevels, "entered_by", "author")
teamOnly <- teamLevels[author == "all"]
staffOnly <- teamLevels[author != "all"]
#### case load ####
# fix teams
case_load[, team := cmh_recode(team)]

# keep only CSTS teams
case_load <- setkey(case_load, team)[J(teamsCMH)]
conTeams <-
  case_load[, list(case_load = length(unique(case_no))), by = team]
#### last service ####
# fix teams
last_svc[, team:= cmh_recode(team)]
# fix NA primary staff
last_svc[primary_staff == "", primary_staff := "missing primStaff"]
## if num_days_no_service is NA, should we count them as 180+ days without service? I think so...
last_svc[is.na(num_days_no_service), num_days_no_service := NA]
## make 30-60-90-180 day columns ##
last_svc[num_days_no_service > 30, notSeen30days := 1]
last_svc[num_days_no_service > 60, notSeen60days := 1]
last_svc[num_days_no_service > 90, notSeen90days := 1]
last_svc[num_days_no_service > 180, notSeen180days := 1]
# keep only CMH teams
last_svc <- setkey(last_svc, team)[J(teamsCMH)]
## service list for 90 and 180 days
services_list <-
  last_svc[, .SD, .SDcols = c(
    "team",
    "primary_staff",
    "case_no",
    "num_days_no_service",
    "notSeen90days",
    "notSeen180days"
  )]
services_list <-
  merge(services_list, unique(case_load[, .SD, .SDcols = c("supervisor", "primary_staff")]), by =
          "primary_staff")
setcolorder(
  services_list,
  c(
    "team",
    "supervisor",
    "primary_staff",
    "case_no",
    "num_days_no_service",
    "notSeen90days",
    "notSeen180days"
  )
)
services_list <- services_list[notSeen90days == 1 |
                                 notSeen180days == 1]
#### current IPOS ####
# fix teams
ipos[, team := cmh_recode(team)]
# change dates to date classes
ipos[, ip_effdt := dateConvert(ip_effdt)]
ipos[, ip_expdt := dateConvert(ip_expdt)]
# make a column for interim IPOS greater than 30 days - if exp date > today, use today's date per Nicole E.
setkey(ipos, ipos_type)[J("Interim IPOS"),  interimDiff :=  pmin(ip_expdt, as.Date(Sys.time())) -
                          ip_effdt]
ipos[, ipos30Plus := 0]
ipos[interimDiff > 30, ipos30Plus := 1]
ipos[, interimDiff := NULL]
## ipos_list ##
ipos_list <- ipos
setkey(ipos_list, ipos_type)[J(""), ipos_type := "blank/missing"]
ipos_list[currentDay > ip_expdt, expired := "Yes"]
ipos_list <- setkey(ipos_list, team)[J(teamsCMH)]
# consumers with a missing IPOS
missIPOS <-
  setkey(ipos, ipos_type)[J(""), list(missingIPOS = length(unique(case_no))), by =
                            team]
invisible(setkey(ipos, NULL))
conIPOS <-
  ipos[, list(totalConsumers = length(unique(case_no))), by = team]
# which consumers have a current IPOS
ipos[, current := 0]
ipos[currentDay <= ip_expdt, current := 1]
currentIPOS <-
  ipos[, list(currentIPOS = sum(current, na.rm = TRUE)), by = team]
# which consumers have an expired IPOS
expireIPOS <-
  ipos[, list(expiredIPOS = sum(current, na.rm = TRUE)), by = list(team, case_no)]
expireIPOS <- setkey(expireIPOS, expiredIPOS)[J(0)]
expireIPOS <-
  expireIPOS[, list(expiredIPOS = length(unique(case_no))), by = team]
# which consumers have a full IPOS
ipos_full <-
  setkey(ipos, ipos_type)[J("Full IPOS"), list(Full_IPOS = length(ip_effdt)), by =
                            team]
invisible(setkey(ipos_full, NULL))
ipos_full <- setkey(ipos_full, team)[J(teamsCMH)]
ipos_full[is.na(ipos_full)] <- 0
# which consumers have an Interim IPOS
ipos_interim <-
  setkey(ipos, ipos_type)[J("Interim IPOS"), list(Interim_IPOS = length(ip_effdt)), by =
                            team]
invisible(setkey(ipos_interim, NULL))
ipos_interim <- setkey(ipos_interim, team)[J(teamsCMH)]
ipos_interim[is.na(ipos_interim)] <- 0
# how many consumers have an Interim IPOS 30+ days
ipos_interim_30_plus <-
  ipos[, list(Interim_IPOS_Over_30_Days = sum(ipos30Plus, na.rm = TRUE)), by =
         team]
ipos_interim_30_plus <-
  setkey(ipos_interim_30_plus, team)[J(teamsCMH)]
ipos_interim_30_plus[is.na(ipos_interim_30_plus)] <- 0
# which consumers have an Preliminary IPOS
ipos_prelim <-
  setkey(ipos, ipos_type)[J("Preliminary IPOS"), list(Preliminary_IPOS = length(ip_effdt)), by =
                            team]
invisible(setkey(ipos_prelim, NULL))
ipos_prelim <- setkey(ipos_prelim, team)[J(teamsCMH)]
ipos_prelim[is.na(ipos_prelim)] <- 0
# which consumers have an Single Service IPOS
ipos_single <-
  setkey(ipos, ipos_type)[J("Single Service IPOS"), list(Single_Service_IPOS = length(ip_effdt)), by =
                            team]
invisible(setkey(ipos_single, NULL))
ipos_single <- setkey(ipos_single, team)[J(teamsCMH)]
ipos_single[is.na(ipos_single)] <- 0
### join IPOS data.table's
iposTeam <-
  mmerge(
    l = list(conIPOS, currentIPOS, missIPOS, expireIPOS),
    all.x = TRUE,
    by = "team"
  )

iposTeam[, pctCurrentIPOS := round(currentIPOS / totalConsumers * 100, 0)]
iposTeam[, pctMissingIPOS := round(missingIPOS / totalConsumers * 100, 0)]
iposTeam[, pctExpireIPOS := round(expiredIPOS / totalConsumers * 100, 0)]
# keep only CMH teams
iposTeam <- iposTeam[J(teamFix(teamsCMH))]
iposTeam[is.na(iposTeam)] <- 0
### the table of different types of IPOS ###
# part a - grand totals
iposTeam2a <-
  mmerge(l = list(conIPOS, currentIPOS),
         all = TRUE,
         by = "team")
iposTeam2a <- setkey(iposTeam2a, team)[J(teamsCMH)]
# part b - all ipos types
iposTeam2b <-
  setkey(ipos, team)[teamsCMH, list(currentIPOS = sum(current, na.rm = TRUE)), by =
                       list(team, ipos_type)]
iposTeam2b <-
  dcast.data.table(
    iposTeam2b,
    formula = team ~ ipos_type,
    value.var = "currentIPOS",
    fill = 0
  )
# part c - blank/missing and expired IPOS
iposTeam2cExpire <-
  ipos[ipos_type != "", list(expired = length(current) - sum(current)), by =
         list(team, case_no)]
iposTeam2cExpire <-
  iposTeam2cExpire[, list(expiredIPOS = sum(expired)), by = team]
iposTeam2cBlank <-
  ipos[ipos_type == "", list(blank_missing_IPOS = length(unique(case_no))), by = team]
iposTeam2cBlank <-
  setkey(iposTeam2cBlank, team)[teamsCMH, nomatch = NA]
iposTeam2cBlank[is.na(iposTeam2cBlank)] <- 0
iposTeam2c <-
  mmerge(
    l = list(iposTeam2cBlank, iposTeam2cExpire),
    by = "team",
    all.x = TRUE
  )
# combine parts a-c
iposTeam2 <-
  mmerge(
    l = list(iposTeam2a, iposTeam2b, iposTeam2c),
    by = "team",
    all = TRUE
  )
### IPOS tables ###
## ipos table 1
ipos_table1 <-
  mmerge(
    l = list(
      ipos_full,
      ipos_interim,
      ipos_interim_30_plus,
      ipos_prelim,
      ipos_single
    ),
    by = "team"
  )
ipos_table1[, Grand_Total := psum(Full_IPOS,
                                  Interim_IPOS,
                                  Preliminary_IPOS,
                                  Single_Service_IPOS)]
ipos_table1 <-
  cbind(month = as.character(as.yearmon(paste(
    current_month, calendar_year
  ))), ipos_table1)
## ipos table 2
ipos_table2 <- mmerge(l = list(currentIPOS, iposTeam2c), by = "team")
ipos_table2 <-
  cbind(month = as.character(as.yearmon(paste(
    current_month, calendar_year
  ))), ipos_table2)
## ipos table 3
ipos_table3 <- ipos[current == 0 | ipos_type == "",
                    list(expired_missing_IPOS = length(unique(case_no))), by =
                      list(team, supervisor, primary_staff)]
ipos_table3 <- setkey(ipos_table3, team)[teamsCMH, nomatch = NA]
ipos_table3 <- ipos_table3[order(team, supervisor)]
ipos_table3 <- ipos_table3[order(team, supervisor, primary_staff)]
ipos_table3[is.na(expired_missing_IPOS), expired_missing_IPOS := 0]
ipos_table3[is.na(supervisor), supervisor := "none/missing"]
ipos_table3 <- rbindlist(list(ipos[current == 0 | ipos_type == "",
                                   list(primary_staff = "",
                                        expired_missing_IPOS = length(unique(case_no))), by = list(team, supervisor)],
                              ipos_table3))
ipos_table3 <-
  ipos_table3[order(team, supervisor, primary_staff, expired_missing_IPOS)]
#### unsigned/draft Documents ####
# keep only Draft and Unsigned
unsigned_docs <-
  setkey(unsigned_docs, doc_status, f2f)[J(c("Draft", "Unsigned"), "Y")]
invisible(setkey(unsigned_docs, NULL))
# keep only wanted documents
unsigned_docs <-
  merge(unsigned_docs,
        data.table(do_title = docsKeep),
        by = "do_title",
        all.x = TRUE)
# fix team column
unsigned_docs[, team := teamFix(team)]
# keep all teams, per Laura H
setkey(unsigned_docs, team)[J("non-core CSTS Team"), team := "Other Team(s)"]
invisible(setkey(unsigned_docs, NULL))
unsignTeam <-
  unsigned_docs[, list(docs_unsigned = length(case_no)), by = team]
# keep only CMH teams
unsignTeam <- setkey(unsignTeam, team)[J(teamsCMH)]
unsignTeam[, month := as.character(as.yearmon(paste(current_month, calendar_year)))]

### unsigned/draft docs by supervisor and author ###
unsigned_docs[supervisor == "", supervisor := "missing supervisor"]
unsignTeam2 <-
  unsigned_docs[, list(unsigned_draft_docs = length(do_title)),
                by = list(team, supervisor, author)]
## assign levels ##
unsignTeam2 <-
  mmerge(
    l = list(unsignTeam2, teamOnly[, c("team", "level"), with = FALSE]),
    by = c("team"),
    all.x = TRUE
  )
unsignTeam2 <- mmerge(
  l = list(unsignTeam2, staffOnly),
  by = c("team", "author"),
  all.x = TRUE
)
# combine level columns into one column
unsignTeam2[, level := combCols(level.x, level.y)]
unsignTeam2[, c("level.x", "level.y") := NULL]
unsignTeam2 <- unsignTeam2[order(team, supervisor, author)]
### make unsign table for pdf doc
unsign_t1a <- unsignTeam2[, list(
  author = "",
  unsigned_draft_docs = sum(unsigned_draft_docs, na.rm =
                              TRUE)
), by = list(supervisor)]
unsign_t1b <-
  unsignTeam2[, list(unsigned_draft_docs = sum(unsigned_draft_docs, na.rm =
                                                 TRUE)), by = list(supervisor, author)]
unsign_table <- rbindlist(list(unsign_t1a, unsign_t1b))
unsign_table <- unsign_table[order(supervisor, author)]
unsign_table[author != "", supervisor := ""]
unsign_table[, unsigned_draft_docs := as.character(unsigned_draft_docs)]
unsign_table[author == "", unsigned_draft_docs := paste0("\\hspace{2cm}\\textbf{", unsigned_draft_docs, "}")]
unsign_table[author == "", supervisor := paste0("\\textbf{", supervisor, "}")]
unsign_table <- rbindlist(list(
  unsign_table,
  data.table(
    supervisor = "\\textbf{Grand total}",
    author = "",
    unsigned_draft_docs =
      paste0("\\textbf{", unsignTeam2[, sum(unsigned_draft_docs)], "}")
  )
))
#### consumers with Demographic Errors ####
## demo_error list ##
demo_errors[, team := teamFix(team)]
demo_error_list <-
  demo_errors[num_qi_errors > 0][order(team, supervisor, primary_staff)]
demo_error_list <-
  setkey(demo_error_list, team)[J(teamsCMH), nomatch = 0]
# fix teams
demo_team <- demo_errors[num_qi_errors > 0,
                         list(con_errors = length(unique(case_no))), keyby =
                           team]
demo_team <- demo_team[teamsCMH, nomatch = NA]
demo_team[is.na(demo_team)] <- 0
demo_team <- setkey(demo_team, team)[conTeams, nomatch = NA]
demo_team[is.na(demo_team)] <- 0
demo_team[, pct_errors := specify_decimal(con_errors / case_load * 100, 1)]
demo_team[, month := as.character(as.yearmon(paste(current_month, calendar_year)))]
#### consumers with Health Errors ####
# list of ordered errors
colOrder <-
  c(
    "team",
    "primary_staff_supervisor",
    "primary_staff",
    "case_no",
    "data_issue_type",
    "notes"
  )
health_error_list <- health_errors[, colOrder, with = FALSE]
setcolorder(health_error_list, colOrder)
health_error_list[, team := teamFix(team)]
health_error_list <-
  setkey(health_error_list, team)[J(teamsCMH), nomatch = 0]
# keep only wanted columns
keepCols <- c("case_no", "team", "data_issue_type")
health_errors[, setdiff(names(health_errors), keepCols) := NULL]
# fix team
health_errors[, team := teamFix(team)]
# remove duplicates
health_errors <- unique(health_errors)
# aggregate data
health_team <-
  health_errors[, list(con_errors = length(unique(case_no))), by = list(team)]
health_team <- setkey(health_team, team)[teamsCMH, nomatch = NA]
health_team <-     setkey(health_team, team)[conTeams, nomatch = NA]
health_team[is.na(health_team)] <- 0
health_team[, pct_errors := round(con_errors / case_load * 100, 2)]
health_team[, month := as.character(as.yearmon(paste(current_month, calendar_year)))]
#### consumers with Minimum Wage Errors ####
wage_errors[, team := teamFix(team)]
setcolorder(
  wage_errors,
  c(
    "case_no",
    "team",
    "supervisor",
    "primary_staff",
    "employment_status",
    "minimum_wage_error",
    "minimum_wage"
  )
)
## list of ordered errors
wage_error_list <-
  wage_errors[minimum_wage_error > 0][order(team, supervisor, primary_staff)]
wage_error_list <-
  setkey(wage_error_list, team)[J(teamsCMH), nomatch = 0]
# keep wanted columns
keep_cols <- c("case_no", "team", "minimum_wage_error")
wage_errors <- wage_errors[, .SD, .SDcols = keep_cols]
# remove duplicates (just in case)
wage_errors <- unique(wage_errors)
# aggregate data
wageTeam <-
  wage_errors[, list(con_errors = sum(minimum_wage_error, na.rm = TRUE)), nomatch =
                NA, by = team]
wageTeam <- setkey(wageTeam, team)[conTeams, nomatch = NA]
wageTeam[is.na(wageTeam)] <- 0
wageTeam[, pct_errors := specify_decimal(con_errors / case_load * 100, 1)]
wageTeam[, month := as.character(as.yearmon(paste(current_month, calendar_year)))]
#### self sufficiency matrix for MI Adult and ACT ####
# change column names automatically
setnames(
  ssm_summary,
  old = names(ssm_summary),
  new = gsub(
    x = tolower(names(ssm_summary)),
    pattern = "[.]",
    replace = "_"
  )
)
setnames(
  ssm_summary,
  old = grep(
    x = names(ssm_summary),
    pattern = "supervisor",
    value = TRUE
  ),
  new = "supervisor"
)
ssm_summary[, at_least2_ssms := at_least2_ssms * 100]
# order supervisors for ggplot
ssm_summary[, supervisor := factor(supervisor, levels = ssm_summary[order(at_least2_ssms), supervisor])]
#### last services in 30/60/90 ####
## service by team and staff ##
svcTeamStaff <-
  setkey(last_svc, team)[J(teamsCMH), list(
    case_load = length(unique(case_no)),
    notSeen30days = sum(notSeen30days, na.rm = TRUE),
    notSeen60days = sum(notSeen60days, na.rm = TRUE),
    notSeen90days = sum(notSeen90days, na.rm = TRUE),
    notSeen180days = sum(notSeen180days, na.rm = TRUE)
  ), by = list(team, primary_staff)]
## service by team ##
svcTeam <-
  setkey(last_svc, team)[J(teamsCMH), list(
    case_load = length(unique(case_no)),
    primary_staff = "",
    level = "top",
    notSeen30days = sum(notSeen30days, na.rm = TRUE),
    notSeen60days = sum(notSeen60days, na.rm = TRUE),
    notSeen90days = sum(notSeen90days, na.rm = TRUE),
    notSeen180days = sum(notSeen180days, na.rm = TRUE)
  ), by = team]
## service - grand total ##
invisible(setkey(last_svc, NULL))
svcAll <- last_svc[, list(
  team = "\\textbf{Grand Total}",
  primary_staff = "",
  case_load = paste("\\textbf{", length(unique(case_no)), "}"),
  level = "top",
  notSeen30days = paste0("{\\textbf{", sum(notSeen30days, na.rm = TRUE), "}"),
  notSeen60days = paste0("{\\textbf{", sum(notSeen60days, na.rm = TRUE), "}"),
  notSeen90days = paste0("{\\textbf{", sum(notSeen90days, na.rm = TRUE), "}"),
  notSeen180days = paste0("{\\textbf{", sum(notSeen180days, na.rm = TRUE), "}")
)]
## case load and not seen 30-60-90-180 days ##
svcTeam2 <-
  setkey(last_svc, team)[J(teamsCMH), list(
    case_load = length(unique(case_no)),
    notSeen30days = sum(notSeen30days, na.rm = TRUE),
    notSeen60days = sum(notSeen60days, na.rm = TRUE),
    notSeen90days = sum(notSeen90days, na.rm = TRUE),
    notSeen180days = sum(notSeen180days, na.rm = TRUE)
  ),
  by = list(team, primary_staff)]
svcTeam2 <- svcTeam2[order(team, primary_staff)]
### add levels based on staffOnly and Team Level Rules
setnames(staffOnly, old = "author",  new = "primary_staff")
svcTeam2 <-
  merge(svcTeam2, staffOnly[, c("primary_staff", "level"), with = FALSE], all.x =
          TRUE, by = "primary_staff")
setkey(svcTeam2, team)[J("Child"), level := "3"]
setkey(svcTeam2, team)[J(c("ACT", "Child Home Based")), level := "4"]
svcTeam2[is.na(level), level := "-"]
setcolorder(
  svcTeam,
  c(
    "team",
    "primary_staff",
    "case_load",
    "level",
    "notSeen30days",
    "notSeen60days",
    "notSeen90days",
    "notSeen180days"
  )
)
setcolorder(
  svcTeam2,
  c(
    "team",
    "primary_staff",
    "case_load",
    "level",
    "notSeen30days",
    "notSeen60days",
    "notSeen90days",
    "notSeen180days"
  )
)
svc_table <- rbindlist(list(svcTeam, svcTeam2))
svc_table[level == "", level := "-"]
svc_table[, level := factor(level,
                            levels = c("top", "DM", "respite", "Supervisor", "Therapy", 5:1, "-"))]
svc_table <- svc_table[order(team, level, primary_staff)]
svc_table[primary_staff != "", team := ""]
svc_table[primary_staff == "", level := "-"]
svc_table[primary_staff == "", team := paste0("\\textbf{", team, "}")]
svc_table[, case_load := as.character(case_load)]
svc_table[, notSeen30days := as.character(notSeen30days)]
svc_table[, notSeen60days := as.character(notSeen60days)]
svc_table[, notSeen90days := as.character(notSeen90days)]
svc_table[, notSeen180days := as.character(notSeen180days)]
svc_table[primary_staff == "", case_load := paste0("\\textbf{", case_load, "}")]
svc_table[primary_staff == "", notSeen30days := paste0("\\textbf{", notSeen30days, "}")]
svc_table[primary_staff == "", notSeen60days := paste0("\\textbf{", notSeen60days, "}")]
svc_table[primary_staff == "", notSeen90days := paste0("\\textbf{", notSeen90days, "}")]
svc_table[primary_staff == "", notSeen180days := paste0("\\textbf{", notSeen180days, "}")]
svc_table <- rbindlist(list(svc_table, svcAll))
svc_table[level == "top", level := ""]
svc_table[grep(x = team, pattern = "ACT"), team := "\\textbf{ACT Team}"]
#### GRAPH RESULTS ####
# make a list of all possible combinations
all_combinations <- data.table(expand.grid(
  mon_fy = as.character(as.yearmon(fy_start) + 0:11 / 12),
  team = c("MI Adult", "ACT", "DD Adult", "Child", "Child Home Based")
))
### ipos results ###
iposGraph <- iposTeam[, list(
  team = team,
  pctCurrentIPOS = pctCurrentIPOS,
  pctMissingIPOS = pctMissingIPOS,
  pctExpireIPOS = pctExpireIPOS
)]
iposGraph[, mon_fy := as.yearmon(paste(current_month, calendar_year))]

## remove current month, add it back in to make sure current month is most up-to-date, then save ##
if (!file.exists(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("ipos fy ", current_fy, ".csv")
))) {
  write.csv(iposGraph[order(team, mon_fy)],
            file.path(
              baseWD,
              dataWD,
              current_fy,
              paste0("ipos fy ", current_fy, ".csv")
            ),
            row.names = FALSE)
  warning(paste(
    "A new file was created:",
    file.path(
      baseWD,
      dataWD,
      current_fy,
      paste0("ipos fy ", current_fy, ".csv")
    )
  ))
}
iposAllMonths <- read.dtable(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("ipos fy ", current_fy, ".csv")
))
iposAllMonths[, mon_fy := as.yearmon(gsub(
  x = mon_fy,
  pattern = "-",
  replace = " 20"
))]
iposAllMonths <-
  setkey(iposAllMonths, mon_fy)[!J(as.yearmon(paste(current_month, current_fy)))]
invisible(setkey(iposAllMonths, NULL))
iposGraph <- rbindlist(list(iposAllMonths, iposGraph))
rm(iposAllMonths)
iposGraph <- unique(iposGraph)
write.csv(iposGraph[order(team, mon_fy)],
          file.path(
            baseWD,
            dataWD,
            current_fy,
            paste0("ipos fy ", current_fy, ".csv")
          ),
          row.names = FALSE)

## ipos_table1
if (!file.exists(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("ipos t1 fy ", current_fy, ".csv")
))) {
  write.csv(ipos_table1,
            file.path(
              baseWD,
              dataWD,
              current_fy,
              paste0("ipos t1 fy ", current_fy, ".csv")
            ),
            row.names = FALSE)
  warning(paste(
    "A new file was created:",
    file.path(
      baseWD,
      dataWD,
      current_fy,
      paste0("ipos t1 fy ", current_fy, ".csv")
    )
  ))
}
full_ipos_t1 <- read.dtable(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("ipos t1 fy ", current_fy, ".csv")
))
full_ipos_t1[, month := as.character(as.yearmon(gsub(
  x = month,
  pattern = "-",
  replace = " 20"
)))]
full_ipos_t1 <-
  setkey(full_ipos_t1, month)[!J(as.character(as.yearmon(paste(
    current_month, calendar_year
  ))))]
full_ipos_t1 <- rbindlist(list(full_ipos_t1, ipos_table1))
full_ipos_t1 <- full_ipos_t1[!is.na(month)]
write.csv(full_ipos_t1[order(month, team)],
          file.path(
            baseWD,
            dataWD,
            current_fy,
            paste0("ipos t1 fy ", current_fy, ".csv")
          ),
          row.names = FALSE)
full_ipos_t1 <-
  read.dtable(file.path(
    baseWD,
    dataWD,
    current_fy,
    paste0("ipos t1 fy ", current_fy, ".csv")
  ))
full_ipos_t1 <- full_ipos_t1[order(team, as.yearmon(month))]
setnames(full_ipos_t1,
         names(full_ipos_t1),
         gsub(
           x = names(full_ipos_t1),
           pattern = "_",
           replace = " "
         ))

## ipos_table2
if (!file.exists(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("ipos t2 fy ", current_fy, ".csv")
))) {
  write.csv(ipos_table2,
            file.path(
              baseWD,
              dataWD,
              current_fy,
              paste0("ipos t2 fy ", current_fy, ".csv")
            ),
            row.names = FALSE)
  warning(paste(
    "A new file was created:",
    file.path(
      baseWD,
      dataWD,
      current_fy,
      paste0("ipos t2 fy ", current_fy, ".csv")
    )
  ))
}
full_ipos_t2 <- read.dtable(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("ipos t2 fy ", current_fy, ".csv")
))
full_ipos_t2[, month := as.character(as.yearmon(gsub(
  x = month,
  pattern = "-",
  replace = " 20"
)))]
full_ipos_t2 <-
  setkey(full_ipos_t2, month)[!J(as.character(as.yearmon(paste(
    current_month, calendar_year
  ))))]
full_ipos_t2 <- rbindlist(list(full_ipos_t2, ipos_table2))
write.csv(full_ipos_t2[order(team, month)],
          file.path(
            baseWD,
            dataWD,
            current_fy,
            paste0("ipos t2 fy ", current_fy, ".csv")
          ),
          row.names = FALSE)
full_ipos_t2 <- read.dtable(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("ipos t2 fy ", current_fy, ".csv")
))
full_ipos_t2 <- full_ipos_t2[order(team, as.yearmon(month))]
setnames(
  full_ipos_t2,
  old = c("blank_missing_IPOS", "currentIPOS", "expiredIPOS"),
  new = c("blank/missing IPOS", "current IPOS", "expired IPOS")
)
### ipos_table3
full_ipos_t3 <- ipos_table3
full_ipos_t3[, supText := supervisor]
setkey(full_ipos_t3, primary_staff)[J(""), supText := paste0("\\textbf{", supText, "}")]
full_ipos_t3[, expired_missing_IPOS := as.character(expired_missing_IPOS)]
setkey(full_ipos_t3, primary_staff)[J(""), expired_missing_IPOS := paste0("\\textbf{", expired_missing_IPOS, "}")]
setkey(full_ipos_t3, primary_staff)[J(""), expired_missing_IPOS := paste0("\\hspace{2cm}\\textbf{", expired_missing_IPOS, "}")]
# setkey(full_ipos_t3, expired_missing_IPOS)["", expired_missing_IPOS := paste0("\\textbf{", expired_missing_IPOS, "}")]
invisible(setkey(full_ipos_t3, NULL))
full_ipos_t3 <- full_ipos_t3[order(team, supervisor, primary_staff)]
full_ipos_t3[duplicated(supervisor), supervisor := ""]
full_ipos_t3[primary_staff != "", supText := ""] # testing to see if works
setnames(
  full_ipos_t3,
  old = c("primary_staff", "expired_missing_IPOS"),
  new = c("primary_staff", "expired/missing IPOS")
)
setcolorder(
  full_ipos_t3,
  c(
    "team",
    "supText",
    "supervisor",
    "primary_staff",
    'expired/missing IPOS'
  )
)
# reshape/melt data for ggplot
iposGraph <- data.table(reshape::melt(
  iposGraph,
  id = c("team", "mon_fy"),
  variable.name = c("pctCurrentIPOS", "missingIPOS")
))
# keep only pctCurrentIPOS per Laura H. 9/10/2014
iposGraph <- setkey(iposGraph, variable)["pctCurrentIPOS"]
iposGraph[, mon_fy := as.character(mon_fy)]
# merge with all possibilities
iposGraph <-
  merge(all_combinations,
        iposGraph,
        all = TRUE,
        by = c("team", "mon_fy"))
# factor based on 12 consecutive months
iposGraph[, mon_fy := factor(mon_fy, levels = as.character(as.yearmon(fy_start) +
                                                             0:11 / 12))]

# create graph
team_list <- iposGraph[, unique(team)]
hist_ipos <- iposTable <- list()

for (i in seq_along(team_list)) {
  # start for loop
  hist_ipos[[i]] = ggplot(data = iposGraph[team == team_list[i]],
                          aes(
                            x = mon_fy,
                            y = value,
                            fill = variable,
                            ymax = 1.25 * value
                          )) +
    geom_bar(
      width = 0.3,
      stat = "identity",
      color = "black",
      size = 1,
      position = position_dodge(0.3)
    ) +
    labs(
      title = paste0(
        team_list[i],
        " Graph ",
        i,
        ".1.1 - % Current IPOS (Report 2003: ran ",
        gsub(
          x = run_date,
          pattern = "_",
          replace = "-"
        ),
        ")"
      ),
      y = "percent",
      fill = ""
    ) +
    geom_text(
      data = iposGraph[team == team_list[i]],
      position = position_dodge(0.3),
      aes(
        x = mon_fy,
        y = value,
        fill = variable,
        label = paste0(value, "%")
      ),
      hjust = 0.5,
      vjust = -0.5,
      size = 3,
      na.rm = TRUE
    ) +
    scale_fill_manual(values = c("#C282D6"),
                      label = c("current IPOS")) +
    my_theme +
    theme(
      panel.grid.major = element_line(colour = "grey80", size = 0.2),
      panel.grid.minor = element_line(colour = "grey80",
                                      size = .2),
      axis.title.y = element_text(colour = "grey30", size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = unit(c(-.05, 0, -.2, 0) , "in")
    ) +
    scale_y_discrete(breaks = seq(0, 100, 25)) +
    theme(panel.grid.major.x = element_blank())
}

### unsigned/draft docs ###
if (!file.exists(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("unsign_draft_docs t1 fy ", current_fy, ".csv")
))) {
  write.csv(unsignTeam,
            file.path(
              baseWD,
              dataWD,
              current_fy,
              paste0("unsign_draft_docs t1 fy ", current_fy, ".csv")
            ),
            row.names = FALSE)
  warning(paste(
    "A new file was created:",
    file.path(
      baseWD,
      dataWD,
      current_fy,
      paste0("unsign_draft_docs t1 fy ", current_fy, ".csv")
    )
  ))
}
unsign_draft_t1 <-
  read.dtable(file.path(
    baseWD,
    dataWD,
    current_fy,
    paste0("unsign_draft_docs t1 fy ", current_fy, ".csv")
  ))
unsign_draft_t1[, month := as.character(as.yearmon(gsub(
  x = month,
  pattern = "-",
  replace = " 20"
)))]
unsign_draft_t1 <-
  setkey(unsign_draft_t1, month)[!J(as.character(as.yearmon(paste(
    current_month, calendar_year
  ))))]
unsign_draft_t1 <- rbindlist(list(unsign_draft_t1, unsignTeam))
write.csv(unsign_draft_t1[order(team, as.yearmon(month))],
          file.path(
            baseWD,
            dataWD,
            current_fy,
            paste0("unsign_draft_docs t1 fy ", current_fy, ".csv")
          ),
          row.names = FALSE)
unsign_draft_t1 <-
  read.dtable(file.path(
    baseWD,
    dataWD,
    current_fy,
    paste0("unsign_draft_docs t1 fy ", current_fy, ".csv")
  ))

# re-name to match
setnames(all_combinations, old = "mon_fy", new = "month")
# merge with all possibilities
unsign_draft_t1 <-
  merge(all_combinations,
        unsign_draft_t1,
        all = TRUE,
        by = c("team", "month"))
# re-order - may not be necessary
unsign_draft_t1 <- unsign_draft_t1[order(team, as.yearmon(month))]
# factor based on 12 consecutive months
unsign_draft_t1[, month := factor(month, levels = as.character(as.yearmon(fy_start) +
                                                                 0:11 / 12))]

p_unsign <- list()
for (i in seq_along(team_list)) {
  # create graph p_unsign
  p_unsign[[i]] = ggplot(data = unsign_draft_t1[team == team_list[i]],
                         aes(
                           x = month,
                           fill = month,
                           y = docs_unsigned,
                           ymax = 1.25 * docs_unsigned
                         )) +
    geom_bar(
      width = 0.5,
      stat = "identity",
      color = "black",
      size = 1,
      fill = "#C282D6",
      position = position_dodge(0.5)
    ) +
    labs(
      title = paste0(
        team_list[i],
        " Graph ",
        i,
        ".2.1 - # Unsigned/Draft Docs (Report 2004: ran ",
        gsub(
          x = run_date,
          pattern = "_",
          replace = "-"
        ),
        ")"
      ),
      y = "# unsigned/draft documents"
    ) +
    geom_text(
      vjust = 0.5,
      hjust = -0.2,
      aes(
        x = month,
        fill = month,
        y = docs_unsigned,
        label = docs_unsigned
      ),
      angle = 90,
      position = position_dodge(0.5),
      size = 2.75,
      na.rm = TRUE
    ) +
    my_theme +
    theme(
      legend.position = "top",
      panel.grid.major = element_line(colour = "grey80", size = 0.2),
      panel.grid.minor = element_line(colour = "grey80", size = .2),
      axis.title.y = element_text(colour = "grey30", size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = unit(c(-.05, 0, -.2, 0) , "in")
    ) +
    theme(panel.grid.major.x = element_blank())
}
setnames(unsign_draft_t1, old = "docs_unsigned", new = "unsigned/draft")

### demographic errors ###
if (!file.exists(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("demo table fy ", current_fy, ".csv")
))) {
  write.csv(demo_team[order(team, month)],
            file.path(
              baseWD,
              dataWD,
              current_fy,
              paste0("demo table fy ", current_fy, ".csv")
            ),
            row.names = FALSE)
  warning(paste(
    "A new file was created:",
    file.path(
      baseWD,
      dataWD,
      current_fy,
      paste0("demo table fy ", current_fy, ".csv")
    )
  ))
}
demo_table <- read.dtable(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("demo table fy ", current_fy, ".csv")
))
demo_table[, month := as.character(as.yearmon(gsub(
  x = month,
  pattern = "-",
  replace = " 20"
)))]
demo_table <-
  setkey(demo_table, month)[!J(as.character(as.yearmon(paste(
    current_month, calendar_year
  ))))]
setcolorder(demo_team, names(demo_table))
demo_table <- rbindlist(list(demo_table, demo_team))
write.csv(demo_table[order(team, as.yearmon(month))],
          file.path(
            baseWD,
            dataWD,
            current_fy,
            paste0("demo table fy ", current_fy, ".csv")
          ),
          row.names = FALSE)
demo_table <- read.dtable(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("demo table fy ", current_fy, ".csv")
))
demo_table[is.na(con_errors), con_errors := 0]
# merge with all possibilities
demo_table <-
  merge(all_combinations,
        demo_table,
        all = TRUE,
        by = c("team", "month"))
# re-order - may not be necessary
demo_table <- demo_table[order(team, as.yearmon(month))]
# factor based on 12 consecutive months
demo_table[, month := factor(month, levels = as.character(as.yearmon(fy_start) +
                                                            0:11 / 12))]

p_demo <- list()
for (i in seq_along(team_list)) {
  # create graph p_demo
  p_demo[[i]] = ggplot(data = demo_table[team == team_list[i]],
                       aes(
                         x = month,
                         fill = month,
                         y = con_errors,
                         ymax = 1.35 * con_errors
                       )) +
    geom_bar(
      width = 0.5,
      stat = "identity",
      color = "black",
      size = 1,
      fill = "#C282D6",
      position = position_dodge(0.5)
    ) +
    labs(
      title = paste0(
        team_list[i],
        " Graph ",
        i,
        ".3.1 - # Consumers with Demographic Errors\n (Report 2023: ran ",
        gsub(
          x = run_date,
          pattern = "_",
          replace = "-"
        ),
        ")"
      ),
      y = "# consumer errors"
    ) +
    geom_text(
      vjust = 0.5,
      hjust = -.5,
      aes(
        x = month,
        fill = month,
        y = con_errors,
        label = con_errors
      ),
      angle = 90,
      position = position_dodge(0.5),
      size = 2.75,
      na.rm = TRUE
    ) +
    my_theme +
    theme(
      legend.position = "top",
      panel.grid.major = element_line(colour = "grey80", size = 0.2),
      panel.grid.minor = element_line(colour = "grey80", size = .2),
      axis.title.y = element_text(colour = "grey30", size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = unit(c(-.05, 0, -.2, 0) , "in"),
      panel.grid.major.x = element_blank()
    )
}

### Health Errors ###
if (!file.exists(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("health table fy ", current_fy, ".csv")
))) {
  write.csv(health_team[order(team, month)],
            file.path(
              baseWD,
              dataWD,
              current_fy,
              paste0("health table fy ", current_fy, ".csv")
            ),
            row.names = FALSE)
  warning(paste(
    "A new file was created:",
    file.path(
      baseWD,
      dataWD,
      current_fy,
      paste0("health table fy ", current_fy, ".csv")
    )
  ))
}
health_table <-
  read.dtable(file.path(
    baseWD,
    dataWD,
    current_fy,
    paste0("health table fy ", current_fy, ".csv")
  ))
health_table[, month := as.character(as.yearmon(gsub(
  x = month,
  pattern = "-",
  replace = " 20"
)))]
health_table <-
  setkey(health_table, month)[!J(as.character(as.yearmon(paste(
    current_month, calendar_year
  ))))]
setcolorder(health_team, names(health_table))
health_table <- rbindlist(list(health_table, health_team))
write.csv(health_table[order(team, as.yearmon(month))],
          file.path(
            baseWD,
            dataWD,
            current_fy,
            paste0("health table fy ", current_fy, ".csv")
          ),
          row.names = FALSE)
health_table <-
  read.dtable(file.path(
    baseWD,
    dataWD,
    current_fy,
    paste0("health table fy ", current_fy, ".csv")
  ))
health_table[is.na(con_errors), con_errors := 0]
# merge with all possibilities
health_table <-
  merge(all_combinations,
        health_table,
        all = TRUE,
        by = c("team", "month"))
# re-order - may not be necessary
health_table <- health_table[order(team, as.yearmon(month))]
# factor based on 12 consecutive months
health_table[, month := factor(month, levels = as.character(as.yearmon(fy_start) +
                                                              0:11 / 12))]

p_health <- list()
for (i in seq_along(team_list)) {
  # create graph p_health
  p_health[[i]] = ggplot(data = health_table[team == team_list[i]],
                         aes(
                           x = month,
                           fill = month,
                           y = con_errors,
                           ymax = 1.3 * con_errors,
                           ymin = 0
                         )) +
    geom_bar(
      hjust = 0.5,
      vjust = -0.5,
      width = 0.5,
      stat = "identity",
      color = "black",
      size = 1,
      fill = "#C282D6",
      position = position_dodge(0.5)
    ) +
    labs(
      title = paste0(
        team_list[i],
        " Graph ",
        i,
        ".4.1 - # Consumers w/ Health & Other Conditions Errors\n(Report 2145-2: ran ",
        gsub(
          x = run_date,
          pattern = "_",
          replace = "-"
        ),
        ")"
      ),
      y = "# health errors"
    ) +
    geom_text(
      vjust = 0.5,
      hjust = -0.5,
      aes(
        x = month,
        fill = month,
        y = con_errors,
        label = con_errors
      ),
      angle = 90,
      position = position_dodge(0.5),
      size = 3,
      na.rm = TRUE
    ) +
    my_theme +
    theme(
      legend.position = "top",
      panel.grid.major = element_line(colour = "grey80", size = 0.2),
      panel.grid.minor = element_line(colour = "grey80", size = .2),
      axis.title.y = element_text(colour = "grey30", size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = unit(c(-.05, 0, -.2, 0) , "in"),
      panel.grid.major.x = element_blank()
    ) +
    coord_cartesian(ylim = c(0, health_table[team == team_list[i], max(1.3 *
                                                                         con_errors, 4, na.rm = TRUE)]))
}

### Minimum Wage Errors ###
if (!file.exists(file.path(
  baseWD,
  dataWD,
  current_fy,
  paste0("wage table fy ", current_fy, ".csv")
))) {
  write.csv(wageTeam[order(team, month)],
            file.path(
              baseWD,
              dataWD,
              current_fy,
              paste0("wage table fy ", current_fy, ".csv")
            ),
            row.names = FALSE)
  warning(paste(
    "A new file was created:",
    file.path(
      baseWD,
      dataWD,
      current_fy,
      paste0("wage table fy ", current_fy, ".csv")
    )
  ))
}
wage_table <-
  read.dtable(file.path(
    baseWD,
    dataWD,
    current_fy,
    paste0("wage table fy ", current_fy, ".csv")
  ))
wage_table[, month := as.character(as.yearmon(gsub(
  x = month,
  pattern = "-",
  replace = " 20"
)))]
wage_table <-
  setkey(wage_table, month)[!J(as.character(as.yearmon(paste(
    current_month, calendar_year
  ))))]
setcolorder(wageTeam, names(wage_table))
wage_table <- rbindlist(list(wage_table, wageTeam))
write.csv(wage_table[order(team, as.yearmon(month))],
          file.path(
            baseWD,
            dataWD,
            current_fy,
            paste0("wage table fy ", current_fy, ".csv")
          ),
          row.names = FALSE)
wage_table <-
  read.dtable(file.path(
    baseWD,
    dataWD,
    current_fy,
    paste0("wage table fy ", current_fy, ".csv")
  ))
# merge with all possibilities
wage_table <-
  merge(all_combinations,
        wage_table,
        all = TRUE,
        by = c("team", "month"))
# re-order - may not be necessary
wage_table <- wage_table[order(team, as.yearmon(month))]
# factor based on 12 consecutive months
wage_table[, month := factor(month, levels = as.character(as.yearmon(fy_start) +
                                                            0:11 / 12))]

p_wage <- list()
for (i in seq_along(team_list)) {
  # create graph p_health
  p_wage[[i]] = ggplot(data = wage_table[team == team_list[i]], aes(
    x = month,
    fill = month,
    y = con_errors,
    ymax = max(round(con_errors) + 1, 1.25 * con_errors)
  )) +
    geom_bar(
      width = 0.5,
      stat = "identity",
      color = "black",
      size = 1,
      fill = "#C282D6",
      position = position_dodge(0.5)
    ) +
    labs(
      title = paste0(
        team_list[i],
        " Graph ",
        i,
        ".5.1 - # Consumers w/ Minimum Wage Errors\n(Report 2205 ran ",
        gsub(
          x = run_date,
          pattern = "_",
          replace = "-"
        ),
        ")"
      ),
      y = "# consumers with\n minimum wage errors"
    ) +
    geom_text(
      vjust = 0.5,
      hjust = -0.5,
      aes(
        x = month,
        fill = month,
        y = con_errors,
        label = con_errors
      ),
      angle = 90,
      position = position_dodge(0.5),
      size = 3,
      na.rm = TRUE
    ) +
    my_theme +
    theme(
      legend.position = "top",
      panel.grid.major = element_line(colour = "grey80", size = 0.2),
      panel.grid.minor = element_line(colour = "grey80", size = .2),
      axis.title.y = element_text(colour = "grey30", size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = unit(c(-.05, 0, -.2, 0) , "in"),
      panel.grid.major.x = element_blank()
    ) +
    # integer only in breaks
    scale_y_discrete(breaks = my_breaks(n = wage_table[team == team_list[i]][, max(con_errors, na.rm =
                                                                                     TRUE)]))
}

### self sufficiency matrix ###

ssm_summary[, supervisor := factor(supervisor, levels = ssm_summary[order(at_least2_ssms), supervisor])]
p_ssm <-
  ggplot(data = ssm_summary, aes(
    x = supervisor,
    y = at_least2_ssms,
    ymax = 120,
    ymin = 0
  )) +
  geom_bar(
    stat = "identity",
    color = "black",
    fill = "#C282D6",
    width = 0.5
  ) +
  # labs(title="Percent Case Load with at least 1 SSM in Last Year\n Note: Case Load is in Parentheses")+
  ggtitle(expression(atop(
    "Percent Case Load with at least 2 SSM since 3/1/14",
    atop(italic("Note: Case Load is in Parentheses"), "")
  ))) +
  coord_flip() + my_theme +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  geom_text(
    vjust = 0.5,
    hjust = -0.1,
    width = 0.5,
    aes(
      x = supervisor,
      y = at_least2_ssms,
      label = paste0(round(at_least2_ssms), "% (", case_load, ")")
    ),
    angle = 0,
    size = 3,
    na.rm = TRUE
  )

#### save results ####
if (!file.exists(file.path(baseWD, resultsWD, current_fy, current_month))) {
  dir.create(file.path(baseWD, resultsWD, current_fy, current_month),
             recursive = TRUE)
}

### wage file ###
## create workbook ##
wb_error <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb_error) + Font(wb_error, isBold=TRUE) + Border()

# create wage error sheet
sheet_detail <- createSheet(wb_error, sheetName="wage_detail")
addDataFrame(x=wage_error_list, sheet=sheet_detail, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create ACT sheet
sheet_ACT <- createSheet(wb_error, sheetName="ACT")
addDataFrame(x=wage_error_list[team=="ACT"], sheet=sheet_ACT, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create MI Adult sheet
sheet_MI <- createSheet(wb_error, sheetName="MI Adult")
addDataFrame(x=wage_error_list[team=="MI Adult"], sheet=sheet_MI, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create DD Adult sheet
sheet_DD <- createSheet(wb_error, sheetName="DD Adult")
addDataFrame(x=wage_error_list[team=="DD Adult"], sheet=sheet_DD, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create Child sheet
sheet_Ch <- createSheet(wb_error, sheetName="Child")
addDataFrame(x=wage_error_list[team=="Child"], sheet=sheet_Ch, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create Child HB sheet
sheet_Ch_HB <- createSheet(wb_error, sheetName="Child HB")
addDataFrame(x=wage_error_list[team=="Child Home Based"], sheet=sheet_Ch_HB, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
## save workbook ##
saveWorkbook(wb=wb_error, file=file.path(baseWD, resultsWD, current_fy, current_month, "wage errors.xlsx"))
rm(wb_error)

### health file ###
## create workbook ##
wb_health <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb_health) + Font(wb_health, isBold=TRUE) + Border()
# create health error sheet
sheet_detail <- createSheet(wb_health, sheetName="health_detail")
addDataFrame(x=health_error_list, sheet=sheet_detail, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create ACT sheet
sheet_ACT <- createSheet(wb_health, sheetName="ACT")
addDataFrame(x=health_error_list[team=="ACT"], sheet=sheet_ACT, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create MI Adult sheet
sheet_MI <- createSheet(wb_health, sheetName="MI Adult")
addDataFrame(x=health_error_list[team=="MI Adult"], sheet=sheet_MI, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create DD Adult sheet
sheet_DD <- createSheet(wb_health, sheetName="DD Adult")
addDataFrame(x=health_error_list[team=="DD Adult"], sheet=sheet_DD, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create Child sheet
sheet_Ch <- createSheet(wb_health, sheetName="Child")
addDataFrame(x=health_error_list[team=="Child"], sheet=sheet_Ch, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create Child HB sheet
sheet_Ch_HB <- createSheet(wb_health, sheetName="Child HB")
addDataFrame(x=health_error_list[team=="Child Home Based"], sheet=sheet_Ch_HB, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
## save workbook ##
saveWorkbook(wb=wb_health, file=file.path(baseWD, resultsWD, current_fy, current_month, "health errors.xlsx"))
rm(wb_health)

### demo file ###
## create workbook ##
wb_demo <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb_demo) + Font(wb_demo, isBold=TRUE) + Border()
# create demo error sheet
sheet_detail <- createSheet(wb_demo, sheetName="demo_detail")
addDataFrame(x=demo_error_list, sheet=sheet_detail, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create ACT sheet
sheet_ACT <- createSheet(wb_demo, sheetName="ACT")
addDataFrame(x=demo_error_list[team=="ACT"], sheet=sheet_ACT, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create MI Adult sheet
sheet_MI <- createSheet(wb_demo, sheetName="MI Adult")
addDataFrame(x=demo_error_list[team=="MI Adult"], sheet=sheet_MI, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create DD Adult sheet
sheet_DD <- createSheet(wb_demo, sheetName="DD Adult")
addDataFrame(x=demo_error_list[team=="DD Adult"], sheet=sheet_DD, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create Child sheet
sheet_Ch <- createSheet(wb_demo, sheetName="Child")
addDataFrame(x=demo_error_list[team=="Child"], sheet=sheet_Ch, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create Child HB sheet
sheet_Ch_HB <- createSheet(wb_demo, sheetName="Child HB")
addDataFrame(x=demo_error_list[team=="Child Home Based"], sheet=sheet_Ch_HB, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
## save workbook ##
saveWorkbook(wb=wb_demo, file=file.path(baseWD, resultsWD, current_fy, current_month, "demo errors.xlsx"))
rm(wb_demo)

### services file ###
## create workbook ##
wb_service <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb_service) + Font(wb_service, isBold=TRUE) + Border()
# create no services sheet
sheet_detail <- createSheet(wb_service, sheetName="services_detail")
addDataFrame(x=services_list, sheet=sheet_detail, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create ACT sheet
sheet_ACT <- createSheet(wb_service, sheetName="ACT")
addDataFrame(x=services_list[team=="ACT"], sheet=sheet_ACT, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create MI Adult sheet
sheet_MI <- createSheet(wb_service, sheetName="MI Adult")
addDataFrame(x=services_list[team=="MI Adult"], sheet=sheet_MI, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create DD Adult sheet
sheet_DD <- createSheet(wb_service, sheetName="DD Adult")
addDataFrame(x=services_list[team=="DD Adult"], sheet=sheet_DD, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create Child sheet
sheet_Ch <- createSheet(wb_service, sheetName="Child")
addDataFrame(x=services_list[team=="Child"], sheet=sheet_Ch, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create Child HB sheet
sheet_Ch_HB <- createSheet(wb_service, sheetName="Child HB")
addDataFrame(x=services_list[team=="Child Home Based"], sheet=sheet_Ch_HB, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
## save workbook ##
saveWorkbook(wb=wb_service, file=file.path(baseWD, resultsWD, current_fy, current_month, "no service.xlsx"))
rm(wb_service)

### ipos file ###
## create workbook ##
wb_ipos <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb_ipos) + Font(wb_ipos, isBold=TRUE) + Border()
# create ipos sheet
sheet_detail <- createSheet(wb_ipos, sheetName="ipos_detail")
addDataFrame(x=ipos_list, sheet=sheet_detail, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create ACT sheet
sheet_ACT <- createSheet(wb_ipos, sheetName="ACT")
addDataFrame(x=ipos_list[team=="ACT"], sheet=sheet_ACT, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create MI Adult sheet
sheet_MI <- createSheet(wb_ipos, sheetName="MI Adult")
addDataFrame(x=ipos_list[team=="MI Adult"], sheet=sheet_MI, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create DD Adult sheet
sheet_DD <- createSheet(wb_ipos, sheetName="DD Adult")
addDataFrame(x=ipos_list[team=="DD Adult"], sheet=sheet_DD, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create Child sheet
sheet_Ch <- createSheet(wb_ipos, sheetName="Child")
addDataFrame(x=ipos_list[team=="Child"], sheet=sheet_Ch, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create Child HB sheet
sheet_Ch_HB <- createSheet(wb_ipos, sheetName="Child HB")
addDataFrame(x=ipos_list[team=="Child Home Based"], sheet=sheet_Ch_HB, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
## save workbook ##
saveWorkbook(wb=wb_ipos, file=file.path(baseWD, resultsWD, current_fy, current_month, "ipos.xlsx"))
rm(wb_ipos)

### Self Sufficiency Matrix (SSM) ###
## create workbook ##
wb_ssm <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb_ssm) + Font(wb_ssm, isBold=TRUE) + Border()
# ssm detail
sheet_detail <- createSheet(wb_ssm, sheetName="ssm_detail")
addDataFrame(x=smm_case_detail, sheet=sheet_detail, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# ssm detail
sheet_summary <- createSheet(wb_ssm, sheetName="ssm_summary")
addDataFrame(x=ssm_summary, sheet=sheet_summary, showNA=FALSE,
             row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
## save workbook ##
saveWorkbook(wb=wb_ssm, file=file.path(baseWD, resultsWD,
                                       current_fy, current_month, "ssm.xlsx"))
rm(wb_ssm)