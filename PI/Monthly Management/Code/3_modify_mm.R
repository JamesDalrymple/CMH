# import copy of all data -----------------------------------------------------
case_load <- copy(sql$output$case_load)
ipos <- copy(sql$output$ipos)
last_svc <- copy(sql$output$last_svc)
unsigned_docs <- copy(sql$output$unsigned_docs)
demo_errors <- copy(sql$output$demo_errors)
health_errors <- copy(sql$output$health_errors)
wage_errors <- copy(sql$output$wage_errors)
ssm <- copy(sql$output$ssm)
team_levels <- copy(sql$output$team_levels)
state_hosp <- copy(sql$output$state_hosp)
# state hosp - filter these people out of all reports -------------------------
modify$state_hosp_cases <- state_hosp[, unique(case_no)]
# team levels -----------------------------------------------------------------
team_levels[, team := cmh_recode(team)]
setnames(team_levels, "entered_by", "author")
modify$team_only <- team_levels[author == "all"]
modify$staff_only <- team_levels[author != "all"]
# case load -------------------------------------------------------------------
case_load[, team := cmh_recode(team)]
# remove state hospital consumers (should be only 1 or 2 cases)
case_load <- setkey(case_load, case_no)[!J(modify$state_hosp_cases)]
# keep only CSTS teams - this removes Access
case_load <- setkey(case_load, team)[J(aux$teamsCMH)]
setkey(case_load, NULL)
modify$con_teams <- case_load[, list(case_load = length(unique(case_no))), by = team]
# last service ----------------------------------------------------------------
# remove open state hospital consumers
last_svc <- setkey(last_svc, case_no)[!J(modify$state_hosp_cases)]
last_svc[, team := cmh_recode(team)]
# fix NA primary staff
last_svc[primary_staff == "", primary_staff := "missing prim staff"]
# if num_days_no_service is NA, count them as 180+ days without service
last_svc[is.na(num_days_no_service), num_days_no_service := 181]
# make 30-60-90-180 day columns ---
last_svc[num_days_no_service > 30, `days > 30` := 1]
last_svc[num_days_no_service > 60, `days > 60` := 1]
last_svc[num_days_no_service > 90, `days > 90` := 1]
last_svc[num_days_no_service > 180, `days > 180` := 1]
# keep only CMH teams
last_svc <- setkey(last_svc, team)[J(aux$teamsCMH)]
setkey(last_svc, NULL)
# service list for 90 and 180 days ---
services_list <- last_svc[, .SD,
  .SDcols = c("team", "primary_staff", "case_no",
  "num_days_no_service", "days > 90", "days > 180")]
services_list <- merge(services_list,
  case_load[, unique(.SD), .SDcols = c("supervisor", "primary_staff")],
  by="primary_staff")
setcolorder(services_list,
  c("team", "supervisor", "primary_staff", "case_no", "num_days_no_service",
    "days > 90", "days > 180"))
services_list <- services_list[`days > 90` == 1 | `days > 180` == 1]
# current IPOS ----------------------------------------------------------------
ipos[, team := cmh_recode(team)]
ipos <- setkey(ipos, team)[J(aux$teamsCMH)]
# change dates to date classes
ipos[, ip_effdt := date_convert(ip_effdt)]
ipos[, ip_expdt := date_convert(ip_expdt)]
# creat col interim IPOS >= 30 days; if exp date > today, use today's date per Nicole E.
setkey(ipos, ipos_type)[J("Interim IPOS"),
  interimDiff :=  pmin(ip_expdt, Sys.Date())-ip_effdt]
ipos[, ipos30Plus := 0]
ipos[interimDiff>30, ipos30Plus := 1]
ipos[, interimDiff := NULL]
# ipos_list
modify$ipos_list <- ipos
setkey(modify$ipos_list, ipos_type)[J(""), ipos_type := "blank/missing"]
modify$ipos_list[input$current_day > ip_expdt, expired := "Yes"]
# consumers with a missing IPOS ---
modify$missIPOS <- setkey(ipos, ipos_type)[J(c("", NA)),
  list(missingIPOS=length(unique(case_no))), by=team]
setkey(ipos, NULL)
modify$conIPOS <- ipos[, list(totalConsumers=length(unique(case_no))), by=team]
# which consumers have a current IPOS ---
ipos[, current := 0]
ipos[input$current_day <= ip_expdt, current := 1]
modify$currentIPOS <-
  ipos[, list(currentIPOS = sum(current, na.rm=TRUE)), by=team]
# which consumers have an expired IPOS ---
modify$expireIPOS <-
  ipos[, list(expiredIPOS = sum(current, na.rm=TRUE)), by=list(team, case_no)]
modify$expireIPOS <- setkey(modify$expireIPOS, expiredIPOS)[J(0)]
modify$expireIPOS <-
  modify$expireIPOS[, list(expiredIPOS = length(unique(case_no))), by=team]
# which consumers have a full IPOS ---
modify$ipos_full <- setkey(ipos, ipos_type)[J("Full IPOS"),
  list(Full_IPOS = length(ip_effdt)), by=team]
setkey(modify$ipos_full, NULL)
modify$ipos_full[is.na(modify$ipos_full)] <- 0
# which consumers have an Interim IPOS ---
modify$ipos_interim <- setkey(ipos, ipos_type)[J("Interim IPOS"),
  list(Interim_IPOS = length(ip_effdt)), by = team]
setkey(modify$ipos_interim, NULL)
modify$ipos_interim[is.na(modify$ipos_interim)] <- 0
# how many consumers have an Interim IPOS 30+ days ---
modify$ipos_interim_30_plus <- ipos[,
  list(Interim_IPOS_Over_30_Days = sum(ipos30Plus, na.rm = TRUE)), by = team]
modify$ipos_interim_30_plus <-
  setkey(modify$ipos_interim_30_plus, team)[J(aux$teamsCMH)]
modify$ipos_interim_30_plus[is.na(modify$ipos_interim_30_plus)] <- 0
# which consumers have an Preliminary IPOS ---
modify$ipos_prelim <- setkey(ipos, ipos_type)[J("Preliminary IPOS"),
  list(Preliminary_IPOS = length(ip_effdt)), by = team]
setkey(modify$ipos_prelim, NULL)
modify$ipos_prelim[is.na(modify$ipos_prelim)] <- 0
# which consumers have an Single Service IPOS ---
modify$ipos_single <-
  setkey(ipos, ipos_type)[J("Single Service IPOS"),
    list(Single_Service_IPOS = length(ip_effdt)), by=team]
setkey(modify$ipos_single, NULL)
modify$ipos_single[is.na(modify$ipos_single)] <- 0
# join IPOS data.table's ---
modify$ipos_team <-
  mmerge(l = list(modify$conIPOS, modify$currentIPOS, modify$missIPOS,
                  modify$expireIPOS), all.x=TRUE, by="team")
modify$ipos_team[is.na(modify$ipos_team)] <- 0
modify$ipos_team[, pctCurrentIPOS := round(currentIPOS/totalConsumers*100, 0)]
modify$ipos_team[, pctMissingIPOS := round(missingIPOS/totalConsumers*100, 0)]
modify$ipos_team[, pctExpireIPOS := round(expiredIPOS/totalConsumers*100, 0)]
### the table of different types of IPOS ###
# part a - grand totals
modify$ipos_team2a <-
  mmerge(l = list(modify$conIPOS, modify$currentIPOS), all=TRUE, by="team")
# part b - all ipos types
modify$ipos_team2b <-ipos[, list(currentIPOS = sum(current, na.rm=TRUE)),
                          by=list(team, ipos_type)]
modify$ipos_team2b <- dcast.data.table(modify$ipos_team2b, formula = team ~ ipos_type,
                               value.var="currentIPOS", fill=0)
# part c - blank/missing and expired IPOS
modify$ipos_team2c_expire <-
  ipos[ipos_type!="", list(expired = length(current)-sum(current)),
       by=list(team, case_no)]
modify$ipos_team2c_expire <-
  modify$ipos_team2c_expire[, list(expiredIPOS = sum(expired)), by = team]
modify$ipos_team2c_blank <- ipos[ipos_type=="",
  list(blank_missing_IPOS = length(unique(case_no))), by = team]
modify$ipos_team2c_blank <-
  setkey(modify$ipos_team2c_blank, team)[aux$teamsCMH, nomatch = NA]
modify$ipos_team2c_blank[is.na(modify$ipos_team2c_blank)] <- 0
iposTeam2c <-
  mmerge(l = list(modify$ipos_team2c_blank, modify$ipos_team2c_expire),
         by="team", all.x=TRUE)
# combine parts a-c
iposTeam2 <-
  mmerge(l = list(modify$ipos_team2a, modify$ipos_team2b, iposTeam2c),
         by="team", all=TRUE)
# IPOS tables ---
## ipos table 1
ipos_table1 <-
  mmerge(l = list(modify$ipos_full, modify$ipos_interim,
    modify$ipos_interim_30_plus, modify$ipos_prelim, modify$ipos_single),
    by="team" )
ipos_table1[, `Grand Total` :=
  psum(Full_IPOS, Interim_IPOS, Preliminary_IPOS, Single_Service_IPOS)]
ipos_table1 <- cbind(month=as.character(as.yearmon(
  paste(input$current_month, input$calendar_year))), ipos_table1)
## ipos table 2
ipos_table2 <- mmerge(l = list(modify$currentIPOS, iposTeam2c), by="team")
ipos_table2 <- cbind(month=as.character(as.yearmon(
  paste(input$current_month, input$calendar_year))), ipos_table2)
## ipos table 3
ipos_table3 <- ipos[current == 0 | ipos_type=="",
  list(expired_missing_IPOS = length(unique(case_no))),
  by=list(team, supervisor, primary_staff)]
ipos_table3 <- setkey(ipos_table3, team)[aux$teamsCMH, nomatch = NA]
ipos_table3 <- ipos_table3[order(team, supervisor)]
ipos_table3 <- ipos_table3[order(team, supervisor, primary_staff)]
ipos_table3[is.na(expired_missing_IPOS), expired_missing_IPOS := 0]
ipos_table3[is.na(supervisor), supervisor := "none/missing"]
ipos_table3 <- rbindlist( list(
  ipos[current == 0 | ipos_type == "",
       list(primary_staff = "",
            expired_missing_IPOS = length(unique(case_no))),
       by=list(team, supervisor)],
  ipos_table3))
ipos_table3 <-
  ipos_table3[order(team, supervisor, primary_staff, expired_missing_IPOS)]
# unsigned/draft Documents -----------------------------------------------------
# keep only Draft and Unsigned
unsigned_docs <-
  setkey(unsigned_docs, doc_status, f2f)[J(c("Draft", "Unsigned"), "Y")]
setkey(unsigned_docs, NULL)
# fix team column
unsigned_docs[, team := cmh_recode(team)]
# keep all teams, per Laura H
setkey(unsigned_docs, team)[!J(aux$teamsCMH), team := "other team(s)"]
setkey(unsigned_docs, NULL)
unsignTeam <- unsigned_docs[, list(docs_unsigned = length(case_no)), by = team]
# keep only CMH teams
unsignTeam <- setkey(unsignTeam, team)[J(aux$teamsCMH)]
unsignTeam[, month :=
  as.character(as.yearmon(paste(input$current_month, input$calendar_year)))]

# unsigned/draft docs by supervisor and author ---
unsigned_docs[supervisor=="", supervisor := "missing supervisor"]
unsignTeam2 <- unsigned_docs[, list(unsigned_draft_docs = length(do_title)),
                             by=list(team, supervisor, author)]
# assign levels ---
unsignTeam2 <-
  mmerge(l = list(unsignTeam2, modify$team_only[, c("team", "level"), with=FALSE]),
         by=c("team"), all.x=TRUE)
unsignTeam2 <-
  mmerge(l = list(unsignTeam2, modify$staff_only), by=c("team", "author"), all.x=TRUE)
# combine level columns into one column
unsignTeam2[, level := aux$comb_cols(level.x, level.y)]
unsignTeam2[, c("level.x", "level.y") := NULL]
unsignTeam2 <- unsignTeam2[order(team, supervisor, author)]
# make unsign table for pdf doc ---
modify$unsign_t1a <- unsignTeam2[, list(author = "",
  unsigned_draft_docs = sum(unsigned_draft_docs, na.rm=TRUE)),
  by = list(supervisor)]
modify$unsign_t1b <- unsignTeam2[, list(unsigned_draft_docs =
  sum(unsigned_draft_docs, na.rm=TRUE)), by=list(supervisor, author)]
unsign_table <-rbindlist(list(modify$unsign_t1a, modify$unsign_t1b))
unsign_table <- unsign_table[order(supervisor, author)]
unsign_table[author != "", supervisor := ""]
unsign_table[, unsigned_draft_docs := as.character(unsigned_draft_docs)]
unsign_table[author == "", unsigned_draft_docs :=
  paste0("\\hspace{2cm}\\textbf{", unsigned_draft_docs, "}")]
unsign_table[author=="", supervisor := paste0("\\textbf{", supervisor, "}")]
unsign_table <- rbindlist(list(unsign_table,
  data.table(supervisor="\\textbf{Grand total}",
  author="", unsigned_draft_docs = paste0("\\textbf{",
  unsignTeam2[, sum(unsigned_draft_docs)], "}") )))
# consumers with Demographic Errors -------------------------------------------
# demo_error list ----
demo_errors[, team := cmh_recode(team)]
demo_error_list <- demo_errors[num_qi_errors > 0]
setorder(demo_error_list, team, supervisor, primary_staff)
demo_error_list <- setkey(demo_error_list, team)[J(aux$teamsCMH), nomatch = 0]
# fix teams
demo_team <- demo_errors[num_qi_errors > 0,
  list(con_errors = length(unique(case_no))), keyby = team]
demo_team <- demo_team[J(aux$teamsCMH), nomatch = NA]
demo_team[is.na(demo_team)] <- 0
demo_team <- setkey(demo_team, team)[modify$con_teams, nomatch = NA]
demo_team[is.na(demo_team)] <- 0
demo_team[, pct_errors := aux$specify_decimal(con_errors/case_load*100, 1)]
demo_team[, month := as.character(as.yearmon(
  paste(input$current_month, input$calendar_year)))]
# consumers with Health Errors ------------------------------------------------
# list of ordered errors
modify$col_order <- c("team", "primary_staff_supervisor", "primary_staff",
              "case_no", "data_issue_type", "notes")
health_error_list <- health_errors[, unique(.SD), .SDc = modify$col_order]
setcolorder(health_error_list, modify$col_order)
health_error_list[, team := cmh_recode(team)]
health_error_list <-
  setkey(health_error_list, team)[J(aux$teamsCMH), nomatch = 0]
# keep only wanted columns
modify$keep_cols <- c("case_no", "team", "data_issue_type")
health_errors[, setdiff(names(health_errors), modify$keep_cols) := NULL]
# fix team
health_errors[, team := cmh_recode(team)]
# remove duplicates
health_errors <- unique(health_errors)
# aggregate data
health_team <-
  health_errors[, list(con_errors = length(unique(case_no))), by = list(team)]
health_team <- setkey(health_team, team)[aux$teamsCMH, nomatch = NA]
health_team <- setkey(health_team, team)[modify$con_teams, nomatch = NA]
health_team[is.na(health_team)] <- 0
health_team[, pct_errors := round(con_errors/case_load*100, 2)]
health_team[, month :=
  as.character(as.yearmon(paste(input$current_month, input$calendar_year)))]
# consumers with Minimum Wage Errors ------------------------------------------
wage_errors[, team := cmh_recode(team)]
setcolorder(wage_errors, c("case_no", "team", "supervisor", "primary_staff",
  "employment_status", "minimum_wage_error", "minimum_wage"))
## list of ordered errors
wage_error_list <- wage_errors[minimum_wage_error > 0]
setorder(wage_error_list, team, supervisor, primary_staff)
wage_error_list <- setkey(wage_error_list, team)[J(aux$teamsCMH), nomatch = 0]
# keep wanted columns
keep_cols <- c("case_no", "team", "minimum_wage_error")
wage_errors <- wage_errors[, unique(.SD), .SDcols = keep_cols]
# aggregate data
wageTeam <-
  wage_errors[, list(con_errors = sum(minimum_wage_error, na.rm = TRUE)),
              nomatch = NA, by = team]
wageTeam <- setkey(wageTeam, team)[modify$con_teams, nomatch = NA]
wageTeam[is.na(wageTeam)] <- 0
wageTeam[, pct_errors := aux$specify_decimal(con_errors/case_load*100, 1)]
wageTeam[, month :=
  as.character(as.yearmon(paste(input$current_month, input$calendar_year)))]

# self sufficiency matrix for MI Adult and ACT --------------------------------
# detail ---
ssm[, team := cmh_recode(team)]
ssm[is.na(supervisor), supervisor := "missing"]
ssm[is.na(primary_staff), primary_staff := "missing"]
ssm[, `2+ SSMs` := ifelse(num_SSMs >= 2, 1, 0)]
# summary by team. supervisor, author (primary staff) ---
ssm_tsa <- ssm[, list(num_cases = length(unique(case_no)),
           `2+ SSMs` = sum(`2+ SSMs`, na.rm = TRUE)),
    keyby = list(team, supervisor, primary_staff)]
ssm_tsa[, `pct 2+` := `2+ SSMs`/num_cases*100]
ssm_tsa[, ts:= paste(team, supervisor, sep = ":")]
# summary by team. supervisor ---
ssm_ts <- ssm[, list(num_cases = length(unique(case_no)),
                      `2+ SSMs` = sum(`2+ SSMs`, na.rm = TRUE)),
               keyby = list(team, supervisor)]
ssm_ts[, `pct 2+` := `2+ SSMs`/num_cases*100]
ssm_ts[, ts:= paste(team, supervisor, sep = ":")]
# last services in 30/60/90 ----------------------------------------------------
# service by team and staff ----
svcTeamStaff <-
  setkey(last_svc, team)[J(aux$teamsCMH),
                         list(case_load = length(unique(case_no)),
                              `days > 30` = sum(`days > 30`, na.rm=TRUE),
                              `days > 60` = sum(`days > 60`, na.rm=TRUE),
                              `days > 90` = sum(`days > 90`, na.rm=TRUE),
                              `days > 180` = sum(`days > 180`, na.rm=TRUE)),
                         by=list(team, primary_staff)]
## service by team ##
svcTeam <- setkey(last_svc, team)[J(aux$teamsCMH),
  list(case_load = length(unique(case_no)),
       primary_staff = "", level = "top",
       `days > 30` = sum(`days > 30`, na.rm=TRUE),
       `days > 60` = sum(`days > 60`, na.rm=TRUE),
       `days > 90` = sum(`days > 90`, na.rm=TRUE),
       `days > 180` = sum(`days > 180`, na.rm=TRUE)), by = team]
## service - grand total ##
invisible(setkey(last_svc, NULL))
svcAll <- last_svc[, list(team= "\\textbf{Grand Total}",
                          primary_staff = "",
                          case_load = paste("\\textbf{", length(unique(case_no)), "}" ),
                          level = "top",
                          `days > 30` = paste0("{\\textbf{", sum(`days > 30`, na.rm=TRUE), "}"),
                          `days > 60` = paste0("{\\textbf{", sum(`days > 60`, na.rm=TRUE), "}"),
                          `days > 90` = paste0("{\\textbf{", sum(`days > 90`, na.rm=TRUE), "}"),
                          `days > 180` = paste0("{\\textbf{", sum(`days > 180`, na.rm=TRUE), "}") )]
## case load and not seen 30-60-90-180 days ##
svcTeam2 <- setkey(last_svc, team)[J(aux$teamsCMH),
  list(case_load = length(unique(case_no)),
       `days > 30` = sum(`days > 30`, na.rm=TRUE),
       `days > 60` = sum(`days > 60`, na.rm=TRUE),
       `days > 90` = sum(`days > 90`, na.rm=TRUE),
       `days > 180` = sum(`days > 180`, na.rm=TRUE)),
                                   by=list(team, primary_staff)]
svcTeam2 <- svcTeam2[order(team, primary_staff)]
### add levels based on modify$staff_only and Team Level Rules
setnames(modify$staff_only, old="author",  new="primary_staff")
svcTeam2 <-
  merge(svcTeam2,
        modify$staff_only[, c("primary_staff", "level"), with=FALSE],
        all.x=TRUE, by="primary_staff")
setkey(svcTeam2, team)[J("Child"), level := "3"]
setkey(svcTeam2, team)[J(c("ACT", "Child Home Based")), level := "4"]
svcTeam2[is.na(level), level := "-"]
setcolorder(svcTeam, c("team", "primary_staff", "case_load", "level",
  "days > 30", "days > 60", "days > 90", "days > 180"))
setcolorder(svcTeam2, c("team", "primary_staff", "case_load", "level",
  "days > 30", "days > 60", "days > 90", "days > 180"))
svc_table <- rbindlist(list(svcTeam, svcTeam2))
svc_table[level=="", level := "-"]
svc_table[, level :=
  factor(level,
         levels=c("top", "DM", "respite", "Supervisor", "Therapy", 5:1, "-") )]
svc_table <- svc_table[order(team, level, primary_staff)]
svc_table[primary_staff!="", team := ""]
svc_table[primary_staff=="", level := "-"]
svc_table[primary_staff=="", team := paste0("\\textbf{", team, "}")]
svc_table[, case_load := as.character(case_load)]
svc_table[, `days > 30` := as.character(`days > 30`)]
svc_table[, `days > 60` := as.character(`days > 60`)]
svc_table[, `days > 90` := as.character(`days > 90`)]
svc_table[, `days > 180` := as.character(`days > 180`)]
svc_table[primary_staff == "", case_load := paste0("\\textbf{", case_load, "}")]
svc_table[primary_staff == "",
          `days > 30` := paste0("\\textbf{", `days > 30`, "}")]
svc_table[primary_staff == "",
          `days > 60` := paste0("\\textbf{", `days > 60`, "}")]
svc_table[primary_staff == "",
          `days > 90` := paste0("\\textbf{", `days > 90`, "}")]
svc_table[primary_staff == "",
          `days > 180` := paste0("\\textbf{", `days > 180`, "}")]
svc_table <- rbindlist(list(svc_table, svcAll))
svc_table[level == "top", level := ""]
svc_table[grep(x = team, pattern = "ACT"), team := "\\textbf{ACT Team}"]