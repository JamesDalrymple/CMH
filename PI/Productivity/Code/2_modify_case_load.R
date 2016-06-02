modify <- new.env(parent = .GlobalEnv)

for(i in seq_along(input$names)) { # i = 1
  # load data for current month -----------------------------------------------
  modify$current_folder <-
    file.path(paste("FY", gsub(x=input$fy, pattern="20", replace="")))
  modify$start_date <- input$start_dates[i]
  modify$end_date <- input$end_dates[i]
  time$local_start <- Sys.time()
  source(file.path(input$code_wd, "1_sql_case_load.R"))
  staff_hr <- copy(sql$output$staff_hr)
  prescribers <- copy(sql$output$prescribers)
  services <- copy(sql$output$services)
  cmh_adm <- copy(sql$output$cmh_adm)
  # dates ---------------------------------------------------------------------
  setf(staff_hr, Cs(last_login_dt), as.POSIXct)
  setf(staff_hr, Cs(hire_dt, user_add_date, last_login_dt), as.Date)
  date_cols <- c("doc_date")
  setf(services, j = date_cols, as.Date)
  date_cols <- c("team_effdt", "team_expdt", "cmh_effdt", "cmh_expdt")
  setf(cmh_adm, j = date_cols, as.Date)
  modify$date_range <- paste(modify$start_date, "through", modify$end_date)
  modify$start_date <- date_convert(modify$start_date)
  modify$end_date <- date_convert(modify$end_date)
  # prescribers ---------------------------------------------------------------
  # force prescribers to be supervised by Dr. Florence per Laura H. 6/17/2014
  prescribers <- unique(c(
    prescribers[, staff_name],
    cmh_adm[staff_type=="Psychiatrist", unique(assigned_staff)],
    services[staff_type=="Psychiatrist", unique(author)] # possibly redundant
  ))
  setkey(cmh_adm, assigned_staff)[J(prescribers),
                                  supervisor := "Florence, Timothy"]
  setkey(cmh_adm, supervisor)[J(prescribers),
                              supervisor := "Florence, Timothy"]
  setkey(cmh_adm, NULL)
  setkey(services, author)[J(prescribers),
                           supervisor := "Florence, Timothy"]
  setkey(services, supervisor)[J(prescribers),
                               supervisor := "Florence, Timothy"]
  setkey(services, NULL)
  # cmh admission records -----------------------------------------------------
  cmh_adm[is.na(supervisor), supervisor := "missing"]
  cmh_adm[, team := cmh_recode(team)]
  cmh_adm[, program := recode_team_prog(team)]
  # staff HR ------------------------------------------------------------------
  # people who have multiple staff start/end dates are going to look like they
  # have been continuously employed... not anything we can do about this, part
  # of the database design unfortunately.
  staff_hr <- staff_hr[, unique(.SD),
    .SDc = c("staff", "hire_dt", "user_add_date",
             "last_login_dt", "termination_dt")]
  staff_hr[, user_start := pmin(hire_dt, user_add_date, na.rm = TRUE)]
  staff_hr[, user_end := pmax(as.Date(last_login_dt),
                              as.Date(termination_dt), na.rm = TRUE)]
  # remove people who only were active one day
  setf(staff_hr, j = Cs(user_start, user_end), as.Date)
  staff_hr <- staff_hr[!(user_start == user_end)]
  modify$staff_emp <-
    staff_hr[, unique(.SD), .SDcols = c("staff", "user_start", "user_end")]
  # eliminate people active after end_date and inactive prior to start_date
  modify$staff_emp <- modify$staff_emp[user_start <= modify$end_date]
  modify$staff_emp <- modify$staff_emp[user_end >= modify$start_date]
  # determine how much of the time frame a staff was active
  modify$staff_emp[, active_start :=
                     as.Date(ifelse(user_start <= modify$start_date,
                            modify$start_date, user_start))]
  modify$staff_emp[, active_end :=
                     as.Date(ifelse(user_end >= modify$end_date,
                            modify$end_date, user_end))]
  modify$staff_emp[, active_start :=
    min(active_start, na.rm = TRUE), by = staff]
  modify$staff_emp[, active_end :=
                     max(active_end, na.rm = TRUE), by = staff]
   modify$staff_emp[, amt_active :=
    (as.num(active_end - active_start) + 1)/
      (as.num(modify$end_date - modify$start_date) + 1)]
  # services ---
  services <-
    sqldf("select distinct
    srv.case_no, srv.doc_date, srv.begin_time, srv.end_time, srv.author,
    srv.staff_type, srv.supervisor, srv.f2f, srv.doc_type, adm.team, adm.program
    from services as srv
    left join cmh_adm as adm on srv.case_no = adm.case_no
    and srv.doc_date >= adm.team_effdt and
    (srv.doc_date <= adm.team_expdt or adm.team_expdt is null) ")
  services <- data.table(services)
  services[is.na(team), team := "non-core CSTS Team"]
  # change staff type to uknown when missing ... staff errors from adm record
  services[is.na(staff_type), staff_type := "unknown"]
  # change staff type to uknown when missing
  services[is.na(supervisor) | supervisor=="", supervisor := "unknown"]
  # remove and save errors ---
  services[, elapsed_time :=
           aux$elapsed_time(start = begin_time, end = end_time)]
  services[, error := NA_character_] # initialize
  # only midnight staff allowed to do this
  services[begin_time > end_time,
           error := "begin_time after end_time OR midnight staff"]
  # I assume no one works more than 10 hours a day here at CSTS
  services[is.na(error) & elapsed_time > 10*60, error := "10+ hrs"]
  services[is.na(error) & between(begin_time, 0, 600) & elapsed_time > 200,
           error := "begin_time marked AM instead of PM"]
  services[is.na(error) & between(end_time, 1700, 2400) & elapsed_time > 200]
  services[is.na(error) & is.na(end_time) & f2f != "N",
           error := "end_time missing but marked F2F"]
  services[is.na(error) & is.na(end_time) & f2f == "N",
           error := "end_time missing, marked NF2F"]
  # services[author %in% midnight_staff,
  #  error := "midnight_staff"] # this goes last, highest priority
  time_errors <- services[!is.na(error)] # errors
  services <- services[is.na(error)]
  setorder(services, author, doc_date, begin_time)
  # remove un-needed columns
  services[, c("staff_type", "doc_type", "error") := NULL]
  # trim whitespace
  char_cols <-
    names(sapply(services, class)[sapply(services, class)=="character"])
  for (j in seq_along(char_cols)) {
    set(services, j=j, value = stringi::stri_trim(services[[j]], side = "both"))
  }
  rm(char_cols, j)
  services <- unique(services)
  # aggregating ---------------------------------------------------------------
  # Team, Supervisor, Author ---
  tsa_cl_seen <- sqldf(
    "select adm.team, adm.supervisor, adm.assigned_staff as author,
    count(distinct adm.case_no) as case_load,
    count(distinct srv.case_no) as cl_seen
    from cmh_adm as adm
    left join services as srv on adm.case_no = srv.case_no and
      adm.assigned_staff = srv.author
    group by adm.team, adm.supervisor, adm.assigned_staff")
  tsa_cl_seen <- data.table(tsa_cl_seen)
  # Team, Supervisor ---
  # supervisors are responsible for at least 1 staff seeing each person,
  # doesnt matter if staff is not supervised by them
  ts_cl_seen <- sqldf(
    "select adm.team, adm.supervisor,
    count(distinct adm.case_no) as case_load,
    count(distinct srv.case_no) as cl_seen
    from cmh_adm as adm
    left join services as srv on adm.case_no = srv.case_no and adm.assigned_staff = srv.author
    group by adm.team, adm.supervisor
    order by adm.team, adm.supervisor")
  ts_cl_seen <- data.table(ts_cl_seen)
  # Team ---
  # team's responsible for at least one staff seeing a consumer,
  # w/o regard to what staff saw the consumer ##
  t_cl_seen <- sqldf(
    "select adm.team,
    count(distinct adm.case_no) as case_load,
    count(distinct srv.case_no) as cl_seen
    from cmh_adm as adm
    left join services as srv on adm.case_no = srv.case_no
    group by adm.team
    order by adm.team")
  t_cl_seen <- data.table(t_cl_seen)
  # Supervisor, Author ---
  sa_cl_seen <- sqldf(
    "select adm.supervisor, adm.assigned_staff as author,
    count(distinct adm.case_no) as case_load,
    count(distinct srv.case_no) as cl_seen
    from cmh_adm as adm
    left join services as srv on adm.case_no = srv.case_no and adm.assigned_staff = srv.author
    group by adm.supervisor, adm.assigned_staff")
  sa_cl_seen <- data.table(sa_cl_seen)
  # Supervisor ---
  s_cl_seen <- sqldf(
    "select adm.supervisor,
    count(distinct adm.case_no) as case_load,
    count(distinct srv.case_no) as cl_seen
    from cmh_adm as adm
    left join services as srv on
      adm.case_no = srv.case_no and adm.assigned_staff = srv.author
    group by adm.supervisor
    order by adm.supervisor")
  s_cl_seen <- data.table(s_cl_seen)
  # FACE-to-FACE DURATION -----------------------------------------------------
  # TEAM ---
  # face-to-face hours by team, supervisor, author
  F2FhrsTSA <-
    setkey(services, f2f)[J("Y"), list(
      F2F_Hours = aux$duration(start = begin_time, end = end_time)/60),
      by = c("team", "supervisor", "author", "doc_date")]
  setkey(F2FhrsTSA, NULL)
  F2FhrsTSA <- F2FhrsTSA[, list(F2F_Hours = sum(F2F_Hours, na.rm = TRUE)),
                         by = c("team", "supervisor", "author")]
  # face-to-face hours by team, supervisor
  F2FhrsTS <- setkey(services, f2f)[J("Y"),
    list(F2F_Hours = aux$duration(start = begin_time, end = end_time)/60),
         by = c("team", "supervisor", "author", "doc_date")]
  setkey(F2FhrsTS, NULL)
  F2FhrsTS <- F2FhrsTS[, list(F2F_Hours = sum(F2F_Hours, na.rm = TRUE)),
                       by = c("team", "supervisor", "author")]
  F2FhrsTS <- F2FhrsTS[, list(F2F_Hours = sum(F2F_Hours, na.rm = TRUE)),
                       by = c("team", "supervisor")]
  # face-to-face hours by team
  F2FhrsT <- setkey(services, f2f)[J("Y"),
    list(F2F_Hours = aux$duration(start = begin_time, end = end_time)/60),
         by = c("team", "author", "doc_date")]
  setkey(F2FhrsT, NULL)
  F2FhrsT <- F2FhrsT[, list(F2F_Hours = sum(F2F_Hours, na.rm = TRUE)),
                     by = c("team", "author")]
  F2FhrsT <- F2FhrsT[, list(F2F_Hours = sum(F2F_Hours, na.rm = TRUE)),
                     by = c("team")]
  # NO TEAM -------------------------------------------------------------------
  # face-to-face hours by supervisor, author
  F2FhrsSA <- setkey(services, f2f)[J("Y"),
    list(F2F_Hours = aux$duration(start = begin_time, end = end_time)/60),
         by = c("supervisor", "author", "doc_date")]
  setkey(F2FhrsSA, NULL)
  F2FhrsSA <- F2FhrsSA[, list(F2F_Hours = sum(F2F_Hours, na.rm = TRUE)),
                       by = c("supervisor", "author")]
  ## face-to-face hours by supervisor
  F2FhrsS <- setkey(services, f2f)[J("Y"),
    list(F2F_Hours = aux$duration(start = begin_time, end = end_time)/60),
    keyby = c("supervisor", "author", "doc_date")]
  setkey(F2FhrsS, NULL)
  F2FhrsS <- F2FhrsS[, list(F2F_Hours = sum(F2F_Hours, na.rm = TRUE)),
                     by = c("supervisor", "author")]
  F2FhrsS <- F2FhrsS[, list(F2F_Hours = sum(F2F_Hours, na.rm = TRUE)),
                     by = c("supervisor")]
  # TOTAL DURATION ---
  setkey(services, NULL)
  # TEAM ---
  # total hours by team, supervisor, author
  totalHrsTSA <- services[,
    list(totalHrs = aux$duration(start = begin_time, end = end_time)/60),
         by=c("team", "supervisor", "author", "doc_date")]
  totalHrsTSA <- totalHrsTSA[, list(totalHrs = sum(totalHrs, na.rm = TRUE)),
    by = c("team", "supervisor", "author")]
  # total hours by team, supervisor
  setkey(services, NULL)
  totalHrsTS <- services[,
    list(totalHrs = aux$duration(start = begin_time, end = end_time)/60),
         by = c("team", "supervisor", "author", "doc_date")]
  totalHrsTS <- totalHrsTS[, list(totalHrs = sum(totalHrs, na.rm = TRUE)),
                           by = c("team", "supervisor", "author")]
  totalHrsTS <- totalHrsTS[, list(totalHrs = sum(totalHrs, na.rm = TRUE)),
                           by = c("team", "supervisor")]
  # total hours by team ---
  setkey(services, NULL)
  totalHrsT <- services[,
    list(totalHrs = aux$duration(start = begin_time, end = end_time)/60),
         by = c("team", "author", "doc_date")]
  totalHrsT <- totalHrsT[, list(totalHrs = sum(totalHrs, na.rm = TRUE)),
                         by = c("team", "author")]
  totalHrsT <- totalHrsT[, list(totalHrs = sum(totalHrs, na.rm = TRUE)),
                         by = c("team")]
  # NO TEAM ---
  setkey(services, NULL)
  ## total hours by supervisor ##
  totalHrsS <- services[,
    list(totalHrs = aux$duration(start = begin_time, end = end_time)/60),
         by=c("supervisor", "author", "doc_date")]
  totalHrsS <- totalHrsS[, list(totalHrs = sum(totalHrs, na.rm = TRUE)),
                         by = c("supervisor", "author")]
  totalHrsS <- totalHrsS[, list(totalHrs = sum(totalHrs, na.rm = TRUE)),
                         by = c("supervisor")]
  # total hours by supervisor, author
  setkey(services, NULL)
  totalHrsSA <- services[,
    list(totalHrs = aux$duration(start = begin_time, end = end_time)/60),
         by = c("supervisor", "author", "doc_date")]
  totalHrsSA <- totalHrsSA[, list(totalHrs = sum(totalHrs, na.rm = TRUE)),
    by = c("supervisor", "author")]
  # all_TSA: total hours and F2F hours combined ---
  all_TSA <- merge(totalHrsTSA, F2FhrsTSA,
                   by = c("team", "supervisor", "author"), all = TRUE)
  all_TSA[, NF2F_Hours := totalHrs-F2F_Hours]

  # all_TS ---
  all_TS <- merge(totalHrsTS, F2FhrsTS, by=c("team", "supervisor"), all=TRUE)
  all_TS[, NF2F_Hours := totalHrs-F2F_Hours]
  # all_SA ---
  all_SA <- merge(totalHrsSA, F2FhrsSA, by=c("supervisor", "author"), all=TRUE)
  all_SA[, NF2F_Hours := totalHrs-F2F_Hours]
  #  all_T ---
  all_T <- merge(totalHrsT, F2FhrsT, by=c("team"), all=TRUE)
  all_T[, NF2F_Hours := totalHrs-F2F_Hours]
  # all_S ---
  all_S <- merge(totalHrsS, F2FhrsS, by=c("supervisor"), all=TRUE)
  all_S[, NF2F_Hours := totalHrs-F2F_Hours]

  rm(totalHrsTSA, F2FhrsTSA, totalHrsTS, F2FhrsTS, totalHrsSA, F2FhrsSA,
     totalHrsT, F2FhrsT, totalHrsS, F2FhrsS)

  # contact + unique consumer summaries ---
  # team summary
  t_summary <- mmerge(l = list(
    services[f2f=="Y", list(f2f_contacts = length(case_no),
                            f2f_consumers = length(unique(case_no))),
             by=list(team)],
    services[f2f=="N", list(nf2f_contacts = length(case_no),
                            nf2f_consumers = length(unique(case_no))),
             by=list(team)],
    services[, list(total_contacts = length(case_no),
                    total_consumers = length(unique(case_no))),
             by=list(team)] ),
    by="team", all=TRUE)
  # team + supervisor summary
  ts_summary <- mmerge(l = list(
    services[f2f=="Y", list(f2f_contacts = length(case_no),
                            f2f_consumers = length(unique(case_no))),
             by=list(team, supervisor)],
    services[f2f=="N", list(nf2f_contacts = length(case_no),
                            nf2f_consumers = length(unique(case_no))),
             by=list(team, supervisor)],
    services[, list(total_contacts = length(case_no),
                    total_consumers = length(unique(case_no))),
             by=list(team, supervisor)] ),
    by=c("team", "supervisor"), all=TRUE)
  # supervisor summary
  s_summary <- mmerge(l = list(
    services[f2f=="Y", list(f2f_contacts = length(case_no),
                            f2f_consumers = length(unique(case_no))),
             by=list(supervisor)],
    services[f2f=="N", list(nf2f_contacts = length(case_no),
                            nf2f_consumers = length(unique(case_no))),
             by=list(supervisor)],
    services[, list(total_contacts = length(case_no),
                    total_consumers = length(unique(case_no))),
             by=list(supervisor)] ),
    by=c("supervisor"), all=TRUE)

  # supervisor + author summary
  sa_summary <- mmerge(l = list(
    services[f2f=="Y", list(f2f_contacts = length(case_no),
                            f2f_consumers = length(unique(case_no))),
             by=list(supervisor, author)],
    services[f2f=="N", list(nf2f_contacts = length(case_no),
                            nf2f_consumers = length(unique(case_no))),
             by=list(supervisor, author)],
    services[, list(total_contacts = length(case_no),
                    total_consumers = length(unique(case_no))),
             by=list(supervisor, author)] ),
    by=c("supervisor", "author"), all=TRUE)

  # team + supervisor + author summary
  tsa_summary <- mmerge(l = list(
    services[f2f=="Y", list(f2f_contacts = length(case_no),
                            f2f_consumers = length(unique(case_no))),
             by=list(team, supervisor, author)],
    services[f2f=="N", list(nf2f_contacts = length(case_no),
                            nf2f_consumers = length(unique(case_no))),
             by=list(team, supervisor, author)],
    services[, list(total_contacts = length(case_no),
                    total_consumers = length(unique(case_no))),
             by = list(team, supervisor, author)] ),
    by = c("team", "supervisor", "author"), all = TRUE)

  # author combining ----------------------------------------------------------
  modify$staff_active <-
    modify$staff_emp[, unique(.SD), .SDc = c("staff", "amt_active")]
  # combine hour summaries and contact/consumer summaries ---
  comb_TSA <- mmerge(l = list(all_TSA, tsa_summary, tsa_cl_seen),
                     by = c("team", "supervisor", "author"), all = TRUE)
  comb_TSA[modify$staff_active, amt_active := round(i.amt_active, 2),
           on = c("author" = "staff")]
  comb_TS <- mmerge(l = list(all_TS, ts_summary, ts_cl_seen),
                    by = c("team", "supervisor"), all = TRUE)
  comb_T <- mmerge(l = list(all_T, t_summary, t_cl_seen),
                   by = c("team"), all = TRUE)
  comb_SA <- mmerge(l = list(all_SA, sa_summary, sa_cl_seen),
                    by = c("supervisor", "author"), all = TRUE)
  comb_SA[modify$staff_active, amt_active := round(i.amt_active, 2),
           on = c("author" = "staff")]
  comb_S <- mmerge(l = list(all_S, s_summary, s_cl_seen),
                   by = c("supervisor"), all = TRUE)
  source(file.path(input$code_wd, "3_export_case_load.R"))
}
print(paste("This project ran for",
            round(time$end_time - time$global_start, 2)))