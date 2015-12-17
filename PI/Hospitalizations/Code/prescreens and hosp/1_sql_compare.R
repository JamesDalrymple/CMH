# sql <- new.env(parent = .GlobalEnv)
# sql$channel <- odbcConnect("WSHSQLGP")

sql <- list(
  query = list(hosp = "", prescreen = ""),
  output = list(hosp = data.table(),
                prescreen = data.table()),
  f = NA,
  channel = odbcConnect("WSHSQLGP")
  )
attach(sql)
sql <- as.environment('sql')

# 2015 hospital data
sql$query$hosp <-
  sprintf(
  "select distinct
  	county, case_no, auth_eff, auth_exp, hosp_disc
    from tblE2_Hosp
    where county = 'Washtenaw' and Contract_PanelType <> 'State Facility'
    and auth_eff between '%1$s' and '%2$s' and cpt_code not like '09%%'",
  input$start_date, input$end_date)

# 2015 prescreen data
sql$query$prescreen <-
  sprintf(
  "select distinct
	  cast(doc_date as date) as doc_date, doc, ma.case_no, intake_type, disposition
    from E2_Fn_Monitor_Access_Activity('Washtenaw', '%1$s', '%2$s') as ma
    where Disposition = 'Admission & Discharge Plan' and
    Intake_Type = 'Inpatient Admission'", input$start_date, input$end_date)
sql$query$prescreen2 <-
  sprintf(
  "select distinct
    cast(doc_date as date) as doc_date, doc, ma.case_no, intake_type, disposition
    from E2_Fn_Monitor_Access_Activity('Washtenaw', '%1$s', '%2$s') as ma
    where Disposition = 'Admission & Discharge Plan'",
  input$start_date, input$end_date)
sql$query$prescreen3 <-
  sprintf(
  "select distinct
  cast(doc_date as date) as doc_date, doc, ma.case_no, intake_type, disposition
  from E2_Fn_Monitor_Access_Activity('Washtenaw', '%1$s', '%2$s') as ma",
  input$start_date, input$end_date)

# replace sql$f with query function
f <<- function(x) {
  output <-
    sqlQuery(query = x,
             channel = sql$channel, stringsAsFactors = FALSE)
  output <- data.table(output)
  return(output)
}
# save results into sql$output
sql$output <- sql %$% rapply(query, f, how = "list")
