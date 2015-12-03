sql <- new.env(parent = .GlobalEnv)
sql$channel <- odbcConnect("WSHSQLGP")

# dates we need to filter datasets with
sql$hosp_dates <-
  aux$fy_combn(start_date = input$start_date, end_date = input$end_date)

sql$query$hosp_query <-
  sprintf(
    "select distinct
	    hosp.case_no, hosp.auth_eff, hosp.auth_exp, hosp.hosp_disc,
      hosp.team_at_admit, hosp.auth_days
    from encompass.dbo.tblE2_Hosp as hosp
    where hosp.county = 'Washtenaw' and hosp.cpt_code not like '09%%'
      and hosp.auth_eff between '%1$s' and '%2$s'
      and hosp.contract_paneltype not like 'State Facility%%'",
    input$start_date, input$end_date)

sql$query$adm_query <-
  sprintf(
    "select distinct
    adm.case_no, adm.provider_eff as team_eff, adm.provider_exp as team_exp,
    adm.provider as team
    from encompass.dbo.tblE2_Adm_Consumers as adm
    where adm.county = 'Washtenaw' and adm.provider in
      ('WSH - ACT', 'WSH - ATO' , 'WSH - Children''s Services',
       'WSH - Children''s Services - Home Based', 'WSH - DD Adult',
       'WSH - Access/Engagement', 'Washtenaw County Community Mental Health',
       'Washtenaw County Community Mental Health-External',
       'WSH - MI - Adult')
      and adm.providertype = 'Direct Provider'
      and adm.provider_eff <= '%2$s' and
      (adm.provider_exp >= '%1$s' or adm.provider_exp is null)",
    input$start_date, input$end_date)

sql$fn_served <- function(start_date, end_date) {
  sprintf(
    "select distinct
	Claim.case_no, CMH.team, cast('%1$s' as date) as span_start, cast('%2$s' as date) as span_end
    from encompass..E2_Fn_Medicaid_Consumers_Served('Washtenaw', '%1$s', '%2$s') Claim
    join encompass..PCCClient C on C.CL_RCDID = Claim.CLTID
    left join encompass..E2_Fn_Active_Clients_Between2 ('Washtenaw', '%1$s', '%2$s') CMH on CMH.clientID = Claim.CLTID and CMH.county = Claim.county
    where Claim.serviceType = 'MH'
    union
    select distinct
    Claim.case_no, CMH.team, cast('%1$s' as date) as span_start, cast('%2$s' as date) as span_end
    from encompass..E2_Fn_ABW_Consumers_Served('Washtenaw', '%1$s', '%2$s') Claim
    join encompass..PCCClient C on C.CL_RCDID = Claim.CLTID
    left join encompass..E2_Fn_Medicaid_Consumers_Served('Washtenaw', '%1$s', '%2$s') Claim2 on Claim.county = Claim2.county and
    Claim.case_no = Claim2.case_no and Claim2.serviceType = 'MH'
    left join E2_Fn_Active_Clients_Between2 ('Washtenaw', '%1$s', '%2$s') CMH on CMH.clientID = Claim.CLTID and CMH.county = Claim.county
    where Claim.serviceType = 'MH' and  Claim2.case_no is null
    union
    select
    Claim.case_no, CMH.team, cast('%1$s' as date) as span_start, cast('%2$s' as date) as span_end
    from encompass..tblE2_SAL_claims_for4 Claim
    join encompass..PCCClient C on C.CL_RCDID = Claim.CLTID
    left join encompass..E2_Fn_Medicaid_Consumers_Served('Washtenaw', '%1$s', '%2$s') Claim2 on Claim.county = Claim2.county and
    Claim.case_no = Claim2.case_no and Claim2.serviceType = 'MH'
    left join E2_Fn_ABW_Consumers_Served('Washtenaw', '%1$s', '%2$s') Claim3 on Claim.county = Claim3.county and
    Claim.case_no = Claim3.case_no and Claim3.serviceType = 'MH'
    left join encompass..E2_Fn_Active_Clients_Between2 ('Washtenaw', '%1$s', '%2$s') CMH on CMH.clientID = Claim.CLTID and CMH.county = Claim.county
    where Claim.service_date between  '%1$s' and '%2$s'
    and Claim.county like 'Washtenaw%%'
    and  Claim2.case_no is null
    and  Claim3.case_no is null", start_date, end_date)
}

sql$output <- sapply(
  names(sql$query),
  FUN = function(x) {
    output <-
      sqlQuery(query = get(x, with(sql, query)),
               channel = sql$channel, stringsAsFactors = FALSE)
    output <- data.table(output)
    # assign(x, output, envir = sql)
    return(output)
  },
  USE.NAMES = TRUE
)

sql$serve_start <- Sys.time()
sql$output$served <- NULL
for (i in seq_len(nrow(sql$hosp_dates))) {
  sql$tmp$svc_query <- sql$fn_served(start_date = sql$hosp_dates[i, span_start],
                                     end_date = sql$hosp_dates[i, span_end])
  sql$tmp$svc_ouput <-
    sqlQuery(query = sql$tmp$svc_query,
             channel = sql$channel, stringsAsFactors = FALSE)
  sql$output$served  <- rbindlist(list(sql$tmp$svc_ouput, sql$output$served ))
  print(paste0("served is ", i/nrow(sql$hosp_dates)*100,
               "% complete")); flush.console()
}
sql$serve_end <- Sys.time()
print(sql$serve_end - sql$serve_start); flush.console()
