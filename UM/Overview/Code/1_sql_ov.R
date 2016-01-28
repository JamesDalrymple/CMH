sql <- list(
  channel = odbcConnect("WSHSQLGP"),
  query = list()
  )

sql$query$comm_hosp <-
  sprintf(
    "select distinct
    hosp.case_no, hosp.auth_eff, hosp.auth_exp, hosp.hosp_disc,
    hosp.auth_days
    from encompass.dbo.tblE2_Hosp as hosp
    where hosp.county = 'Washtenaw' and hosp.cpt_code not like '09%%'
    and hosp.auth_eff between '%1$s' and '%2$s'
    and hosp.contract_paneltype not like 'State Facility%%'",
    input$min_start, input$max_end)

sql$query$state_hosp <-
  sprintf(
    "select distinct
    hosp.case_no, hosp.auth_eff, hosp.auth_exp, hosp.hosp_disc,
    hosp.auth_days
    from encompass.dbo.tblE2_Hosp as hosp
    where hosp.county = 'Washtenaw' and hosp.cpt_code not like '09%%'
    and hosp.auth_eff between '%1$s' and '%2$s'
    and hosp.contract_paneltype like 'State Facility%%'",
    input$min_start, input$max_end)

sql$query$adm <-
  sprintf(
"select distinct
  adm.case_no, adm.provider_eff as team_eff, adm.provider_exp as team_exp,
  adm.provider as team, adm.assigned_staff as staff, adm.staff_eff,
  adm.staff_exp
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
    input$min_start, input$max_end)

sql$query$served <-
  sprintf(
    "select distinct
    svc.case_no, cast(svc.service_date as date) as service_date
    from encompass.dbo.tblE2_SAL_claims_w_SUD as svc
    where svc.county = 'Washtenaw' and
    svc.provider_type <> 'SUD Treatment Agency' and
    svc.service_date between '%1$s' and '%2$s'",
    input$min_start, input$max_end)

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