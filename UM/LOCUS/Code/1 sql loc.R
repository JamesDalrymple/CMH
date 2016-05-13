sql <- list(
  channel = odbcConnect("WSHSQLGP"),
  query = list()
  )
# admissions
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
    input$start_dt, input$end_dt)
# services
sql$query$services <- sprintf("select distinct
  svc.case_no, cast(svc.service_date as date) as service_date,
  svc.cpt_code, units, cat
from encompass.dbo.tblE2_SAL_claims_w_SUD as svc
where svc.county = 'Washtenaw' and
  svc.provider_type <> 'SUD Treatment Agency' and
  svc.service_date between '%1$s' and '%2$s'",
input$start_dt, input$end_dt)

sql$query$locus <-
  sprintf("select distinct
  doc.case_no, doc.do_date as locus_date, doc.staff,
  locus.ah_score as locus_score,
  RecommendedDisposition.CO_NAME as init_disp,
  OveriddenDisposition.CO_NAME as ovr_disp
from encompass.dbo.LCSAssessmentHeader as locus
join encompass.dbo.tblE2_Document as doc on locus.AH_RCDID  = Doc.DO_RCDID
left join encompass.dbo.PCFCode RecommendedDisposition on
  Locus.AHF_RDISP	= RecommendedDisposition.CO_RCDID
left join encompass.dbo.PCFCode OveriddenDisposition on
  Locus.AHF_ODISP	= OveriddenDisposition.CO_RCDID
where Doc.Do_date between '%1$s' and '%2$s'
  and Doc.County like 'Washtenaw' and (locus.ah_score is not null or
  RecommendedDisposition.CO_NAME is not null)", input$start_dt, input$end_dt)


# generate output based on sql$query list
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