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
    adm.staff_exp, adm.adm_effdt, adm.adm_expdt,
    adm.primary_provide_or_not as prim
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

sql$query$fake <- "select distinct top 2 * from encompass.dbo.tblE2_Adm_Consumers"

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