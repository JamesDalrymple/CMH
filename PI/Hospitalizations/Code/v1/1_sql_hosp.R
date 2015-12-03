sql <- new.env(parent = .GlobalEnv)
sql$channel <- odbcConnect("WSHSQLGP")

sql$hosp_dates <-
  aux$fy_combn(input$start_date, input$end_date)

sql$fn$hosp_served <- function(start_date, end_date, span_label, span_type) {
  sql_query <-
    sprintf(
      "select
      '%3$s' as span_label,
      '%4$s' as span_type,
      case
      when AC.team = 'WSH - ACT' then 'ACT'
      when AC.team = 'WSH - Children''s Services' then 'Child'
      when AC.team = 'WSH - Children''s Services - Home Based' then 'Child HB'
      when AC.team = 'WSH - DD Adult' then 'DD'
      when AC.team in ('WSH - MI - Adult', 'WSH - ATO') then 'MI'
      else team end as team,
      count( distinct Claim.Case_No ) as num_served_consumers,
      num_hosp_consumers, num_hosp_adms, num_hosp_adms_w_disc,
      num_Hosp_consumers * 1.0/count( distinct Claim.Case_No )
        as pct_hosp_consumers,
      total_hosp_days,
      -- total_hosp_days*1.0/num_hosp_adms_w_disc as avg_los,
      max_los,
      num_readmit_30_days,
      consumers_readmit_30_days,
      num_readmit_90_days,
      consumers_readmit_90_days
      from E2_Fn_Active_Clients_Between ( 'Washtenaw', '%1$s', '%2$s') AC
      join tblE2_SAL_claims_for4 Claim on AC.County = Claim.county and
        AC.Case_No = Claim.Case_no
      left join E2_Fn_CMH_Comm_Hosp_adms ( 'Washtenaw', '%1$s', '%2$s') Hosp
        on Hosp.Team_at_admit = AC.team
      where Team  not in ( 'WSH - Utilization Management',
        'Crisis Residential Services',
        'Washtenaw County Community Mental Health',
        'WSH - Access/Engagement')
      group by Team, num_Hosp_consumers, num_Hosp_Adms, num_Hosp_Adms_w_disc,
      num_Hosp_consumers, Total_hosp_days,
      num_readmit_30_days, max_los,
      Consumers_readmit_30_days,
      num_readmit_90_days,
      Consumers_readmit_90_days",
      start_date,
      end_date,
      span_label,
      span_type
    )
  return(sql_query)
}

hosp_served <- NULL
for (i in seq_len(nrow(sql$hosp_dates))) {
  sql$tmp$query <- sql$fn$hosp_served(
    start_date = sql$hosp_dates[i, span_start],
    end_date = sql$hosp_dates[i, span_end],
    span_label = sql$hosp_dates[i, span_label],
    span_type = sql$hosp_dates[i, span_type]
  )
  sql$tmp$output <- sqlQuery(query = sql$tmp$query,
                             channel = sql$channel, stringsAsFactors = FALSE)
  sql$tmp$output <- data.table(sql$tmp$output)
  hosp_served <- rbindlist(list(sql$tmp$output, hosp_served))
}
rm(i)
hosp_served[is.na(hosp_served)] <- 0
hosp_served[num_hosp_adms_w_disc != 0,
            avg_los := total_hosp_days/num_hosp_adms_w_disc]

# admit, E2 2181 sheet1
sql$query$admit <-
  sprintf(
    "select distinct
    case_no, team, team_effdt, team_expdt, cmh_effdt,
    cmh_expdt, primary_provide_or_not
    from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA
    where county = 'Washtenaw' and CMH_effdt<= '%2$s'
    and (CMH_expdt is null or CMH_expdt >= '%1$s')",
    input$start_date,
    input$end_date
  )
admit <- sqlQuery(query = sql$query$admit,
                  channel = sql$channel, stringsAsFactors = FALSE)
admit <- data.table(admit)