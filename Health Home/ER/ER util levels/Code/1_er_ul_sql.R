sql <- list(
  channel = odbcConnect("WSHSQL002"),
  query = list()
)

sql$query$er <- # borrowed/shortened from V_CC360_IP_or_ERs_within_2_years
  sprintf("select distinct
	cc.case_no, cc.service_from_date as er_start_date
  -- cc.service_to_date as er_disc_date, cc.admission_date, cc.discharge_date,
  -- Billing_Provider_Name
  from Frank_data.dbo.tblCC360_sinceFY14 as cc
  where cc.service_from_date between '%1$s' and '%2$s'
  and cc.county = 'Washtenaw'
  and (cast(cc.Revenue_Code as int) between 450 and 459 or
      cast(cc.Revenue_Code as int) = 981)",
          date_convert(input$end_date)-365*2+1, date_convert(input$end_date)
  )

sql$query$adm <-
  sprintf(
    "select distinct
    adm.case_no, adm.provider_eff as team_eff, adm.provider_exp as team_exp,
    adm.provider as team, adm.assigned_staff as staff, adm.staff_eff,
    adm.staff_exp, adm.staff_type
    from encompass.dbo.tblE2_Adm_Consumers as adm
    where adm.county = 'Washtenaw' and adm.provider in ('WSH - Health Home')
    and adm.providertype = 'Direct Provider'
    and adm.provider_eff <= '%2$s' and
    (adm.provider_exp >= '%1$s' or adm.provider_exp is null)",
    input$start_date, input$end_date)

sql$output <- sapply(
  names(sql$query),
  FUN = function(x) {
    output <-
      sqlQuery(channel = sql$channel,
               query = get(x, with(sql, query)), stringsAsFactors = FALSE)
    output <- data.table(output)
    # assign(x, output, envir = sql)
    return(output)
  },
  USE.NAMES = TRUE
)

sql$hh_util <- fread(file.path(project_wd$data, "HH_util_FY15.csv"))