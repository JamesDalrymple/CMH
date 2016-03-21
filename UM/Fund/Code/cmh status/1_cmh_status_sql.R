# funding bucket -- must download manually -- ranged point in  time data
sql <- new.env(parent=.GlobalEnv)
sql$channel <- odbcConnect("WSHSQLGP")

# current state hospital consumers
sql$query$cmh_adm <-
"select distinct
	case_no, provider, provider_eff, provider_exp
from encompass.dbo.tblE2_Open_Consumers
where county = 'Washtenaw' and provider like 'WSH%'"

sql$output$cmh_adm <-
  sqlQuery(query = sql$query$cmh_adm,
           channel = sql$channel, stringsAsFactors = FALSE)
sql$output$cmh_adm <- data.table(sql$output$cmh_adm)

wb_fund <- loadWorkbook(file.path(project_wd$data, "Medicare TPP 12 21 15.xlsx"))
sql$fund_names <- names(getSheets(wb_fund))
