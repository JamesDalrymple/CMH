sql <- list(
  channel = odbcConnect("WSHSQLGP"),
  query = list())

sql$query$adm <- "select distinct
  adm.case_no, adm.team2 as team, adm.cmh_effdt, adm.team_effdt,
  adm.primary_provide_or_not, adm.client_name
  -- adm.assigned_staff, adm.staff_type, adm.primary_staff_or_not, adm.supervisor,
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as adm
where adm.county = 'Washtenaw' and adm.team_expdt is null and adm.cmh_expdt is null"

sql$query$staff <- "select staff.case_no, staff.assigned_staff,
  staff.primary_staff_or_not, staff.supervisor, staff.staff_type,
  staff.staff_eff, staff.staff_exp
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as staff
where staff.staff_exp is null and staff.cmh_expdt is null and staff.team_expdt is null"

sql$output <- sapply(
  names(sql$query),
  FUN = function(x) {
    output <-
      sqlQuery(query = get(x, with(sql, query)),
               channel = sql$channel, stringsAsFactors = FALSE)
    output <- data.table(output)
    # assign(x, output, envir = sql)
    return(output)
  },  USE.NAMES = TRUE)