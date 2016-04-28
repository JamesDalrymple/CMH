sql <- list(
  channel = odbcConnect("WSHSQLGP"),
  query = list()
)

# CMH admissions ---
sql$query$cmh_adm <-
  sprintf("select distinct
case_no, team2 as team, cmh_effdt, cmh_expdt, team_effdt, team_expdt
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA
where county = 'Washtenaw' and cmh_effdt <= '%2$s' and (cmh_expdt >= '%1$s' or cmh_expdt is null)",
          input$start_dt, input$end_dt)
# staff_eff, staff_exp, assigned_staff, staff_type, supervisor as current_sup


# IR report 2076 ---
sql$query$ir <- sprintf("select distinct
	ir.case_no, isNULL(prov.vendor, ir.provider) as vendor,
  ir.IR_number, ir.discovery_date, ir.begintime, ir.endtime, ir.classification,
  ir.whathappened
from encompass.dbo.tblE2_IRs as ir
left join encompass.dbo.E2_fn_Contracted_Providers('Washtenaw', '%1$s', '%2$s') as prov on
ir.provider = prov.provider
where ir.county = 'Washtenaw'
  and ir.discovery_Date between '%1$s' and '%2$s'
  and ir.classification in ('Missed Meds', 'Pharmacy error', 'Refused Meds',
    'Wrong dose', 'Wrong med', 'Double dose')
  and ir.Provider_Type not in ( 'SUD Treatment Agency' , 'Vendor')
  and ir.Exclude_from_Reporting is null
union
select distinct
  ir.case_no, ir.provider as vendor, ir.IR_number, ir.discovery_date,
  ir.begintime, ir.endtime, ir.classification, ir.whathappened
from encompass.dbo.tblE2_IRs as ir
where ir.county = 'Washtenaw'
  and ir.discovery_date between '%1$s' and '%2$s'
  and ir.classification in ('Missed Meds', 'Pharmacy error', 'Refused Meds',
    'Wrong dose', 'Wrong med', 'Double dose')
  and ir.provider_type = 'Vendor' and ir.Exclude_from_Reporting is null",
        input$start_dt, input$end_dt)

sql$query$vendor_auth <- sprintf("select distinct
	auth.auth_eff, auth.auth_exp,
	isNull(prov.vendor, auth.pr_name) as vendor, auth.case_no
from encompass.dbo.tblE2_Auth_Approved as auth
left join encompass.dbo.E2_fn_Contracted_Providers('Washtenaw', '%1$s', '%2$s')
  as prov	on auth.pr_name = prov.provider
where
	auth.Provider_Type in ('Contracted Service Location', 'Hospital') and
	auth.county = 'Washtenaw' and (auth.auth_eff <= '%2$s' and
  auth.auth_exp >= '%1$s')
union
select distinct
	cls.ca_effdt as auth_eff, cls.ca_expdt as auth_exp,
	isNull(prov.vendor, cls.service_provider) as vendor, cls.case_no
from encompass.dbo.tblE2_CLS_Consumers_Auth_E2 as cls
left join encompass.dbo.E2_fn_Contracted_Providers('Washtenaw', '%1$s', '%2$s')
  as prov	on cls.Service_Provider = prov.provider
where cls.county = 'Washtenaw' and cls.CA_EFFDT <= '%2$s' and
  cls.CA_EXPDT > '%1$s'", input$start_dt, input$end_dt)

# current medications
sql$query$cur_meds <-
  "select distinct
cmh.county, cmh.team, cmh.primary_staff, cmh.supervisor, cmh.cmh_effdt, cmh.case_no,
CMH.medicaid_related, INS.waiver, drug, formula, SIG, quantity, startDate,
provider
from tblE2_CMH_Open_Consumers_w_OBRA CMH
left join tblE2_Consumer_Current_Medications_HIT Med on Med.Case_No = CMH.Case_No
left join tblE2_Consumer_Ins_Current2 INS on CMH.Case_No = INS.Case_No
where cmh.County = 'Washtenaw'"

sql$output <- sapply(
  names(sql$query),
  FUN = function(x) {
    output <-
      sqlQuery(query = get(x, with(sql, query)),
               channel = sql$channel, stringsAsFactors = FALSE)
    output <- data.table(output)
    return(output)
  },
  USE.NAMES = TRUE
)