sql <- list(
  channel = odbcConnect("WSHSQLGP"),
  query = list())

# consumer demograhic
sql$query$address <-
  sprintf("select distinct
    CMH.county, CMH.case_no,
    ltrim(rtrim(address.AD_ADDR1)) as Address_line1,
    ltrim(rtrim(address.AD_ADDR2)) as Address_line2,
    ltrim(rtrim(C.CL_FNAME)) as Client_First,
    ltrim(rtrim(C.CL_LNAME)) as client_last,
    address.AD_CITY as City, address.AD_STATE as state,
    left( ltrim(address.AD_ZIP), 5) as zipCode,
    ltrim(rtrim(MailingAddress.AD_ADDR1)) as MailAddress_line1,
    ltrim(rtrim(MailingAddress.AD_ADDR2)) as MailAddress_line2,
    MailingAddress.AD_CITY as MailCity, MailingAddress.AD_STATE  as Mailstate,
    left( ltrim( MailingAddress.AD_ZIP), 5) as MailzipCode,
    CMH.Team, CMH.MI, CMH.DD, Primary_Staff, CMH.CMH_EFFDT as CMH_Adm_date,
    C.CL_HPHONE  as home_Phone, C.CL_WPHONE  as work_phone,
    CMH.LivingArrangement,
    Case when S.Case_no is not null then 'Y' else 'N' end as service_in_x_days,
    Demo.CD_EFFDT as Last_demo_update,
    Case when Guard.Case_No IS not null then 'Y' else null end as Guardian_in_sheet2
from encompass.dbo.ENCClient as ENC
join encompass.dbo.PCCClient as C on C.CL_RCDID = ENC.CL_RCDID
left join encompass.dbo.PCCClientDemographics Demo on Demo.CD_RCDID = C.CLF_CDEID
left join encompass.dbo.PCFCode ResidentialArrangement on
  ResidentialArrangement.CO_RCDID = Demo.CDF_QIRESI
join encompass.dbo.tblE2_CMH_Open_Consumers CMH on CMH.Case_No = cast( C.CL_CASENO as Int)
left join encompass.dbo.PCFAddress address on address.AD_RCDID = C.CLF_ADRID
left join encompass.dbo.PCFAddress MailingAddress on Mailingaddress.AD_RCDID = ENC.CLF_ADRID
left join tblE2_Bio_Psych_Social Bio on Bio.county like
  'Washtenaw' and Bio.case_no = CMH.case_no
join encompass.dbo.tblE2_SALs S on S.county = CMH.county and
  S.Case_no = CMH.Case_no and S.CPT_Code in ('H2014', 'H2023', 'T2015')
  and S.SA_SRVDATE between '%1$s' and '%2$s'
left join encompass.dbo.E2_Fn_CMH_Consumer_Guardian ('Washtenaw', '%2$s', '%2$s')
  as Guard on Guard.Case_No = CMH.Case_No
where CMH.county like 'Washtenaw'", input$start_date, input$end_date)
# last service
sql$query$guardian <-
  sprintf("select distinct
	CMH.county, CMH.case_no,
	ltrim(rtrim(C.CL_FNAME)) as Client_First,
  ltrim(rtrim(C.CL_LNAME)) as client_last,
	cast(GuardianshipType.CO_NAME as varchar(32)) as GuardianshipType,
	ltrim(rtrim(Contact.CC_FNAME)) as Guardian_First,
  ltrim(rtrim(Contact.CC_LNAME)) as Guardian_Last,
	ltrim(rtrim(Guardian_address.AD_ADDR1)) as MailAddress_line1,
	ltrim(rtrim(Guardian_address.AD_ADDR2)) as MailAddress_line2,
	Guardian_address.AD_CITY as MailCity,
  Guardian_address.AD_STATE as Mailstate,
	Guardian_address.AD_ZIP as MailzipCode
from encompass.dbo.ENCClient ENC
join encompass.dbo.PCCClient C on C.CL_RCDID = ENC.CL_RCDID
join encompass.dbo.tblE2_CMH_Open_Consumers CMH on
  CMH.Case_No = cast( C.CL_CASENO as Int) and CMH.county like 'Washtenaw'
join encompass.dbo.ENCClientContact Contact on Contact.CCF_CLTID = ENC.CL_RCDID
  and CC_OKTOUSE = 'Y' and CC_EXPDT is null
join encompass.dbo.PCFCode GuardianshipType on
  Contact.CCF_CCOGTP = GuardianshipType.CO_RCDID
join encompass.dbo.PCFAddress as Guardian_address on
  Contact.CCF_ADRID = Guardian_address.AD_RCDID and
  Guardian_address.AD_EXPDT is null
join encompass.dbo.tblE2_SALs S on S.county = CMH.county and
  S.Case_no = CMH.Case_no and S.CPT_Code in ('H2014', 'H2023', 'T2015')
  and S.SA_SRVDATE between '%1$s' and '%2$s'
where cast(GuardianshipType.CO_NAME as varchar(32)) in
	('Public Guardian', 'Family Guardian', 'Temporary Wardship',
  'Court-Appointed Guardian', 'Permanent State Wardship')",
input$start_date, input$end_date)

# query and collect all results -----------------------------------------------
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