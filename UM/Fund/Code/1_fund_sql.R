# funding bucket -- must download manually -- ranged point in  time data
sql <- new.env(parent=.GlobalEnv)
# sql$channel <- odbcConnect("WSHSQLGP")
sql$channel <- odbcConnect("WSHSQL002")

# current state hospital consumers
sql$query$state_hosp <-
  sprintf("select distinct case_no
from encompass.dbo.tblE2_hosp
where county = 'Washtenaw' and contract_paneltype = 'State Facility' and
	(auth_exp >= '%s' or auth_exp is null)",
          date_convert(input$end_date))
# data.table(sqlQuery(channel=sql$channel, query=sql$query$state_hosp))

# insure -- CMH Open Ins 2046 sheet1 -- point in  time data
sql$query$insure <- "select distinct
  CMH.case_no, CMH.age, CMH.medicaid_related, INS2.waiver, INS2.primary_ins,
  INS2.secondary_ins, INS2.other_ins,
max(case when  INS.IC_name = 'MEDICARE' and
  (INS.IP_MEDA = 'Y' and INS.IP_MEDB = 'N')
  then 'Y' else null end) as medicare_A,
max(case when INS.IC_name = 'MEDICARE' and
  (INS.IP_MEDA = 'N' and INS.IP_MEDB = 'Y')
  then 'Y' else null end) as medicare_B,
max(case when INS.IC_name = 'MEDICARE' and
  (INS.IP_MEDA = 'Y' and INS.IP_MEDB = 'Y')
  then 'Y' else null end) as medicare_A_B,
max(case when INS.IC_name = 'MEDICARE PART D'
  then 'Y' else null end) as medicare_D
from encompass.dbo.tblE2_CMH_Open_Consumers_w_OBRA as CMH
left join encompass.dbo.tblE2_IPOS as IPOS on
  CMH.County = IPOS.County and CMH.Case_No = IPOS.Case_No
  and getdate() between IP_effdt and IP_expdt
left join encompass.dbo.PCCClientDemographics as demo
  on CMH.CLF_CDEID  = Demo.CD_RCDID
left join encompass.dbo.PCFCode as ParentalStatus
  on Demo.CDF_QIPSTS = ParentalStatus.CO_RCDID
left join encompass.dbo.tblE2_Consumer_Ins_Current INS
  on INS.case_No = CMH.case_no
and INS.IC_name in ( 'MEDICARE PART D', 'MEDICARE')
left join encompass.dbo.tblE2_Consumer_Ins_Current2 INS2
  on INS2.case_No = CMH.case_no
left join encompass.dbo.PARInsurancePolicyMedicarePartD as MedicarePartD
  on MedicarePartD.IP_RCDID = INS.IP_RCDID and INS.IC_name = 'MEDICARE PART D'
left join encompass.dbo.PCFCode PrescriptionPlan on
  PrescriptionPlan.CO_RCDID = MedicarePartD.IPF_PPLAN
where CMH.County = 'Washtenaw'
group by CMH.case_no, CMH.age, CMH.Medicaid_Related, INS2.Waiver,
  INS2.primary_ins, INS2.secondary_ins, INS2.other_ins"

# ins_detail -- CMH Open Ins 2046 sheet2 -- point in time data
sql$query$pvt_insure <- "select distinct
	CMH.case_no, INS.IC_name as insurance_name
from encompass.dbo.tblE2_CMH_Open_Consumers_w_OBRA CMH
join encompass.dbo.tblE2_Consumer_Ins_Current INS on
  INS.county = CMH.county and INS.case_No = CMH.case_no
	and INS.IC_name not in ('MEDICARE', 'Child Waiver', 'HAB Waiver',
  'SED Waiver', 'MEDICARE PART D',
	'Medicaid Deductible',  'Financial Determination', 'SDA,SSI,SSDI',
  'Adoption Subsidy',	'Assessed for Medicaid Expansion - Eligible',
  'Assessed for Medicaid Expansion - Not Eligible',	'SOBRIETY COURT',
  'Autism', 'MENTAL HEALTH COURT')
where CMH.County = 'Washtenaw'
group by CMH.case_no, INS.IC_name"

# case_load -- duplicate rows/consumer dealt with shortly, point in time data
sql$query$case_load <- "select distinct
	AC.case_no, CMH.primary_staff, AC.staff_type as primary_staff_type,
  AC.supervisor
from encompass.dbo.tblE2_Open_Consumers as AC
join encompass.dbo.E2_Fn_Active_Clients_Between2
  ('Washtenaw', GETDATE(), GETDATE()) as CMH on
AC.county = CMH.county and AC.case_no = CMH.case_no
where AC.County = 'Washtenaw' and CMH.primary_staff = AC.assigned_staff"

# admit, E2 2181 sheet1
sql$query$admit <- "select distinct
case_no, team2 as team, team_effdt, team_expdt, cmh_effdt, cmh_expdt
                           from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA
                           where county = 'Washtenaw' and team_expdt is null"

# court -- 2061 sheet1, court order repetition & PRR -- point in time data
sql$query$court <- "select distinct
	case_no, ordertype, cs_order_date
from encompass.dbo.E2_Fn_Court_Orders ('Washtenaw')
where prr_Date > = GETDATE()"

# demo -- CMH demo 2105 -- point in time data
sql$query$demo <- "select distinct
	CMH.case_no,
  COALESCE( ltrim(rtrim(PrimaryCarePhysician.PC_LNAME)) +',
  '+ ltrim(rtrim(PrimaryCarePhysician.PC_FNAME)) ,
  ltrim(rtrim(PrimaryCarePhysician.PC_LNAME))) as PrimaryCarePhysician,
  PrimaryCareClinic.PC_CNAME  as PrimaryCareClinic
from encompass.dbo.tblE2_CMH_Open_Consumers CMH
join encompass.dbo.PCCClient C on C.CL_RCDID = CMH.ClientID
left join encompass.dbo.PCCClientDemographics CD on CD.CD_RCDID = C.CLF_CDEID
left join encompass.dbo.PCCPrimaryCareClinic PrimaryCareClinic on
  C.CLF_PCCID = PrimaryCareClinic.PC_RCDID and
  PrimaryCareClinic.PC_OKTOUSE ='Y'
left join encompass.dbo.PCCPrimaryCarePhysician PrimaryCarePhysician on
  C.CLF_PCPID = PrimaryCarePhysician.PC_RCDID and
  PrimaryCarePhysician.PC_OKTOUSE  ='Y'
where CMH.County = 'Washtenaw'"

# diagnoses -- diagnoses 2157 -- point in time data
sql$query$diagnoses <- "select distinct
	CMH.case_no, CMH.diag1, CMH.diag1_desc, CMH.diag2, CMH.diag2_desc
from encompass.dbo.E2_Fn_CMH_Consumers_Diagnoses3
  ('Washtenaw', getdate(), getdate()) as CMH
where CMH.team not in ('WSH - PATH/PORT', 'WSH - Sobriety Court',
	'Crisis Residential Services', 'WSH - OBRA', 'WSH - MH Court')"
# locus -- locus 2227 -- ranged point in time data
sql$query$locus <- sprintf(
"select distinct
doc.case_no, Doc.DO_date as locus_date,
RecommendedDisposition.CO_NAME as recommended_disposition,
overiddenDisposition.CO_NAME as overidden_disposition,
AC.CMH_effdt as cmh_adm_date
from encompass.dbo.LCSAssessmentHeader as locus
join encompass.dbo.tblE2_Document as doc on locus.AH_RCDID  = doc.DO_RCDID
left join encompass.dbo.E2_Fn_Active_Clients_Between
  ('Washtenaw', '%1$s', '%2$s') as AC on
  DOC.county = AC.county and AC.Case_no = DOC.Case_no
left join encompass.dbo.PCFCode as RecommendedDisposition on
  Locus.AHF_RDISP	= RecommendedDisposition.CO_RCDID
left join encompass.dbo.PCFCode as OveriddenDisposition
  on Locus.AHF_ODISP	= OveriddenDisposition.CO_RCDID
where doc.do_date between '%1$s' and '%2$s'
and Doc.County = 'Washtenaw'",
  date_convert(input$start_date),
  date_convert(input$end_date)
)

sql$fb_full_list <- list.files(path = "G:/CSTS Data Analyst Archives/FB_archives/rds",
                      full.names = TRUE)
sql$fb_read <- grep(x = sql$fb_full_list,
pattern = paste(
  # format(date_convert(input$end_date) - 365, "%m_%d_%y"),
  format(date_convert(input$end_date), "%m_%d_%y"),
  sep = "_to_"
), value = TRUE)

sql$fb_read <-
  grep(x = sql$fb_read,
    pattern = format(as.Date(input$run_par, format = "%m_%d_%Y"), "%m_%d_%y"),
    value = TRUE)

if (length(sql$fb_read) != 2) p_stop("you are not reading in 2 funding bucket
                              files, please look at sql$fb_read", sql$fb_read)

# fb_data <- NULL
# for (i in seq_along(sql$fb_read)) {
# fb_data <- rbindlist(list(fb_data, readRDS(sql$fb_read[i])), use.names = TRUE)
# }
fb_data <-
  Reduce(
    f = function(...)
      rbindlist(list(...), use.names = TRUE),
    x = lapply(sql$fb_read, readRDS)
  )

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
