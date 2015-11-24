# funding bucket -- must download manually -- ranged point in  time data
sql <- new.env(parent = .GlobalEnv)
sql$channel <- odbcConnect("WSHSQLGP")

# case_load -- duplicate rows/consumer dealt with shortly, point in time data
sql$query$case_load <- "select distinct
	AC.case_no, CMH.primary_staff, AC.staff_type as primary_staff_type,
  AC.supervisor
from encompass.dbo.tblE2_Open_Consumers as AC
join encompass.dbo.E2_Fn_Active_Clients_Between2
  ('Washtenaw', GETDATE(), GETDATE()) as CMH on
AC.county = CMH.county and AC.case_no = CMH.case_no
where AC.County = 'Washtenaw' and CMH.primary_staff = AC.assigned_staff"

# current state hospital consumers
sql$query$state_hosp <-
  sprintf("select distinct case_no
          from encompass.dbo.tblE2_hosp
          where county = 'Washtenaw' and contract_paneltype = 'State Facility' and
          (auth_exp >= '%1$s' or auth_exp is null)",
          input$end_date)

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
  input$start_date, input$end_date)

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
from encompass.dbo.tblE2_CMH_Open_Consumers_w_OBRA CMH
left join encompass.dbo.tblE2_IPOS IPOS on
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

# current state hospital consumers
sql$query$cmh_crisis <-
sprintf("select distinct *
from encompass.dbo.E2_Fn_CMH_H0018_Services('%1$s') as CMH
union
select distinct *
from encompass.dbo.E2_Fn_CMH_H2011_Services('%1$s') as CMH
union
select distinct *
from encompass.dbo.E2_Fn_CMH_Same_dates_w_H0018_Services('%1$s') as CMH",
input$end_date)

# data.table(sqlQuery(channel=sql$channel, query=sql$query$state_hosp))

# insure -- CMH Open Ins 2046 sheet1 -- point in  time data
sql$query$non_cmh_crisis <- sprintf("select distinct
  MH.case_no, cat, service_date, service_end_date, cpt_code,
  MH.cpt_modifier, units, medicaid_related, team2, team_effdt, team_expdt
from encompass..tblE2_SAL_MH_claims_w_0 as MH
left join
  encompass..E2_Fn_Active_Clients_Between('Washtenaw', '%1$s', '%2$s')
  as CMH on MH.county = CMH.county and MH.case_no = CMH.case_no
left join encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as adm
  on MH.County = Adm.County and MH.Case_no = Adm.Case_no
  and team_effdt <= '%2$s'
  and ( team_expdt > = '%1$s' or team_expdt is null)
  and Team2 in ('WSH - OBRA', 'WSH - PATH/PORT',
  'WSH - Sobriety Court', 'WSH - MH Court')
where MH.county = 'Washtenaw'
  and service_date between '%1$s' and '%2$s'
  and Units > 0
  and CMH.Case_No is null", input$start_date, input$end_date)

# sql$fb_full_list <-
#   list.files(path = "G:/CSTS Data Analyst Archives/FB_archives/rds",
#                       full.names = TRUE)
# read_these_fb <-
#   grep(x=sql$fb_full_list,
#   pattern="8_31_14_to_8_31_15 ran 11_03_15", value = TRUE)
#
# fb_data <-
#   Reduce(
#     f = function(...)
#       rbindlist(list(...), use.names = TRUE),
#     x = lapply(read_these_fb, readRDS)
#   )
# --- fb_unit_cost_by_cpt_mod.rda ---
# setnames(fb_data, c("PRI PROCEDURE CODE", "MOD", "ALLOWED AMOUNT", "UNITS"),
#          c("cpt", "mod", "cost", "units"))
# fb2 <- fb_data[, list(units = sum(units, na.rm = TRUE),
#                       cost = sum(cost, na.rm = TRUE)),
#                by = list(cpt, mod)]
# fb2[mod == "  ", mod := NA_character_]
# fb2[, unit_cost :=
#         round(mean(cost/units, na.rm = TRUE), 2),
#         by = list(cpt, mod)]
# fb_cpt_mod_cost <- fb2
# save(fb_cpt_mod_cost, file = "C:/Users/dalrymplej/Documents/GitHub/wccmh/data/fb_unit_cost_by_cpt_mod.rda")

# --- fb_unit_type.RDS ---
# fb_unit_type <-
#   unique(fb_data[, .SD, .SDc = c("PRI PROCEDURE CODE", "MOD", "UNIT TYPE")])
# setnames(fb_unit_type, names(fb_unit_type), c("cpt", "mod", "unit_type"))
# fb_unit_type <- trim(fb_unit_type)
# saveRDS(fb_unit_type, file = "fb_unit_type.RDS")

sql$output <- sapply(
  names(sql$query),
  FUN = function(x) {
    output <-
      sqlQuery(query = get(x, with(sql, query)),
        channel = sql$channel, stringsAsFactors = FALSE)
    output <- data.table(output)
    assign(x, output, envir = sql)
  },
  USE.NAMES = TRUE
)
