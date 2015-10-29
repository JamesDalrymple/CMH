#### L1: SQL importing, saving results in RDS in dataWD ####

#### read in sql_data/csv_files ####
sql <- new.env(parent = .GlobalEnv)

sql$channel <- odbcConnect("wshsqlgp")
sql$q_db_name <- "use James_CSTS"
odbcQuery(channel=sql$channel, query=sql$q_db_name)

sql$q_input['sql_cls'] <- sprintf(
  "IF OBJECT_ID('tempdb..#cls') IS NOT NULL
  DROP TABLE #cls
  select
  case_no, SUM(paid_units) as paid_units -- CL_PROCCD as CPT
  into #cls
  from encompass.dbo.tblE2_Claim_paid
  where county = 'Washtenaw'
  and CL_FRMDT between '%1$s' and '%2$s'
  and CL_PROCCD in ('H0043', 'H2015', 'T2025')
  group by case_no",
  format(date_convert(input$end_date) - 181, "%m/%d/%Y"),
  input$end_date
)

# base SSM table with team/supervisor/primary_staff
sql$q_input['sql_base_SSM'] <- sprintf("
IF OBJECT_ID('tempdb..#1_first_last_ssm') IS NOT NULL
DROP TABLE #1_first_last_ssm
select distinct
CMH.case_no,
case when CMH.team = 'WSH - MI - Adult' then 'MI'
when CMH.team = 'WSH - ACT' then 'ACT' else CMH.team end as team,
CMH.supervisor, CMH.primary_staff,
sum(case when SSM.matrix_date is not null then 1 else 0 end) as num_SSMs,
min(SSM.matrix_date) as first_matrix_date,
max(SSM.matrix_date) as last_matrix_date
into #1_first_last_ssm
from encompass.dbo.tblE2_CMH_Open_Consumers as CMH
left join encompass.dbo.tblE2_SelfSufficiencyMatrix as SSM on
  CMH.county = SSM.County and SSM.County ='Washtenaw'
and CMH.case_no = SSM.case_no and SSM.provider is null
and SSM.Matrix_date between '%1$s' and '%2$s'
and (SSM.Income is not null or SSM.Employment is not null or
  SSM.Shelter is not null
  or SSM.Food is not null or SSM.Education is not null or
  SSM.Legal is not null
  or SSM.Health_Care is not null or SSM.Life_Skills is not null or
  SSM.Mental_Health is not null or SSM.Substance is not null or
  SSM.Family_Friends is not null or SSM.Mobility is not null or
  SSM.Community is not null or SSM.Childcare is not null or
  SSM.Child_Education is not null or SSM.Healthcare is not null)
where CMH.Team in ('WSH - MI - Adult', 'WSH - ACT')
  and CMH.CMH_EFFDT <= cast('%2$s' as datetime) - 227
  and CMH.County = 'Washtenaw'
group by CMH.case_no, CMH.Team, CMH.Supervisor, CMH.Primary_Staff",
input$start_date,
input$end_date
)

# details of first/last SSM
sql$q_input['sql_details_SSM'] <-
"IF OBJECT_ID('tempdb..#2_first_last_ssm') IS NOT NULL
DROP TABLE #2_first_last_ssm
select distinct
  base.case_no, base.team, base.supervisor, base.primary_staff,
  base.first_matrix_date,
  base.last_matrix_date, SSM1.Staff,
  -- first SSM
  SSM1.income as income1, SSM1.employment as employment1, SSM1.shelter as
  shelter1, SSM1.food as food1, SSM1.education as education1,
  SSM1.legal as legal1, SSM1.health_care as health_care1,
  SSM1.life_skills as life_skills1, SSM1.mental_health as mental_health1,
  SSM1.substance as substance1, SSM1.family_friends as family_friends1,
  SSM1.mobility as mobility1, SSM1.community as community1,
  SSM1.childcare as childcare1, SSM1.child_education as child_education1,
  SSM1.healthcare as healthcare1,
  -- most recent SSM
  SSM2.income as income2, SSM2.employment as employment2,
  SSM2.shelter as shelter2, SSM2.food as food2, SSM2.education as education2,
  SSM2.legal as legal2, SSM2.health_care as health_care2,
  SSM2.life_skills as life_skills2, SSM2.mental_health as mental_health2,
  SSM2.substance as substance2, SSM2.family_friends as family_friends2,
  SSM2.mobility as mobility2, SSM2.community as community2,
  SSM2.childcare as childcare2, SSM2.child_education as child_education2,
  SSM2.healthcare as healthcare2
into #2_first_last_ssm
from #1_first_last_SSM as base
left join encompass.dbo.tblE2_SelfSufficiencyMatrix as SSM1
  on SSM1.County = 'Washtenaw' and SSM1.case_no = base.case_no and
  base.first_matrix_date = SSM1.matrix_date and SSM1.provider is null
left join encompass.dbo.tblE2_SelfSufficiencyMatrix as SSM2
  on SSM2.County = 'Washtenaw' and SSM2.case_no = base.case_no and
  base.last_matrix_date = SSM2.matrix_date and SSM2.provider is null
where base.num_SSMs > 1"

sql$q_input['sql_demo'] <-
"IF OBJECT_ID('tempdb..#demo_open_consumers') IS NOT NULL
DROP TABLE #demo_open_consumers
select distinct
  cmh.county, cmh.case_no, cmh.cmh_effdt,
  cmh.gender,
  CMH.MI, CMH.DD, CMH.SUD, CMH.age,
  cast(hispanic.co_name as varchar(32)) as hispanic,
  race1.co_name as race1, race2.co_name as race2, race3.co_name as race3,
  education.co_name as education_status,
  cast(correctionStatus.co_name as varchar(48)) as correction_status,
  cast(employmentStatus.co_name as varchar(36)) as employment_status,
  cast(residentialArrangement.co_name as varchar(60)) as res_arrange,
  fosterCareFacility.pr_name as foster_care_fac,
  county.co_name as county_res, address.ad_zip as zipcode_res,
  COALESCE(ltrim(rtrim(primaryCarePhysician.pc_lname)) +', '+
           ltrim(rtrim(PrimaryCarePhysician.pc_fname)),
           ltrim(rtrim(PrimaryCarePhysician.PC_LNAME))) as prim_care_dr,
  primaryCareClinic.pc_cname  as pc_clinic,
  adm.assigned_staff as psychiatrist,
  cast(dhs_an.co_name as varchar(3)) as dhs_an,
  cast(dhs_other.co_name as varchar(3)) as dhs_other
into #demo_open_consumers
from encompass.dbo.tblE2_CMH_Open_Consumers as CMH
join encompass.dbo.PCCClient as C on C.CL_RCDID = CMH.ClientID
left join encompass.dbo.PCCClientDemographics as CD
  on CD.CD_RCDID = C.CLF_CDEID
left join encompass.dbo.PCFCode as hispanic
  on CD.CDF_QIHISP = Hispanic.CO_RCDID
left join encompass.dbo.PCFCode as race1 on CD.CDF_QIRAC1 = Race1.CO_RCDID
left join encompass.dbo.PCFCode as race2 on CD.CDF_QIRAC2 = Race2.CO_RCDID
left join encompass.dbo.PCFCode as race3 on CD.CDF_QIRAC3 = Race3.CO_RCDID
left join encompass.dbo.PCFCode as education
  on CD.CDF_QIEDUC = Education.CO_RCDID
left join encompass.dbo.PCFCode as residentialArrangement
  on ResidentialArrangement.CO_RCDID = CD.CDF_QIRESI
left join encompass.dbo.PCFCode as employmentStatus
  on EmploymentStatus.CO_RCDID = CD.CDF_QIEMPL
left join encompass.dbo.PCFCode as correctionStatus
  on CorrectionStatus.CO_RCDID = CD.CDF_QICORR
left join encompass.dbo.PCCProvider as fosterCareFacility
  on CD.CDF_PRVID = FosterCareFacility.PR_RCDID
left join encompass.dbo.PCFCode as DHS_AN on DHS_AN.CO_RCDID = CD.CDF_QIFIAA
left join encompass.dbo.PCFCode as DHS_OTHER
  on DHS_OTHER.CO_RCDID = CD.CDF_QIFIAO
left join encompass.dbo.PCFAddress as address
  on address.AD_RCDID = C.CLF_ADRID
left join encompass.dbo.PCFCode as county
  on address.ADF_COUNTY = county.CO_RCDID
left join encompass.dbo.PCCPrimaryCareClinic as primaryCareClinic
  on C.CLF_PCCID = PrimaryCareClinic.PC_RCDID and
  PrimaryCareClinic.PC_OKTOUSE ='Y'
left join encompass.dbo.PCCPrimaryCarePhysician as primaryCarePhysician
  on C.CLF_PCPID = PrimaryCarePhysician.PC_RCDID and
  PrimaryCarePhysician.PC_OKTOUSE  ='Y'
left join encompass.dbo.tblE2_Open_Consumers as adm
  on adm.County = CMH.County and Adm.Case_No = CMH.Case_No and
  Adm.Staff_Type = 'Psychiatrist'
where CMH.County like 'Washtenaw%'"

# problem cases that I need to discuss with Laura Higle
sql$q_input['sql_problems_SSM'] <-
"IF OBJECT_ID('tempdb..#SSM_data_problems') IS NOT NULL
DROP TABLE #SSM_data_problems
select count(case_no) as num_cases, case_no
-- into #SSM_data_problems
from #2_first_last_ssm
group by case_no
having count(case_no) >1"

sql$q_output <- list(details = "select * from #2_first_last_ssm",
                     demo = "select * from #demo_open_consumers",
                     cls = "select * from #cls")

sql$check_output <- lapply(sql$q_input, function(x)
    sqlQuery(
      channel = sql$channel,
      query = x,
      stringsAsFactors = FALSE,
      max = 0
    ))
if (!identical(unlist(sql$check_output), character())) {
  p_stop("Please see sql$check_output, something is wrong.",
         sql$check_output)
}

sql$sql_output <-
  lapply(sql$q_output, function(x)
    data.table(
      sqlQuery(
        channel = sql$channel,
        query = x,
        stringsAsFactors = FALSE,
        max = 0
      )
    ))

# making a modification in the list prior to saving as *.rds
with(sql$sql_output, details)[, time_diff :=
  as.numeric(as.Date(last_matrix_date) - as.Date(first_matrix_date))]

saveRDS(sql$sql_output,
        file=file.path(input$dataWD, "ssm_details.rds"))