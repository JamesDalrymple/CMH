# self sufficiency modeling

#### read in sql_data/csv_files ####
channel <- odbcConnect("wshsqlgp")
odbcQuery(channel=channel, query="use James_CSTS")

# base SSM table with team/supervisor/primary_staff
sql_base_SSM <- sprintf("
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
left join encompass.dbo.tblE2_SelfSufficiencyMatrix as SSM on CMH.county = SSM.County and SSM.County ='Washtenaw'
  and CMH.case_no = SSM.case_no and SSM.provider is null
  and SSM.Matrix_date between '%1$s' and '%2$s'
  and (SSM.Income is not null or SSM.Employment is not null or SSM.Shelter is not null
  or SSM.Food is not null or SSM.Education is not null or SSM.Legal is not null
  or SSM.Health_Care is not null or SSM.Life_Skills is not null or SSM.Mental_Health is not null
  or SSM.Substance is not null or SSM.Family_Friends is not null or SSM.Mobility is not null
  or SSM.Community is not null or SSM.Childcare is not null or SSM.Child_Education is not null
  or SSM.Healthcare is not null)
where CMH.Team in ('WSH - MI - Adult', 'WSH - ACT')
  and CMH.CMH_EFFDT <= cast('%2$s' as datetime) - 227
  and CMH.County = 'Washtenaw'
group by CMH.case_no, CMH.Team, CMH.Supervisor, CMH.Primary_Staff", start_date, end_date)

# details of first/last SSM
sql_details_SSM <- "IF OBJECT_ID('tempdb..#2_first_last_ssm') IS NOT NULL
DROP TABLE #2_first_last_ssm
select distinct
  base.case_no, base.team, base.supervisor, base.primary_staff,
  base.first_matrix_date,
  base.last_matrix_date,
  SSM1.income as income1, SSM1.employment as employment1, SSM1.shelter as shelter1,
  SSM1.food as food1, SSM1.education as education1, SSM1.legal as legal1,
  SSM1.health_care as health_care1, SSM1.life_skills as life_skills1,
  SSM1.mental_health as mental_health1, SSM1.substance as substance1,
  SSM1.family_friends as family_friends1, SSM1.mobility as mobility1,
  SSM1.community as community1, SSM1.childcare as childcare1,
  SSM1.child_education as child_education1, SSM1.healthcare as healthcare1,
  SSM2.income as income2, SSM2.employment as employment2, SSM2.shelter as shelter2,
  SSM2.food as food2, SSM2.education as education2,
  SSM2.legal as legal2, SSM2.health_care as health_care2,
  SSM2.life_skills as life_skills2, SSM2.mental_health as mental_health2,
  SSM2.substance as substance2, SSM2.family_friends as family_friends2,
  SSM2.mobility as mobility2, SSM2.community as community2,	SSM2.childcare as childcare2,
  SSM2.child_education as child_education2, SSM2.healthcare as healthcare2
into #2_first_last_ssm
from #1_first_last_SSM as base
left join encompass.dbo.tblE2_SelfSufficiencyMatrix as SSM1
  on SSM1.County = 'Washtenaw' and SSM1.case_no = base.case_no and base.first_matrix_date = SSM1.matrix_date
  and SSM1.provider is null
left join encompass.dbo.tblE2_SelfSufficiencyMatrix as SSM2
  on SSM2.County = 'Washtenaw' and SSM2.case_no = base.case_no and base.last_matrix_date = SSM2.matrix_date
  and SSM2.provider is null
where base.num_SSMs > 1"

# problem cases that I need to discuss with Laura Higle
sql_problems_SSM <- "select count(case_no) as num_cases, case_no
from #2_first_last_ssm
group by case_no
having count(case_no) >1"

queryList <- list(sql_base_SSM=sql_base_SSM, sql_details_SSM=sql_details_SSM, sql_problems_SSM=sql_problems_SSM)
sqlOutput <- lapply(queryList, function(x) data.table(sqlQuery(channel=channel, query=x, stringsAsFactors=FALSE, max=0)))

if(nrow(sqlOutput$sql_problems_SSM)>0) {
  stop("Duplicate SSM by non-substance abuse providers, investigate why and report to Laura Higle.")
}

ssm_details <- data.table(sqlQuery(channel=channel, query="select * from #2_first_last_ssm", stringsAsFactors=FALSE, max=0))
ssm_details[, time_diff := as.numeric(as.Date(last_matrix_date) - as.Date(first_matrix_date))]
saveRDS(ssm_details, file=file.path(baseWD, dataWD, current_fy, current_month, "ssm_details.rds"))