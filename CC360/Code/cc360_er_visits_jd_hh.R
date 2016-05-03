#### initializing working directory and input parameters ####
# clear RAM
rm(list = ls())

# working directory
if (!Sys.info()["nodename"] == "WSHSQLGP") {
  print("error, not PCE 107 WSHSQLGP")
}
baseWD <-  "D:/James/CC360" # computer name "WSHSQLGP"

base <- list(pce107 = "D:/James/CC360", # computer name "WSHSQLGP"
             dropbox = "C:/Users/dalrymplej/Dropbox",
             hh = "Health Home Dashboard/Health Home Analysis")

## user input
user_input <- list(
  analysis_mon = "Feb 2016",
  save_loc = "4_12_2016 Jan16"
)

# setup for working directories - data and results
work_dir <- list(
  data = file.path(base$dropbox, base$hh, "Data"),
  results = file.path(base$dropbox, base$hh,
                      "Results", user_input$save_loc),
  code = file.path(base$pce107, "R Code")
)
#   # setup for working directories - data and results
#   dataWD <- "Data"
#   resultsWD <- "C:/Users/dalrymplej/Dropbox/Mike's Projects/Health Home/Results/9_1_2015"
#   codeWD <- "R Code"

### source files ###
source(file.path(base$dropbox, "WCCMH/R/begin script R code.r"))
source(file.path(work_dir$code, "cc360_base auxillary.r"))

user_input$sql_end_dt <-
  format(as.Date(as.yearmon(user_input$analysis_mon),
                                   frac = 1), format = "%m/%d/%Y")

### date parameters - need to change to current month ###
end_date <- dateConvert(user_input$sql_end_dt)
qtr_dt <- dt_create(n=5, end_date = end_date, type = "qtr")
mon_dt <- dt_create(n=11, end_date = end_date, type = "mon")


qry_db <- "use Frank_data"

qry_U_tblYM <- "
IF OBJECT_ID('tempdb..#U_tblYM') IS NOT NULL
DROP TABLE #U_tblYM
select
ROW_NUMBER()
OVER (ORDER BY t_start) AS per_order,
t_start as per_start, t_end as per_end,
cast(month(t_start) as nvarchar(2)) + '/' +
SUBSTRING(cast(E2_funding_bucket.dbo.fn_FiscalYear(t_start) as nvarchar(4)), 3, 4) as period
into #U_tblYM
from E2_funding_bucket.dbo.fn_fy_months('10/1/2012', getdate())
-- select * from #U_tblYM"

qry_U_dx_codes <- "-- U.dx_codes
IF OBJECT_ID('tempdb..#U_dx_codes') IS NOT NULL
DROP TABLE #U_dx_codes
select distinct diagnosis_code, diag_short_desc
into #U_dx_codes
from Frank_data.dbo.Diagnosis_Reference
-- select * from #U_dx_codes"

qry_U_rawCC360 <- "-- U.rawCC360
IF OBJECT_ID('tempdb..#U_rawCC360') IS NOT NULL
DROP TABLE #U_rawCC360
select distinct *
into #U_rawCC360
from frank_data.dbo.care_connection_360"

qry_U_rawCC360_active <- "-- U.rawCC360_active ... this was as close as I could get to Jessica's version
IF OBJECT_ID('tempdb..#U_rawCC360_active') IS NOT NULL
DROP TABLE #U_rawCC360_active
select distinct
TCN, Line_TCN, Beneficiary_ID, Birth_Date, Consumer_Unique_ID,
Transaction_Type, Invoice_Type, Member_ID_Type, Service_From_Date,
Service_To_Date, Admission_Date, Discharge_Date, Patient_Status,
Procedure_Code, Procedure_Code_Qualifier, Revenue_Code, Place_of_Service,
Facility_Type_Code, Paid_Units, National_Drug_Code, Days_Supply, Specific_Therapeutic_Drug_Class,
Originator_Plan_ID, Related_Plan_ID, Billing_Provider_NPI, Billing_Provider_Taxonomy,
Billing_Provider_Name, Rendering_Provider_NPI, Rendering_Provider_Taxonomy, Rendering_Provider_Name,
Attending_Provider_NPI, Attending_Provider_Taxonomy, Attending_Provider_Name, Prescribing_Provider_NPI,
Prescribing_Provider_Name, Header_Diagnosis_Code1, Header_Diagnosis_Category1,
Header_Diagnosis_Code2, Header_Diagnosis_Category2, Header_Diagnosis_Code3, Header_Diagnosis_Category3,
Header_Diagnosis_Code4, Header_Diagnosis_Category4, Header_Diagnosis_Code5, Header_Diagnosis_Category5,
Header_Diagnosis_Code6, Header_Diagnosis_Category6, Header_Diagnosis_Code7, Header_Diagnosis_Category7,
Header_Diagnosis_Code8, Header_Diagnosis_Category8, Header_Admitting_Diagnosis, Header_Admitting_Diagnosis_Category,
Line_Diagnosis_Code1, Line_Diagnosis_Category1, Line_Diagnosis_Code2, Line_Diagnosis_Category2,
Line_Diagnosis_Code3, Line_Diagnosis_Category3, Line_Diagnosis_Code4, Line_Diagnosis_Category4,
DRG, Surgical_Procedure_Code1, Surgical_Procedure_Code2, Surgical_Procedure_Code3,
Procedure_Modifier1, Procedure_Modifier2, Procedure_Modifier3, Procedure_Modifier4,
Parent_TCN, Original_TCN, Claim_Submission_Reason_Code, Business_Status_Code, Adjudication_Date, Extract_Number,
ERN,
case when
max(Adjudication_Date) = Adjudication_Date and
Claim_Submission_Reason_Code !=  8 then 1
else 0 end as active_record /* These are what the instructions say to do */
into #U_rawCC360_active
from #U_rawCC360 as a
group by TCN, Line_TCN, Beneficiary_ID, Birth_Date, Consumer_Unique_ID,
Transaction_Type, Invoice_Type, Member_ID_Type, Service_From_Date,
Service_To_Date, Admission_Date, Discharge_Date, Patient_Status,
Procedure_Code, Procedure_Code_Qualifier, Revenue_Code, Place_of_Service,
Facility_Type_Code, Paid_Units, National_Drug_Code, Days_Supply, Specific_Therapeutic_Drug_Class,
Originator_Plan_ID, Related_Plan_ID, Billing_Provider_NPI, Billing_Provider_Taxonomy,
Billing_Provider_Name, Rendering_Provider_NPI, Rendering_Provider_Taxonomy, Rendering_Provider_Name,
Attending_Provider_NPI, Attending_Provider_Taxonomy, Attending_Provider_Name, Prescribing_Provider_NPI,
Prescribing_Provider_Name, Header_Diagnosis_Code1, Header_Diagnosis_Category1,
Header_Diagnosis_Code2, Header_Diagnosis_Category2, Header_Diagnosis_Code3, Header_Diagnosis_Category3,
Header_Diagnosis_Code4, Header_Diagnosis_Category4, Header_Diagnosis_Code5, Header_Diagnosis_Category5,
Header_Diagnosis_Code6, Header_Diagnosis_Category6, Header_Diagnosis_Code7, Header_Diagnosis_Category7,
Header_Diagnosis_Code8, Header_Diagnosis_Category8, Header_Admitting_Diagnosis, Header_Admitting_Diagnosis_Category,
Line_Diagnosis_Code1, Line_Diagnosis_Category1, Line_Diagnosis_Code2, Line_Diagnosis_Category2,
Line_Diagnosis_Code3, Line_Diagnosis_Category3, Line_Diagnosis_Code4, Line_Diagnosis_Category4,
DRG, Surgical_Procedure_Code1, Surgical_Procedure_Code2, Surgical_Procedure_Code3,
Procedure_Modifier1, Procedure_Modifier2, Procedure_Modifier3, Procedure_Modifier4,
Parent_TCN, Original_TCN, Claim_Submission_Reason_Code, Business_Status_Code, Adjudication_Date, Extract_Number,
ERN
-- having active_record = 1
-- order by Original_TCN, Adjudication_Date, TCN
-- select * from #U_rawCC360_active"

qry_U_cc360_firstdate <- "-- U.cc360_firstdate ... first and last date in cc360 for consumers
IF OBJECT_ID('tempdb..#U_cc360_firstdate') IS NOT NULL
DROP TABLE #U_cc360_firstdate
select distinct
beneficiary_id,
min(service_from_date) as first_cc360_date,
max(service_from_date) as last_cc360_date
into #U_cc360_firstdate
from #U_rawCC360_active
group by Beneficiary_ID
-- select * from #U_cc360_firstdate"

qry_U_hosp <- "-- U.hosp ... our own auths for pysch inpatient admissions
IF OBJECT_ID('tempdb..#U_hosp') IS NOT NULL
DROP TABLE #U_hosp
select distinct
county, case_no, hosp, auth_eff, auth_exp, hosp_disc, medicaid,
team_at_admit, team_at_disc, current_team, contract_paneltype,
case when auth_exp < getdate()-90 and hosp_disc is NULL
then 1 else 0 end as potential_remove
into #U_hosp
from encompass.dbo.tblE2_Hosp
-- select * from #U_hosp"

#*****************
# Analysis Data Sets
#*****************
#*** finding CMH open people ***/

qry_local_CMHteams <- "-- local_CMHteams ... all CMH teams OVER ALL TIME, does not include PORT or OBRA
IF OBJECT_ID('tempdb..#local_CMHteams') IS NOT NULL
DROP TABLE #local_CMHteams
select distinct
county, case_no, maidno, dob, provider, provider_eff, provider_exp, adm_effdt, adm_expdt,
primary_provide_or_not,
max(case when provider like '%PATH%' and primary_provide_or_not='Y' then 1 else 0 end) as delete_primpath,
case when provider like '%PATH%' then 1 else 0 end as delete_path
into #local_CMHteams
from encompass.dbo.tblE2_Adm_Consumers
where providertype='Direct Provider' and
provider not like '%OBRA%' and
provider not like '%SBIRT%' and
provider not like '%Genoa%' and
provider not like '%Sobriety Court%' and
provider_eff != provider_exp
group by county, case_no, maidno, dob, provider, provider_eff, provider_exp, adm_effdt, adm_expdt,
primary_provide_or_not"

qry_test1_affiliate <- "-- all consumers ever open to us (affiliate) by region
select distinct county, count(distinct case_no) as 'CMH Open All Time'
from #local_CMHteams
group by county"

qry_local_CMHopen_withteam <- "-- local.CMHopen_withteams ... consumers open to CMH since start_date
IF OBJECT_ID('tempdb..#local_CMHopen_w_team_a') IS NOT NULL
DROP TABLE #local_CMHopen_w_team_a
select distinct
a.case_no, a.maidno, b.first_cc360_date, b.last_cc360_date, a.county, a.provider,
a.adm_effdt as CMH_effdt, a.adm_expdt as CMH_expdt, -- misleading, adm_blah is about E2open/closed
a.provider_eff as team_effdt,
a.provider_exp as team_expdt,
case when a.primary_provide_or_not = 'Y' then 1 else 0 end as prim,
case when a.adm_expdt is NULL then 1 else 0 end as open_CMH
into #local_CMHopen_w_team_a
from #local_CMHteams as a
left join #U_cc360_firstdate as b on cast(a.maidno as int)  = cast(b.beneficiary_id as int)
where delete_path=0 and -- remove path teams
(a.provider_exp is null or a.provider_exp > '10/1/2012') -- replace with @start_date later
-- select * from #local_CMHopen_w_team_a where first_cc360_date is null"

qry_local_CMHopen_w_team_b <- "IF OBJECT_ID('tempdb..#local_CMHopen_w_team_b') IS NOT NULL
DROP TABLE #local_CMHopen_w_team_b --9952 rows
select distinct
a.case_no, a.maidno, a.first_cc360_date, a.last_cc360_date, a.county, a.provider,
a.CMH_effdt, a.CMH_expdt, a.team_effdt, a.team_expdt, a.prim, a.open_CMH
into #local_CMHopen_w_team_b
from #local_CMHopen_w_team_a as a
-- if at least 1 is primary, pulls that. otherwise pulls all for given admision
left join #local_CMHopen_w_team_a as a2 on
a.case_no=a2.case_no and a2.prim > a.prim
where a2.prim is null"

qry_test2_local_CMHopen_withteams <- "-- this is just a data check for local.CMHopen_withteams
select distinct
county, count(distinct case_no) as 'CMH open after FY13 and after',
min(team_expdt) as 'min team expdt' -- should be ~10/1/12
from #local_CMHopen_w_team_b
group by county"

qry_U_cc360_indata <- "-- U.cc360_indata ... tells what period we should expect people to be in data for each consumer found in cc360
IF OBJECT_ID('tempdb..#U_cc360_indata') IS NOT NULL
DROP TABLE #U_cc360_indata
select distinct
a.beneficiary_id, a.first_cc360_date, a.last_cc360_date,
b.per_start, b.per_end, b.per_order, b.period,
case when a.first_cc360_date <= b.per_end and a.last_cc360_date >= b.per_start
then 1 else 0 end as indata
into #U_cc360_indata
from #U_tblYM as b, #U_cc360_firstdate as a -- notice no condition, cartesian product"

qry_ADM0 <- "-- #ADM0 ... not sure if this approach is optimal ... need to revisit later
IF OBJECT_ID('tempdb..#ADM0') IS NOT NULL
DROP TABLE #ADM0
select distinct
case_no, maidno, first_cc360_date, last_cc360_date, county,
provider as team, CMH_effdt, CMH_expdt, team_effdt, team_expdt,
case when open_CMH=1 then 2
when CMH_expdt=max(CMH_expdt) then 1
else 0  end as priority
into #ADM0
from #local_CMHopen_w_team_b
group by case_no, maidno, first_cc360_date, last_cc360_date, county,
provider, CMH_effdt, team_effdt, team_expdt, prim, CMH_expdt, open_CMH
having prim=max(prim) -- if 1+ is prim, catches that; else pulls all teams for that adm"

qry_ADM1 <- "-- ADM1 ... first picks open admission. If none, picks latest discharge.
IF OBJECT_ID('tempdb..#ADM1a') IS NOT NULL
DROP TABLE #ADM1a
select distinct
adm.case_no, adm.maidno, adm.first_cc360_date, adm.last_cc360_date,
adm.county, adm.team, adm.CMH_effdt, adm.CMH_expdt, adm.team_effdt,
adm.team_expdt, adm.priority
into #ADM1a
from #ADM0 as adm
left join #ADM0 as max_adm on
adm.case_no = max_adm.case_no and
max_adm.priority > adm.priority
where max_adm.priority is null"

qry_ADM1b <- "IF OBJECT_ID('tempdb..#ADM1b') IS NOT NULL
DROP TABLE #ADM1b
select a.case_no, a.maidno, a.first_cc360_date, a.last_cc360_date, a.county,
a.team, a.CMH_effdt, a.CMH_expdt, a.team_effdt, a.team_expdt, a.priority
into #ADM1b
from #ADM1a as a
left join #ADM1a as a2 on
a2.case_no = a.case_no and
a2.team_expdt > a.team_expdt
where a2.team_expdt is null"

qry_local_CMH_adm <- "-- for teams with the same most recent team_expdt, pick most recent team_effdt
IF OBJECT_ID('tempdb..#local_CMH_adm') IS NOT NULL
DROP TABLE #local_CMH_adm
select
b1.case_no, b1.maidno, b1.first_cc360_date, b1.last_cc360_date,
b1.county, b1.team, b1.CMH_effdt, b1.CMH_expdt, b1.team_effdt, b1.team_expdt,
b1.priority
into #local_CMH_adm
from #ADM1b as b1
left join #ADM1b as b2 on
b1.case_no = b2.case_no and
b2.team_effdt < b1.team_effdt
where b2.team_effdt is null"

#*** local.CMHopen_indata
# What periods the consumers since datastart are potentially found in cc360
# ***

qry_local_CMHopen_indata <- "IF OBJECT_ID('tempdb..#local_CMHopen_indata') IS NOT NULL
DROP TABLE #local_CMHopen_indata
select distinct
case_no, county, maidno, cmh_effdt, cmh_expdt, first_cc360_date,
last_cc360_date, per_start, per_end, per_order, period,
case when first_cc360_date is null then 0
when first_cc360_date <= per_end and last_cc360_date >= per_start then 1
else 0 end as incc360
into #local_CMHopen_indata
from #U_tblYM as b, #local_CMH_adm as a
/*where cmh_effdt<=per_end and
(cmh_expdt is null or cmh_expdt>=per_start)*/"

qry_test3 <- "-- checking data again
select
county, count(distinct case_no) as 'CMH open FY13 and after'
from #local_CMHopen_indata
group by county"

qry_test4 <- "-- shouldnt print if everyone is still in data set. :)
select distinct *
from #local_CMH_adm
where case_no not in (select case_no from #ADM1b)"

qry_test5 <- "-- shouldnt print if dup free
select distinct *
from #local_CMH_adm
group by
case_no, maidno, first_cc360_date, last_cc360_date, county,
team, CMH_effdt, CMH_expdt, team_effdt, team_expdt, priority
having count(*)>1"

qry_test6 <- "-- shouldnt print if dup free? ask Jessica
select
case_no, team_effdt, -- team,
count(*) as dups
from #local_CMH_adm
group by
case_no, team_effdt, team -- there are dups if team is not included
having count(*)>1"

qry_test7 <- "-- testing again
select county, count(distinct case_no) as
'CMHopen since datastart w unique team - will be smaller'
from #local_CMH_adm
group by county"

qry_local_pool <- "-- local_pool
IF OBJECT_ID('tempdb..#local_pool') IS NOT NULL
DROP TABLE #local_pool
select distinct
case_no, county, maidno, first_cc360_date, last_cc360_date
into #local_pool
from #local_CMH_adm"

qry_test8 <- "-- duplicate checking again
select distinct *
from #local_CMH_adm
group by
case_no, maidno, first_cc360_date, last_cc360_date, county, team,
CMH_effdt, CMH_expdt, team_effdt, team_expdt, priority
having count(*)>1"

qry_test9 <- "-- final pool of consumers
select county, count(distinct case_no) as 'final pool'
from #local_pool
group by county"

#****** CC360 data ******
#*** local_cc360_pool ... (43min-2hrs?) restricts just to services
# for those in overall pool... potential improvement code area
# ***/

qry_local_CC360_pool <- "IF OBJECT_ID('tempdb..#local_CC360_pool') IS NOT NULL
DROP TABLE #local_CC360_pool
select distinct
a.case_no, a.maidno, a.county, origplan.plan_name as originator_plan_name,
relplan.plan_name as related_plan_name,
b.*
into #local_CC360_pool
from #local_pool as a
left join #U_rawCC360_active as b on cast(a.maidno as int) = cast(b.Beneficiary_ID as int)
left join (select distinct * from frank_data.dbo.Plan_Reference where Plan_Subtype='MHP') as
ORIGPLAN on b.Originator_Plan_ID=ORIGPLAN.Plan_ID
left join (select distinct * from frank_data.dbo.Plan_Reference where Plan_Subtype='MHP')  as
RELPLAN on b.related_Plan_ID=RELPLAN.Plan_ID
-- select * from #local_CC360_pool"

qry_local_CC360_poolERIP <- "IF OBJECT_ID('tempdb..#local_CC360_poolERIP') IS NOT NULL
DROP TABLE #local_CC360_poolERIP /*restricts just to potential ER and IP services for those in overall pool */
select distinct *
--case_no, maidno, county, * -- not sure what the point of this line is for SAS-SQL
into #local_CC360_poolERIP
from #local_CC360_pool
where procedure_code in ('99281', '99282', '99283', '99284', '99285','99221', '99222', '99223', '99231', '99232', '99233', '99234', '99235', '99236','99238','99239')
or revenue_code in ('0100','0101','0110','0111','0112','0113','0114','0119','0120','0121','0122','0123','0124','0129','0130','0131','0132','0133','0134','0139','0140',
  '0141','0142','0143','0144','0149','0150','0151','0152','0153','0154','0159','0160','0164','0167','0169','0200','0201','0202','0203','0204','0206','0207','0208',
  '0209', '0210','0211','0212','0213','0214','0219','0720','0721','0722','0723','0724','0729','0987')
or (revenue_code >= 450 and revenue_code <= 459)
or revenue_code = 981
-- select * from #local_CC360_poolERIP"

qry_local_CC360_poolERIP2 <- "IF OBJECT_ID('tempdb..#local_CC360_poolERIP2') IS NOT NULL
DROP TABLE #local_CC360_poolERIP2
select distinct /* determines which services are IP and ER */
a.case_no, a.maidno, county, Procedure_Code, CPT.PRCDR_SHORT_DESC,
Revenue_Code, REV.REV_SHORT_DESC, Place_of_Service, POS.data_value_description as POS_desc,
Service_From_Date, Service_To_Date, Admission_Date, Discharge_Date,
case when admission_date is null and service_from_date is not null then service_from_date
when admission_date is not null and service_from_date is null then admission_date
when admission_date <= service_from_date then admission_date
when service_from_date < admission_date then service_from_date
else NULL end as from_date,
case when discharge_date is null and service_from_date is not null then service_to_date
when discharge_date is not null and service_to_date is null then discharge_date
when discharge_date >= service_to_date then discharge_date
when service_to_date > discharge_date then service_to_date
else NULL end as through_date,
--,min(admission_date, service_from_date) as from_date -- cant do this in T-SQL
--,max(discharge_date, service_to_date) as through_date -- cant do this in T-SQL
case
when a.Related_Plan_ID  in ('2813566', '4390138') then 1
when a.Billing_provider_name = 'COUNTY OF WASHTENAW' and Rendering_Provider_Name = 'PENROSE, ALICE' then 0
when a.Billing_provider_name in ('COUNTY OF WASHTENAW'
, 'LENAWEE COMMUNITY MENTAL HEALTH AUTHORITY'
, 'MONROE COMMUNITY MENTAL HEALTH AUTHORITY'
, 'LIVINGSTON COUNTY COMMUNITY MENTAL HEALTH AUTHORITY') then 1
else 0
end as CMH_paid
,case /*** http://thehappyhospitalist.blogspot.com/2013/06/CPT-Admission-Codes-Flow-Tree-Diagram-Help.html ***/
when a.procedure_code in ('99221', '99222', '99223', '99231', '99232', '99233', '99234', '99235', '99236','99238','99239') then 1
when substring(a.revenue_code, 1, 3) in ('011','012','013','014','015','016','017') then 1 /*these are revenue codes for room and board*/
/*when POS.data_value_description like  '%Inpatient%' then 1*/
else 0
end as IP
,case
when a.procedure_code in ('99221', '99222', '99223', '99231', '99232', '99233', '99234', '99235', '99236','99238','99239')
or substring(a.revenue_code, 1, 3) in ('011','012','013','014','015','016','017') then 0
when procedure_code in ('99281', '99282', '99283', '99284', '99285') then 1
when (cast(a.revenue_code as int) >= 450 and cast(a.revenue_code as int) <= 459) or cast(a.revenue_code as int) = 981 then 1
/*when POS.data_value_description like '%Emergency%' then 1 */
else 0
end as ER,
Birth_Date,  Procedure_Code_Qualifier, Line_TCN,	TCN, Original_TCN, Parent_TCN, Adjudication_Date,
max(Adjudication_Date) as maxadj, Claim_Submission_Reason_Code, Beneficiary_ID, a.Billing_provider_name,
a.Related_Plan_ID, a.Rendering_Provider_Name, a.Header_Diagnosis_Code1, a.Header_Diagnosis_Code2,
a.Header_Diagnosis_Code3, a.originator_plan_id, a.Originator_Plan_Name
-- ,a.*
into #local_CC360_poolERIP2
from #local_CC360_poolERIP as a
left join (select distinct data_value, data_value_description from frank_data.dbo.code_values where field_name = 'Place of Service') as POS on a.Place_of_Service=POS.data_value
left join frank_data.dbo.procedure_reference as CPT on a.procedure_code=CPT.PRCDR_CODE
left join frank_data.dbo.revenue_code_reference as REV on a.revenue_code !='' and a.revenue_code = REV.rev_CODE
group by a.case_no, a.maidno, county, Procedure_Code, CPT.PRCDR_SHORT_DESC,
a.Revenue_Code, REV.REV_SHORT_DESC, a.Place_of_Service, POS.data_value_description,
a.Service_From_Date, a.Service_To_Date, a.Admission_Date, a.Discharge_Date, a.Birth_Date,
a.Procedure_Code_Qualifier, a.Line_TCN, a.TCN, a.Original_TCN, a.Parent_TCN, a.Adjudication_Date,
a.Claim_Submission_Reason_Code, a.Beneficiary_ID, a.Billing_provider_name, a.Related_Plan_ID,
a.Rendering_Provider_Name, a.Header_Diagnosis_Code1, a.Header_Diagnosis_Code2, a.Header_Diagnosis_Code3,
a.originator_plan_id, a.Originator_Plan_Name
-- select distinct * from #local_CC360_poolERIP2"

qry_local_CC360_poolERIP3 <- "IF OBJECT_ID('tempdb..#local_CC360_poolERIP3') IS NOT NULL
DROP TABLE #local_CC360_poolERIP3
select distinct
a.*,
dx.DIAG_SHORT_DESC as diag1_desc, dx2.DIAG_SHORT_DESC as diag2_desc,
dx3.DIAG_SHORT_DESC as diag3_desc
into #local_CC360_poolERIP3
from #local_CC360_poolERIP2  as a
left join #U_dx_codes as DX on a.Header_Diagnosis_Code1=DX.DIAGNOSIS_CODE
left join #U_dx_codes as DX2 on a.Header_Diagnosis_Code2=DX2.DIAGNOSIS_CODE
left join #U_dx_codes as DX3 on a.Header_Diagnosis_Code3=DX3.DIAGNOSIS_CODE
-- select * from #local_CC360_poolERIP3"

qry_local_pre_IP <- "IF OBJECT_ID('tempdb..#local_pre_IP') IS NOT NULL
DROP TABLE #local_pre_IP
select
'auths' as source, hosp.case_no, 1 as RB_sort, 'BH' as RB_type, '' as diag1_desc,
hosp.auth_eff as from_date, hosp.auth_exp as through_date, '' as REV_SHORT_DESC,
'' as revenue_code, 1 as CMH_paid, 1 as IP, 0 as ER, '' as originator_plan_id,
'' as Originator_Plan_Name
into #local_pre_IP
from #U_hosp as hosp where hosp.potential_remove=0
union
select distinct
'cc360' as source, case_no, case
when substring(revenue_code, 1, 3) in ('011','012','013','014','015','016','017')  then 1
when revenue_code is null then 2
end  as RB_sort,
case
when substring(revenue_code, 1, 3) in ('011','012','013','014','015','016','017') and REV_SHORT_DESC like '%Psychiatric%' then 'BH'
when substring(revenue_code, 1, 3) in ('011','012','013','014','015','016','017') and REV_SHORT_DESC not like '%Psychiatric%' then 'PH'
end as RB_type,
diag1_desc, from_date, through_date, REV_SHORT_DESC, revenue_code, CMH_paid,
IP, ER, originator_plan_id, Originator_Plan_Name
from #local_CC360_poolERIP3
where IP = 1
-- select * from #local_pre_IP"

qry_local_IP <- "IF OBJECT_ID('tempdb..#local_IP') IS NOT NULL
DROP TABLE #local_IP

select distinct
ROW_NUMBER()
OVER (ORDER BY case_no) as rownum, ip.*
into #local_IP
from #local_pre_IP as ip"

qry_local_pre_ER <- "IF OBJECT_ID('tempdb..#local_pre_ER') IS NOT NULL
DROP TABLE #local_pre_ER

select distinct
'cc360' as source, case_no, NULL as RB_sort,
'' as  RB_type, diag1_desc, from_date, through_date,
REV_SHORT_DESC, revenue_code, CMH_paid, IP, ER,
originator_plan_id, Originator_Plan_Name
into #local_pre_ER
from #local_CC360_poolERIP3
where ER = 1
-- select * from #local_pre_ER"

qry_local_ER <- "IF OBJECT_ID('tempdb..#local_ER') IS NOT NULL
DROP TABLE #local_ER

select distinct
ROW_NUMBER()
OVER (ORDER BY case_no) as rownum, er.*
into #local_ER
from #local_pre_ER as er
-- select * from #local_ER"

local_ER <- "select * from #local_ER"
local_IP <- "select * from #local_IP"

# qry1 <- "select * from encompass.dbo.tblE2_hosp"
# sqlQuery(ch, qry1)

start_time1 <- Sys.time()
queryList <- list(qry_db, qry_U_tblYM, qry_U_dx_codes, qry_U_rawCC360, qry_U_rawCC360_active,
                  qry_U_cc360_firstdate, qry_U_hosp, qry_local_CMHteams, qry_test1_affiliate, qry_local_CMHopen_withteam,
                  qry_local_CMHopen_w_team_b, qry_test2_local_CMHopen_withteams, qry_U_cc360_indata, qry_ADM0, qry_ADM1,
                  qry_ADM1b, qry_local_CMH_adm, qry_local_CMHopen_indata, qry_test3, qry_test4, qry_test5, qry_test6, qry_test7,
                  qry_local_pool, qry_test8, qry_test9, qry_local_CC360_pool, qry_local_CC360_poolERIP, qry_local_CC360_poolERIP2,
                  qry_local_CC360_poolERIP3, qry_local_pre_IP, qry_local_IP, qry_local_pre_ER, qry_local_ER, local_ER, local_IP)
sqlOutput <- lapply(queryList, function(x) sqlQuery(ch, x))
end_time1 <- Sys.time()
end_time1 - start_time1

#### fix IPs ####
  dt_IP <- data.table(sqlQuery(ch, local_IP))
  dt_IP <- dt_IP[order(case_no, from_date)]
  dt_IP[, Originator_Plan_Name := as.character(Originator_Plan_Name)]
  dt_IP[, diag1_desc := as.character(diag1_desc)]
  dt_IP[is.na(RB_type), RB_type := "PH"]
  dt_IP[, RB_type := factor(x=RB_type, levels = c("BH", "PH"), ordered=TRUE)]
  dt_IP[, source := as.character(source)]
  dt_IP[, c("REV_SHORT_DESC") := NULL]
  dt_IP[, from_date := as.Date(from_date)]
  dt_IP[, through_date := as.Date(through_date)]
  dt_IP[, diag1_desc := gsub(x=diag1_desc, pattern='\\"', replace="")]

### orginator_plan_id is not carried over b/c there doesnt seem to be a point and it poses a merge problem (not unique where it needs to be) ###
fixed_IP <- data.table(case_no = integer(), primdx=character(), from_date = as.Date(as.character()),
                       through_date = as.Date(as.character()), CMH_paid = integer(), 
                       # originator_plan_id = integer(),
                       # Originator_Plan_Name = character(),
                       RB_type = factor(levels = c("BH", "PH"), ordered=TRUE))

for_start <- Sys.time()
for( i in 1:nrow(dt_IP)) {
  tmp_row <- dt_IP[i, list(case_no = case_no, primdx = diag1_desc, from_date = from_date, through_date = through_date,
                           CMH_paid = CMH_paid,
                           RB_type = RB_type)]
  if( nrow(
      fixed_IP[case_no==tmp_row[, case_no] &
               between(tmp_row[, from_date], from_date-1, through_date+1) |
               between(tmp_row[, from_date], from_date-1, through_date+1)
              ]
      ) > 0
  ) {
    fixed_IP <- rbindlist(list(fixed_IP, tmp_row))
    fixed_IP[case_no==tmp_row[, case_no] &
            (between(tmp_row[, from_date], from_date-1, through_date+1) |
            between(tmp_row[, through_date], from_date-1, through_date+1)),
            c("primdx", "from_date", "through_date", "CMH_paid", "RB_type") :=
            list(reduce_dup(primdx), min(from_date), max(through_date), max(CMH_paid), min(RB_type) )
            ]
    fixed_IP <- unique(fixed_IP)
    # fixed_IP
  } else {
    fixed_IP <- rbindlist(list(fixed_IP, tmp_row))
  }
# print(paste0(i, " of ", nrow(dt_IP), ", ", round(i/nrow(dt_IP)*100, 2), "%")); flush.console()
}
for_end <- Sys.time()
for_end-for_start

#### fix ERs ####
dt_ER <- data.table(sqlQuery(ch, local_ER))
dt_ER <- dt_ER[order(case_no, from_date)]
dt_ER[, Originator_Plan_Name := as.character(Originator_Plan_Name)]
dt_ER[, diag1_desc := as.character(diag1_desc)]
dt_ER[, c("REV_SHORT_DESC") := NULL]
dt_ER[, RB_type := as.character(RB_type)]
dt_ER[is.na(RB_type), RB_type := "PH"]
dt_ER[, RB_type := factor(x=RB_type, levels = c("BH", "PH"), ordered=TRUE)]
dt_ER[, source := as.character(source)]
dt_ER[, from_date := as.Date(from_date)]
dt_ER[, through_date := as.Date(through_date)]
dt_ER[, diag1_desc := gsub(x=diag1_desc, pattern='\\"', replace="")]

# orginator_plan_id is not carried over b/c there doesnt seem to be a point and it poses a merge problem (not unique where it needs to be)
# ER visits should not connect to an er visit on the next day. ER visits = 1 per day is our assumption.
fixed_ER <- data.table(case_no = integer(), primdx=character(), from_date = as.Date(as.character()),
                       through_date = as.Date(as.character()), CMH_paid = integer(),
                       RB_type = factor(levels = c("BH", "PH"), ordered=TRUE))

for_start <- Sys.time()
for( i in 1:nrow(dt_ER)) {
  tmp_row <- dt_ER[i, list(case_no = case_no, primdx = diag1_desc, 
                           from_date = from_date, through_date = through_date,
                           CMH_paid = CMH_paid,
                           RB_type = RB_type)]
  if( nrow(
    fixed_ER[case_no==tmp_row[, case_no] &
               between(tmp_row[, from_date], from_date, through_date) |
               between(tmp_row[, from_date], from_date, through_date)
            ]
  ) > 0
  ) {
    fixed_ER <- rbindlist(list(fixed_ER, tmp_row))
    fixed_ER[case_no==tmp_row[, case_no] &
               (between(tmp_row[, from_date], from_date, through_date) |
                  between(tmp_row[, through_date], from_date, through_date)),
             c("primdx", "from_date", "through_date", "CMH_paid", "RB_type") :=
               list(reduce_dup(primdx), min(from_date), max(through_date), max(CMH_paid), min(RB_type) )
             ]
    fixed_ER <- unique(fixed_ER)
  } else {
    fixed_ER <- rbindlist(list(fixed_ER, tmp_row))
  }
  # print(paste0(i, " of ", nrow(dt_ER), ", ", round(i/nrow(dt_ER)*100, 2), "%")); flush.console()
}
for_end <- Sys.time()
for_end-for_start

### hh_dash ###
sql_hh_dash <- paste0("exec encompass.dbo.E2_WSH_CMH_Open_Health_Home_Dashboard 'Washtenaw', '", user_input$sql_end_dt,"'")
hh_dash <- sqlQuery(ch, sql_hh_dash)
hh_dash <- data.table(hh_dash)
hh_dash[, HH_coregroup := as.character(HH_coregroup)]
setnames(hh_dash, c("Case_No", "last_PHR_ID"), c("case_no", "phr_id"))
hh_dash[, HH_effdt := as.Date(HH_effdt)]

### adding hh_dash data to fixed_ER ###
fixed_ER <- merge(fixed_ER, hh_dash[, c("case_no", "HH_effdt", "HH_coregroup"), with=FALSE], all.x=TRUE, by="case_no")
fixed_ER[HH_effdt > from_date, enroll := "pre-HH team"]
fixed_ER[HH_effdt <= from_date, enroll := "post-HH team"]

### adding in_cc360 to fixed_ER and, later, fixed_IP ###
  sql_in_cc360 <- "select distinct case_no, incc360 from #local_CMHopen_indata where county = 'Washtenaw'"
    # "select distinct * from #local_CMHopen_indata where county = 'Washtenaw'"
  in_cc360 <- sqlQuery(ch, sql_in_cc360)
  in_cc360 <- data.table(in_cc360)
  in_cc360 <- in_cc360[, list(incc360 = sum(incc360)), by=list(case_no)]
  # include consumer if they showed up only in ER or only in IP, even once
  setkey(in_cc360, case_no)[J(c(fixed_ER[, unique(case_no)], fixed_IP[, unique(case_no)])), incc360 := 1]

### find all consumers with cc360 but not in ER (next step is to also make sure they have a HH admission) ###
  may_include <- in_cc360[incc360==1, unique(case_no)]
  may_include2 <- hh_dash[case_no %in% may_include, c("case_no", "HH_effdt", "HH_expdt", "HH_coregroup"), with=FALSE]
  may_include3 <- may_include2[!is.na(HH_effdt)]

qtr_er <- qtr_ip_ph <- data.table(case_no = integer())
### quarterly IP-BHs ###
for(i in 1:nrow(qtr_dt)) {
  tmp_col_ip <- paste("IPs", qtr_dt[i, format(start_dates, "%m_%y")], "to", qtr_dt[i, format(end_dates, "%m_%y")], sep="_")
  tmp_col_er <- paste("ERs", qtr_dt[i, format(start_dates, "%m_%y")], "to",  qtr_dt[i, format(end_dates, "%m_%y")], sep="_")
  tmp_ip_result <- fixed_IP[between(x=from_date, qtr_dt[i, start_dates], qtr_dt[i, end_dates]) & RB_type=="PH",
           list(tmp_col_ip = length(from_date)), by=list(case_no)]
  tmp_er_result <- fixed_ER[between(x=from_date, qtr_dt[i, start_dates], qtr_dt[i, end_dates]),
                         list(tmp_col_er = length(from_date)), by=list(case_no)]
  setnames(tmp_ip_result, old="tmp_col_ip", new=tmp_col_ip)
  setnames(tmp_er_result, old="tmp_col_er", new=tmp_col_er)
  qtr_ip_ph <- merge(qtr_ip_ph, tmp_ip_result, by="case_no", all=TRUE)
  qtr_er <- merge(qtr_er, tmp_er_result, by="case_no", all=TRUE)
}

mon_er <- data.table(case_no = integer(), enroll = character(), num_er = numeric(), time = character(), HH_coregroup = character())
mon_ip_ph <- data.table(case_no = integer())

### monthly IP-BHs ###
for(i in 1:nrow(mon_dt)) {
  tmp_col_ip <- mon_dt[i, format(start_dates, "%m/%y")]
  tmp_col_er <- mon_dt[i, format(start_dates, "%m/%y")]
  tmp_ip_result <- fixed_IP[between(x=from_date, mon_dt[i, start_dates], mon_dt[i, end_dates]) & RB_type=="PH",
                            list(tmp_col_ip = length(from_date)), by=list(case_no)]

  ### going to look a year before HH admission for ER visits and as many months are available for each
  ### consumer after their respective HH admission.
  ### fixed_ER[1:3,]
  tmp_er_result <- fixed_ER[(mon_dt[i, end_dates] >= HH_effdt) & between(x=from_date, mon_dt[i, end_dates-365], mon_dt[i, end_dates]),
                            list(num_er = length(from_date), time = tmp_col_er), by=list(case_no, enroll, HH_coregroup)]
  # add people that were active in CC360, HH admission at the first day in the current month, and didnt have any ER visits
  tmp_include <- copy(may_include3)
  tmp_include <- tmp_include[HH_effdt <= mon_dt[i, end_dates]]
  tmp_include <- tmp_include[!(case_no %in% tmp_er_result[, unique(case_no)]) ]
  tmp_include[, num_er := 0]
  tmp_include[, time := tmp_col_er]
  tmp_include[(mon_dt[i, start_dates] >= HH_effdt), enroll := "post-HH team"]
  tmp_include[(mon_dt[i, start_dates] < HH_effdt), enroll := "pre-HH team"]
  tmp_include[, c("HH_effdt", "HH_expdt") := NULL] ### may want to get rid of this line later

  if( nrow(tmp_include) > 0 ) {
    tmp_er_result <- rbindlist(list(tmp_er_result, tmp_include), use.names=TRUE)
  }
    rm(tmp_include)
    # rename
    setnames(tmp_ip_result, old="tmp_col_ip", new=tmp_col_ip)
    # combine results
    mon_ip_ph <- merge(mon_ip_ph, tmp_ip_result, by="case_no", all=TRUE)
    tmp_er_result[, num_er := num_er/nrow(mon_dt)]
    mon_er <- rbindlist(list(mon_er, tmp_er_result), use.names=TRUE)
  }


ip_180_case <- fixed_IP[between(from_date, end_date-180, end_date), unique(case_no)]
er_180_case <- fixed_ER[between(from_date, end_date-180, end_date), unique(case_no)]

### consumers that are pool of CMH open datasets. HH consumers are a subset of this dataset ###
  qry_openCMH <- "select * from #local_CMHopen_indata"
  openCMH_in_cc360 <- sqlQuery(ch, qry_openCMH)
  openCMH_in_cc360 <- data.table(openCMH_in_cc360)

  setkey(openCMH_in_cc360)[J(ip_180_case), ip_180 := 1]
  setkey(openCMH_in_cc360)[J(er_180_case), er_180 := 1]

### phr conditions part 1 (except for hosp/ER data), part 2 has wellness etc ###
  sql_phr1 <- "select * from encompass_extra.dbo.jd_fn_phr_id4()"
  phr_cond1 <- sqlQuery(ch, sql_phr1)
  phr_cond1 <- data.table(phr_cond1)

  # fix factor columns to character columns
  reason_cols <- grep(x=colnames(phr_cond1), pattern="reason", value=TRUE)
  for (j in reason_cols) set(phr_cond1, j=j, value = as.character(phr_cond1[[j]]))
  rm(reason_cols)

### phr conditipms part 2
  sql_phr2 <- "select * from encompass_extra.dbo.jd_fn_well_phr_a()"
  phr_cond2 <- sqlQuery(ch, sql_phr2)
  phr_cond2 <- data.table(phr_cond2)
  phr_cond2 <- phr_cond2[!is.na(case_no)]
  phr_cond2[, max_dt_health := max(date_for_health_rating), by=case_no]
  phr_cond2 <- phr_cond2[date_for_health_rating==max_dt_health]
  phr_cond2[, max_dt_health := NULL]
  # different information on Wellness note and PHR
  phr_cond2[, health_rating := as.numeric(health_rating)]
  phr_cond2[, health_rating := hh_min(health_rating), by=case_no]
  phr_cond2[, poorfair := as.numeric(poorfair)]
  phr_cond2[, poorfair := hh_min(poorfair), by=case_no]
  phr_cond2[, level_of_pain := as.numeric(level_of_pain)]
  phr_cond2[, level_of_pain := hh_min(level_of_pain), by=case_no]
  # remove duplicates
  phr_cond2 <- unique(phr_cond2)

### combine phr_cond with er data
  phr_cond <- merge(phr_cond1, hh_dash[, c("case_no", "phr_id"), with=FALSE], by = "phr_id")
  phr_cond <- merge(phr_cond,
        openCMH_in_cc360[, c("case_no", "incc360", "ip_180", "er_180"), with=FALSE ],
        all.x=TRUE, by="case_no")
  phr_cond[ip_180 > 0 | er_180 > 0, reason1 := "Recent_ER/Hospitalization"]
  phr_cond[is.na(reason1), reason1 := ""]
  phr_cond <- merge(phr_cond, phr_cond2, by="case_no", all.x=TRUE)
  phr_cond[poorfair == 1, reason15 := "Poor/Fair_Health"]
  phr_cond[is.na(reason15), reason15 := ""]
  invisible(setkey(phr_cond, NULL))
  phr_cond <- unique(phr_cond)

  # phr_cond[, reason := NULL]
  phr_cond[, index := .I]
  phr_cond[(ip_180==1 | er_180==1 ) & haschronic>=1,
    reason := my_paste(reason1, reason2, reason3, reason4, reason5, reason6, reason7, reason8, sep="|"), by=index]
  phr_cond[is.na(reason) & (ip_180==0 & er_180==0) & poorfair==1 & smokerisk==1, reason := my_paste(reason14, reason15), by=index]

  phr_cond[is.na(reason) &
           (ip_180==1 | er_180==1 | haschronic==1 | hep==1 | htn==1 | chol==1 | poorfair==1 | riskchronic==1),
           reason := my_paste(reason1, reason2, reason3, reason4, reason5, reason6, reason7, reason8, reason9,
                              reason11, reason12, reason15, reason16, reason17, reason18, reason19, sep="|"), by=index]
  phr_cond[is.na(reason), reason := "most_recent_screen_shows_no_risk", by=index]
  # phr_cond[, reason := gsub(x=reason, pattern="_", replace=" ")]
  rm(phr_cond1, phr_cond2)
#### ER HH core and HH non-core monthly boxplots ####
mon_name <- gsub(x=as.yearmon(dateConvert(user_input$sql_end_dt)), pattern=" ", replace="_")

### testing ###
# mon_er[, list(consumers = length(unique(case_no))), by=list(HH_coregroup, time)]
# mon_er[, list(consumers = length(unique(case_no))), by=list(time)]
# mon_er[, unique(enroll)]
mon_er[, enroll := factor(enroll, levels = c("pre-HH team", "post-HH team") )]
avg_mon <- mon_er[, list(avg_er = mean(num_er)), by=list(time, HH_coregroup)]
mon_levels <- mon_dt[, format(end_dates, "%m/%y")]
mon_er[, time := factor(time, levels = rev(mon_levels))]
mon_er[, list(num_cases = length(unique(case_no))), by=list(enroll, time)]
mon_er[, unique(time)]

# er_core_log <- ggplot(data=mon_er, aes(x=time, y=num_er, fill=enroll))+
#   geom_boxplot(width=0.6, position = position_dodge(0.6), size=0.1) +
#   my_theme + theme(legend.position="right", axis.text.x = element_text(angle=90))+
#   facet_wrap(~HH_coregroup, nrow=2)+labs(title="Log Average Monthly ER visits") +
#   scale_y_log10()
# ggsave(plot=er_core_log, filename=paste0("er_core_log_", mon_name, ".pdf"), width=7, height=8, units="in")
mon_er[HH_coregroup=="yes", status := "RN Assigned"]
mon_er[HH_coregroup=="no", status := "RN Not Assigned"]
mon_er[, time := gsub(x=time, pattern="ERs_", replace="")]
mon_er[, time := gsub(x=time, pattern="_", replace="/")]
mon_er[, time := factor(time, levels = rev(mon_dt[, format(start_dates, "%m/%y")]))]
title.y.axis = "average number of monthly ER visits"
#jitter.x=0.02
#jitter.y=0
# first quartile
mon_er[, q1 := .SD[, quantile(x=num_er, probs=0.25, type=7, na.rm=TRUE)], by=list(time, status, enroll)]
# third quartile
mon_er[, q3 := .SD[, quantile(x=num_er, probs=0.75, type=7, na.rm=TRUE)], by=list(time, status, enroll)]

# outlier column
mon_er[ num_er > q3+1.5*(q3-q1) | num_er < q1-1.5*(q3-q1), outliers := "Y"]
mon_er[!( num_er > q3+1.5*(q3-q1) | num_er < q1-1.5*(q3-q1)), outliers := "N"]

er_sample_size <- mon_er[, list(num_cases = length(unique(case_no))), by=list(time, status, enroll)]
er_outliers <- mon_er[outliers=="Y" & num_er >=4, list(four_plus = length(case_no)), by=list(time, status, enroll)]

# create boxplot
p_er <- ggplot(data=mon_er, aes(x=time, y=num_er, fill=enroll, ymax=4))+
  geom_boxplot(data=mon_er, aes(x=time, y=num_er, fill=enroll), fatten=1,
               outlier.size=NA,
               width=.6) +
  geom_point(data=mon_er[outliers=="Y" & num_er < 4], aes(x=time, y=num_er, fill=enroll, ymax=4),
             position=position_jitterdodge(jitter.width = 0.03, jitter.height = 0, dodge.width = 0.6),
             size=1.5, alpha=0.7, shape=3, color=um_colors[4]) +
  geom_point(data=er_outliers, aes(x=time, y=4, fill=enroll, ymax=4),
             position=position_dodge(0.6), size=7, shape=16, show.legend=FALSE) +
  geom_point(data=er_outliers, aes(x=time, y=4, ymax=4), color="white",
             position=position_dodge(0.6), size=6, shape=16, show.legend=FALSE) +
  geom_text(data=er_outliers, aes(x=time, y=4, label=four_plus, fill=enroll, ymax=4), 
            position = position_dodge(0.6), size=3, fontface="bold")+

  geom_text(data=er_sample_size, aes(x=time, y=-0.1, label=num_cases, fill=enroll, ymax=4),
            position=position_dodge(0.6), size=3, fontface="bold")+
  facet_wrap(~status, scales="free_x", ncol=1) +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(fill = NA, colour = "grey80"),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_line(colour = "grey85", size = .2),
        strip.background = element_rect(fill = "white", colour = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "grey30", size=8),
        axis.text.x = element_text(size=8, angle=45),
        axis.text.y = element_text(size=8)) +
labs(title = "ER Visits by Month HH enrolled\nOct 14 to Now (2 month lag)", y="avg monthly number of ER visits") +
scale_y_continuous(minor_breaks = seq(0,  4, 0.25), breaks = seq(0,  4, 1), labels=c(seq(0,3,1), "4+")) +
coord_cartesian(ylim = c(0, 4.25))
# ggsave(plot=p_er, filename=paste0("er_core_", mon_name, ".pdf"), width=7, height=8, units="in")

# Creation of mydoc, a mydocx object
er_doc <- docx(title="ER Visits Pre/Post HH Admission" )
# add table v_names into er_doc
# er_doc <- addFlexTable( er_doc,  FlexTable(v_names[order(vendor_short)]))

# add a page break
# er_doc = addPageBreak( er_doc )

# add a plot into er_doc
er_doc <- addPlot( er_doc, fun=print, x=p_er, width=7, height=7)

# add text with stylename "Normal" into er_doc
er_doc = addParagraph( er_doc,
  value = "The closer the boxplot is to zero, the fewer ER visits the group of consumers had in general. We are looking at boxplots of the average monthly ER visits people have. If a boxplot has a median value of 0.5, this indicates that a typical consumer attends the ER every other month. Outlier, denoted by '+', show consumers substantially different than their peers. If we see an outlier with a value of 2, this indicates that this particular consumer typically visits the ER twice a month.",
  stylename = "Normal" )

save_location <- file.path(work_dir$results)
if(!file.exists(save_location)) {
  dir.create(save_location)
  print(paste("file save location created:", save_location))
}
# write the doc
writeDoc(er_doc, file = file.path(work_dir$result, paste("ER_visits_HH.docx")))

#
# er_core <- ggplot(data=mon_er, aes(x=time, y=num_er, fill=enroll))+
#   geom_boxplot(width=0.6, position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2, dodge.width = 0.6), size=0.1) +
#   my_theme + theme(legend.position="right", axis.text.x = element_text(angle=90))+
#   facet_wrap(~status, nrow=2)+labs(title="Average Monthly ER visits")

