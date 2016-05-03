qry_U_dx_codes <- "
select distinct diagnosis_code, diag_short_desc
from Frank_data.dbo.Diagnosis_Reference"

qry_U_rawCC360 <- "
select distinct *
from frank_data.dbo.care_connection_360"

qry_U_rawCC360_active <- "
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
ERN"

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

qry_U_hosp <- "select distinct
county, case_no, hosp, auth_eff, auth_exp, hosp_disc, medicaid,
team_at_admit, team_at_disc, current_team, contract_paneltype,
case when auth_exp < getdate()-90 and hosp_disc is NULL
then 1 else 0 end as potential_remove"


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