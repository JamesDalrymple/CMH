sql <- list(
  channel = odbcConnect("WSHSQL002"),
  query = list())

sql$query$admit <-
  sprintf("select distinct
cmh.case_no, cmh.team_effdt, cmh.team_expdt, cmh.team2 as team,
cmh.cmh_effdt, cmh.cmh_expdt
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as CMH
where cmh.county = 'Washtenaw' and cmh.team_effdt <= '%2$s' and
  (cmh.team_expdt >= '%1$s' or cmh.team_expdt is null) and
  cmh.team2 in ('WSH - DD Adult', 'WSH - MI - Adult', 'WSH - ACT',
  'WSH - Children''s Services', 'WSH - Access/Engagement',
  'WSH - Children''s Services - Home Based' )",
  input$start_dt, input$end_dt)

# main cc360 dataset with a few CMH columns added ---
# skipping these columns: Beneficiary_ID, Birth_Date
sql$query$cc360_main <-
  sprintf("select distinct  TCN, Line_TCN,
Transaction_Type, Transaction_Type2, Invoice_Type,
Member_ID_Type, Service_From_Date, Service_To_Date, Admission_Date,
Discharge_Date, Patient_Status, Procedure_Code, Procedure_Code_Qualifier,
Revenue_Code, Place_of_Service, Facility_Type, Paid_Units, National_Drug_Code,
Drug_Name, Days_Supply, Specific_Therapeutic_Drug_Class, Originator_Plan_ID,
Originator_Plan_Name, Originator_Plan_Name_Type, Related_Plan_ID,
Related_Plan_Name, Related_Plan_Name_Type, Billing_Provider_Name,
Rendering_Provider_Name, Header_Diagnosis_Code1, Diag1_desc,
Header_Diagnosis_Code2, Diag2_desc, Header_Diagnosis_Code3,
Header_Diagnosis_Code4, Surgical_Procedure_Code1, Surgical_Procedure_Code1_desc,
Surgical_Procedure_Code2, Surgical_Procedure_Code2_desc,
Surgical_Procedure_Code3, Surgical_Procedure_Code3_desc, Procedure_Modifier1,
Procedure_Modifier2, Procedure_Modifier3, Procedure_Modifier4,
Adjudication_Date, Extract_Number, ERN,
cmh.case_no
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as CMH
join frank_data.dbo.tblCC360_sinceFY14_w_WCHO CC on cmh.Case_No = CC.case_no
and CC.service_from_date between '%1$s' and '%2$s'
where cmh.county = 'Washtenaw' and cmh.team_effdt <= '%2$s' and
  (cmh.team_expdt >= '%1$s' or cmh.team_expdt is null) and
  cmh.team2 in ('WSH - DD Adult', 'WSH - MI - Adult', 'WSH - ACT',
  'WSH - Children''s Services', 'WSH - Access/Engagement',
  'WSH - Children''s Services - Home Based' )",
  input$start_dt, input$end_dt)

# # cc360 prescriptions/medications ---
# # skipping these columns: MAIDNo, Beneficiary_ID
# sql$query$cc360_med <-
#   sprintf("select distinct TCN, Line_TCN, Birth_Date,
#   Transaction_Type, Transaction_Type2, Service_From_Date, Service_To_Date,
#   paid_Units, National_Drug_Code, Drug_Name, Days_Supply,
#   Specific_Therapeutic_Drug_Class, Drug_class_Name, Billing_Provider_Name,
#   Rendering_Provider_Name, Prescribing_Provider_Name, Adjudication_Date,
#   Extract_Number,
#   cmh.case_no
#   from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as CMH
# join Frank_data.dbo.tblCC360_sinceFY14_Med_Only CC on cmh.Case_No = CC.case_no
#   and CC.service_from_date between '%1$s' and '%2$s'
# where cmh.county = 'Washtenaw' and cmh.team_effdt <= '%2$s' and
#   (cmh.team_expdt >= '%1$s' or cmh.team_expdt is null) and
#   cmh.team2 in ('WSH - DD Adult', 'WSH - MI - Adult', 'WSH - ACT',
#   'WSH - Children''s Services', 'WSH - Access/Engagement',
#   'WSH - Children''s Services - Home Based' )",
#   input$start_dt, input$end_dt)

# diagnoses nearest (before) time of service
sql$query$dx_past <-
  sprintf("select distinct
TCN, Line_TCN, Service_From_Date, Service_To_Date, Admission_Date,
Discharge_Date, Procedure_Code, Revenue_Code, Place_of_Service, Facility_Type,
Billing_Provider_Name, Header_Diagnosis_Code1, Diag1_desc,
Header_Diagnosis_Code2, Diag2_desc, Header_Diagnosis_Code3,
Header_Diagnosis_Code4, cmh.case_no, max(dx.statusDate) as max_status_dt
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as CMH
join frank_data.dbo.tblCC360_sinceFY14_w_WCHO CC on
cmh.Case_No = CC.case_no and
CC.service_from_date between '%1$s' and '%2$s'
left join encompass.dbo.tblE2_Consumer_Diagnosis_HIT_w_ICD10 as dx on
dx.ICD9_or_10 = 'ICD9' and dx.sequence = 1 and
dx.statusDate <= CC.Service_From_Date and dx.case_no = cmh.case_no
where cmh.county = 'Washtenaw' and cmh.team_effdt <= '%2$s' and
(cmh.team_expdt >= '%1$s' or cmh.team_expdt is null) and
cmh.team2 in ('WSH - DD Adult', 'WSH - MI - Adult', 'WSH - ACT',
'WSH - Children''s Services', 'WSH - Access/Engagement',
'WSH - Children''s Services - Home Based')
group by
TCN, Line_TCN, Service_From_Date, Service_To_Date, Admission_Date,
Discharge_Date, Procedure_Code, Revenue_Code, Place_of_Service,
Facility_Type, Billing_Provider_Name, Header_Diagnosis_Code1, Diag1_desc,
Header_Diagnosis_Code2, Diag2_desc, Header_Diagnosis_Code3,
Header_Diagnosis_Code4, cmh.case_no",
  input$start_dt, input$end_dt)

sql$query$dx_present <- sprintf("select distinct
	dx.Case_No, dx.DG_CODE as icd_code, dx.ICD9_or_10,
	dx.DG_DESC as icd_desc, MAX(dx.statusDate) as max_status_dt
from encompass.dbo.tblE2_Consumer_Diagnosis_HIT_w_ICD10 as dx
join encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as CMH
	on dx.case_no = cmh.case_no
where dx.sequence = 1 and cmh.county = 'Washtenaw' and
  cmh.team_effdt <= '10/1/2014' and
  (cmh.team_expdt >= '9/30/2015' or cmh.team_expdt is null) and
  cmh.team2 in ('WSH - DD Adult', 'WSH - MI - Adult', 'WSH - ACT',
  'WSH - Children''s Services', 'WSH - Access/Engagement',
  'WSH - Children''s Services - Home Based' )
group by dx.case_no, dx.ICD9_or_10, dx.dg_code, dx.dg_desc",
input$start_dt, input$end_dt)

sql$query$dx_dt <- sprintf("select distinct
	dx.case_no, dx.sequence, dx.dg_code as icd9_code, dx.dg_desc as icd9_desc,
  dx.statusDate as status_dt
from encompass.dbo.tblE2_Consumer_Diagnosis_HIT_w_ICD10 as dx
where dx.statusDate < '%1$s' and dx.sequence = 1
and dx.ICD9_or_10 = 'ICD9'", input$end_dt)

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