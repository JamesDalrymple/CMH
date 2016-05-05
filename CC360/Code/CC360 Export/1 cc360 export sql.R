sql <- list(
  channel = odbcConnect("WSHSQL002"),
  query = list(),
  start_dt = '2/1/2014',
  end_dt = '1/31/2016'
)
# main cc360 dataset with a few CMH columns added ---
sql$query$cc360_main <-
  sprintf("select distinct  TCN, Line_TCN, Beneficiary_ID, Birth_Date,
Consumer_Unique_ID, Transaction_Type, Transaction_Type2, Invoice_Type,
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
HH.case_no as E2_Case_no, team_effdt as Home_Health_adm_date,
team_expdt as Home_Health_disc_date
from encompass.dbo.E2_Fn_Health_Home_Consumers('%1$s', getdate()) as HH
join frank_data.dbo.tblCC360_sinceFY14_w_WCHO CC on HH.Case_No = CC.case_no
and CC.service_from_date between '%1$s' and '%2$s'", sql$start_dt, sql$end_dt)
# cc360 prescriptions/medications ---
sql$query$cc360_med <-
  sprintf("select distinct MAIDNo, TCN, Line_TCN, Beneficiary_ID, Birth_Date,
Transaction_Type, Transaction_Type2, Service_From_Date, Service_To_Date,
Paid_Units, National_Drug_Code, Drug_Name, Days_Supply,
Specific_Therapeutic_Drug_Class, Drug_class_Name, Billing_Provider_Name,
Rendering_Provider_Name, Prescribing_Provider_Name, Adjudication_Date,
Extract_Number, HH.case_no as E2_Case_no, team_effdt as Home_Health_adm_date,
team_expdt as Home_Health_disc_date
from encompass.dbo.E2_Fn_Health_Home_Consumers('%1$s', getdate()) as HH
join Frank_data.dbo.tblCC360_sinceFY14_Med_Only CC on HH.Case_No = CC.case_no
and CC.service_from_date between '%1$s' and '%2$s'", sql$start_dt, sql$end_dt)
# tiers
# nurse tiers

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