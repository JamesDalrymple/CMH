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

# health home activities

# hh_nurse, PCP, MI/DD designation, gender, HH start/stop
sql$query$hh_detail <- sprintf("select distinct
  cmh.case_no, cmh.team2 as team, cmh.team_effdt, cmh.team_expdt, cmh.dob,
  cmh.gender, cmh.staff_eff, cmh.staff_exp,
  case when cmh.staff_type = 'SAMHSA Staff' then 'Y' else 'N' end as hh_nurse,
  dg.MI, dg.DD,
  Isnull( PrimaryCareClinic.PC_CNAME, ltrim(rtrim(PrimaryCarePhysician.PC_LNAME))) as primary_care
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as cmh
left join encompass.dbo.E2_Fn_CMH_Consumers_Diagnoses2('Washtenaw',
  '%1$s', '%2$s') as dg on dg.Case_No = cmh.Case_No and cmh.County = dg.county
join encompass.dbo.PCCClient as C on cast(C.CL_CASENO as int) = cmh.case_no
left join encompass.dbo.PCCPrimaryCareClinic PrimaryCareClinic on C.CLF_PCCID = PrimaryCareClinic.PC_RCDID and PrimaryCareClinic.PC_OKTOUSE ='Y'
left join encompass.dbo.PCCPrimaryCarePhysician PrimaryCarePhysician on C.CLF_PCPID = PrimaryCarePhysician.PC_RCDID and PrimaryCarePhysician.PC_OKTOUSE  ='Y'
where cmh.county = 'Washtenaw' and cmh.team2 = 'WSH - Health Home'",
  sql$start_dt, sql$end_dt)

sql$query$hh_services <- sprintf("select distinct
  HH.team_effdt, HH.team_expdt, PN.Case_No,
	'Progress Notes' as Document,
	PN.PR_RCDID, PN.Progress_Note_date as Doc_date,
	PN.CPT_CODE, PN.Modifier as MOD, PN.SA_Units, PN.facetoface,
	PN.Staff, PN.begintime, PN.endtime, PN.elapsed_time,
	HH_NA Encounter_not_Health_Home,
	HH_DSCCCM ComprehensiveCareManagment,
	HH_DSCCC CareCoordination,
	HH_DSCHP HealthPromotion,
	HH_DSCCTC ComprehensiveTransitionalCare,
	HH_DSCSSVC Indiv_and_FamilySupportServices,
	HH_DSCREF Referrals_To_Community_and_Social_Supp_Services
	from encompass.dbo.E2_Fn_Health_Home_Consumers('%1$s', '%2$s') as HH
	join encompass.dbo.tblE2_Progress_Note PN on PN.County = 'Washtenaw' and
HH.Case_No = PN.Case_No and
		PN.Progress_Note_date between '%1$s' and '%2$s' and PN.Progress_Note_date
between HH.team_effdt and COALESCE( HH.team_expdt, getdate())
	join encompass.dbo.ENCHealthHomeWorksheet HHW on HHW.HH_SRCFILE = 'ENCPRNPF'
and HHW.HH_SRCRCD = PN.PR_RCDID and HH_OKTOUSE = 'Y'
	union
	select distinct
		HH.team_effdt, HH.team_expdt, WN.Case_No, 'Wellness Notes' as Document,
		WN.WN_RCDID, WN.WellnessNote_date,
		WN.CPT_CODE, WN.MOD, WN.SA_Units, WN.facetoface,
		WN.Staff, WN.begintime, WN.endtime,
  encompass.dbo.GetElaspedTime (Begintime, EndTime) as elapsed_time,
	HH_NA notAddressed,
	HH_DSCCCM ComprehensiveCareManagment,
	HH_DSCCC CareCoordination,
	HH_DSCHP HealthPromotion,
	HH_DSCCTC ComprehensiveTransitionalCare,
	HH_DSCSSVC Indiv_and_FamilySupportServices,
	HH_DSCREF Referrals_To_Community_and_Social_Supp_Services
	from encompass.dbo.E2_Fn_Health_Home_Consumers('%1$s', '%2$s') as HH
	join encompass.dbo.tblE2_WellnessNote_Header as WN on WN.County = 'Washtenaw'
  and HH.Case_No = WN.Case_No and WN.WellnessNote_date between '%1$s' and
  '%2$s' and WN.WellnessNote_date between HH.team_effdt and
  COALESCE(HH.team_expdt, getdate())
join encompass.dbo.ENCHealthHomeWorksheet HHW on HHW.HH_SRCFILE = 'ENCWNHPF'
  and HHW.HH_SRCRCD = WN.WN_RCDID and HH_OKTOUSE = 'Y'",
  sql$start_dt, sql$end_dt)

sql$query$hh_bucket <- "select distinct
enter_date, e2_case_no, hh_bucket
from frank_data.dbo.tblE2_HH_bucket_from_State"

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

# tiers --- perhaps not pretty, but more convenient to coding wise ------------
sql$tiers_fn <- function(end_dt) {
  stopifnot(class(end_dt) == "Date")
  sprintf("select distinct
CMH.case_no, cmh.num_ERs as num_ER_last180,
cmh.num_PH_IPs as num_PH_IPs_last180,
cmh.num_ER_or_PH_IP as num_ER_or_PH_IP_last180, '%1$s' as end_date,
Case when (cmh.num_ERs>= 4 OR cmh.num_PH_IPs >= 3) then 3
when (cmh.num_ERs>0 OR cmh.num_PH_IPs >0) then 2
when CC2.case_no IS not null then 1
else 0 end as util_level,
Case when (cmh.num_ERs> 0 OR cmh.num_PH_IPs >0) and cmh.Last_PHR_4_majors = 'Y' then 1
when cmh.num_ER_or_PH_IP = 0 and cmh.Poor_or_fair = 'Y' and
cmh.Last_PHR_smoke_Chol_HBP = 'Y' then 2
  when (cmh.num_ERs> 0 OR cmh.num_PH_IPs >0) OR cmh.Poor_or_fair = 'Y'
or cmh.Last_PHR_smoke_Chol_HBP = 'Y'
OR cmh.Last_PHR_4_majors_risk = 'Y'
then 3
when cmh.Last_PHR_date IS not null then 4
else 5 end as tier
from encompass.dbo.E2_Fn_Active_Clients_Between_w_team_dates3 ('%1$s') as CMH
left join frank_data.dbo.tblCC360_sinceFY14_w_WCHO CC2
  on CC2.county = 'Washtenaw' and CMH.case_no = CC2.case_no and
  CC2.service_from_date between cast('%1$s' as datetime) - 180 and '%1$s'
left join encompass.dbo.v_WSH_Health_Home HH2 on HH2.Case_No = CMH.case_no",
end_dt)
}

sql$mons <- date_expansion(start_date = sql$start_dt,
                           end_date = sql$end_dt, types = "mon")

sql$tier_dt <- NULL
for (i in seq(sql$mons)) {
  sql$tier_dt <- rbindlist(list(sql$tier_dt,
    data.table(sqlQuery(query = sql$tiers_fn(sql$mons[i, span_end]),
           channel = sql$channel, stringsAsFactors = FALSE))))
}; rm(i)

sql$tier_dt
