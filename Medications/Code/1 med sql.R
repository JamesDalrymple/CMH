sql <- list(
  channel = odbcConnect("WSHSQLGP"),
  query = list()
)

# CMH admissions ---
sql$query$cmh_adm <-
  "select distinct
case_no, team2 as team, cmh_effdt, cmh_expdt, team_effdt, team_expdt, dob,
staff_eff, staff_exp, assigned_staff, staff_type, supervisor as current_sup
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA
where county = 'Washtenaw'"

# IR report 2076 ---
sql$query$ir <- sprintf("select distinct
CodeCategory as code_cat, classification as classify, ir_number as ir_num,
discovery_Date as disc_dt, occurrence_Date as occ_dt, case_no,
ir_status, provider, consumer_role, team_at_service, primary_staff, author
from encompass..E2_Fn_Incident_Report_by_Reviewers('Washtenaw', '%1$s', '@%2$s')",
        input$start_dt, input$end_dt)

# current medications
sql$query$cur_meds <-
  "select distinct
cmh.county, cmh.Team, cmh.primary_staff, cmh.supervisor, cmh.cmh_effdt, cmh.case_no,
CMH.Medicaid_Related, INS.Waiver, Drug, Formula, SIG, quantity, Refills, startDate,
fillDate as Last_Fill_Date, Provider, Diag1, Diag1_desc, Status1, SA1, Diag2, Diag2_desc,
Status2, SA2, Diag3, Diag3_desc, Status3, SA3
from tblE2_CMH_Open_Consumers_w_OBRA CMH
left join tblE2_Consumer_Current_Medications_HIT Med on Med.Case_No = CMH.Case_No
left join tblE2_Consumer_Ins_Current2 INS on CMH.Case_No = INS.Case_No
where cmh.County = 'Washtenaw'"

sql$query$ir_detail <-
  sprintf("select * from james_csts.dbo.jd_ir_detail('%1$s', '%2$s')",
          input$start_dt, input$end_dt)
# full detail for personal review and drilldown
sql$query$full_detail <-
  sprintf("select * from james_csts.dbo.jd_ir_full_detail('%1$s', '%2$s')",
          input$start_dt, input$end_dt)

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