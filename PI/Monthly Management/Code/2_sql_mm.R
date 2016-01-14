sql <- list(
  channel = odbcConnect("WSHSQLGP"),
  query = list())
# state hospital - filter out any current state hosp consumers
sql$query$state_hosp <-
  sprintf(
    "select distinct
    hosp.case_no, hosp.auth_eff, hosp.auth_exp, hosp.hosp_disc,
    hosp.auth_days
    from encompass.dbo.tblE2_Hosp as hosp
    where hosp.county = 'Washtenaw' and hosp.cpt_code not like '09%%'
    and (hosp.auth_exp >= '%1$s' or
         hosp.hosp_disc >= '%1$s')
    and hosp.contract_paneltype like 'State Facility%%'",
    input$report_end)
# case load
sql$query$case_load <-
  "select distinct
    CMH.team, CMH.supervisor, CMH.primary_staff, CMH.staff_type,  CMH.case_no
  from encompass.dbo.tblE2_CMH_Open_Consumers_w_OBRA as CMH
  where CMH.County = 'Washtenaw'
    and CMH.team in ('WSH - Access/Engagement',
    'Community Support and Treatment Services - CSTS',
    'WSH - ACT' , 'WSH - Children''s Services' ,
    'WSH - Children''s Services - Home Based',
    'WSH - MI - Adult', 'WSH - ATO', 'WSH - DD Adult',
    'Washtenaw County Community Mental Health',
    'Washtenaw County Community Mental Health-External')"
# IPOS
sql$query$ipos <-
  "select distinct
  	CMH.team, CMH.primary_staff,
    CMH.case_no, CMH.supervisor,
    ip_effdt, ip_expdt, ipos_type
  from encompass.dbo.E2_Fn_Consumer_Most_Recent_IPOS_Date('Washtenaw') as CMH
  where CMH.team in ('WSH - Access/Engagement',
    'Community Support and Treatment Services - CSTS',
    'WSH - ACT' , 'WSH - Children''s Services',
    'WSH - Children''s Services - Home Based',
    'WSH - MI - Adult', 'WSH - ATO', 'WSH - DD Adult',
    'Washtenaw County Community Mental Health',
    'Washtenaw County Community Mental Health-External')"
# last service
sql$query$last_svc <-
  sprintf(
    "select distinct
      CMH.team, CMH.primary_staff, CMH.case_no,
      DATEDIFF( dd, MAX( case when Cat = 'SAL'
        then Claim.service_date else null end),
        '%1$s') as num_days_no_service
    from encompass.dbo.E2_Fn_CMH_Open_N_IPOS ('Washtenaw') as CMH
    left join encompass.dbo.tblE2_SAL_claims_for4 Claim on
      Claim.county = CMH.County and Claim.Case_No = CMH.Case_No
    and Claim.service_date between cast('%1$s' as datetime) - 365 and
      cast('%1$s' as datetime)
    group by CMH.team, CMH.primary_staff, CMH.case_no",
    input$report_end)
# demographic errors
sql$query$demo_errors <-
  sprintf("select distinct
          team, case_no, num_qi_errors, supervisor, primary_staff
          from encompass.dbo.E2_Fn_QI_Demo_Errors2('Washtenaw', '%1$s', '%2$s')",
          input$fy_start, input$report_end)
# health errors
sql$query$health_errors <- "select distinct
    HI.team, HI.primary_staff_supervisor, HI.primary_staff,
    HI.case_no, HI.data_issue_type, HI.notes
  from encompass.dbo.E2_Fn_Data_Issues_Health_Info_Download ('Washtenaw') as HI
  join encompass.dbo.tblE2_Health_N_Other_Conditions as C on
    C.case_no = HI.Case_No"
# wage errors
sql$query$wage_errors <-
  sprintf("select distinct
    qi.case_no, qi.team, qi.primary_staff, cmh.supervisor,
    qi.EmploymentStatus as employment_status, qi.MinimumWage as minimum_wage,
    qi.MinimumWage_Error as minimum_wage_error
  from encompass.dbo.E2_Fn_Min_Wage_Missed('Washtenaw', '%1$s', '%2$s') as qi
  join encompass.dbo.tblE2_CMH_Open_Consumers_w_OBRA as CMH on
    CMH.County = 'Washtenaw' and
    CMH.team in ('WSH - Access/Engagement',
    'Community Support and Treatment Services - CSTS',
          'WSH - ACT' , 'WSH - Children''s Services',
          'WSH - Children''s Services - Home Based',
          'WSH - MI - Adult', 'WSH - ATO', 'WSH - DD Adult',
          'Washtenaw County Community Mental Health',
          'Washtenaw County Community Mental Health-External')
          and qi.case_no = cmh.case_no", input$fy_start, input$report_end)
# unsigned drafts by supervisor and author
sql$query$unsigned_docs <-
  sprintf("select distinct
      doc.DO_TITLE as do_title, doc.doc_status, doc.do_date,
      doc.DO_RCDID as doc_id, doc.case_no, doc.Entered_By as author,
      doc.Entered_by_Sup as supervisor, doc.team, doc.FaceToFace as f2f
    from encompass.dbo.E2_Fn_Draft_Documents_by_Staff_Date3
      ('Washtenaw', '%1$s', '%2$s') as doc
    group by DO_TITLE, doc.Doc_Status, doc.DO_RCDID, doc.DO_date, doc.Case_No,
      doc.Entered_By, doc.Entered_by_Sup, doc.team, doc.entered_by_sup,
      doc.Primary_Staff, doc.FaceToFace", input$fy_start, input$report_end)
# self sufficiency matrix
sql$query$ssm <-
  sprintf("select distinct cmh.case_no, cmh.team, cmh.supervisor,
  cmh.primary_staff, num_SSMs, last_matrix_date
  from James_CSTS..JD_E2_fn_MI_prog('3/1/2014', '%1$s') as cmh",
  input$report_end)
# query and collect all results -----------------------------------------------
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
# team levels
sql$output$team_levels <- fread(file.path(input$data_wd,
                              "teamStaffLevels.csv"))