  #### create workbook ####
  wb <- createWorkbook()
  # bold option and underline
  cs3 <- CellStyle(wb) + Font(wb, isBold = TRUE) + Border()
  ### create team_fy_sheet ###
  team_fy_sheet <- createSheet(wb, sheetName = "fy_team")
  addDataFrame(
    x = team_hosp_summary[span_type == "fy"],
    sheet = team_fy_sheet,
    showNA = FALSE,
    row.names = FALSE,
    startRow = 1,
    startColumn = 1,
    colnamesStyle = cs3
  )
  ### create team_qtr_sheet ###
  team_qtr_sheet <- createSheet(wb, sheetName = "qtr_team")
  addDataFrame(
    x = team_hosp_summary[span_type == "qtr"],
    sheet = team_qtr_sheet,
    showNA = FALSE,
    row.names = FALSE,
    startRow = 1,
    startColumn = 1,
    colnamesStyle = cs3
  )
  ### create cmh_fy_sheet ###
  cmh_fy_sheet <- createSheet(wb, sheetName = "fy_cmh")
  addDataFrame(
    x = cmh_hosp_summary[span_type == "fy"],
    sheet = cmh_fy_sheet,
    showNA = FALSE,
    row.names = FALSE,
    startRow = 1,
    startColumn = 1,
    colnamesStyle = cs3
  )
  ### create team_qtr_sheet ###
  cmh_qtr_sheet <- createSheet(wb, sheetName = "qtr_cmh")
  addDataFrame(
    x = cmh_hosp_summary[span_type == "qtr"],
    sheet = cmh_qtr_sheet,
    showNA = FALSE,
    row.names = FALSE,
    startRow = 1,
    startColumn = 1,
    colnamesStyle = cs3
  )
  ### create access_summary_sheet ###
  access_summary_sheet <- createSheet(wb, sheetName = "access_summary")
  addDataFrame(
    x = modify$wide_access_summary,
    sheet = access_summary_sheet,
    showNA = FALSE,
    row.names = FALSE,
    startRow = 1,
    startColumn = 1,
    colnamesStyle = cs3
  )
  ### create access_detail_sheet ###
  access_detail_sheet <- createSheet(wb, sheetName = "access_detail")
  addDataFrame(
    x = modify$access_adm,
    sheet = access_detail_sheet,
    showNA = FALSE,
    row.names = FALSE,
    startRow = 1,
    startColumn = 1,
    colnamesStyle = cs3
  )
  ### create sheet data information ###
  about_file <- data.table(report_date = input$report_date,
                           start_date = input$start_date,
                           end_date = input$end_date,
                           data_sources = "SQL: encompass database",
                           last_updated = Sys.time())


  sheet_info = createSheet(wb, sheetName = "Data Info")
  addDataFrame(
    x = about_file,
    sheet = sheet_info,
    showNA = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    startRow = 1,
    startColumn = 1,
    colnamesStyle = cs3
  )

  ### save workbook ###
  if(!dir.exists(file.path(project_wd$results))) {
    dir.create(file.path(project_wd$results))
    paste("folder created", file.path(project_wd$results),
          sep = "...") ; flush.console()
  }

  saveWorkbook(wb=wb, file=file.path(project_wd$results, "hosp_summary.xlsx"))
  rm(wb)

