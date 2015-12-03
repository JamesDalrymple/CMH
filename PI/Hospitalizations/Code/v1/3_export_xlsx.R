  #### create workbook ####
  wb <- createWorkbook()
  # bold option and underline
  cs3 <- CellStyle(wb) + Font(wb, isBold = TRUE) + Border()
  ### create hosp_fy ###
  hosp_fy_sheet <- createSheet(wb, sheetName = "summary_fy")
  addDataFrame(
    x = full_summary[span_type == "fy"],
    sheet = hosp_fy_sheet,
    showNA = FALSE,
    row.names = FALSE,
    startRow = 1,
    startColumn = 1,
    colnamesStyle = cs3
  )
  ### create hosp_qtr ###
  hosp_qtr_sheet <- createSheet(wb, sheetName = "summary_qtr")
  addDataFrame(
    x = full_summary[span_type == "qtr"],
    sheet = hosp_qtr_sheet,
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

