#### create workbook ####
wb <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb) + Font(wb, isBold = TRUE) + Border()
### create con_sheet ###
full_sheet <- createSheet(wb, sheetName = "full_sheet1")
addDataFrame(
  x = address,
  sheet = full_sheet, showNA = FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1, colnamesStyle = cs3)
### create con_sheet ###
con_sheet <- createSheet(wb, sheetName = "no_guardian")
addDataFrame(
  x = address[is.na(guardian_in_sheet2)],
  sheet = con_sheet, showNA = FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1, colnamesStyle = cs3)
### create guardian_sheet ###
guardian_sheet <- createSheet(wb, sheetName = "guardian")
addDataFrame(
  x = guardian,
  sheet = guardian_sheet, showNA = FALSE, row.names = FALSE,
  startRow = 1, startColumn = 1, colnamesStyle = cs3)
### create sheet data information ###
about_file <- data.table(report_date = input$report_date,
                         start_date = input$start_date,
                         end_date = input$end_date,
                         data_sources = "C:/Users/dalrymplej/Documents/GitHub/CMH/Misc/Addresses/Code",
                         last_updated = Sys.time())
sheet_info = createSheet(wb, sheetName = "Data Info")
addDataFrame(
  x = about_file, sheet = sheet_info, showNA = FALSE, row.names = FALSE,
  col.names = TRUE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
### save workbook ###
if(!dir.exists(file.path(project_wd$results))) {
  dir.create(file.path(project_wd$results))
  paste("folder created", file.path(project_wd$results),
        sep = "...") ; flush.console()
}
saveWorkbook(wb=wb, file=file.path(project_wd$results, "addresses.xlsx"))
rm(wb)