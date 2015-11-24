# create workbook -------------------------------------------------------------

# svc_agg$cmh_crisis$svc_summary
# svc_agg$cmh_crisis$team_summary
# svc_agg$cmh_crisis$mi_level_summary
# svc_agg$non_cmh_crisis$svc_summary
# svc_agg$non_cmh_crisis$team_summary


wb <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()

# CMH summary ---
cmh_sheet <- createSheet(wb, sheetName = "cmh summary")
addDataFrame(
  x = svc_agg$cmh_crisis$svc_summary,
  sheet = cmh_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# team summary ---
team_sheet <- createSheet(wb, sheetName = "teams_overview")
addDataFrame(
  x = svc_agg$cmh_crisis$team_summary,
  sheet = team_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
addDataFrame(
  x = svc_agg$cmh_crisis$mi_level_summary,
  sheet = team_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = nrow(svc_agg$cmh_crisis$team_summary)+3,
  startColumn = 1,
  colnamesStyle = cs3
)
addDataFrame(
  x = svc_agg$non_cmh_crisis$team_summary,
  sheet = team_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = nrow(svc_agg$cmh_crisis$team_summary)+
    nrow(svc_agg$cmh_crisis$mi_level_summary)+5,
  startColumn = 1,
  colnamesStyle = cs3
)
# non-CMH summary ---
noncmh_sheet <- createSheet(wb, sheetName = "noncmh summary")
addDataFrame(
  x = svc_agg$non_cmh_crisis$svc_summary,
  sheet = noncmh_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)

# data info ---
# create information about the file to share with end-users ---
about_file <- data.table(
  Report_Date = input$run_date,
  Last_Updated = as.character(Sys.time()),
  Data_Sources = c(list.files(file.path(project_wd$data),
  pattern = "[.]"), "sql: Encompass", "funding bucket")
)
data_sheet <- createSheet(wb, sheetName = "about_file")
addDataFrame(
  x = about_file,
  sheet = data_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)

# save workbook ---------------------------------------------------------------
if(!dir.exists(file.path(project_wd$results))) {
  dir.create(file.path(project_wd$results), recursive = TRUE)
  paste("folder created", file.path(project_wd$results),
        sep="...")
}
# file name
export_file <- paste0("service_summary_analysis", input$run_par, ".xlsx")

gc(reset = TRUE)
saveWorkbook(wb=wb, file=file.path(project_wd$results,
                                   export_file))
rm(wb)