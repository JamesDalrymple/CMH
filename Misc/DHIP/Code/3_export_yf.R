# create information about the file to share with end-users ---
pkg_loader("xlsx")
aboutFile <- data.table(
  rbind(
    report_input = input$fy_value,
    last_updated = as.chr(Sys.time()),
    data_source = "https://app.fasoutcomes.com",
    code_location =project_wd$code_wd),
  keep.rownames=TRUE
)
setnames(aboutFile, colnames(aboutFile), c("About", "Details"))
# create workbook ---
wb <- createWorkbook()
cs3 <- CellStyle(wb) + Font(wb, isBold = TRUE) + Border()
# CAFAS ---
sheet_cafas <- createSheet(wb, sheetName = "CAFAS")
addDataFrame(x = cafas_results, sheet = sheet_cafas, showNA = FALSE,
             row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# PECFAS ---
sheet_pecfas <- createSheet(wb, sheetName = "PECFAS")
addDataFrame(x = pecfas_results, sheet = sheet_pecfas, showNA = FALSE,
             row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create sheet data information ---
sheet_info <- createSheet(wb, sheetName = "data info")
addDataFrame(x = aboutFile, sheet = sheet_info, showNA = FALSE,
             row.names = FALSE, col.names = TRUE, startRow = 1,
             startColumn = 1, colnamesStyle = cs3)
# save workbook ---
if (!dir.exists(file.path(project_wd$results_wd))) {
  dir.create(path = project_wd$results_wd, recursive = TRUE)
  print(paste("this folder path created:", project_wd$results_wd))
}
saveWorkbook(wb = wb,
  file = file.path(project_wd$results_wd, paste0(input$fy_value, ".xlsx")))
