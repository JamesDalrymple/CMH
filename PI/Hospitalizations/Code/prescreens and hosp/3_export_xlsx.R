#### create workbook ####
wb <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb) + Font(wb, isBold = TRUE) + Border()
### create hosp_base ###
hosp_base_sheet <- createSheet(wb, sheetName = "hosp_base")
addDataFrame(x = hosp, sheet = hosp_base_sheet, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1,
             colnamesStyle = cs3)
### create prescreen_base ###
prescreen_base_sheet <- createSheet(wb, sheetName = "prescreen_base")
addDataFrame(x = prescreen, sheet = prescreen_base_sheet, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
### prescreen full sheet
prescreen_full_sheet <- createSheet(wb, sheetName = "full_prescreen")
addDataFrame(x = full_prescreen, sheet = prescreen_full_sheet,
             showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
             colnamesStyle = cs3)
### differences summary ###
diff_sheet <- createSheet(wb, sheetName = "differences_summary")
addDataFrame(x = modify$nomatch_hosp_summary, sheet = diff_sheet,
             showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
             colnamesStyle = cs3)
addDataFrame(x = modify$nomatch_prescreen_summary, sheet = diff_sheet,
             showNA = FALSE, row.names = FALSE,
             startRow = nrow(modify$nomatch_hosp_summary)+3, startColumn = 1,
             colnamesStyle = cs3)

### create sheet data information ###
about_file <- list(sql$query$hosp, sql$query$prescreen)
about_file <- rapply(about_file,
  f = function(x) gsub(x=x, pattern="\n|\t", replace=""))
about_file <-
  rapply(list(about_file),
         f = function(x) gsub(x=x, pattern="  ", replace=" "))
sheet_info = createSheet(wb, sheetName = "data_sources")
addDataFrame(
  x = data.frame(about_file), sheet = sheet_info, showNA = FALSE,
  row.names = FALSE, col.names = TRUE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)

### save workbook ###
if(!dir.exists(file.path(project_wd$results))) {
    dir.create(file.path(project_wd$results))
    paste("folder created", file.path(project_wd$results),
          sep = "...") ; flush.console()
}

saveWorkbook(wb=wb, file=file.path(project_wd$results,
  "hosp_vs_prescreen.xlsx"))
rm(wb)