#### create workbook ####
wb <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb) + Font(wb, isBold = TRUE) + Border()

### create hosp_diff_sheet ###
hosp_diff_sheet <- createSheet(wb, sheetName = "hosp_diff")
addDataFrame(
  x = hosp_differences,
  sheet = hosp_diff_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
### create hosp1 ###
hosp1_sheet <- createSheet(wb, sheetName = "hosp_Aug31")
addDataFrame(
  x = hosp1,
  sheet = hosp1_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
### create hosp2 ###
hosp2_sheet <- createSheet(wb, sheetName = "hosp_Sep30")
addDataFrame(
  x = hosp2,
  sheet = hosp2_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)

### create sheet data information ###
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
# create result folder
### R 3.2.0 approach
if (!dir.exists(project_wd$results)) {
  dir.create(file.path(project_wd$results))
  paste("folder created", file.path(project_wd$results),
        sep = "...")
}

# file name
export_file <- "hospital_team_differences.xlsx"

gc(reset = TRUE)
saveWorkbook(wb = wb,
             file = file.path(project_wd$results,
                              export_file))
rm(wb)
