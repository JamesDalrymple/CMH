### create information about the file to share with end-users ###
about_file <- data.table(
  data_source = "E2 database",
  date_range = paste("7/1/14 to", format(input$end_date), "%m/%d/%y"),
  date_ran = format(input$run_date, "%m/%d/%y"),
  last_updated = Sys.time(),
  project_location = project_wd$project)
# create summary outcomes data workbook ----------------
# create raw outcomes data workbook -------------------------------------------
wb_sum <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb_sum) + Font(wb_sum, isBold=TRUE) + Border()
hh_sheet <- createSheet(wb_sum, sheetName = "hh")
addDataFrame(x = cmg$hh, sheet = hh_sheet, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
hh_lev_sheet <- createSheet(wb_sum, sheetName = "hh_levels")
addDataFrame(x = cmg$hh_lev, sheet = hh_lev_sheet, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
hh_cmh_sheet <- createSheet(wb_sum, sheetName = "hh_cmh")
addDataFrame(x = cmg$hh_cmh, sheet = hh_cmh_sheet, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
hh_lev_cmh_sheet <- createSheet(wb_sum, sheetName = "hh_levels_cmh")
addDataFrame(x = cmg$hh_lev_cmh, sheet = hh_lev_cmh_sheet, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create sheet data information ---
sheet_info = createSheet(wb_sum, sheetName="about")
addDataFrame(x = about_file, sheet = sheet_info, showNA = FALSE,
             row.names = FALSE, col.names = TRUE, startRow = 1,
             startColumn = 1, colnamesStyle = cs3)
# save workbook ----
saveWorkbook(wb = wb_sum, file = file.path(project_wd$results,
  paste0("outcomes_summary ", format(input$run_date, "%b_%d_%Y"), ".xlsx")))
# create raw outcomes data workbook -------------------------------------------
wb_raw <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb_raw) + Font(wb_raw, isBold=TRUE) + Border()
bmi_raw_sheet <- createSheet(wb_raw, sheetName = "bmi_raw")
addDataFrame(x = saved$bmi, sheet = bmi_raw_sheet, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
bp_raw_sheet <- createSheet(wb_raw, sheetName = "bp_raw")
addDataFrame(x = saved$bp, sheet = bmi_raw_sheet, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
labs_raw_sheet <- createSheet(wb_raw, sheetName = "labs_raw")
addDataFrame(x = saved$labs, sheet = labs_raw_sheet, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# pulse_raw_sheet <- createSheet(wb_raw, sheetName = "pulse_raw")
# addDataFrame(x = saved$pulse, sheet = pulse_raw_sheet, showNA = FALSE,
#   row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# resp_raw_sheet <- createSheet(wb_raw, sheetName = "resp_raw")
# addDataFrame(x = saved$resp, sheet = resp_raw_sheet, showNA = FALSE,
#   row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
wn_oh_raw_sheet <- createSheet(wb_raw, sheetName = "wn_oh")
addDataFrame(x = saved$wn_oh, sheet = wn_oh_raw_sheet, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
wn_pain_raw_sheet <- createSheet(wb_raw, sheetName = "wn_pain")
addDataFrame(x = saved$wn_pain, sheet = wn_oh_raw_sheet, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# # create sheet data information ---
# sheet_info = createSheet(wb_raw, sheetName="about")
# addDataFrame(x = about_file, sheet = sheet_info, showNA = FALSE,
#              row.names = FALSE, col.names = TRUE, startRow = 1,
#              startColumn = 1, colnamesStyle = cs3)
# # save workbook ----
saveWorkbook(wb = wb_raw, file = file.path(project_wd$results,
  paste0("outcomes_raw ", format(input$run_date, "%b_%d_%Y"), ".xlsx")))
