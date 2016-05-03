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
cmh_sheet <- createSheet(wb_sum, sheetName = "full summary")
addDataFrame(x = cmg$castc, sheet = cmh_sheet, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
hh_sheet <- createSheet(wb_sum, sheetName = "HH summary")
addDataFrame(x = cmg$hh_only, sheet = hh_sheet, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# abbreviations
aux$abr_cat <- t(data.table(hh = "health home",
  lev = "modifier to hh whichs seperates hh by with and without HH Nurse",
  cmh = "WCCMH team assigment (based on admission records and service date)",
  cat = "all combinations of hh, lev, cmh",
  hh_cat = "CMH only, HH, HH Nurse, HH no nurse",
  maintained = "number of consumers that have maintained",
  regressed = "number of consumers that have regressed",
  improved = "number of consumers that have improved"))
aux$abr_var <- data.table(var = c("var = the variable we are measuring",
        "oh = overall health",
        "dia = diastolic blood pressure",
        "sys = systolic blood pressure",
        "CHOL = cholesterol",
        "TRIG = triglycerides",
        "GLU = glucose (fasting and not fasting) levels",
        "bmi = body mass index",
        "note: all other vars are self-defining
        (i.e. LDL = LDL cholesterol level)"))
hh_sheet <- createSheet(wb_sum, sheetName = "abbreviations")
addDataFrame(x = aux$abr_cat, sheet = hh_sheet, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, col.names = FALSE)
addDataFrame(x = aux$abr_var, sheet = hh_sheet, showNA = FALSE,
  row.names = FALSE, startRow = nrow(aux$abr_cat) + 1,
  startColumn = 1, col.names = FALSE)
# create sheet data information ---
sheet_info = createSheet(wb_sum, sheetName="about")
addDataFrame(x = about_file, sheet = sheet_info, showNA = FALSE,
             row.names = FALSE, col.names = TRUE, startRow = 1,
             startColumn = 1, colnamesStyle = cs3)
# save workbook ----
saveWorkbook(wb = wb_sum, file = file.path(project_wd$results,
  paste0("outcomes_summary ", format(input$run_date, "%b_%d_%Y"), ".xlsx")))
# # create raw outcomes data workbook -------------------------------------------
# wb_raw <- createWorkbook()
# # bold option and underline
# cs3 <- CellStyle(wb_raw) + Font(wb_raw, isBold=TRUE) + Border()
# bmi_raw_sheet <- createSheet(wb_raw, sheetName = "bmi_raw")
# addDataFrame(x = saved$bmi, sheet = bmi_raw_sheet, showNA = FALSE,
#   row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# bp_raw_sheet <- createSheet(wb_raw, sheetName = "bp_raw")
# addDataFrame(x = saved$bp, sheet = bmi_raw_sheet, showNA = FALSE,
#   row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# labs_raw_sheet <- createSheet(wb_raw, sheetName = "labs_raw")
# addDataFrame(x = saved$labs, sheet = labs_raw_sheet, showNA = FALSE,
#   row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# # pulse_raw_sheet <- createSheet(wb_raw, sheetName = "pulse_raw")
# # addDataFrame(x = saved$pulse, sheet = pulse_raw_sheet, showNA = FALSE,
# #   row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# # resp_raw_sheet <- createSheet(wb_raw, sheetName = "resp_raw")
# # addDataFrame(x = saved$resp, sheet = resp_raw_sheet, showNA = FALSE,
# #   row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# wn_oh_raw_sheet <- createSheet(wb_raw, sheetName = "wn_oh")
# addDataFrame(x = saved$wn_oh, sheet = wn_oh_raw_sheet, showNA = FALSE,
#   row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# wn_pain_raw_sheet <- createSheet(wb_raw, sheetName = "wn_pain")
# addDataFrame(x = saved$wn_pain, sheet = wn_oh_raw_sheet, showNA = FALSE,
#   row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# # # create sheet data information ---
# # sheet_info = createSheet(wb_raw, sheetName="about")
# # addDataFrame(x = about_file, sheet = sheet_info, showNA = FALSE,
# #              row.names = FALSE, col.names = TRUE, startRow = 1,
# #              startColumn = 1, colnamesStyle = cs3)
# # # save workbook ----
# saveWorkbook(wb = wb_raw, file = file.path(project_wd$results,
#   paste0("outcomes_raw ", format(input$run_date, "%b_%d_%Y"), ".xlsx")))
