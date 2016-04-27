excel <- new.env(parent = .GlobalEnv)

# create information about the file to share with end-users -------------------
excel$aboutFile <- data.table(Report_Date = input$report_dt,
                       Last_Updated = input$today,
                       data_source = "E2 EM|PCE 107|SQL|JamesD",
                       start_param = input$start_dt, end_param = input$end_dt,
                       id = 1)
excel$aboutFile <- melt(excel$aboutFile, id="id", variables=names(excel$aboutFile)[-1])
excel$aboutFile[, id := NULL]
# create current medications workbook ------------------------------------------
excel$wb_cur <- createWorkbook()
# bold option and underline
excel$cs_cur <-
  CellStyle(excel$wb_cur) + Font(excel$wb_cur, isBold = TRUE) + Border()
# create current medication detail sheet ---
excel$sheet$med <- createSheet(excel$wb_cur, sheetName = "current med detail")
addDataFrame(x = prep$cur_meds, sheet = excel$sheet$med, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs_cur)
# create aggregated data ---
excel$sheet$agg <- createSheet(excel$wb_cur, sheetName="aggregated")
addDataFrame(x = agg$cur_med$results, sheet = excel$sheet$agg, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs_cur)
# add benzos
addDataFrame(x = agg$benzo$provider, sheet = excel$sheet$agg, showNA = FALSE,
  row.names = FALSE, startRow = nrow(agg$cur_med$results) + 3, startColumn = 1,
  colnamesStyle = excel$cs_cur)
# add stimulants
addDataFrame(x = agg$stim$provider, sheet = excel$sheet$agg, showNA = FALSE,
  row.names = FALSE, startRow = nrow(agg$benzo$provider) +
  nrow(agg$cur_med$results) + 5, startColumn = 1, colnamesStyle = excel$cs_cur)
# create benzo_sheet ---
excel$sheet$benzo <- createSheet(excel$wb_cur, sheetName = "benzo meds")
addDataFrame(x = data.table(benzos = aux$benzoList), sheet = excel$sheet$benzo,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs_cur)
# create stim_sheet ---
excel$sheet$stim <- createSheet(excel$wb_cur, sheetName = "stimulant meds")
addDataFrame(x = data.table(stimulants = aux$stimList),
  sheet = excel$sheet$stim, showNA = FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1,  colnamesStyle = excel$cs_cur)
# create antiPys_sheet ---
excel$sheet$antiPys <- createSheet(excel$wb_cur, sheetName = "anti-psych meds")
# add stimList to stim_sheet
addDataFrame(x = data.table(anti_pyschotic = aux$antiPysList),
  sheet = excel$sheet$antiPys, showNA=FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1, colnamesStyle = excel$cs_cur)
# create antiPys_sheet ---
excel$sheet$antidepress <-
  createSheet(excel$wb_cur, sheetName = "anti-depress meds")
addDataFrame(x = data.table(anti_depress = aux$antidepressList),
  sheet = excel$sheet$antidepress, showNA=FALSE, row.names = FALSE,
  startRow = 1, startColumn = 1, colnamesStyle = excel$cs_cur)
### create atypical_sheet ### - sleeping this code per Pat Cowan 6/19/2014
#  atypical_sheet = createSheet(wb, sheetName="atypical meds")
#  # add atypicalList to atypical_sheet
#  addDataFrame(x=atypicalList, sheet=atypical_sheet, showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
#               colnamesStyle=excel$cs_cur)

# create cstsBenzo_sheet ---
excel$sheet$cmh_benzo <- createSheet(excel$wb_cur, sheetName="CMH Benzos")
addDataFrame(x=agg$benzo$drug_dt, sheet = excel$sheet$cmh_benzo,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs_cur)
# create cstsStim_sheet ---
excel$sheet$cmh_stim <- createSheet(excel$wb_cur, sheetName = "CMH Stimulants")
addDataFrame(x = agg$stim$drug_dt, sheet = excel$sheet$cmh_stim,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs_cur)
# create cstsAntipsych_sheet ---
excel$sheet$cmh_antipsych <-
  createSheet(excel$wb_cur, sheetName = "CMH Antipsych")
addDataFrame(x = agg$anti_psych$drug_dt, sheet = excel$sheet$cmh_antipsych ,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs_cur)
### create cstsAntipsych_sheet ###
excel$sheet$cmh_antidepress <-
  createSheet(excel$wb_cur, sheetName = "CSTS Antidepress")
# add cstsAntidepress to cstsAntidepress_sheet
addDataFrame(x = agg$anti_depress$drug_dt, sheet = excel$sheet$cmh_antidepress,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs_cur)
# create sheet data information ---
excel$sheet$info <- createSheet(excel$wb_cur, sheetName = "data info")
addDataFrame(x = excel$aboutFile, sheet = excel$sheet$info, showNA = FALSE,
  row.names = TRUE, col.names = FALSE, startRow = 1, startColumn = 1,
  rownamesStyle = excel$cs_cur)
# save workbook ---
saveWorkbook(wb = excel$wb_cur, file = file.path(proj_wd$cur_res,
  paste("current medicines ", input$today, ".xlsx", sep="")))