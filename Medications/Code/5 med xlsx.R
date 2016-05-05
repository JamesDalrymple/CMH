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
excel$wb <- createWorkbook()
# bold option and underline
excel$cs <-
  CellStyle(excel$wb) + Font(excel$wb, isBold = TRUE) + Border()

# raw ir detail
excel$sheet$ir_detail <- createSheet(excel$wb, sheetName = "ir details")
addDataFrame(x = prep$ir, sheet = excel$sheet$ir_detail, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = excel$cs)
# medication incidents
excel$sheet$med_inc <- createSheet(excel$wb, sheetName = "med incidents")
addDataFrame(x = agg$med_inc$comb, sheet = excel$sheet$med_inc, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = excel$cs)
# create missed medication IR vendor sheet ---
excel$sheet$mm_ir$vendor <- createSheet(excel$wb, sheetName = "missed med IR vendor")
addDataFrame(x = agg$mm_ir$ven_comb, sheet = excel$sheet$mm_ir$vendor, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = excel$cs)
# create missed medication IR consumer detail sheet
excel$sheet$mm_ir$con <- createSheet(excel$wb, sheetName = "missed med IR consumer")
addDataFrame(x = agg$mm_ir$con_comb, sheet = excel$sheet$mm_ir$con, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = excel$cs)
# create current medication detail sheet ---
excel$sheet$med <- createSheet(excel$wb, sheetName = "current med detail")
addDataFrame(x = prep$cur_meds, sheet = excel$sheet$med, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs)
# create aggregated data ---
excel$sheet$agg <- createSheet(excel$wb, sheetName="aggregated")
addDataFrame(x = agg$cur_med$results, sheet = excel$sheet$agg, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs)
# add benzos
addDataFrame(x = agg$benzo$provider, sheet = excel$sheet$agg, showNA = FALSE,
  row.names = FALSE, startRow = nrow(agg$cur_med$results) + 3, startColumn = 1,
  colnamesStyle = excel$cs)
# add stimulants
addDataFrame(x = agg$stim$provider, sheet = excel$sheet$agg, showNA = FALSE,
  row.names = FALSE, startRow = nrow(agg$benzo$provider) +
  nrow(agg$cur_med$results) + 5, startColumn = 1, colnamesStyle = excel$cs)
# create benzo_sheet ---
excel$sheet$benzo <- createSheet(excel$wb, sheetName = "benzo meds")
addDataFrame(x = data.table(benzos = aux$benzoList), sheet = excel$sheet$benzo,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs)
# create stim_sheet ---
excel$sheet$stim <- createSheet(excel$wb, sheetName = "stimulant meds")
addDataFrame(x = data.table(stimulants = aux$stimList),
  sheet = excel$sheet$stim, showNA = FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1,  colnamesStyle = excel$cs)
# create antiPys_sheet ---
excel$sheet$antiPys <- createSheet(excel$wb, sheetName = "anti-psych meds")
# add stimList to stim_sheet
addDataFrame(x = data.table(anti_pyschotic = aux$antiPysList),
  sheet = excel$sheet$antiPys, showNA=FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1, colnamesStyle = excel$cs)
# create antiPys_sheet ---
excel$sheet$antidepress <-
  createSheet(excel$wb, sheetName = "anti-depress meds")
addDataFrame(x = data.table(anti_depress = aux$antidepressList),
  sheet = excel$sheet$antidepress, showNA=FALSE, row.names = FALSE,
  startRow = 1, startColumn = 1, colnamesStyle = excel$cs)
### create atypical_sheet ### - sleeping this code per Pat Cowan 6/19/2014
#  atypical_sheet = createSheet(wb, sheetName="atypical meds")
#  # add atypicalList to atypical_sheet
#  addDataFrame(x=atypicalList, sheet=atypical_sheet, showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
#               colnamesStyle=excel$cs)

# create cstsBenzo_sheet ---
excel$sheet$cmh_benzo <- createSheet(excel$wb, sheetName="CMH Benzos")
addDataFrame(x=agg$benzo$drug_dt, sheet = excel$sheet$cmh_benzo,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs)
# create cstsStim_sheet ---
excel$sheet$cmh_stim <- createSheet(excel$wb, sheetName = "CMH Stimulants")
addDataFrame(x = agg$stim$drug_dt, sheet = excel$sheet$cmh_stim,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs)
# create cstsAntipsych_sheet ---
excel$sheet$cmh_antipsych <-
  createSheet(excel$wb, sheetName = "CMH Antipsych")
addDataFrame(x = agg$anti_psych$drug_dt, sheet = excel$sheet$cmh_antipsych ,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs)
### create cstsAntipsych_sheet ###
excel$sheet$cmh_antidepress <-
  createSheet(excel$wb, sheetName = "CSTS Antidepress")
# add cstsAntidepress to cstsAntidepress_sheet
addDataFrame(x = agg$anti_depress$drug_dt, sheet = excel$sheet$cmh_antidepress,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = excel$cs)
# create sheet data information ---
excel$sheet$info <- createSheet(excel$wb, sheetName = "data info")
addDataFrame(x = excel$aboutFile, sheet = excel$sheet$info, showNA = FALSE,
  row.names = TRUE, col.names = FALSE, startRow = 1, startColumn = 1,
  rownamesStyle = excel$cs)
# saving document
if (!dir.exists(proj_wd$results)) {
  dir.create(proj_wd$results)
  p_msg("directory created", proj_wd$results)
}
# save workbook ---
saveWorkbook(wb = excel$wb, file = file.path(proj_wd$results,
  paste("current medicines ", input$today, ".xlsx", sep="")))