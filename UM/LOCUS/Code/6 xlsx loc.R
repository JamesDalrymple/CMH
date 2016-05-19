xl <- new.env(parent = .GlobalEnv)
# create information about the file to share with end-users -------------------
xl$aboutFile <- data.table(
  Last_Updated = format(Sys.Date(), "%m/%d/%Y"),
  data_source = "E2 EMR|PCE 107|SQL|JamesD",
  start_param = input$start_dt, end_param = input$end_dt,
  id = 1)
xl$aboutFile <- melt(xl$aboutFile, id="id", variables=names(xl$aboutFile)[-1])
xl$aboutFile[, id := NULL]
# create current medications workbook ------------------------------------------
xl$wb <- createWorkbook()
# bold option and underline
xl$cs <-
  CellStyle(xl$wb) + Font(xl$wb, isBold = TRUE) + Border()
# admission status wccmh
xl$sheet$adm_stat_wccmh <- createSheet(xl$wb, sheetName = "adm status WCCMH")
addDataFrame(x = agg$adm_status$all, sheet = xl$sheet$adm_stat_wccmh, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xl$cs)
# admission status by program
xl$sheet$adm_stat_mi <- createSheet(xl$wb, sheetName = "adm status prog")
addDataFrame(x = agg$adm_status$prog, sheet = xl$sheet$adm_stat_mi, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xl$cs)
# locus service array 3 months
xl$sheet$loc_3mon_all <- createSheet(xl$wb, sheetName = "locus 3mon WCCMH")
addDataFrame(x = agg$loc_3mon$all, sheet = xl$sheet$loc_3mon_all, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xl$cs)
xl$sheet$loc_3mon_prog <- createSheet(xl$wb, sheetName = "locus 3mon prog")
addDataFrame(x = agg$loc_3mon$prog, sheet = xl$sheet$loc_3mon_prog, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xl$cs)
# locus service array 6 months
xl$sheet$loc_6mon_all <- createSheet(xl$wb, sheetName = "locus 6mon WCCMH")
addDataFrame(x = agg$loc_6mon$all, sheet = xl$sheet$loc_6mon_all, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xl$cs)
xl$sheet$loc_6mon_prog <- createSheet(xl$wb, sheetName = "locus 6mon prog")
addDataFrame(x = agg$loc_6mon$prog, sheet = xl$sheet$loc_6mon_prog, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xl$cs)
# locus level placement WCCMH
xl$sheet$ll_all <- createSheet(xl$wb, sheetName = "locus level WCCMH")
addDataFrame(x = agg$locus_levels$all, sheet = xl$sheet$ll_all, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xl$cs)
# locus level placement program
xl$sheet$ll_prog <- createSheet(xl$wb, sheetName = "locus level prog")
addDataFrame(x = agg$locus_levels$prog, sheet = xl$sheet$ll_prog, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xl$cs)
# create sheet data information ---
xl$sheet$info <- createSheet(xl$wb, sheetName = "data info")
addDataFrame(x = xl$aboutFile, sheet = xl$sheet$info, showNA = FALSE,
  row.names = TRUE, col.names = FALSE, startRow = 1, startColumn = 1,
  rownamesStyle = xl$cs)
# saving document
if (!dir.exists(project_wd$results)) {
  dir.create(project_wd$results)
  p_msg("directory created", project_wd$results)
}
# save workbook ---
saveWorkbook(wb = xl$wb, file = file.path(project_wd$results,
  paste("locus summary datasets ", format(Sys.Date(), "%b_%Y"), ".xlsx", sep="")))