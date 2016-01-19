# wage file -------------------------------------------------------------------
wb_error <- createWorkbook()
cs3 <- CellStyle(wb_error) + Font(wb_error, isBold = TRUE) + Border()
# create wage error sheet
sheet_detail <- createSheet(wb_error, sheetName = "wage_detail")
  addDataFrame(x = wage_error_list, sheet = sheet_detail, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create ACT sheet
sheet_ACT <- createSheet(wb_error, sheetName = "ACT")
addDataFrame(
  x = wage_error_list[team == "ACT"], sheet = sheet_ACT, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create MI Adult sheet
sheet_MI <- createSheet(wb_error, sheetName = "MI Adult")
addDataFrame(x = wage_error_list[team == "MI Adult"], sheet = sheet_MI,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create DD Adult sheet
sheet_DD <- createSheet(wb_error, sheetName = "DD Adult")
addDataFrame(
  x = wage_error_list[team == "DD Adult"],
  sheet = sheet_DD, showNA = FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1, colnamesStyle = cs3)
# create Child sheet
sheet_Ch <- createSheet(wb_error, sheetName = "Child")
addDataFrame(x = wage_error_list[team == "Child"],
  sheet = sheet_Ch, showNA = FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1, colnamesStyle = cs3)
# create Child HB sheet
sheet_Ch_HB <- createSheet(wb_error, sheetName = "Child HB")
addDataFrame(x = wage_error_list[team == "Child Home Based"],
  sheet = sheet_Ch_HB, showNA = FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1, colnamesStyle = cs3)
# save workbook
saveWorkbook(wb = wb_error,
  file = file.path(input$results_wd, "wage errors.xlsx"))
rm(wb_error)
# health file -----------------------------------------------------------------
wb_health <- createWorkbook()
cs3 <- CellStyle(wb_health) + Font(wb_health, isBold = TRUE) + Border()
# create health error sheet
sheet_detail <- createSheet(wb_health, sheetName = "health_detail")
addDataFrame(x = health_error_list, sheet = sheet_detail, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create ACT sheet
sheet_ACT <- createSheet(wb_health, sheetName = "ACT")
addDataFrame(x = health_error_list[team == "ACT"], sheet = sheet_ACT,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create MI Adult sheet
sheet_MI <- createSheet(wb_health, sheetName = "MI Adult")
addDataFrame(x=health_error_list[team == "MI Adult"], sheet = sheet_MI,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create DD Adult sheet
sheet_DD <- createSheet(wb_health, sheetName = "DD Adult")
addDataFrame(x = health_error_list[team == "DD Adult"], sheet = sheet_DD,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create Child sheet
sheet_Ch <- createSheet(wb_health, sheetName = "Child")
addDataFrame(x = health_error_list[team == "Child"], sheet = sheet_Ch,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create Child HB sheet
sheet_Ch_HB <- createSheet(wb_health, sheetName = "Child HB")
addDataFrame(x = health_error_list[team == "Child Home Based"],
  sheet = sheet_Ch_HB, showNA = FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1, colnamesStyle = cs3)
# save workbook
saveWorkbook(wb = wb_health, file = file.path(input$results_wd,
  "health errors.xlsx"))
rm(wb_health)
# demo file -------------------------------------------------------------------
wb_demo <- createWorkbook()
cs3 <- CellStyle(wb_demo) + Font(wb_demo, isBold = TRUE) + Border()
# create demo error sheet
sheet_detail <- createSheet(wb_demo, sheetName = "demo_detail")
addDataFrame(x = demo_error_list, sheet = sheet_detail, showNA = FALSE,
  row.names=FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create ACT sheet
sheet_ACT <- createSheet(wb_demo, sheetName = "ACT")
addDataFrame(x = demo_error_list[team == "ACT"], sheet = sheet_ACT,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create MI Adult sheet
sheet_MI <- createSheet(wb_demo, sheetName = "MI Adult")
addDataFrame(x = demo_error_list[team == "MI Adult"], sheet = sheet_MI, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create DD Adult sheet
sheet_DD <- createSheet(wb_demo, sheetName = "DD Adult")
addDataFrame(x = demo_error_list[team == "DD Adult"], sheet = sheet_DD, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create Child sheet
sheet_Ch <- createSheet(wb_demo, sheetName = "Child")
addDataFrame(x = demo_error_list[team == "Child"], sheet = sheet_Ch, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create Child HB sheet
sheet_Ch_HB <- createSheet(wb_demo, sheetName = "Child HB")
addDataFrame(x = demo_error_list[team == "Child Home Based"],
  sheet = sheet_Ch_HB, showNA = FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1, colnamesStyle = cs3)
## save workbook ##
saveWorkbook(wb = wb_demo,
  file = file.path(input$results_wd, "demo errors.xlsx"))
rm(wb_demo)
# services file ----------------------------------------------------------------
wb_service <- createWorkbook()
cs3 <- CellStyle(wb_service) + Font(wb_service, isBold = TRUE) + Border()
# create no services sheet
sheet_detail <- createSheet(wb_service, sheetName = "services_detail")
addDataFrame(x = services_list, sheet = sheet_detail, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create ACT sheet
sheet_ACT <- createSheet(wb_service, sheetName = "ACT")
addDataFrame(x = services_list[team == "ACT"], sheet = sheet_ACT,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create MI Adult sheet
sheet_MI <- createSheet(wb_service, sheetName = "MI Adult")
addDataFrame(x = services_list[team == "MI Adult"], sheet = sheet_MI,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create DD Adult sheet
sheet_DD <- createSheet(wb_service, sheetName = "DD Adult")
addDataFrame(x = services_list[team == "DD Adult"], sheet = sheet_DD,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create Child sheet
sheet_Ch <- createSheet(wb_service, sheetName = "Child")
addDataFrame(x = services_list[team == "Child"], sheet = sheet_Ch,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create Child HB sheet
sheet_Ch_HB <- createSheet(wb_service, sheetName = "Child HB")
addDataFrame(x = services_list[team == "Child Home Based"],
  sheet = sheet_Ch_HB, showNA = FALSE, row.names = FALSE, startRow = 1,
  startColumn = 1, colnamesStyle = cs3)
# save workbook
saveWorkbook(wb = wb_service,
  file = file.path(input$results_wd, "no service.xlsx"))
rm(wb_service)
# ipos file -------------------------------------------------------------------
wb_ipos <- createWorkbook()
cs3 <- CellStyle(wb_ipos) + Font(wb_ipos, isBold = TRUE) + Border()
# create ipos sheet
sheet_detail <- createSheet(wb_ipos, sheetName = "ipos_detail")
addDataFrame(x = modify$ipos_list, sheet = sheet_detail, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create ACT sheet
sheet_ACT <- createSheet(wb_ipos, sheetName = "ACT")
addDataFrame(x = modify$ipos_list[team == "ACT"], sheet = sheet_ACT, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create MI Adult sheet
sheet_MI <- createSheet(wb_ipos, sheetName = "MI Adult")
addDataFrame(x = modify$ipos_list[team == "MI Adult"], sheet = sheet_MI,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create DD Adult sheet
sheet_DD <- createSheet(wb_ipos, sheetName = "DD Adult")
addDataFrame(x = modify$ipos_list[team == "DD Adult"], sheet = sheet_DD,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
# create Child sheet
sheet_Ch <- createSheet(wb_ipos, sheetName = "Child")
addDataFrame(x = modify$ipos_list[team == "Child"], sheet = sheet_Ch, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create Child HB sheet
sheet_Ch_HB <- createSheet(wb_ipos, sheetName = "Child HB")
addDataFrame(x = modify$ipos_list[team == "Child Home Based"], sheet = sheet_Ch_HB,
  showNA = FALSE, row.names = FALSE, startRow = 1, startColumn = 1,
  colnamesStyle = cs3)
## save workbook ##
saveWorkbook(wb = wb_ipos, file = file.path(input$results_wd, "ipos.xlsx"))
rm(wb_ipos)
# Self Sufficiency Matrix (SSM) -----------------------------------------------
wb_ssm <- createWorkbook()
cs3 <- CellStyle(wb_ssm) + Font(wb_ssm, isBold = TRUE) + Border()
# ssm_tsa
sheet_ssm_tsa <- createSheet(wb_ssm, sheetName = "ssm_tsa")
addDataFrame(x = ssm_tsa, sheet = sheet_ssm_tsa, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# ssm_ts
sheet_ssm_ts <- createSheet(wb_ssm, sheetName = "ssm_ts")
addDataFrame(x = ssm_ts, sheet = sheet_ssm_ts, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs3)
# ssm detail
sheet_detail <- createSheet(wb_ssm, sheetName = "ssm_detail")
addDataFrame(x = ssm, sheet = sheet_detail, showNA = FALSE, row.names = FALSE,
  startRow = 1, startColumn = 1, colnamesStyle = cs3)
## save workbook ##
saveWorkbook(wb = wb_ssm, file = file.path(input$results_wd, "ssm.xlsx"))
rm(wb_ssm)