# create workbook -------------------------------------------------------------
wb <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()

# DD Adult ---
dd_sheet <- createSheet(wb, sheetName = "DD")
addDataFrame(
  x = svc_summary[program == "DD"],
  sheet = dd_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# MI Adult ---
mi_sheet <- createSheet(wb, sheetName = "MI")
addDataFrame(
  x = svc_summary[program == "MI"],
  sheet = mi_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# MI - L0 ---
l0_sheet <- createSheet(wb, sheetName = "L0_No_TCM")
addDataFrame(
  x = svc_summary[level == "L0_No_TCM"],
  sheet = l0_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# MI - L1 ---
l1_sheet <- createSheet(wb, sheetName = "L1")
addDataFrame(
  x = svc_summary[level == "L1"],
  sheet = l1_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# MI - L2 ---
l2_sheet <- createSheet(wb, sheetName = "L2")
addDataFrame(
  x = svc_summary[level == "L2"],
  sheet = l2_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# MI - L3 ---
l3_sheet <- createSheet(wb, sheetName = "L3")
addDataFrame(
  x = svc_summary[level == "L3"],
  sheet = l3_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# MI - L4 ACT ---
l4_sheet <- createSheet(wb, sheetName = "L4_ACT")
addDataFrame(
  x = svc_summary[level == "L4_ACT"],
  sheet = l4_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# MI - L5 Residential ---
l5_sheet <- createSheet(wb, sheetName = "L5_Res")
addDataFrame(
  x = svc_summary[level == "L5_Residential"],
  sheet = l5_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# Y&F ---
yf_sheet <- createSheet(wb, sheetName = "Y&F")
addDataFrame(
  x = svc_summary[program == "Y&F"],
  sheet = yf_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# CLS - Community
cls_comm_sheet <- createSheet(wb, sheetName = "CLS_Comm")
addDataFrame(
  x = svc_summary[cpt_desc=="CLS | Community"],
  sheet = cls_comm_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# CLS - Specialized Residential
cls_res_sheet <- createSheet(wb, sheetName = "CLS_Res")
addDataFrame(
  x = svc_summary[cpt_desc=="CLS | Specialized Residential"],
  sheet = cls_res_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# CLS - Home Modification/Housing Assistance
cls_home_sheet <- createSheet(wb, sheetName = "CLS_home")
addDataFrame(
  x = svc_summary[cpt_desc=="Home Modification | Children/Housing Assistance"],
  sheet = cls_home_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# CLS - independent living
cls_ind_sheet <- createSheet(wb, sheetName = "CLS_ind")
addDataFrame(
  x = svc_summary[cpt_desc=="CLS | independent living"],
  sheet = cls_ind_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)
# non-CMH
noncmh_sheet <- createSheet(wb, sheetName = "non-CMH")
addDataFrame(
  x = svc_summary[program == "non-CMH"],
  sheet = noncmh_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3
)

# all funds - skip for now # services[, unique(fund)]
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
addDataFrame(
  x = modify$team_summary,
  sheet = data_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = nrow(about_file)+2,
  startColumn = 1,
  colnamesStyle = cs3
)
addDataFrame(
  x = modify$mi_level_summary,
  sheet = data_sheet,
  showNA = FALSE,
  row.names = FALSE,
  startRow = nrow(about_file)+
    nrow(modify$team_summary)+4,
  startColumn = 1,
  colnamesStyle = cs3
)

# save workbook ---------------------------------------------------------------
if(!dir.exists(file.path(project_wd$results))) {
  dir.create(file.path(project_wd$results))
  paste("folder created", file.path(project_wd$results),
        sep="...")
}
# file name
export_file <- paste0("service_summary_analysis", input$run_par, ".xlsx")

gc(reset = TRUE)
saveWorkbook(wb=wb, file=file.path(project_wd$results,
                                   export_file))
rm(wb)