export <- new.env(parent = .GlobalEnv)
# save results --------------------------------------------------------------
### create information about the file to share with end-users ###
export$about_file <- data.table(
  rbind(
    run_date = format(input$run_date, "%m_%d_%y"),
    start_date = input$start_date,
    end_date = input$end_date,
    data_source = "Encompass database via SQL & R",
    sql_location = "~GitHub/CMH/Health Home/ER/ER util levels.R"),
  keep.rownames = TRUE
)
setnames(export$about_file, names(export$about_file), c("About", "Details"))
# create workbook ---
wb <- createWorkbook()
cs3 <- CellStyle(wb) + Font(wb, isBold = TRUE) + Border()
# create sheet ER visits ---
sheet_er <- createSheet(wb, sheetName = "ER")
addDataFrame(x = modify$er_agg, sheet = sheet_er, showNA = FALSE,
             row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
sheet_l0 <- createSheet(wb, sheetName = "ER_L0")
addDataFrame(x = modify$er_l0, sheet = sheet_l0, showNA = FALSE,
             row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create sheet movement ---
sheet_movement <- createSheet(wb, sheetName = "movement")
addDataFrame(x = modify$lvl_agg, sheet = sheet_movement, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1,
             colnamesStyle = cs3)
# create sheet data information ---
sheet_info <- createSheet(wb, sheetName = "data info")
addDataFrame(x = export$about_file, sheet = sheet_info, showNA = FALSE,
             row.names = FALSE, col.names = TRUE, startRow = 1,
             startColumn = 1, colnamesStyle = cs3)
# save workbook ---
if (!file.exists(file.path(project_wd$results))) {
  dir.create(path = file.path(project_wd$results),
             recursive = TRUE)
  print(paste("this folder path created:",
              file.path(project_wd$results)))
}
export$save_name <- paste0("ER_util ", " ran ",
                           format(Sys.Date(), "%m_%d_%y"), ".xlsx")
saveWorkbook(wb = wb,
             file = file.path(project_wd$results, export$save_name))
