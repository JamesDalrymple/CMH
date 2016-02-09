# save results --------------------------------------------------------------
### create information about the file to share with end-users ###
aboutFile <- data.table(
  rbind(
    date_range = modify$date_range,
    last_updated = as.chr(Sys.time()),
    num_services = nrow(services),
    num_errors = nrow(time_errors),
    data_source = "Encompass database via SQL & R",
    sql_location = "D:/James/Performance Improvement/Productivity/productivity.sql",
    note1 = "cl_seen for team gives credit if the consumer under the team are seen by anyone.",
    note2 = "cl seen for supervisor and team/supervisor require staff seeing consumer to be assigned to supervisor.",
    note3 = "cl seen for team/supervisor/author require staff to see their own consumers, covering by another staff is not counted."
  ),
  keep.rownames=TRUE
)
setnames(aboutFile, colnames(aboutFile), c("About", "Details"))
# a second data information table for end users
aboutFile2 <- data.table(tab_letters = c("T", "S", "A"),
                         aggregation_by = c("Team", "Supervisor", "Author"))
# create workbook ---
wb <- createWorkbook()
cs3 <- CellStyle(wb) + Font(wb, isBold = TRUE) + Border()
# create sheet TSA ---
sheet_TSA <- createSheet(wb, sheetName = "TSA")
addDataFrame(x = comb_TSA, sheet = sheet_TSA, showNA = FALSE,
             row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create sheet TS ---
sheet_TS <- createSheet(wb, sheetName = "TS")
addDataFrame(x = comb_TS, sheet = sheet_TS, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1,
             colnamesStyle = cs3)
# create sheet T ---
sheet_T <- createSheet(wb, sheetName = "T")
addDataFrame(x=comb_T, sheet=sheet_T, showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
             colnamesStyle=cs3)
# create sheet SA ---
sheet_SA <- createSheet(wb, sheetName = "SA")
addDataFrame(x = comb_SA, sheet = sheet_SA, showNA = FALSE,
             row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# create sheet S ---
sheet_S <- createSheet(wb, sheetName="S")
addDataFrame(x = comb_S, sheet = sheet_S, showNA = FALSE, row.names = FALSE,
             startRow = 1, startColumn = 1, colnamesStyle = cs3)
# create sheet data information ---
sheet_info <- createSheet(wb, sheetName = "data info")
addDataFrame(x = aboutFile, sheet = sheet_info, showNA = FALSE,
             row.names = FALSE, col.names = TRUE, startRow = 1,
             startColumn = 1, colnamesStyle = cs3)
addDataFrame(x = aboutFile2, sheet = sheet_info, showNA = FALSE,
             row.names = FALSE, col.names = TRUE,
             startRow = nrow(aboutFile) + 3, startColumn = 1,
             colnamesStyle = cs3)
# create error worksheet ---
sheet_error <- createSheet(wb, sheetName = "time errors")
addDataFrame(x = time_errors, sheet = sheet_error, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1,
             colnamesStyle = cs3)
# save workbook ---
if (!file.exists(file.path(input$results_wd, modify$current_folder))) {
  dir.create(path = file.path(input$results_wd, modify$current_folder),
             recursive = TRUE)
  print(paste("this folder path created:",
              file.path(input$results_wd, modify$current_folder)))
}
modify$save_name <- paste0("productivity ", input$names[i], " ran ",
                           format(Sys.Date(), "%m_%d_%y"), ".xlsx")
saveWorkbook(wb = wb,
             file = file.path(input$results_wd, modify$current_folder, modify$save_name))
time$end_time <- Sys.time()

print(paste("This result", input$names[i], "ran for",
            round(time$end_time - time$local_start, 2)))