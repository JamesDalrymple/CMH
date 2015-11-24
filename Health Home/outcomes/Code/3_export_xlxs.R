#### create workbook ####
wb <- createWorkbook()
# bold option and underline
cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()

### create xx ###
xx_sheet <- createSheet(wb, sheetName="xx")
addDataFrame(x=xx, sheet=xx_sheet, showNA=FALSE,
             row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
### create sheet data information ###
sheet_info = createSheet(wb, sheetName="Data Info")
addDataFrame(x=about_file, sheet=sheet_info, showNA=FALSE,
             row.names=FALSE, col.names=TRUE, startRow=1,
             startColumn = 1, colnamesStyle=cs3)

  ### save workbook ###
  # create result folder
  ### R 3.2.0 approach
  if(!dir.exists(file.path(project_wd$results))) {
    dir.create(file.path(project_wd$results))
    paste("folder created", file.path(project_wd$results),
          sep="...")
  }

  # file name
  export_file <- paste0(
    aux$fund_name(keep_funds),
    "_service_array_", input$run_par, ".xlsx")

  gc(reset = TRUE)
  saveWorkbook(wb=wb, file=file.path(project_wd$results,
                                     export_file))
  print(paste(keep_funds, "completed"))
  rm(wb)
}
