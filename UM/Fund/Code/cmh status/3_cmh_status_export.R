export <- new.env(parent = .GlobalEnv)

# save workbook ---------------------------------------------------------------
if (!dir.exists(file.path(project_wd$results))) {
  dir.create(file.path(project_wd$results))
  paste("folder created", file.path(project_wd$results),
        sep="...")
}
# file name
export$export_file <-
  paste0("Medicaid TPP cmh_status ",
         format(input$run_date, "%m_%d_%y"), ".xlsx")
saveWorkbook(wb = wb_fund, file = file.path(project_wd$results,
                                            export$export_file))
