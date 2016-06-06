if (!dir.exists(input$results_wd)) {
  dir.create(input$results_wd, recursive = TRUE)
}
write.csv(adm, file.path(input$results_wd,
  "CMH core teams duplicate admissions.csv"), row.names = FALSE)
