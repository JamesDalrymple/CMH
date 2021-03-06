pkg_loader(packages = c("ggplot2", "RODBC", "ReporteRs", "xlsx",
                        "stringi", "sqldf"))

# project_wd$results <- file.path(project_wd$results, input$result_subfolder)
# if (!dir.exists(project_wd$results)) {
#   dir.create(project_wd$results)
# }


aux <- new.env(parent  = .GlobalEnv )

aux$mon_dt <- date_expansion(input$start_dt, input$end_dt, type = "mon")
project_wd$results <- file.path(project_wd$results, input$result_subfolder)


aux$um_desc <- read.xlsx(file.path(project_wd$github,
  "CMH/UM/UM_Desc_MDCH_2015.xlsx"), 1, stringsAsFactors = FALSE)
aux$um_desc <- data.table(aux$um_desc)
setf(aux$um_desc, j = names(aux$um_desc), stringi::stri_trim)

setnames(aux$um_desc, old = names(aux$um_desc),
         new = tolower(gsub(pattern = "[.]", replace = "_",
                            x = names(aux$um_desc))))
aux$um_desc[, state_svc_desc := NULL]
