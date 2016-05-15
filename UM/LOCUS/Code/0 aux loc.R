pkg_loader(packages = c("ggplot2", "RODBC", "ReporteRs", "xlsx",
                        "stringi", "sqldf"))
aux <- new.env(parent  = .GlobalEnv )

aux$mon_dt <- date_expansion(input$start_dt, input$end_dt, type = "mon")
project_wd$results <- file.path(project_wd$results, input$result_subfolder)


aux$um_desc <- read.xlsx(file.path(project_wd$github,
                                   "CMH/UM/UM_Desc_MDCH_2015.xlsx"), 1)
aux$um_desc <- data.table(aux$um_desc)

setnames(aux$um_desc, old = names(aux$um_desc),
         new = tolower(gsub(pattern = "[.]", replace = "_",
                            x = names(aux$um_desc))))
aux$um_desc[, state_svc_desc := NULL]
