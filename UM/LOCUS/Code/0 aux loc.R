pkg_loader(packages = c("ggplot2", "RODBC", "ReporteRs", "xlsx",
                        "stringi", "sqldf"))
aux <- new.env(parent  = .GlobalEnv )

aux$mon_dt <- date_expansion(input$start_dt, input$end_dt, type = "mon")
project_wd$results <- file.path(project_wd$results, input$result_subfolder)
