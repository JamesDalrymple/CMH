pkg_loader(packages = c("RODBC", "xlsx"))
aux <- new.env(parent  = .GlobalEnv )

aux$time_dt <- date_expansion(input$start_dt,
  input$end_dt, type = c("qtr", "mon","fy"))