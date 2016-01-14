# data_wd without month (up 1 level)
input$data_fy_wd <- substr(input$data_wd,
                           1, regexpr("/[^/]*$", text=input$data_wd)[1]-1)

# create results directory if does not exist
if (!dir.exists(input$results_wd)) {
  dir.create(input$results_wd)
  print(paste("created directory:", input$results_wd))
}

# create data directory if does not exist
if (!dir.exists(input$data_wd)) {
  # load most recent teamStaffLevels.csv
  aux$fy_mons_completed <-
    grep(x=list.files(input$data_fy_wd),
         pattern=".csv", invert = TRUE, value=TRUE)
  aux$recent_mon <- (as.yearmon(paste("Oct", as.numeric(calendar_year)-1))+
    0:(length(aux$fy_mons_completed)-1)/12)[aux$length(fy_mons_completed)]
  dir.create(input$data_wd, recursive = TRUE)
  print(paste("created directory:", input$data_wd))
  if(length(fy_mons_completed) > 0) {
    team_staff_levels <- fread(list.files(list.files(input$data_fy_wd,
      pattern=substr(aux$recent_mon, 1, 3), full.names = TRUE),
      pattern="team", full.names = TRUE))
  } else {
    team_staff_levels <- fread(list.files(list.files(
      file.path(input$dropbox_wd, "Data/Fiscal Year", input$calendar_year),
      pattern="September", full.names = TRUE),
      pattern="team", full.names = TRUE))
  }
  write.csv(team_staff_levels, file.path(input$data_wd,
                                         "teamStaffLevels.csv"), row.names = FALSE)
  print(paste("team_staff_levels added:",
              file.path(input$data_wd, "teamStaffLevels.csv")))
}