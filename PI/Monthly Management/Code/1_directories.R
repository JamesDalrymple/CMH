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
  dir.create(input$data_wd, recursive = TRUE)
  print(paste("created directory:", input$data_wd))
}

if (!file.exists(file.path(input$data_wd, "teamStaffLevels.csv"))) {
  # load most recent teamStaffLevels.csv
  aux$fy_mons_completed <-
    grep(x=list.files(input$data_fy_wd, full.names = TRUE),
         pattern=".csv", invert = TRUE, value=TRUE)
  aux$fy_mons_completed <-
    lapply(aux$fy_mons_completed, function(x) {
      r_file <- list.files(x, pattern = "teamStaffLevels.csv")
      if (length(r_file) >= 1) {return(x)} else {return(NULL)}
      })
  aux$fy_mons_completed <- lapply(aux$fy_mons_completed,
         FUN = function(x) {
           pieces <- unlist(strsplit(x, split = "/"))
           last_p <- pieces[length(pieces)]
           if (last_p %in% c("October", "November", "December")) {
             result <- as.yearmon(paste(last_p, as.num(input$current_fy)-1))
           } else {
             result <- as.yearmon(paste(last_p, input$current_fy))
           }
           return(result)
         })
  aux$most_recent_month <- as.yearmon(max(unlist(aux$fy_mons_completed)))
  aux$most_recent_month <-
    file.path(input$data_fy_wd, format(aux$most_recent_month, "%B"))

  if (input$current_month == "October") {
    team_staff_levels <- fread(list.files(list.files(
      file.path(input$dropbox_wd, "Data/Fiscal Year", input$calendar_year),
      pattern="September", full.names = TRUE),
      pattern="teamStaffLevels.csv", full.names = TRUE))
  } else {
    team_staff_levels <- fread(list.files(
      aux$most_recent_month, pattern="teamStaffLevels.csv", full.names = TRUE))
  }
  write.csv(team_staff_levels, file.path(input$data_wd,
    "teamStaffLevels.csv"), row.names = FALSE)
  print(paste("team_staff_levels added:",
    file.path(input$data_wd, "teamStaffLevels.csv")))
}