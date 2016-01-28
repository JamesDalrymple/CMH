proj_data <- new.env(parent = .GlobalEnv)
# state data files ------------------------------------------------------------
proj_data$state_rates <- fread(file.path(project_wd$data, "state_hosp_fy_rates.csv"))
proj_data$ngri <- fread(file.path(project_wd$data,
                            "NGRI prison monthly numbers.csv"))
proj_data$state_budget <- fread(file.path(project_wd$data, "state hosp budget.csv"))
# overall data files ----------------------------------------------------------
proj_data$fb_files <-
  list.files(project_wd$fb_archives, full.names = TRUE,
             pattern = paste(format(input$min_start, "%m_%d_%y"),
                             format(input$max_end, "%m_%d_%y"), sep = "_to_"))
proj_data$fb_files <-
  grep(x = proj_data$fb_files, pattern = input$rate_set, value = TRUE)

proj_data$fb_len <- length(proj_data$fb_files)

if (proj_data$fb_len < 2) {
  p_stop(proj_data$fb_len, "FB files found, at least two required:",
         proj_data$fb_files)
} else if (proj_data$fb_len > 2) {
  proj_data$fb_files <-
    grep(x = proj_data$fb_files, pattern = paste("ran",
      format(date_convert(input$fb_run_date), "%m_%d_%y")), value = TRUE)
  proj_data$fb_len <- length(proj_data$fb_files)
}
if (proj_data$fb_len != 2) {
  p_stop(proj_data$fb_len, "FB files found where exactly two are expected,
         please investigate!", proj_data$fb_files)
}
fb_data <- rbindlist(list(
  readRDS(proj_data$fb_files[1]),
  readRDS(proj_data$fb_files[2])),
  use.names = TRUE)
message("The FB files read in:\n",
        proj_data$fb_files[1], "\n",
        proj_data$fb_files[2])