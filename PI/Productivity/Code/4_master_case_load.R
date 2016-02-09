#### initializing working directory and input parameters ####
library(wccmh)
scrub()
# which computer results in correct base working directory
base_wd <- switch(Sys.info()["nodename"],
                 "JAMES" = "B:",
                 "JAMES-PC" = "D:",
                 "CMHJ3DTVZ1-LT" = "C:/Users/dalrymplej",
                 "WSHSQLGP" = "C:/Users/dalrymplej")
# read in source file - personal library
# source(file.path(base_wd, "Dropbox/WCCMH/R/begin script R code.r") )

# inputs ----------------------------------------------------------------------
input <- list(
  fy = "2016",
  names = c("Jul FY15", "Aug FY15", "Sep FY15"),
  start_dates = c("7/1/2015", "8/1/2015", "9/1/2015"),
  end_dates = c("7/31/2015", "8/31/2015", "9/30/2015"),
  github_wd = "C:/Users/dalrymplej/Documents/GitHub/CMH/PI/Productivity",
  dropbox_wd = file.path(base_wd, "Dropbox/PI Projects/Case Load"))
input$datawd <- file.path(base_wd, input$dropbox_wd, "Data")
input$code_wd <- file.path(input$github_wd, "Code")
input$results_wd <- file.path(input$dropbox_wd, "Results")
rm(base_wd)

time <- list(
  global_start = Sys.time()
)

source(file.path(input$code_wd, "0_auxillary_case_load.R"))
source(file.path(input$code_wd, "2_modify_case_load.R"))
