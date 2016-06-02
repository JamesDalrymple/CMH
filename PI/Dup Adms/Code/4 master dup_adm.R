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
  names = c("Jan FY16", "Feb FY16", "Mar FY16"),
  start_dates = c("1/1/2016", "2/1/2016", "3/1/2016"),
  end_dates = c("1/31/2016", "2/29/2016", "3/31/2016"),
  github_wd = "C:/Users/dalrymplej/Documents/GitHub/CMH/PI/Dup Adms",
  dropbox_wd = file.path(base_wd, "Dropbox/PI Projects/Dup Adms"))
input$datawd <- file.path(base_wd, input$dropbox_wd, "Data")
input$code_wd <- file.path(input$github_wd, "Code")
input$results_wd <- file.path(input$dropbox_wd, "Results")
rm(base_wd)

time <- list(
  global_start = Sys.time()
)

source(file.path(input$code_wd, "0 aux dup_adm.R"))
source(file.path(input$code_wd, "1 sql dup_adm.R"))
source(file.path(input$code_wd, "2 modify dup_adm.R"))
source(file.path(input$code_wd, "3 export dup_adm.R"))
