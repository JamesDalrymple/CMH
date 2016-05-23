# initializing working directory and input parameters ------------------------ #
library(wccmh)
scrub() # clear RAM and hidden environments
# which computer results in correct base working directory
cmh_wd <-
  data.table(expand.grid(stringsAsFactors = FALSE,
    dir_names = c("Dropbox", "GitHub"),
    comp_names = c("WSHSQLGP", "DESKTOP-45K7RRN", "JAMES-2"),
    base = "filler"))
setkey(cmh_wd, dir_names, comp_names)[
  J("Dropbox", "WSHSQLGP"), base := "C:/Users/dalrymplej/Dropbox"]
setkey(cmh_wd, dir_names, comp_names)[
  J("GitHub", "WSHSQLGP"), base := "C:/Users/dalrymplej/Documents/GitHub"]
project_wd <- list()
project_wd$github <- cmh_wd[J("GitHub", "WSHSQLGP"), base]
project_wd$dropbox <- cmh_wd[J("Dropbox", "WSHSQLGP"), base]
project_wd$project <- "CMH/UM/LOCUS"
project_wd$code <- file.path(project_wd$github, project_wd$project, "Code")
project_wd$results <- file.path(project_wd$dropbox,
  "Utilization Management/LOCUS/Results")
project_wd$data <- file.path(project_wd$github, project_wd$project, "Data")
project_wd$fb_archives <- "G:/CSTS Data Analyst Archives/FB_archives/rds"
rm(cmh_wd)
# user input ------------------------------------------------------------------
input <- list(
  start_dt = "10/1/2013", end_dt = "4/30/2016",
  report_date = format(Sys.Date(), "%m_%d_%y"),
  result_subfolder = "May 2016 v3",
  locus_range = 30,
  fy = "2016")

# load packages, source files -------------------------------------------------
source(file.path(project_wd$code, "0 aux loc.R"))
source(file.path(project_wd$code, "1 sql loc.R"))
source(file.path(project_wd$code, "2 modify loc.R"))
source(file.path(project_wd$code, "3 prep loc.R"))
source(file.path(project_wd$code, "4 agg loc.R"))
source(file.path(project_wd$code, "5 graph loc.R"))
source(file.path(project_wd$code, "6 xlsx loc.R"))
source(file.path(project_wd$code, "7 word loc.R"))