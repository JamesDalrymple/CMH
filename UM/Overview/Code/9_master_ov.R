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
project_wd$project <- "CMH/UM/Overview"
project_wd$code <- file.path(project_wd$github, project_wd$project, "Code")
project_wd$results <- file.path(project_wd$dropbox,
  "Utilization Management/UM Monthly Reports/Results")
project_wd$data <- file.path(project_wd$github, project_wd$project, "Data")
project_wd$fb_archives <- "G:/CSTS Data Analyst Archives/FB_archives/rds"
rm(cmh_wd)
# user input ------------------------------------------------------------------
input <- list(
  report_date = format(Sys.Date(), "%m_%d_%y"),
  months = c("October", "November", "December"),
  fy = "2016", rate_set = "fye14", fb_run_date = "1/28/2016")
# load packages, source files -------------------------------------------------
source(file.path(project_wd$code, "0_auxillary_ov.R"))
source(file.path(project_wd$code, "1_sql_ov.R"))
source(file.path(project_wd$code, "2_data_ov.R"))
source(file.path(project_wd$code, "3_prepare_ov.R"))
source(file.path(project_wd$code, "4a_modify_state_hosp.R"))

# source(file.path(project_wd$code, "4a_modify_comm_hosp.R"))
# source(file.path(project_wd$code, "7_graphs_ov.R"))
# source(file.path(project_wd$code, "8_export_xlsx.R"))
