## to do:: remove state hospital consumers from all spreadsheets
library(wccmh)
scrub()
# initializing working directory and input parameters -------------------------
rm(list = ls()) # clear RAM
# which computer results in correct base working directory
cmh_wd <-
  data.table::data.table(expand.grid(stringsAsFactors = FALSE,
                                     dir_names = c("Dropbox", "GitHub"),
                                     comp_names = c("WSHSQLGP", "DESKTOP-45K7RRN", "JAMES-2"),
                                     base = "filler"))
data.table::setkey(cmh_wd, dir_names, comp_names)[
  J("Dropbox", "WSHSQLGP"), base := "C:/Users/dalrymplej/Dropbox"]
data.table::setkey(cmh_wd, dir_names, comp_names)[
  J("GitHub", "WSHSQLGP"), base := "C:/Users/dalrymplej/Documents/GitHub"]
project_wd <- list()
project_wd$github <- cmh_wd[J("GitHub", "WSHSQLGP"), base]
project_wd$dropbox <- cmh_wd[J("Dropbox", "WSHSQLGP"), base]
project_wd$project <- "CMH/PI/Hospitalizations"

project_wd$code <- file.path(project_wd$github, project_wd$project,
                             "Code/prescreens and hosp")
project_wd$results <- file.path(project_wd$dropbox,
  "PI Projects/Hospitalizations/Results/12_01_15")
rm(cmh_wd)
# user input ------------------------------------------------------------------
input <- list(
  report_date = "12_01_15",
  start_date = '10/1/2014',
  end_date = '9/30/2015')

# load packages, source files -------------------------------------------------
source(file.path(project_wd$code, "0_aux_compare.R"))
source(file.path(project_wd$code, "1_sql_compare.R"))
source(file.path(project_wd$code, "2_modify_compare.R"))
source(file.path(project_wd$code, "3_export_xlsx.R"))

