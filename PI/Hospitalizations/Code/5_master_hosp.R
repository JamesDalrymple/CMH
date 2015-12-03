## to do:: remove state hospital consumers from all spreadsheets

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
project_wd$code <- file.path(project_wd$github, project_wd$project, "Code")
project_wd$results <- file.path(project_wd$dropbox,
                                "PI Projects/Hospitalizations/Results")
project_wd$data <- file.path(project_wd$github, project_wd$project,
                                "Data")
rm(cmh_wd)
# user input ------------------------------------------------------------------
input <- list(
  report_date = "12_01_15",
  # report_date = format(Sys.Date(), "%m_%d_%y"),
  start_date = '10/1/2010',
  end_date = '9/30/2015'
)
# for folder
project_wd$data <- file.path(project_wd$data,
                             gsub(
                               x = input$run_date,
                               pattern = "/",
                               replace = "_"
                             ))
# load packages, source files -------------------------------------------------
library(wccmh)
source(file.path(project_wd$code, "0_auxillary_hosp.R"))
source(file.path(project_wd$code, "1_sql_hosp.R"))
source(file.path(project_wd$code, "2_base_hosp.R"))
source(file.path(project_wd$code, "3_export_xlsx.R"))
