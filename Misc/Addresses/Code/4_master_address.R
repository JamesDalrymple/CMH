library(wccmh)
# initializing working directory and input parameters ------------------------ #
scrub() # clear RAM and hidden environments
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
project_wd$project <- "CMH/Misc/Addresses"
project_wd$code <- file.path(project_wd$github, project_wd$project, "Code")
project_wd$results <- file.path(project_wd$dropbox,
                                "CMH/Misc/Addresses/Results")
project_wd$data <- file.path(project_wd$github, project_wd$project,
                             "Data")
rm(cmh_wd)
# user input ------------------------------------------------------------------
input <- list(
  report_date = format(Sys.Date(), "%m_%d_%y"),
  end_date = Sys.Date(),
  service_days = 365)
input$start_date <- input$end_date - input$service_days
# load packages, source files -------------------------------------------------
source(file.path(project_wd$code, "0_aux_address.R"))
source(file.path(project_wd$code, "1_sql_voc_address.R"))
source(file.path(project_wd$code, "2_modify_address.R"))
source(file.path(project_wd$code, "3_export_address.R"))
