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
project_wd$project <- "CMH/UM/Fund"
project_wd$code <- file.path(project_wd$github, project_wd$project, "Code")
project_wd$data <- file.path(project_wd$dropbox,
                             "Utilization Management/Fund Only/Data")
project_wd$results <- "Utilization Management/Fund Only/Results"
rm(cmh_wd)
# user input ------------------------------------------------------------------
input <- list(
  current_cost_qtr = "2016 Q1",
  run_date = "3/7/2016",
  end_date = "12/31/2015" # data parameter end
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
# source(file.path(project_wd$dropbox, "WCCMH/R/global library.R"))
source(file.path(project_wd$code, "0_service auxillary.R"))
source(file.path(project_wd$code, "1_fund_sql.R"))
source(file.path(project_wd$code, "/partial_fy_costs/2_pfc_service.R"))
source(file.path(project_wd$code, "/partial_fy_costs/3_pfc_export.R"))