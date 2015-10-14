## to do:: remove state hospital consumers from all spreadsheets

# initializing working directory and input parameters -------------------------
rm(list = ls()) # clear RAM
# which computer results in correct base working directory
cmh_wd <-
  data.table::data.table(expand.grid(stringsAsFactors = FALSE,
    dir_names = c("dropbox", "github"),
    comp_names = c("WSHSQLGP", "DESKTOP-45K7RRN", "JAMES-2"),
    base = "filler"))
setkey(cmh_wd, dir_names, comp_names)[
  J("Dropbox", "WSHSQLGP"), base := "C:/Users/dalrymplej/Dropbox"]
setkey(cmh_wd, dir_names, comp_names)[
  J("GitHub", "WSHSQLGP"), base := "C:/Users/dalrymplej/Documents/GitHub"]
project_wd <- list()
project_wd$github <- cmh_wd[J("github", "WSHSQLGP"), base]
project_wd$dropbox <- cmh_wd[J("dropbox", "WSHSQLGP"), base]
project_wd$project <- "UM/Fund"
project_wd$code <- file.path(wd$github, wd$project, "Code")
project_wd$data <- "Utilization Management/Fund Only/Data"
project_wd$results <- "Utilization Management/Fund Only/Results"
rm(cmh_wd)
# user input ------------------------------------------------------------------
run_date <- "10/14/2015" # for folder
end_date <- "8/31/2015" # data parameter end
# filtering
all_funds <- services[, unique(fund)]
# load packages, source files -------------------------------------------------
library(EquaPac)
# library(CMH) ... replacing global script
source(file.path(project_wd$code, "0_service auxillary.R"))
source(file.path(project_wd$code, "1_base_service.R"))
source(file.path(project_wd$code, "2_export_xlxs.R"))


