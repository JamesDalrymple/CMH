# initializing working directory and input parameters -------------------------
library(wccmh)
scrub()
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
project_wd$code <- file.path(project_wd$github,
                             project_wd$project, "Code/cmh status")
project_wd$data <- file.path(project_wd$github,
                             project_wd$project, "Data")
project_wd$results <- file.path(project_wd$dropbox,
                                "Utilization Management/Fund Only/Results")
rm(cmh_wd)
# user input ------------------------------------------------------------------
input <- list(run_date = Sys.Date(),
              add_cmh_status = c("Sheri", "John"))
 # for folder
project_wd$results <- file.path(project_wd$results,
                             format(input$run_date, "%m_%d_%y"))
# load packages, source files -------------------------------------------------
source(file.path(project_wd$code, "0_cmh_status_auxillary.R"))
source(file.path(project_wd$code, "1_cmh_status_sql.R"))
source(file.path(project_wd$code, "2_cmh_status_modify.R"))
source(file.path(project_wd$code, "3_cmh_status_export.R"))
