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
project_wd$project <- "CMH/Health Home/outcomes"
project_wd$code <- file.path(project_wd$github, project_wd$project, "Code")
project_wd$data <- file.path(project_wd$dropbox,
                             "CMH/Health Home/outcomes/Data")
project_wd$results <- "CMH/Health Home/outcomes/Results"
rm(cmh_wd)
# user input ------------------------------------------------------------------
input <- list(
  run_date = Sys.Date(),
  end_date = Sys.Date() # data parameter end
)
 # for folder
project_wd$data <- file.path(project_wd$data,
                             gsub(
                               x = input$run_date,
                               pattern = "/",
                               replace = "_"
                             ))
# load packages, source files -------------------------------------------------
# library(CMH) ... replacing global script at some point
# source(file.path(project_wd$dropbox, "WCCMH/R/global library.R"))
source(file.path(project_wd$code, "0_outcome auxillary.R"))
source(file.path(project_wd$code, "1_outcome_sql.R"))
source(file.path(project_wd$code, "2_outcome_base.R"))
source(file.path(project_wd$code, "3_export_xlxs.R"))