# Initialization --------------------------------------------------------------
library(wccmh)
scrub()
# which computer results in correct base working directory
base_wd <- switch(Sys.info()["nodename"],
                  "JAMES" = "B:",
                  "JAMES-PC" = "D:",
                  "CMHJ3DTVZ1-LT" = "C:/Users/dalrymplej",
                  "WSHSQLGP" = "C:/Users/dalrymplej")
# input -----------------------------------------------------------------------
input <- list(cafas_file = "CAFAS_FY15.csv",
              pecfas_file = "PECFAS_FY15.csv",
              fy_value = "DHIPFY15")
project_wd <- list(dropbox = file.path(base_wd, "Dropbox"),
                   github = file.path(base_wd, "Documents/GitHub/CMH"))
project_wd$data_wd <- file.path(project_wd$github, "Misc/DHIP/Data")
project_wd$results_wd <- file.path(project_wd$github, "Misc/DHIP/Results")
project_wd$code_wd <- file.path(project_wd$github, "Misc/DHIP/Code")

source(file.path(project_wd$code_wd, "1_read_yf.R"))
source(file.path(project_wd$code_wd, "2a_cafas_yf.R"))
source(file.path(project_wd$code_wd, "2b_pecfas_yf.R"))
source(file.path(project_wd$code_wd, "3_export_yf.R"))

