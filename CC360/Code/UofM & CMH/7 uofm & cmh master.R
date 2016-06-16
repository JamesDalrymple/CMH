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
project_wd <- list(
  github = cmh_wd[J("GitHub", "WSHSQLGP"), base],
  results = "W:/E2_Health_Home_Raw_data/UofM Med Match")

# project_wd$dropbox <- cmh_wd[J("Dropbox", "WSHSQLGP"), base]
project_wd$code <- file.path(project_wd$github, "CMH/CC360/Code/UofM & CMH")
rm(cmh_wd)


# user input ------------------------------------------------------------------
input <- list(
  start_dt = '10/1/2014',
  end_dt = '9/30/2015'
)
# load packages, source files -------------------------------------------------

source(file.path(project_wd$code, "0 uofm & cmh aux.R"))
source(file.path(project_wd$code, "1 uofm & cmh sql.R"))
source(file.path(project_wd$code, "2 uofm & cmh modify.R"))
source(file.path(project_wd$code, "3 uofm & cmh agg.R"))
source(file.path(project_wd$code, "4 uofm & cmh export.R"))