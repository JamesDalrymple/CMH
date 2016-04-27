library(wccmh)
scrub()
baseWD <- switch(Sys.info()["nodename"],
                 WSHSQLGP = "C:/Users/dalrymplej")
proj_wd <- list(
  git_base = file.path(baseWD, "Documents/GitHub/CMH/Medications"),
  dropbox = file.path(baseWD, "Dropbox/Medications/Results")
) ; rm(baseWD)
proj_wd$data <- file.path(proj_wd$git_base, "Data")
proj_wd$code <- file.path(proj_wd$git_base, "Code")
proj_wd$cur_res <- file.path(proj_wd$dropbox, "Current")
proj_wd$miss_res <- file.path(proj_wd$dropbox, "Missed")
proj_wd$inc_res <- file.path(proj_wd$dropbox, "Incidents")
# user input ---
input <-list(
  report_dt = "4/27/2016",
  start_dt = "10/1/2015",
  end_dt = "3/31/2016",
  today = format(Sys.Date(), "%b_%d_%y")
  )
input$run_dt <- gsub(input$report_dt, pattern = "/", replace = "_")
# sourcing files
source(file.path(proj_wd$code, "0 med auxillary.R"))
source(file.path(proj_wd$code, "1 med sql.R"))
source(file.path(proj_wd$code, "2 med prep.R"))
source(file.path(proj_wd$code, "3 med agg.R"))
source(file.path(proj_wd$code, "4 med graph.R"))
source(file.path(proj_wd$code, "5 med xlsx.R"))