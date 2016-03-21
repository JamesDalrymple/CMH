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
  project_wd$project <- "CMH/Health Home/ER/ER util levels"
  project_wd$code <- file.path(project_wd$github, project_wd$project, "Code")
  project_wd$data <- file.path(project_wd$github,
                               "CMH/Health Home/ER/ER util levels/Data")
  project_wd$results <- file.path(project_wd$dropbox,
                                  "Health Home Dashboard/ER util levels/Results")
  rm(cmh_wd)
  # user input ------------------------------------------------------------------
  input <- list(
    run_date = Sys.Date(),
    start_date = "10/1/2014",
    end_date = "9/30/2015"
  )

  # load packages, source files -------------------------------------------------
  source(file.path(project_wd$code, "0_er_ul_auxillary.R"))
  source(file.path(project_wd$code, "1_er_ul_sql.R"))
  source(file.path(project_wd$code, "2_er_ul_modify.R"))
  source(file.path(project_wd$code, "3_er_ul_graph.R"))
  source(file.path(project_wd$code, "4a_er_ul_export.R"))
  source(file.path(project_wd$code, "4b_er_ul_xlsx.R"))


