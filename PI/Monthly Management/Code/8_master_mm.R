#### initializing working directory and input parameters ####
library(wccmh)
scrub()
# which computer results in correct base working directory
base_wd <- switch(Sys.info()["nodename"],
                  "JAMES-2" = "B:",
                  "JAMES" = "B:", # laptop
                  "JAMES-PC" = "B:", # home PC
                  "CMHJ3DTVZ1-LT" = "C:/Users/dalrymplej",
                  "WSHSQLGP" = "C:/Users/dalrymplej" # county PC
)
input <- list(
  current_month = "December",
  current_fy = "2016",
  run_date = "1/4/2016",
  github_wd = file.path(base_wd,
                        "Documents/GitHub/CMH/PI/Monthly Management"),
  dropbox_wd = file.path(base_wd,
                         "Dropbox/PI Projects/Monthly Management")
)
rm(base_wd)
input$data_wd <- file.path(input$dropbox_wd, "Data/Fiscal Year",
                           input$current_fy, input$current_month)
input$code_wd <- file.path(input$github_wd, "Code")
input$results_wd <- file.path(input$dropbox_wd, "Results/Fiscal Year",
                              input$current_fy, input$current_month)
input$run_underscore <- gsub(input$run_date, pattern = "/", replacement = "_")

########### NOT EVEN CLOSE TO WORKING YET!!!! #################################

knitr::knit(input = file.path(input$code_wd, "LaTeX/7_knitr_mm.Rnw"),
            output = file.path(input$code_wd, "LaTeX/test.txt"),
            envir = globalenv())
tools::texi2pdf(file = file.path(input$code_wd, "LaTeX/test.txt"))

