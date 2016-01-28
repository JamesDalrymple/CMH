#### initializing working directory and input parameters ####

# which computer results in correct base working directory
baseWD <- switch(Sys.info()["nodename"],
                 "JAMES" = "B:",
                 "JAMES-PC" = "D:",
                 "CMHJ3DTVZ1-LT" = "C:/Users/dalrymplej",
                 "WSHSQLGP" = "C:/Users/dalrymplej")

# read in source files
source(file.path(baseWD, "Dropbox/WCCMH/R/begin script R code.r"))
initialWD <- "Dropbox/Utilization Management/UM Monthly Reports"
source(file.path(baseWD, initialWD, "R Code/Overall Picture/ovr auxillary.r"))

## current date parameters ##
# current fiscal year
fiscalYear <- "2016"
# current month list for which months need to be ran
monthList <- c("October", "November", "December")
# type: initial or recent
type <- "initial"
# set working directory for data upload and aux code - same no matter the month
type <- tolower(type)
project <- "Overall Picture" # user input required
initialWD <- "Dropbox/Utilization Management/UM Monthly Reports"

### sql connection ###
channel <- odbcConnect("wshsqlgp")
odbcQuery(channel=channel, query="use James_CSTS")

for(m in seq_along(monthList)) {
  t1 <- Sys.time()
  ### dates ###
  enddate <- format(as.Date(as.yearmon(
    paste(monthList[m], fiscalYear)), frac=0), "%m/%d/%Y")
  three_yrs_ago <- format(dateConvert(enddate)-365*3+1, "%m/%d/%Y")
  last3qtrs <- sqlQuery(channel=channel,
                        query=paste0("select * from dbo.fn_last_3years_qtrs('", enddate, "')"), stringsAsFactors=FALSE, max=0)
  last3qtrs <- data.table(last3qtrs)
  last3fys <- sqlQuery(channel=channel,
                       query=paste0("select * from dbo.fn_last_3years_fys('", enddate, "')"), stringsAsFactors=FALSE, max=0)
  last3fys <- data.table(last3fys)
  t1 <- Sys.time()
  dataWD <- file.path(baseWD, initialWD, "Data",
                      project, paste("fy", fiscalYear), monthList[m], type)
  resultsWD <- file.path(baseWD, initialWD, "Results",
                         project, paste("fy", fiscalYear), monthList[m], type)

  # create folders if they are not there
  if( !file.exists(dataWD)) {
    dir.create(dataWD, recursive=TRUE)
  }
  if( !file.exists(resultsWD)) {
    dir.create(resultsWD, recursive=TRUE)
  }
  ### fund ###
  sql_fund <-
    paste0("select * from E2_Funding_Bucket.dbo.FB_fn_con_served_Fund('",
           enddate, "')")
  fund_data <- sqlQuery(channel=channel, query=sql_fund)
  fund_data <- data.table(fund_data)
  fund_data[, span := as.character(span)]
  fund_data[, cat_time := as.character(cat_time)]
  fund_data[, fund := as.character(fund)]
  ### program admissions, discharges, and admitted ###
  ## fiscal year ## ... 3 years of data from enddate
  sql_prog_ct_fy <-
    paste0("exec encompass.dbo.E2_CMH_program_active_Count_by_FY '",
                           last3fys[, format(min(t_start), "%m/%d/%Y")], "', '", enddate, "'")
  prog_ct_fy  <- sqlQuery(channel=channel, query=sql_prog_ct_fy,
                          stringsAsFactors=FALSE, max=0)
  prog_ct_fy  <- data.table(prog_ct_fy)
  setnames(prog_ct_fy, old=c("FY", "FY_start", "FY_end"),
           new=c("time", "t_start", "t_end"))
  setnames(prog_ct_fy, colnames(prog_ct_fy), tolower(colnames(prog_ct_fy)))
  ## quarter ## ... 3 years of data from enddate
  sql_prog_ct_qtr <-
    paste0("exec encompass.dbo.E2_CMH_program_active_Count_by_Quarter '",
      last3qtrs[, format(min(t_start), "%m/%d/%Y")], "', '", enddate, "'")
  prog_ct_qtr  <- sqlQuery(channel=channel, query=sql_prog_ct_qtr,
                           stringsAsFactors=FALSE, max=0)
  prog_ct_qtr  <- data.table(prog_ct_qtr)
  setnames(prog_ct_qtr, old=c("Q_Name", "Q_start", "Q_end"), new=c("time", "t_start", "t_end"))
  setnames(prog_ct_qtr, colnames(prog_ct_qtr), tolower(colnames(prog_ct_qtr)))
  ## month ## ... 1 year of data
  sql_prog_ct_mon <-
    paste0("exec encompass.dbo.E2_CMH_program_active_Count_by_Month '",
                            format(dateConvert(enddate)-364, "%m/%d/%Y"), "', '", enddate, "'")
  prog_ct_mon  <- sqlQuery(channel=channel, query=sql_prog_ct_mon, stringsAsFactors=FALSE, max=0)
  prog_ct_mon  <- data.table(prog_ct_mon)
  setnames(prog_ct_mon, old=c("Mon_Name", "Mon_start", "Mon_end"), new=c("time", "t_start", "t_end"))
  setnames(prog_ct_mon, colnames(prog_ct_mon), tolower(colnames(prog_ct_mon)))
  rm(sql_prog_ct_fy, sql_prog_ct_mon, sql_prog_ct_qtr)
  ## program served ##
  sql_prog_fb_served <- paste0("select * from E2_Funding_Bucket.dbo.FB_fn_con_served_Program('", enddate, "')")
  prog_fb_served <- sqlQuery(channel=channel, query=sql_prog_fb_served)
  prog_fb_served <- data.table(prog_fb_served)
  prog_fb_served[, span := as.character(span)]
  prog_fb_served[, cat_time := as.character(cat_time)]
  prog_fb_served[, program := as.character(program)]

  # combine program results
  prog_summary <- list(mon=prog_ct_mon, qtr=prog_ct_qtr, fy=prog_ct_fy)
  prog_summary$fy[1]


  # save results
  saveRDS(object=fund_data, file=file.path(dataWD, "fund_data.rds") )
  saveRDS(object=prog_fb_served, file=file.path(dataWD, "prog_fb_served.rds") )
  saveRDS(object=prog_summary, file=file.path(dataWD, "prog_summary.rds") )
  print(paste("Monthly SQL data-download completed: fy",
              fiscalYear, monthList[m], ":", round(as.numeric(Sys.time()-t1, units="mins"), 2), "mins"))
}
