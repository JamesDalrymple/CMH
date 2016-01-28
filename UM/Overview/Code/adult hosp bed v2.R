#### initializing working directory and input parameters ####
  # clear RAM
  rm(list = ls())

  # which computer results in correct base working directory
  baseWD <- switch(Sys.info()["nodename"],
                   "JAMES" = "B:/",
                   "JAMES-PC" = "D:/",
                   "WSHSQLGP" = "C:/Users/dalrymplej/",
                   "CMHJ3DTVZ1-LT" = "C:/Users/dalrymplej/")

  # read in source file - personal library
  source(file.path(baseWD, "Dropbox/WCCMH/R/begin script R code.r") )
  # setup for working directories - data and results
  dataWD1 <- "Dropbox/Utilization Management/UM Monthly Reports/Data/Community Hospitalizations"
  gafWD <- file.path(dataWD1, "GAF months")
  resultsWD1 <- "Dropbox/Utilization Management/UM Monthly Reports/Results"
  codeWD <- "Dropbox/Utilization Management/UM Monthly Reports/R Code/Community Hospitalizations"
  project <- "Community Hospitalizations/Hospital Beds"

  # manual input
  fiscalYear <- "2015"
  monthList <- c("August", "September")
  # month fy list
  monthFyList = paste(tolower(monthList), "fy", fiscalYear)

#### source auxillary file ####
  source(file.path(baseWD, codeWD, "hosp bed auxillary.r"))
#### Loading/Fixing Files ####
  ### hospital data ###
  hosp <- read.dtable(file.path(baseWD, dataWD1,
            paste0("fy", substr(fiscalYear, 3, 4), "E2report2152.csv")))
  ### Modify Hospital Data - Part I ###
    # change column names
    setnames(hosp, old=colnames(hosp), new=tolower(colnames(hosp)))
    setnames(hosp, old=c("team_at_admit", "case_no1", "funding_source", "auth_eff", "auth_exp", "hosp_disc"),
             new=c("team", "case_no", "fund", "start_date", "exp_date", "disc_date"))
    # keep only wanted column names
    keepCols = c("hosp", "case_no", "fund", "adult", "start_date", "exp_date",
                 "disc_date", "unitsused", "team", "paid_amt")
    hosp[, setdiff(colnames(hosp), keepCols) := NULL]
    # change dates to date class
    dateColumns = c("start_date", "exp_date", "disc_date")
    hosp[, (dateColumns) :=
             lapply(.SD, dateConvert),
           .SDcols = dateColumns  ]
    rm(dateColumns)
    # fix fund column
    hosp[, fund := fundFix(fund)]
    # fix team column
    hosp[, team := teamFix(team)]
    # make freeStanding column
    setkey(hosp, hosp)[J(freeStand), freeStanding := "Y"]
    ## fix hospital errors manually ##
    hosp[case_no==1144539 & start_date==as.Date("2013-12-10"), disc_date := as.Date("2013-12-13")]
    # create clinical LOS
    hosp[, clinLOS := as.numeric(disc_date-start_date)]
    # make unitsused numeric
    hosp[, unitsused := as.numeric(unitsused)]
    # paid amount to numeric
    hosp[, paid_amt := moneyRm(paid_amt)]
    # keep unique records only
    invisible(setkey(hosp, NULL))
    hosp <- unique(hosp)

#### current month ####
for( m in seq_along(monthFyList)) {
  start_time = Sys.time()
  ### current month ###
  currentMonFy = monthFyList[m]
  ### Modify Hospital Data - Part II ###
    hospDT <- copy(hosp)
    # use current month to filter out hospital data
    startCutoff <- as.Date(as.yearmon(paste("October", as.numeric(fiscalYear)-1) ))
    endCutoff <- as.Date(as.yearmon(gsub(currentMonFy, pattern="fy ", replace="")), frac=1)
    # record number of missing discharges
    missingDisc <- data.table(missing_discharge =
      nrow(unique(hospDT[is.na(disc_date) & between(exp_date,startCutoff, endCutoff)])))
    hospDT <- hospDT[!(disc_date > endCutoff) ]
    # dates for date range
    startDate <- as.Date(as.yearmon(paste("Oct", fiscalYear)))
    dateRange <- paste(startDate, endCutoff, sep=" to ")
    # remove hospitalizations without a discharge date
    hospDT <- hospDT[!is.na(disc_date)] # possibly redundant

  #### aggregation ####
    # initialize variables
    All <- medicaid <- hmp <- NULL
    invisible(setkey(hospDT, NULL))
    ### prevent duplicate counting - due to dup records from fund columns ###
    hospDT[, admID := .GRP, by=list(case_no, start_date)]
    ### ALL ###
    All <- hospDT[, list(consumers = unique(case_no),
                        unitsused = sum(unitsused, na.rm=TRUE),
                        paid_amt = sum(paid_amt, na.rm=TRUE),
                        clinLOS = unique(clinLOS),
                        start_date = unique(start_date)), by=list(admID, hosp, freeStanding)]
    All <- All[, list(admissions = length(unique(admID)),
                     paid_amt = sum(paid_amt, na.rm=TRUE),
                     totalPaidLOS = sum(unitsused, na.rm=TRUE),
                     medianPaidLOS = round(median(unitsused, na.rm=TRUE),1),
                     mean15pctPaidLOS = round(meanTrim(x=unitsused, trim=.15),1),
                     meanPaidLOS = round(mean(unitsused, na.rm=TRUE),1),
                     totalClinicalLOS = sum(clinLOS, na.rm=TRUE),
                     medianClinicalLOS = round(median(clinLOS, na.rm=TRUE),1),
                     mean15pctClinicalLOS = round(meanTrim(x=clinLOS, trim=.15),1),
                     meanClinicalLOS = round(mean(clinLOS, na.rm=TRUE),1)),
              by=list(hosp, freeStanding) ]

    ### All funding - Adult ###
    allAdult <- hospDT[adult=="Y", list(consumers = unique(case_no),
                                       paid_amt = sum(paid_amt, na.rm=TRUE),
                                       unitsused = sum(unitsused, na.rm=TRUE),
                                       clinLOS = unique(clinLOS),
                                       start_date = unique(start_date)),
                      by=list(admID, hosp, freeStanding, adult)]
    allAdult <- allAdult[adult=="Y", list(admissions = length(unique(admID)),
                                       paid_amt = sum(paid_amt, na.rm=TRUE),
                             totalPaidLOS = sum(unitsused, na.rm=TRUE),
                             medianPaidLOS = round(median(unitsused, na.rm=TRUE),1),
                             mean15pctPaidLOS = round(meanTrim(x=unitsused, trim=.15),1),
                             meanPaidLOS = round(mean(unitsused, na.rm=TRUE),1),
                             totalClinicalLOS = sum(clinLOS, na.rm=TRUE),
                             medianClinicalLOS = round(median(clinLOS, na.rm=TRUE),1),
                             mean15pctClinicalLOS = round(meanTrim(x=clinLOS, trim=.15),1),
                             meanClinicalLOS = round(mean(clinLOS, na.rm=TRUE),1)),
                      by=list(hosp, freeStanding) ]
    ### All funding - Child ###
    allChild <- hospDT[adult=="N", list(consumers = unique(case_no),
                                       paid_amt = sum(paid_amt, na.rm=TRUE),
                                       unitsused = sum(unitsused, na.rm=TRUE),
                                       clinLOS = unique(clinLOS),
                                       start_date = unique(start_date)),
                      by=list(admID, hosp, freeStanding, adult)]
    allChild <- allChild[adult=="N", list(admissions = length(unique(admID)),
                                       paid_amt = sum(paid_amt, na.rm=TRUE),
                                       totalPaidLOS = sum(unitsused, na.rm=TRUE),
                                       medianPaidLOS = round(median(unitsused, na.rm=TRUE),1),
                                       mean15pctPaidLOS = round(meanTrim(x=unitsused, trim=.15),1),
                                       meanPaidLOS = round(mean(unitsused, na.rm=TRUE),1),
                                       totalClinicalLOS = sum(clinLOS, na.rm=TRUE),
                                       medianClinicalLOS = round(median(clinLOS, na.rm=TRUE),1),
                                       mean15pctClinicalLOS = round(meanTrim(x=clinLOS, trim=.15),1),
                                       meanClinicalLOS = round(mean(clinLOS, na.rm=TRUE),1)),
                      by=list(hosp, freeStanding) ]
    ### medicaid hospital records ###
    medicaid <- hospDT[fund %in% c("Medicaid", "MIChild"), list(consumers = unique(case_no),
                                                 paid_amt = sum(paid_amt, na.rm=TRUE),
                                       unitsused = sum(unitsused, na.rm=TRUE),
                                       clinLOS = unique(clinLOS),
                                       start_date = unique(start_date)),
                      by=list(admID, hosp, freeStanding, fund)]
    medicaid <- medicaid[fund %in% c("Medicaid", "MIChild"), list(admissions = length(unique(admID)),
                                                 paid_amt = sum(paid_amt, na.rm=TRUE),
                                       totalPaidLOS = sum(unitsused, na.rm=TRUE),
                                       medianPaidLOS = round(median(unitsused, na.rm=TRUE),1),
                                       mean15pctPaidLOS = round(meanTrim(x=unitsused, trim=.15),1),
                                       meanPaidLOS = round(mean(unitsused, na.rm=TRUE),1),
                                       totalClinicalLOS = sum(clinLOS, na.rm=TRUE),
                                       medianClinicalLOS = round(median(clinLOS, na.rm=TRUE),1),
                                       mean15pctClinicalLOS = round(meanTrim(x=clinLOS, trim=.15),1),
                                       meanClinicalLOS = round(mean(clinLOS, na.rm=TRUE),1)),
                      by=list(hosp, freeStanding) ]
    ### HMP hospital records ###
    hmp <- hospDT[fund=="HMP", list(consumers = unique(case_no),
                                       paid_amt = sum(paid_amt, na.rm=TRUE),
                                                 unitsused = sum(unitsused, na.rm=TRUE),
                                                 clinLOS = unique(clinLOS),
                                                 start_date = unique(start_date)),
                      by=list(admID, hosp, freeStanding, fund)]
    hmp <- hmp[fund=="HMP", list(admissions = length(unique(admID)),
                                       paid_amt = sum(paid_amt, na.rm=TRUE),
                                                 totalPaidLOS = sum(unitsused, na.rm=TRUE),
                                                 medianPaidLOS = round(median(unitsused, na.rm=TRUE),1),
                                                 mean15pctPaidLOS = round(meanTrim(x=unitsused, trim=.15),1),
                                                 meanPaidLOS = round(mean(unitsused, na.rm=TRUE),1),
                                                 totalClinicalLOS = sum(clinLOS, na.rm=TRUE),
                                                 medianClinicalLOS = round(median(clinLOS, na.rm=TRUE),1),
                                                 mean15pctClinicalLOS = round(meanTrim(x=clinLOS, trim=.15),1),
                                                 meanClinicalLOS = round(mean(clinLOS, na.rm=TRUE),1)),
                      by=list(hosp, freeStanding) ]

  #### save results ####
    ### create information about the file to share with end-users ###
    aboutFile = data.table(Date_Range = dateRange,
                           Last_Updated = as.character(Sys.time()),
                           Data_Sources = c("E2 hosp report 2152, E2 insurance report 2201"),
                           id=1)
    aboutFile = melt(aboutFile, id="id", variables=colnames(aboutFile)[-1])
    aboutFile = data.table(aboutFile)
    aboutFile[, id := NULL]
    rownames(aboutFile) = aboutFile[, variable]
    aboutFile[, variable := NULL]

    # change to results working directory
      # create working directory if it does not exist
      mainDir = paste(baseWD, resultsWD1,
        paste("fy", fiscalYear), monthList[m], "Community Hospitalizations", sep="/")
      subDir="Hospital Beds"
      createWD(mainDir=mainDir, subDir=subDir)
      # set working directory
      setwd(file.path(mainDir, subDir))
      # remove other files in this folder
      file.remove(dir(
        getwd(),
        pattern = "hospital LOS",
        full.names = TRUE
      ))

    #### create workbook ####
    wb = createWorkbook()
    # bold option and underline
    cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()
    # csAlign
    csAlign <- CellStyle(wb) + Alignment(h="ALIGN_CENTER")
    ### create sheet_all ###
      sheet_all = createSheet(wb, sheetName="All Funding Sources")
      # add All to sheet_all
      addDataFrame(x=All, sheet=sheet_all, showNA=FALSE, row.names = FALSE,
        startRow=1, startColumn=1, colnamesStyle=cs3,
        colStyle=list(`2`=csAlign) )
    ### create sheet_hmp ###
      sheet_hmp = createSheet(wb, sheetName="HMP")
      # add hmp to sheet_hmp
      addDataFrame(x=hmp, sheet=sheet_hmp, showNA=FALSE, row.names = FALSE,
        startRow=1, startColumn=1, colnamesStyle=cs3,
        colStyle=list(`2`=csAlign) )
    ### create sheet_medicaid ###
      sheet_medicaid = createSheet(wb, sheetName="Medicaid")
      # add medicaid to sheet_medicaid
      addDataFrame(x=medicaid, sheet=sheet_medicaid, showNA=FALSE,
        row.names=FALSE, startRow=1, startColumn=1, colnamesStyle=cs3,
        colStyle=list(`2`=csAlign) )
    ### create sheet_all ###
    sheet_all_adult = createSheet(wb, sheetName="All Funding - Adult")
    # add All_adult to sheet_all_adult
    addDataFrame(x=allAdult, sheet=sheet_all_adult, showNA=FALSE, row.names = FALSE,
                 startRow=1, startColumn=1, colnamesStyle=cs3,
                 colStyle=list(`2`=csAlign) )
    ### create sheet_all_child ###
    sheet_all_child = createSheet(wb, sheetName="All Funding - Child")
    # add All_child to sheet_all_child
    addDataFrame(x=allChild, sheet=sheet_all_child, showNA=FALSE, row.names = FALSE,
                 startRow=1, startColumn=1, colnamesStyle=cs3,
                 colStyle=list(`2`=csAlign) )
    ### create sheet missing_disc ###
    sheet_missDisc = createSheet(wb, sheetName="missing discharge")
    # add missingDisc to sheet_missDisc
    addDataFrame(x=missingDisc, sheet=sheet_missDisc, showNA=FALSE, row.names = FALSE,
                 startRow=1, startColumn=1, colnamesStyle=cs3,
                 colStyle=list(`2`=csAlign))
    ### create sheet data information ###
      sheet_info = createSheet(wb, sheetName="data info")
      # add data information
      addDataFrame(x=aboutFile, sheet=sheet_info, showNA=FALSE, row.names=TRUE,
        col.names=FALSE, startRow=1, startColumn = 1, rownamesStyle=cs3,
        colStyle=list(`2`=csAlign) )
    ### save workbook ###
      saveWorkbook(wb=wb, file=paste0("hospital LOS ", monthFyList[m], ".xlsx"))
  end_time = Sys.time()
  end_time-start_time
  print(paste(monthList[m], "completed:", round(end_time-start_time,3), "secs"))
}