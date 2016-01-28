# Data Source: E2: Comunity Hospital Inpatients Auth vs Paid (2152)
# IMPORTANT: Download the file as an .csv file
# Using Amount Paid because that is the amount we actually paid them

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

  # working directories
    project <- "Community Hospitalizations" # user input required
    initialWD <- "Dropbox/Utilization Management/UM Monthly Reports/"
    dataWD <- file.path("Data", project)

  # source auxillary R functions
    source(file.path(baseWD, initialWD, "R Code", project, "UM hosp auxillary.r"))

  # current date parameters
    # current fiscal year
      fiscalYear = "2016"
    # current month list for which months need to be ran
      # monthList = c("October", "November", "December", "January", "February")
      monthList = c("October", "November")
    # error object initialization
      errorList = NULL

  # start time
    # overall time
    startTime = Sys.time()

#### Load Data ####
  # data working directory
    setwd(file.path(baseWD, initialWD, "Data", project))

  # load community hospitalization budget data
    budgetData = read.dtable("comm hosp budget.csv")
  # loading the hospital data - ran the file as '.csv'
    hospfy11 = read.dtable("fy11E2report2152.csv")
    hospfy12 = read.dtable("fy12E2report2152.csv")
    hospfy13 = read.dtable("fy13E2report2152.csv")
    hospfy14 = read.dtable("fy14E2report2152.csv")
    hospfy15 = read.dtable("fy15E2report2152.csv")
  # combine all fiscal years together
    # before combining, need to remove a hospitalization that incorrectly is in fy13
    hospfy13 = hospfy13[!(case_no1==227098 & Auth_eff=="5/26/2012")]
    # combine all fiscal years
    fullHospData = rbindlist( list(hospfy11, hospfy12, hospfy13, hospfy14, hospfy15) )
    # remove un-needed R objects
    rm(hospfy11, hospfy12, hospfy13, hospfy14, hospfy15)

#### Fix Data ####
  # Hand correcting a few records...
    # consumer 227098 spent 2 days not 155 days in the hospital
      fullHospData[case_no1==227098 & Auth_eff=="5/26/2012", Units.Auth := 2L ]
      # because the Auth_Exp date was so messed up, the Units.Auth were wrong (see above)
      fullHospData[case_no1==227098 & Auth_eff=="5/26/2012",
                   Auth_Exp :="5/27/2012" ]
      # fixing the discharge date for consumer 227098
      fullHospData[case_no1==227098 & Auth_eff=="5/26/2012",
                   Hosp_disc := "5/28/2012"]
    # consumer 153005 and any other future cases like it - UnitsUsed cannot be greater than Auth_units
      # Hand correcting records where UnitsUsed is greater than UnitsAuth
      fullHospData[ Auth_units < UnitsUsed, UnitsUsed := Auth_units ]
    # consumer 214078 - hospital discharge is incorrect because at the time of the
      ## transition from E.I to E.II there were databases that were not properly linked,
      ## and so referential integrity issues allowed the discharge to be incorrect
      fullHospData[case_no1==214078 & Auth_eff=="10/18/2010",
                Hosp_disc := "10/20/2010"]
    # consumer 266784 had hospital discharge "2012-07-17", but someone entered the wrong year in E.II
      fullHospData[case_no1==266784 & Auth_eff=="7/10/2012",
                   Hosp_disc := "07/17/2012"]
    # consumer 271672 had hospital discharge "2011-12-28" but the year is incorrect
      fullHospData[case_no1==271672 & Auth_eff=="12/21/2010",
                   Hosp_disc := "12/28/2010"]
    # consumer 1125348 has the wrong year as the discharge date
      fullHospData[case_no1==1125348 & Auth_eff=="11/13/2010",
                   Hosp_disc := "11/15/2010"]
    # consumer 1141063 has the wrong year as the discharge date
      fullHospData[case_no1==1141063 & Auth_eff=="7/14/2013",
                   Hosp_disc := "7/15/2013"]
#### Manipulate Data ####
  ### Community Hospital Data ###
    # change names of case_no1 to case_no
    setnames(fullHospData, old = c("Team_at_admit", "case_no1"),
             new = c("Team", "case_no"))

    # getting rid of unwanted columns
    fullHospData[, c("Hosp", "Hosp_Type", "Unit_Rate", "MI_Child", "CPT_Code",
                     "Units.Auth", "Auth_units") := NULL ]

    # Paid Amount
      # setting blanks to zero
        fullHospData[PAID_AMT=="", PAID_AMT :=NA_character_]

    # function to fix dollar format to numeric format
      fullHospData[, PAID_AMT := moneyRm(PAID_AMT) ]
    # make new column rate for later cost per day calculations
      fullHospData[, rate := PAID_AMT/UnitsUsed ]
    # only one record per consumer admission
    ## Encounters can cause duplicates. Funding_Source can cause duplicates.
    fullHospData = fullHospData[, list(PAID_AMT = sum(PAID_AMT, na.rm = TRUE),
                                       UnitsUsed = sum(UnitsUsed) ),
        by = c("case_no", "adult", "Funding_Source", "Auth_eff",
               "Auth_Exp", "Hosp_disc", "Team")]
    # admID allows us to prevent counting admission duplicates
    fullHospData[, admID := .GRP, by=list(case_no, Auth_eff)]

    ## Dates ##
      # converting dates to Date class, and making month_year variable
        fullHospData[, Auth_eff := as.Date(Auth_eff, format="%m/%d/%Y")]
        fullHospData[, Auth_Exp := as.Date(Auth_Exp, format="%m/%d/%Y")]
        fullHospData[, Hosp_disc := as.Date(Hosp_disc, format="%m/%d/%Y")]
        # create Auth_ID column using case_no and Auth_eff
        fullHospData[, Auth_ID :=
            as.numeric(factor(paste(case_no, Auth_eff, sep=""))) ]
        # make duplicate Auth_ID have min Auth_eff and max Auth_Exp
        fullHospData[, Auth_eff := as.Date(my_min(Auth_eff)), by="Auth_ID" ]
        fullHospData[, Auth_Exp := as.Date(my_max(Auth_Exp)), by="Auth_ID"]
        fullHospData[, Hosp_disc := as.Date(my_max(Hosp_disc)), by="Auth_ID"]
        # remove duplicates
        fullHospData = unique(fullHospData)

    ## Teams ##
      # if team is blank, reassign to Non-CMH
      fullHospData[Team=="", Team :="Non-CMH"]

    ## Program ##
      # making better 'team' names for graphing

      # assign Program
        fullHospData[, Program := sapply(Team, teamSwitch) ]
        # remove R object
        rm(teamSwitch)
        fullHospData[, Program := as.character(Program) ]
        # delete Team column
        fullHospData[, Team := NULL ]
    ## Date Columns ##
      # Month Column
        fullHospData[, month_year := as.yearmon(Auth_eff) ]
      # Fiscal Year column
        fullHospData[, fiscalyr :=
            as.numeric(substr(as.yearqtr(Auth_eff)+.25, 1, 4)) ]
      # Quarter column
        fullHospData[, Quarter:= as.yearqtr(Auth_eff)+.25 ]
    ## Fund ##
      # create column fund
      fullHospData[, fund := my_fund(Funding_Source)]
      # remove R object
      rm(my_fund)
      # delete funding source column
      fullHospData[, Funding_Source := NULL ]

    # Engage column
      setkey(fullHospData, Program)
      fullHospData[ J( c("Non-CMH", "Access/CSTS") ), Status := "Unengaged" ]
      fullHospData[!J( c("Non-CMH", "Access/CSTS") ), Status := "Engaged" ]

    # create Length of Stay (clinical) column via clinicalStay function
      fullHospData[, clinLOS :=
        clinicalStay(start=Auth_eff, expiration=Auth_Exp, discharge=Hosp_disc),
        by = c("case_no", "Auth_eff")]
    # creating bins
      # financial bin labels
        fullHospData[UnitsUsed %between% c(0, 5), finBin := "0-5"]
        fullHospData[UnitsUsed %between% c(6, 10), finBin := "6-10"]
        fullHospData[UnitsUsed %between% c(11, 29), finBin := "11-29"]
        fullHospData[UnitsUsed %between% c(30, 1e7), finBin := "30+"]
        fullHospData$finBin = with(fullHospData,
                             factor(fullHospData$finBin,
                                    levels= c("0-5", "6-10", "11-29", "30+") ))
      # clincial bin labels
        fullHospData[clinLOS %between% c(0, 5), clinBin := "0-5"]
        fullHospData[clinLOS %between% c(6, 10), clinBin := "6-10"]
        fullHospData[clinLOS %between% c(11, 29), clinBin := "11-29"]
        fullHospData[clinLOS %between% c(30, 1e7), clinBin := "30+"]
        fullHospData[, clinBin := factor(clinBin)]
        fullHospData$clinBin = with(fullHospData,
                              factor(fullHospData$clinBin,
                                     levels= c("0-5", "6-10", "11-29", "30+") ))

    # save pre-aggregated results in data working directory
      # save(fullHospData, file = "preAggregatedHosp.RData")
  ### Budget Data ###
    # convert fy to character for later joining
    budgetData[, fy := as.character(fy)]

#### Restrict Data ####
  # restrict budget data to current fiscal year
  setkey(budgetData, fy)
  budgetData <- budgetData[J(fiscalYear)]
  if(budgetData[, sum(is.na(budget))]>0) {stop("missing budget data for current fiscal year")}

#### for loop to run through monthList ####
for( m in seq(monthList) ) {
  #set the month based on monthList
    month = monthList[m]
  # current month in zoo class ( a specific date class)
    if( month %in% c("October", "November", "December") ) {
      currentMonth = as.yearmon(paste(month, as.numeric(fiscalYear)-1))
    } else { currentMonth = as.yearmon(paste(month, fiscalYear))}

    currentQuarter = as.yearqtr(currentMonth)+.25
  # if current month does not include the end of the current quarter,
  ## change current quarter to the previous quarter
    if(as.yearmon(as.Date(as.yearqtr(currentMonth)+.25)-1) > currentMonth) {
      currentQuarter=currentQuarter-.25
    }
  # Restrict data based on current month
  # last day of current month
    tmpLastDay = as.Date(currentMonth+1/12)-1
    tmpFirstDay = as.Date(as.yearmon("Oct 2010"))
  # tmpHospMon based on current month
    hosp_data = fullHospData[Auth_eff %between% c(tmpFirstDay, tmpLastDay)  |
                                Auth_Exp %between% c(tmpFirstDay, tmpLastDay) |
                                Hosp_disc %between% c(tmpFirstDay, tmpLastDay) ]

  # Time Intervals - Months, Fiscal Quarters, Fiscal Years ----
    # all months in the hospital data
      allMonths = hosp_data[, unique(month_year)]
      # order the months sequentially
      allMonths = allMonths[ order(allMonths)]
      # make all months greater than or equal to 'Oct 2010'
      allMonths = allMonths[allMonths >= as.yearmon("Oct 2010")]

    # all quarters in the hospital data
      allQtrs = hosp_data[, unique(Quarter)]
      # order the quarters sequentially
      allQtrs = allQtrs[ order(allQtrs)]
      # make all quarters greater than or equal to '2011 Q1'
      ## and less than the currentQuarter (which is restricted based on currentMonth)
      ## so that an unfinished quarter does not show up in any of the currentMonth ggplots
      allQtrs = allQtrs[allQtrs >= as.yearqtr("2011 Q1") &
                        allQtrs <= currentQuarter]
      # remove NAs from quarters
      allQtrs = allQtrs[!is.na(allQtrs)]

    # all fiscal years in the hospital data
      allFys = hosp_data[, unique(fiscalyr)]
      # order the years sequentially
      allFys = allFys[ order(allFys)]
      # make all years greater than or equal to 2011
      allFys = allFys[allFys >= 2011]

  # Monthly Summary ----
    # initialize variables for 'for-loop'
      monthSummary = NULL
      monFundSummary = NULL
      monProgSummary = NULL

    # for-loop that calculates consumers, admissions, costs, LOS (clinical/financial) per month
    for( i in seq(allMonths) ) {
      # temp Month
      tmpMonth = allMonths[i]

      # Restrict data based on current month
        # last day of current month
        tmpLastDay = as.Date(tmpMonth+1/12)-1
        tmpFirstDay = as.Date(tmpMonth)
        # tmpHospMon based on current month
        tmpHospMon = hosp_data[Auth_eff %between% c(tmpFirstDay, tmpLastDay)  |
                               Auth_Exp %between% c(tmpFirstDay, tmpLastDay) |
                               Hosp_disc %between% c(tmpFirstDay, tmpLastDay) ]

        # use los.clin function to determine how many days each consumer spends
        ## in the hospital that month, per admission record
          tmpHospMon[, clinMonDays :=
            los.clin(Start=as.Date(tmpMonth), End=as.Date(tmpMonth+1/12)-1,
                     admit=Auth_eff, expire=Auth_Exp, discharge=Hosp_disc),
            by = c("case_no", "Auth_eff", "Auth_Exp", "Hosp_disc")]

        # use los.finUnits function to determine how many days each consumer spends
        ## in the hospital that month, per admission record
          tmpHospMon[, monPaidUnits :=
            los.finUnits(Start=as.Date(tmpMonth), End=as.Date(tmpMonth+1/12)-1,
                         admit = Auth_eff, UnitsUsed = UnitsUsed),
            by = c("case_no", "Auth_eff", "UnitsUsed")]

        # calculating rate using units used and paid amounts
          tmpHospMon[, calcRate := PAID_AMT/UnitsUsed ]

        # monthCost based on rate and number of monPaidUnits
          tmpHospMon[, monthCost := monPaidUnits*calcRate ]

        # admissions
          # number of admissions in the month
          tmpAdmitMon = tmpHospMon[Auth_eff %between% c(tmpFirstDay, tmpLastDay),
                                     list(month_year = tmpMonth,
                                          admissions = length(unique(admID)),
                                          consumersAdmit = length(unique(case_no)))]
        # clinical+financial LOS for consumers that had admission in tmpMonth
          # make a copy of tmpHospMon
          tmpLOSMon = copy(tmpHospMon)
          # removing records where paid units > clinicalLOS for LOS calculations - 1/23/2014 by Kelly B
          tmpLOSMon = tmpLOSMon[!clinLOS<UnitsUsed] # removes 25 records FY11-FY13
          # removing records where length of stay is zero
          tmpLOSMon = tmpLOSMon[!clinLOS==0]
          # remove un-needed columns in order to remove duplicates
          keepNames = c("case_no", "Auth_eff", "clinLOS", "UnitsUsed", "Auth_ID")
          tmpLOSMon[, setdiff(colnames(tmpLOSMon), keepNames) := NULL]
          # sum UnitsUsed and keep max clinLOS by Auth_ID
          tmpLOSMon[, UnitsUsed := sum(UnitsUsed, na.rm=TRUE), by = "Auth_ID"]
          tmpLOSMon[, clinLOS := max(clinLOS, na.rm=TRUE), by = "Auth_ID"]
          # remove duplicates (created when we removed columns)
          tmpLOSMon = unique(tmpLOSMon)
          # aggregate LOS calculations
          tmpLOS = tmpLOSMon[Auth_eff %between% c(tmpFirstDay, tmpLastDay),
                                list(clinTop15pctMean = round(meanTrim(clinLOS, trim=.15), 2),
                                clinMedian = median(clinLOS, na.rm = TRUE),
                                finTop15pctMean  = round(meanTrim(UnitsUsed, trim=.15), 2),
                                finMedian = median(UnitsUsed, na.rm = TRUE) ) ]

        # summary by month - consumers, cost, admissions
          # set key to get rid of records with no clinical or paid days in the window of time
          setkey(tmpHospMon, clinMonDays, monPaidUnits)
          tmpMonSummary = tmpHospMon[!J(0,0), list(month_year = tmpMonth,
                                     consumers = length(unique(case_no)),
                                     cost = sum(monthCost, na.rm = TRUE))]
          setkey(tmpHospMon, NULL) # remove key

        # join tmpAdmitMon and tmpMonSummary
          setkey(tmpAdmitMon, month_year); setkey(tmpMonSummary, month_year)
          tmpMonSummary = merge(tmpMonSummary, tmpAdmitMon, all.x=TRUE, by = "month_year")

        # combine two summary objects
          tmpMonSummary = cbind(tmpMonSummary, tmpLOS)

        # combine each monthSummary
          monthSummary = rbindlist( list(monthSummary, tmpMonSummary ))

        # funding source
          # fund admissions
            tmpFundAdmit = tmpHospMon[as.yearmon(Auth_eff)==tmpMonth,
                              list(admissions = length(unique(admID)),
                                   consumerAdmit = length(case_no)), by = "fund"]

          # temp month fund
            setkey(tmpHospMon, clinMonDays, monPaidUnits)
            tmpMonFund = tmpHospMon[!J(0,0),
                          list(cost = sum(monthCost, na.rm = TRUE),
                               consumers = length(unique(case_no))),
                          by = c("fund")]
            invisible(setkey(tmpHospMon, NULL))

            # add current month to tmpMonFund
              tmpMonFund[, month_year := tmpMonth ]

            # get rid of unknown if cost is zero
              tmpMonFund = tmpMonFund[!(fund=="unknown" & cost==0)]

        # combine tmpMonFund and tmpFundAdmit
          # set keys
          setkey(tmpFundAdmit, fund); setkey(tmpMonFund, fund)
          tmpMonFund = merge(tmpMonFund, tmpFundAdmit, all.x=TRUE, by = c("fund"))

          # combine each tmpMonFund
          monFundSummary = rbindlist(list(monFundSummary, tmpMonFund))

        # Program
          # program admissions
          tmpProgAdmit = tmpHospMon[Auth_eff %between% c(tmpFirstDay, tmpLastDay),
                                    list(admissions = length(unique(admID)),
                                         consumerAdmit = length(unique(case_no))), by = "Program"]

          # temp program summary
            # set key to get rid of records with no clinical or paid days in the window of time
            setkey(tmpHospMon, clinMonDays, monPaidUnits)
            tmpProg = tmpHospMon[!J(0,0), list(month_year = tmpMonth,
                                 consumers = length(unique(case_no)),
                                 cost = sum(monthCost, na.rm = TRUE)), by = "Program"]
            invisible(setkey(tmpHospMon, NULL)) # remove key

          # merge tmpProgAdmit and tmpProg
            setkey(tmpProgAdmit, Program); setkey(tmpProg, Program)
            tmpProg = merge(tmpProg, tmpProgAdmit, all.x=TRUE, by = "Program")

          # Dealing with potential missing programs in tmpProg
            # list of programs
              tmpProgList = c("DD Adult", "MI Adult", "Non-CMH", "Youth & Family")
            # if missing program, add it to the current tmpMonth
              setkey(tmpProg, Program)
              tmpProg = tmpProg[ J(tmpProgList) ]
            # replace month NAs with current tmpMonth
            tmpProg[ is.na(month_year), month_year := tmpMonth]
            # replace NA consumers with 0
            tmpProg[ is.na(consumers), consumers := 0L ]
            # replace NA cost with 0
            tmpProg[ is.na(cost), cost := 0]
            # replace NA admissions with 0
            tmpProg[ is.na(admissions), admissions := 0L]

          # combine each month program summary
            monProgSummary = rbindlist(list(monProgSummary, tmpProg))
      }
      # remove R objects
        rm(i, tmpAdmitMon, tmpFirstDay, tmpFundAdmit, tmpHospMon, tmpLOS,
           tmpLastDay, tmpLOS, tmpMonFund, tmpMonSummary, tmpMonth,
           tmpProg, tmpProgAdmit, tmpProgList, tmpLOSMon)

  # Quarterly Summary ----

    # initialize variables for 'for-loop'
      qtrSummary = NULL
      qtrFundSummary = NULL
      qtrProgSummary = NULL

    # for-loop that calculates consumers, admissions, costs, LOS (clinical/financial) per Quarter
    for( i in seq(allQtrs) ) {
      # temp Quarter
        tmpQtr = allQtrs[i]
      # Restrict data based on current quarter
        # last day of current quarter
        tmpLastDay = as.Date(tmpQtr)-1
        tmpFirstDay = as.Date(tmpQtr-.25)
        # hosp_data based on current quarter
        tmpHospQtr = hosp_data[Auth_eff %between% c(tmpFirstDay, tmpLastDay)  |
                                    Auth_Exp %between% c(tmpFirstDay, tmpLastDay) |
                                    Hosp_disc %between% c(tmpFirstDay, tmpLastDay) ]

      # use los.clin function to determine how many days each consumer spends
      ## in the hospital that quarter, per admission record
        tmpHospQtr[, clinQtrDays :=
          los.clin(Start=as.Date(tmpQtr-.25), End=as.Date(tmpQtr)-1,
                   admit=Auth_eff, expire=Auth_Exp, discharge=Hosp_disc),
          by = c("case_no", "Auth_eff", "Auth_Exp", "Hosp_disc")]

      # use los.finUnits function to determine how many days each consumer spends
      ## in the hospital that quarter, per admission record
        tmpHospQtr[, qtrPaidUnits :=
          los.finUnits(Start=as.Date(tmpQtr-.25), End=as.Date(tmpQtr)-1,
                       admit = Auth_eff, UnitsUsed = UnitsUsed),
          by = c("case_no", "Auth_eff", "UnitsUsed")]

      # calculating rate using units used and paid amounts
        tmpHospQtr[, calcRate := PAID_AMT/UnitsUsed ]

      # qtrCost based on rate and number of qtrPaidUnits
        tmpHospQtr[, qtrCost := qtrPaidUnits*calcRate ]

      # admissions
        # number of admissions in the quarter
        tmpAdmitQtr = tmpHospQtr[Auth_eff %between% c(tmpFirstDay, tmpLastDay),
                                 list(qtr = tmpQtr,
                                      admissions = length(unique(admID)),
                                      consumersAdmit = length(unique(case_no)))]

        # clinical+financial LOS for consumers that had admission in tmpQtr
          # make a copy of tmpHospQtr
          tmpLOSQtr = copy(tmpHospQtr)
          # removing records where length of stay is zero
          tmpLOSQtr = tmpLOSQtr[!clinLOS==0]
          # remove un-needed columns in order to remove duplicates
          keepNames = c("case_no", "Auth_eff", "clinLOS", "UnitsUsed", "Auth_ID")
          tmpLOSQtr[, setdiff(colnames(tmpLOSQtr), keepNames) := NULL]
          # sum UnitsUsed and keep max clinLOS by Auth_ID
          tmpLOSQtr[, UnitsUsed := sum(UnitsUsed, na.rm=TRUE), by = "Auth_ID"]
          tmpLOSQtr[, clinLOS := max(clinLOS, na.rm=TRUE), by = "Auth_ID"]
          # remove duplicates (created when we removed columns)
          tmpLOSQtr = unique(tmpLOSQtr)
          # aggregate LOS calculations
          tmpLOS = tmpLOSQtr[my_qtr(Auth_eff)==tmpQtr,
                             list(clinTop15pctMean = round(meanTrim(clinLOS, trim=.15), 2),
                                  clinMedian = median(clinLOS, na.rm = TRUE),
                                  finTop15pctMean  = round(meanTrim(UnitsUsed, trim=.15), 2),
                                  finMedian = median(UnitsUsed, na.rm = TRUE) ) ]

      # summary by quarter - consumers, cost, admissions
        # set key to get rid of records with no clinical or paid days in the window of time
        setkey(tmpHospQtr, clinQtrDays, qtrPaidUnits)
        tmpQtrSummary = tmpHospQtr[!J(0,0), list(qtr = tmpQtr,
                                          consumers = length(unique(case_no)),
                                          cost = sum(qtrCost, na.rm = TRUE)) ]
        invisible(setkey(tmpHospQtr, NULL)) # remove key

      # join tmpAdmitQtr and tmpQtrSummary
        setkey(tmpAdmitQtr, qtr); setkey(tmpQtrSummary, qtr)
        tmpQtrSummary = merge(tmpQtrSummary, tmpAdmitQtr, all.x=TRUE, by = "qtr")

      # combine two summary objects
        tmpQtrSummary = cbind(tmpQtrSummary, tmpLOS)

      # combine each qtrSummary
        qtrSummary = rbindlist( list(qtrSummary, tmpQtrSummary ))

      ## funding source ##
        # fund admissions
          tmpFundAdmit = tmpHospQtr[my_qtr(Auth_eff)==tmpQtr,
                                    list(admissions = length(unique(admID)),
                                         consumerAdmit = length(unique(case_no))), by = "fund"]

        # temp quarter fund
          # set key to get rid of records with no clinical or paid days in the window of time
          setkey(tmpHospQtr, clinQtrDays, qtrPaidUnits)
          tmpQtrFund = tmpHospQtr[!J(0,0), list(cost = sum(qtrCost, na.rm = TRUE),
                                       consumers = length(unique(case_no))),
                                  by = c("fund")]
          invisible(setkey(tmpHospQtr, NULL)) # remove key

        # add current quarter to tmpQtrFund
          tmpQtrFund[, qtr := tmpQtr ]

        # get rid of unknown if cost is zero
          tmpQtrFund = tmpQtrFund[!(fund=="unknown" & cost==0)]

        # combine tmpQtrFund and tmpFundAdmit
          # set keys
          setkey(tmpFundAdmit, fund); setkey(tmpQtrFund, fund)
          tmpQtrFund = merge(tmpQtrFund, tmpFundAdmit, all.x=TRUE, by = c("fund"))

        # combine each tmpQtrFund
          qtrFundSummary = rbindlist(list(qtrFundSummary, tmpQtrFund))

      ## Program ##
        # program admissions
          tmpProgAdmit = tmpHospQtr[my_qtr(Auth_eff)==tmpQtr,
                                    list(admissions = length(unique(admID)),
                                         consumerAdmit = length(unique(case_no))), by = "Program"]
        # temp program summary
          setkey(tmpHospQtr, clinQtrDays, qtrPaidUnits)
          tmpProg = tmpHospQtr[!J(0,0), list(qtr = tmpQtr,
                                      consumers = length(unique(case_no)),
                                      cost = sum(qtrCost, na.rm = TRUE)), by = "Program"]
          setkey(tmpHospQtr, NULL)
        # merge tmpProgAdmit and tmpProg
          setkey(tmpProgAdmit, Program); setkey(tmpProg, Program)
          tmpProg = merge(tmpProg, tmpProgAdmit, all.x=TRUE, by = "Program")

        # Dealing with potential missing programs in tmpProg
          # List of Programs
          tmpProgList = c("DD Adult", "MI Adult", "Non-CMH", "Youth & Family")
          # if missing program, add it to the current tmpQuarter
            setkey(tmpProg, Program)
            tmpProg = tmpProg[ J(tmpProgList) ]
          # replace quarter NAs with current tmpQuarter
            tmpProg[ is.na(qtr), qtr := tmpQtr]
          # replace NA consumers with 0
            tmpProg[ is.na(consumers), consumers := 0L ]
          # replace NA cost with 0
            tmpProg[ is.na(cost), cost := 0]
          # replace NA admissions with 0
            tmpProg[ is.na(admissions), admissions := 0L]

        # combine each quarter program summary
          qtrProgSummary = rbindlist(list(qtrProgSummary, tmpProg))
    }
    # remove R objects
    rm(i, tmpAdmitQtr, tmpFirstDay, tmpFundAdmit, tmpHospQtr, tmpLastDay,
       tmpProg, tmpProgAdmit, tmpProgList, tmpQtr, tmpQtrFund,
       tmpQtrSummary, tmpLOSQtr, tmpLOS)

  # Yearly Summary ----

    # initialize variables for 'for-loop'
      fySummary = NULL
      fyFundSummary = NULL
      fyProgSummary = NULL

    # for-loop that calculates consumers, admissions, costs, LOS (clinical/financial) per Fiscal Year
    for( i in seq(allFys) ) {
      # temp Fiscal Year
        tmpFy = allFys[i]
      # Restrict data based on current fiscal year
        # last day of current fiscal year
        tmpLastDay = as.Date(as.yearmon(paste("Oct", tmpFy)))-1
        tmpFirstDay = as.Date(as.yearmon(paste("Oct", tmpFy-1)))
        # hosp_data based on current fiscal year
        tmpHospFy = hosp_data[Auth_eff %between% c(tmpFirstDay, tmpLastDay)  |
                                    Auth_Exp %between% c(tmpFirstDay, tmpLastDay) |
                                    Hosp_disc %between% c(tmpFirstDay, tmpLastDay) ]

      # use los.clin function to determine how many days each consumer spends
      ## in the hospital that fiscal year, per admission record
        tmpHospFy[, clinFyDays :=
                     los.clin(Start=as.Date(as.yearmon(paste("Oct", tmpFy-1))),
                              End=as.Date(as.yearmon(paste("Oct", tmpFy)))-1,
                              admit=Auth_eff, expire=Auth_Exp, discharge=Hosp_disc),
                   by = c("case_no", "Auth_eff", "Auth_Exp", "Hosp_disc")]

      # use los.finUnits function to determine how many days each consumer spends
      ## in the hospital that fiscal year, per admission record
        tmpHospFy[, fyPaidUnits :=
                     los.finUnits(Start=as.Date(as.yearmon(paste("Oct", tmpFy-1))),
                                  End=as.Date(as.yearmon(paste("Oct", tmpFy)))-1,
                                  admit = Auth_eff, UnitsUsed = UnitsUsed),
                   by = c("case_no", "Auth_eff", "UnitsUsed")]

      # calculating rate using units used and paid amounts
        tmpHospFy[, calcRate := PAID_AMT/UnitsUsed ]

      # fyCost based on rate and number of fyPaidUnits
        tmpHospFy[, fyCost := fyPaidUnits*calcRate ]

      ## admissions ##
        # number of admissions in the fiscal year
          tmpAdmitFy = tmpHospFy[Auth_eff %between% c(tmpFirstDay, tmpLastDay),
                              list(fy = tmpFy,
                                   admissions = length(unique(admID)),
                                   consumersAdmit = length(unique(case_no)))]

        # clinical+financial LOS for consumers that had admission in tmpFy
          # make a copy of tmpHospFy
          tmpLOSFy = copy(tmpHospFy)
          # removing records where length of stay is zero
          tmpLOSFy = tmpLOSFy[!clinLOS==0]
          # remove un-needed columns in order to remove duplicates
          keepNames = c("case_no", "Auth_eff", "clinLOS", "UnitsUsed", "Auth_ID")
          tmpLOSFy[, setdiff(colnames(tmpLOSFy), keepNames) := NULL]
          # sum UnitsUsed and keep max clinLOS by Auth_ID
          tmpLOSFy[, UnitsUsed := sum(UnitsUsed, na.rm=TRUE), by = "Auth_ID"]
          tmpLOSFy[, clinLOS := max(clinLOS, na.rm=TRUE), by = "Auth_ID"]
          # remove duplicates (created when we removed columns)
          tmpLOSFy = unique(tmpLOSFy)
          # aggregate LOS calculations - may lose one or two records that have admissions in other time windows
          tmpLOS = tmpLOSFy[my_fy(Auth_eff, format="%Y-%m-%d")==tmpFy,
                             list(clinTop15pctMean = round(meanTrim(clinLOS, trim=.15), 2),
                                  clinMedian = median(clinLOS, na.rm = TRUE),
                                  finTop15pctMean  = round(meanTrim(UnitsUsed, trim=.15), 2),
                                  finMedian = median(UnitsUsed, na.rm = TRUE) ) ]

        # summary by fiscal year - consumers, cost, admissions
          # set key to get rid of records with no clinical or paid days in the window of time
          setkey(tmpHospFy, clinFyDays, fyPaidUnits)
          tmpFySummary = tmpHospFy[!J(0,0), list(fy = tmpFy,
                                             consumers = length(unique(case_no)),
                                             cost = sum(fyCost, na.rm = TRUE))]
          invisible(setkey(tmpHospFy, NULL)) # remove key

        # join tmpAdmitFy and tmpFySummary
          setkey(tmpAdmitFy, fy); setkey(tmpFySummary, fy)
          tmpFySummary = merge(tmpFySummary, tmpAdmitFy, all.x=TRUE, by = "fy")

        # combine two summary objects
          tmpFySummary = cbind(tmpFySummary, tmpLOS)

        # combine each fySummary
          fySummary = rbindlist( list(fySummary, tmpFySummary ))

      ## funding source ##
        # fund admissions
        tmpFundAdmit = tmpHospFy[my_fy(Auth_eff)==tmpFy,
                                  list(admissions = length(unique(admID)),
                                       consumerAdmit = length(unique(case_no))), by = "fund"]
        # temp fiscal year fund
          # set key to get rid of records with no clinical or paid days in the window of time
          setkey(tmpHospFy, clinFyDays, fyPaidUnits)
          tmpFyFund = tmpHospFy[!J(0,0),list(cost = sum(fyCost, na.rm = TRUE),
                                      consumers = length(unique(case_no))),
                                by = c("fund")]
          setkey(tmpHospFy, NULL) # remove key

        # add current fiscal year to tmpFyFund
          tmpFyFund[, fy := tmpFy ]

        # get rid of unknown if cost is zero
          tmpFyFund = tmpFyFund[!(fund=="unknown" & cost==0)]

        # combine tmpFyFund and tmpFundAdmit
          # set keys
          setkey(tmpFundAdmit, fund); setkey(tmpFyFund, fund)
          # merge
          tmpFyFund = merge(tmpFyFund, tmpFundAdmit, all.x=TRUE, by = c("fund"))

        # combine each tmpFyFund
          fyFundSummary = rbindlist(list(fyFundSummary, tmpFyFund))


          # testing <- tmpHospFy[between(Auth_eff, tmpFirstDay, tmpLastDay),
          #                      unique(.SD),
          #                      .SDc = c("case_no", "Auth_eff", "Program")]
          # testing[, fy := my_fy(Auth_eff)]
          # testing[, list(consumerAdmit = length(unique(case_no))), by = "Program"]

      ## Program ##
        # program admissions
        tmpProgAdmit = tmpHospFy[Auth_eff %between% c(tmpFirstDay, tmpLastDay),
                                  list(admissions = length(unique(admID)),
                                       consumerAdmit = length(unique(case_no))), by = "Program"]
        # temp program summary
          # set key to get rid of records with no clinical or paid days in the window of time
          setkey(tmpHospFy, clinFyDays, fyPaidUnits)
          tmpProg = tmpHospFy[!J(0,0), list(fy = tmpFy,
                                      consumers = length(unique(case_no)),
                                      cost = sum(fyCost, na.rm = TRUE)), by = "Program"]
          invisible(setkey(tmpHospFy, NULL)) # remove key
        # merge tmpProgAdmit and tmpProg
          setkey(tmpProgAdmit, Program); setkey(tmpProg, Program)
          tmpProg = merge(tmpProg, tmpProgAdmit, all.x=TRUE, by = "Program")

      # Dealing with potential missing programs in tmpProg
        # List of Programs
          tmpProgList = c("DD Adult", "MI Adult", "Non-CMH", "Youth & Family")
          # if missing program, add it to the current tmpFy
            setkey(tmpProg, Program)
            tmpProg = tmpProg[ J(tmpProgList) ]
          # replace fiscal year NAs with current tmpFy
            tmpProg[ is.na(fy), fy := tmpFy]
          # replace NA consumers with 0
            tmpProg[ is.na(consumers), consumers := 0L ]
          # replace NA cost with 0
            tmpProg[ is.na(cost), cost := 0]
          # replace NA admissions with 0
            tmpProg[ is.na(admissions), admissions := 0L]

      # combine each fiscal year program summary
        fyProgSummary = rbindlist(list(fyProgSummary, tmpProg))
    }
    # remove R objects
      rm(i, tmpAdmitFy, tmpFirstDay, tmpFundAdmit, tmpFy, tmpFyFund, tmpLOSFy, tmpLOS,
         tmpFySummary, tmpHospFy, tmpLastDay, tmpProg, tmpProgAdmit, tmpProgList)
  # testing and comparing con_hosp
  # fyProgSummary[Program == "MI Adult", .SD,
  # .SDc = c("Program", "fy", "consumerAdmit", "admissions")]


# Results Working directory
  # establishing working directories based on the current month
  resultsWD <- file.path("Results", paste("fy", fiscalYear), month, project)
  # create base results working directory
  baseResults <- file.path(baseWD, initialWD, resultsWD)

#### Save Results ####
  ### length of stay hospitalization summaries
    # monthly
    write.csv(x = monthSummary,
      file = file.path(baseResults, "Monthly/Length of Stay",
        "monthly community hosp summary.csv"),
      row.names = FALSE)
    # quarterly
    write.csv(x = qtrSummary,
      file =paste(baseResults, "Quarterly/Length of Stay",
        "quarterly community hosp summary.csv", sep = "/"),
      row.names = FALSE)
    # yearly
    write.csv(x = fySummary,
      file = paste(baseResults,
        "Yearly/Length of Stay",
        "yearly community hosp summary.csv", sep = "/"),
      row.names = FALSE)

  # Graphing
    # Monthly
      # change date class to factors so that graphs show months in sequential order
      for( i in seq(allFys)) {
        # each fiscal year
        tmpFy = allFys[i]
        # dataset based on tmpFy
        tmpMon = monthSummary[ allFys[i]==as.numeric(my_fy(month_year)) ]
        # convert month to factor
        tmpMon[, month_year := as.factor(month_year) ]
        # re-order levels chronologically
          levelOrder = tmpMon[, as.character(as.yearmon(unique(month_year)))]
        # re-level months without changing data
          tmpMon$month_year = with(tmpMon, factor(tmpMon$month_year, levels= levelOrder ))

        # Consumers Admitted and Admissions
          # ggplot labels
            tmpMon[, ConAdmLab := paste(consumersAdmit, " (", admissions, ")", sep="" )]
          # labels to prevent a legend - only needed for top bar
            tmpMon[paste("Sep", tmpFy)==month_year,
                    ConAdmLab := paste(consumersAdmit, " consumers (", admissions, " admissions)", sep="" )]
          # labels to prevent a lengend if the top bar if current month is the top bar and not the end of the fy
            if(tmpFy==as.numeric(fiscalYear)) {
              tmpMon[as.character(currentMonth)==as.character(month_year),
                     ConAdmLab := paste(consumersAdmit, " consumers (", admissions, " admissions)", sep="" )]
            }
          # number of months
          tmpMonLength = tmpMon[, length(unique(month_year))]

          # ggplot - monthly consumers admitted and admissions
            p.tmpMon = ggplot(data=tmpMon, aes(x=month_year, y=consumersAdmit, ymax=1.5*consumersAdmit))+
              geom_bar(stat="identity", width = 0.6, fill=um_colors[1])+
              coord_flip()+
              my_theme+
              labs(y="", x="",
                   title= paste("Monthly Community Hospitalizations\nConsumers Admitted and Admissions\nFiscal Year ",
                                tmpFy, sep=""))+
              geom_text(data=tmpMon, hjust= -0.01 , vjust= .5, stat="identity",
                        aes( label = ConAdmLab, x = month_year, fill = month_year,
                             y = consumersAdmit ), size = 2.5 )+
              theme(axis.ticks=element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(size=7),
                    plot.title = element_text(size=9))
            # save plot
              ggsave(plot=p.tmpMon,
                     filename= paste(baseResults, "Monthly/Admissions",
                                     paste("monthly consumers admitted & admissions FY ",
                                           tmpFy, ".pdf", sep=""), sep = "/"),
                     width=5.25, height=tmpMonLength*.15+1.15, dpi=600, units="in")
            # remove R objects
            rm(p.tmpMon)

        # Costs and Consumers
          # ggplot labels
            tmpMon[, costCon := paste( sapply(cost, moneyAdd), " (", consumers, ")", sep="" ) ]
          # labels to prevent a legend - only needed for top quarter
            tmpMon[paste("Sep", tmpFy)==month_year,
              costCon := paste( sapply(cost, moneyAdd), " (", consumers, " consumers)", sep="" ) ]
          # labels to prevent a lengend if the top bar if current month is the top bar and not the end of the fy
          if(tmpFy==as.numeric(fiscalYear)) {
            tmpMon[as.character(currentMonth)==as.character(month_year),
                   costCon := paste( sapply(cost, moneyAdd), " (", consumers, " consumers)", sep="" )]
          }

          # ggplot
            p.tmpMon = ggplot(data=tmpMon, aes(x=month_year, y=cost, ymax=1.5*cost))+
              geom_bar(stat="identity", width = 0.6, fill=um_colors[1])+
              coord_flip()+
              my_theme+
              labs(y="", x="",
                   title= paste("Monthly Community Hospitalizations\nCosts and Consumers\n Fiscal Year ",
                                tmpFy, sep=""))+
              geom_text(data=tmpMon, hjust= -.1 , vjust= .5, stat="identity",
                        aes( label = costCon, x = month_year, fill = month_year,
                             y = cost ), size = 2.5 )+
              theme(axis.ticks=element_blank(),
                    axis.text.x = element_blank())
            # save plot
            ggsave(plot=p.tmpMon,
                   filename= paste(baseResults, "Monthly/Costs",
                                   paste("monthly costs & consumers FY ",
                                         tmpFy, ".pdf", sep=""), sep = "/"),
                   width=5.25, height=tmpMonLength*.15+1.1, dpi=600, units="in")
            # Remove R objects
            rm(p.tmpMon)
      }

    # Quarterly
      # convert to character for ggplot
      qtrSummary[, qtr := as.character(qtr)]

      # all 4 quarters for each fiscal year
      for(i in seq(allFys)) {
      tmpFy = allFys[i]
      qtrByFy = qtrSummary[ substr(qtr, 1, 4) == tmpFy ]

      # only run this code if the quarter is complete
        if(nrow(qtrByFy)>0) {
          # Consumers Admitted and Admissions
            # ggplot labels
            qtrByFy[, ConAdmLab := paste(consumersAdmit, " (", admissions, ")", sep="" )]
            # labels to prevent a legend - only needed for top quarter
            qtrByFy[paste(tmpFy, "Q4")==qtr,
              ConAdmLab := paste(consumersAdmit, " consumers (", admissions, " admissions)", sep="" )]
            # labels to prevent a lengend if the top bar if current month is the top bar and not the end of the fy
            if(tmpFy==as.numeric(fiscalYear)) {
              qtrByFy[currentQuarter==qtr,
                     ConAdmLab := paste(consumersAdmit, " consumers (", admissions, " admissions)", sep="" )]
            }
            # number of quarters
            tmpQtrLength = qtrByFy[, length(unique(qtr))]
            # create plot
              p.qtrByFy = ggplot(data=qtrByFy, aes(x=qtr, y=consumersAdmit, ymax=1.5*consumersAdmit))+
                            geom_bar(stat="identity", width = 0.6, fill=um_colors[1])+
                            coord_flip()+
                            my_theme+
                            labs(y="", x="",
                              title=
                              paste("Quarterly Community Hospitalizations \nConsumers Admitted and Admissions \nFY ",
                                              tmpFy, sep=""))+
                            geom_text(data=qtrByFy, hjust= -0.01 , vjust= .5, stat="identity",
                                      aes( label = ConAdmLab, x = qtr, fill = qtr,
                                           y = consumersAdmit ), size = 2.2 )+
                            theme(axis.ticks=element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.text.y = element_text(size=7),
                                  plot.title = element_text(size=9))
              # save plot
              ggsave(plot=p.qtrByFy,
                     filename= paste(baseResults, "Quarterly/Admissions",
                      paste("quarterly consumers admitted & admissions FY ",
                            tmpFy, ".pdf", sep=""), sep = "/"),
                     width=5.25, height=tmpQtrLength*.25+.75, dpi=600, units="in")
              # remove R objects
              rm(p.qtrByFy)

          # Cost and Consumers
            # ggplot labels
            qtrByFy[, costCon := paste( sapply(cost, moneyAdd), " (", consumers, ")", sep="" ) ]
            # labels to prevent a legend - only needed for top quarter
            qtrByFy[paste(tmpFy, "Q4")==qtr,
                    costCon := paste( sapply(cost, moneyAdd), " (", consumers, " consumers)", sep="") ]
            # labels to prevent a lengend if the top bar if current month is the top bar and not the end of the fy
              if(tmpFy==as.numeric(fiscalYear)) {
                qtrByFy[currentQuarter==qtr,
                       costCon := paste( sapply(cost, moneyAdd), " (", consumers, " consumers)", sep="") ]
              }

            # ggplot
              p.qtrByFy = ggplot(data=qtrByFy, aes(x=qtr, y=cost, ymax=1.5*cost))+
                geom_bar(stat="identity", width = 0.6, fill=um_colors[1])+
                coord_flip()+
                my_theme+
                labs(y="", x="",
                     title= paste("Quarterly Community Hospitalizations\nCosts and Consumers\n FY ",
                                  tmpFy, sep=""))+
                geom_text(data=qtrByFy, hjust= -.1 , vjust= .5, stat="identity",
                          aes( label = costCon, x = qtr, fill = qtr,
                               y = cost ), size = 2.5 )+
                theme(axis.ticks=element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_text(size=7),
                      plot.title = element_text(size=9))
              # save plot
              ggsave(plot=p.qtrByFy,
                     filename= paste(baseResults, "Quarterly/Costs",
                                     paste("quarterly costs & consumers FY ",
                                           tmpFy, ".pdf", sep=""), sep = "/"),
                     width=5, height=tmpQtrLength*.25+.75, dpi=600, units="in")
              # remove R objects
              rm(p.qtrByFy)
        }
    }

        # All fiscal years by quarters
          qtrAllFy = qtrSummary

          # Consumers Admitted and Admissions all years by quarters
            # ggplot labels
              qtrAllFy[, ConAdmLab := paste(consumersAdmit, " (", admissions, ")", sep="" )]
            # labels to prevent a legend - only needed for top quarter
              qtrAllFy[paste(fiscalYear, "Q4")==qtr,
                      ConAdmLab := paste(consumersAdmit, " consumers (", admissions, " admissions)", sep="" )]
            # labels to prevent a lengend if the top bar if current month is the top bar and not the fy End
            if(tmpFy==as.numeric(fiscalYear)) {
              qtrAllFy[currentQuarter==qtr,
                     ConAdmLab := paste(consumersAdmit, " consumers (", admissions, " admissions)", sep="" )]
            }

            # plot creation - consumers/admissions all years by quarters
              p.qtrAllFy = ggplot(data=qtrAllFy, aes(x=qtr, y=consumersAdmit, ymax=1.5*consumersAdmit))+
                geom_bar(stat="identity", width = 0.6, fill=um_colors[1])+
                coord_flip()+
                my_theme+
                labs(y="", x="",
                     title= "Quarterly Community Hospitalizations\nConsumers Admitted and Admissions")+
                geom_text(data=qtrAllFy, hjust= -0.01 , vjust= .5, stat="identity",
                          aes( label = ConAdmLab, x = qtr, fill = qtr,
                               y = consumersAdmit ), size = 2.5 )+
                theme(axis.ticks=element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_text(size=7),
                      plot.title = element_text(size=9))
              # save plot - consumers Admitted and admissions
              ggsave(plot=p.qtrAllFy,
                     filename= paste(baseResults, "Quarterly/Admissions",
                                "quarterly consumers admitted & admissions all years.pdf", sep = "/"),
                     width=5.25, height=tmpQtrLength/4+1.5, dpi=600, units="in")
              # remove R objects
              rm(p.qtrAllFy)

          # Cost and Consumers all years by quarters
            # ggplot labels
              qtrAllFy[, costCon := paste( sapply(cost, moneyAdd), " (", consumers, ")", sep="" ), by = c("qtr")]
            # labels to prevent a legend - only needed for top quarter
              qtrAllFy[paste(fiscalYear, "Q4")==qtr,
                       costCon := paste( sapply(cost, moneyAdd), " (", consumers, " consumers)", sep="" )]
            # labels to prevent a lengend if the top bar of current month is the top bar but not FY end
              if(tmpFy==as.numeric(fiscalYear)) {
                qtrAllFy[currentQuarter==qtr,
                       costCon := paste( sapply(cost, moneyAdd), " (", consumers, " consumers)", sep="" ) ]
              }

            # ggplot creation - cost and consumers all years by quarters
              p.qtrAllFy = ggplot(data=qtrAllFy, aes(x=qtr, y=cost, ymax=1.3*cost))+
                geom_bar(stat="identity", width = 0.6, fill=um_colors[1])+
                coord_flip()+
                my_theme+
                labs(y="", x="",
                     title="Quarterly Community Hospitalizations\nCosts and Consumers: All Fiscal Years")+
                geom_text(data=qtrAllFy, hjust= -.1 , vjust= .5, stat="identity",
                          aes( label = costCon, x = qtr, fill = qtr,
                               y = cost ), size = 2.5 )+
                theme(axis.ticks=element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_text(size=7),
                      plot.title = element_text(size=9))
              # save plot - cost and consumers all years by quarters
              ggsave(plot=p.qtrAllFy,
                     filename= paste(baseResults, "Quarterly/Costs",
                                "quarterly costs & consumers all years.pdf", sep = "/"),
                     width=5, height=tmpQtrLength/4+1.5, dpi=600, units="in")
              # remove un-wanted R objects
              rm(p.qtrAllFy, qtrAllFy, tmpQtrLength)

    # Yearly
      # make a copy of fySummary
      tmpFySummary = copy(fySummary)
      # convert year to character for ggplot
      tmpFySummary[, fy := as.character(fy)]

      # Consumers Admitted and Admissions all years
        # ggplot labels
          tmpFySummary[, ConAdmLab := paste(consumersAdmit, " (", admissions, ")", sep="" )]
        # labels to prevent a legend - only needed for top quarter
          tmpFySummary[fiscalYear==fy,
                   ConAdmLab := paste(consumersAdmit, " consumers (", admissions, " admissions)", sep="" )]
      # Cost and Consumers all years
        # ggplot labels
          tmpFySummary[, costCon := paste( sapply(cost, moneyAdd), " (", consumers, ")", sep="") ]
        # labels to prevent a legend - only needed for top quarter
          tmpFySummary[fiscalYear==fy, costCon := paste( sapply(cost, moneyAdd),
            " (", consumers, " consumers)", sep="") ]
        # number of fiscal years
        tmpFyLength = tmpFySummary[, length(unique(fy))]
        # plot creation - cost/consumers all years by quarters
          p.tmpFy = ggplot(data=tmpFySummary, aes(x=fy, y=cost, ymax=1.5*cost))+
                      geom_bar(stat="identity", width = 0.6, fill=um_colors[1])+
                      coord_flip()+
                      my_theme+
                      labs(y="", x="",
                           title="Yearly Community Hospitalizations\nCosts and Consumers\nAll Fiscal Years")+
                      geom_text(data=tmpFySummary, hjust= -.1 , vjust= .5, stat="identity",
                                aes( label = costCon, x = fy, fill = fy, y = cost ), size = 2.5 )+
                      theme(axis.ticks=element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_text(size=7),
                            plot.title = element_text(size=9))
          # save plot - cost/consumers all years by quarters
          ggsave(plot=p.tmpFy,
                 filename= paste(baseResults, "Yearly/Costs",
                 "yearly costs & consumers all years.pdf", sep = "/"),
                 width=5, height=tmpFyLength/4+.75, dpi=600, units="in")
          # remove R objects
          rm(p.tmpFy)

      # Cost and Consumers all years by quarters
        # ggplot labels
          tmpFySummary[, costCon := paste( sapply(cost, moneyAdd), " (", consumers, ")", sep="" ) ]
        # labels to prevent a legend - only needed for top quarter
          tmpFySummary[fiscalYear==fy,
            costCon := paste( sapply(cost, moneyAdd), " (", consumers, " consumers)", sep="" ) ]

        # ggplot creation - cost/consumers all years by quarters
          p.tmpFy = ggplot(data=tmpFySummary, aes(x=fy, y=cost, ymax=1.25*cost))+
                        geom_bar(stat="identity", width = 0.6, fill=um_colors[1])+
                        coord_flip()+
                        my_theme+
                        labs(y="", x="",
                        title="Yearly Community Hospitalizations\nCosts and Consumers\nAll Fiscal Years")+
                        geom_text(data=tmpFySummary, hjust= -.1 , vjust= .5, stat="identity",
                                  aes( label = costCon, x = fy, fill = fy, y = cost ), size = 2.5 )+
                        theme(axis.ticks=element_blank(),
                              axis.text.x = element_blank(),
                              axis.text.y = element_text(size=7),
                              plot.title = element_text(size=9))
          # save plot - cost/consumers all years by quarters
          ggsave(plot=p.tmpFy,
                 filename= paste(baseResults, "Yearly/Costs",
                                       "yearly costs & consumers all years.pdf", sep = "/"),
                       width=5.25, height=tmpFyLength/4+1, dpi=600, units="in")
          # remove R objects
          rm(p.tmpFy, tmpFyLength)

  ### LOS Histograms ###
    # for loop to cycle data through each fiscal year
    for( i in seq(allFys)) {
    # each fiscal year by quarters
    tmpHist = hosp_data[fiscalyr==allFys[i]]
    # remove NAs from data.table so that it doesnt show up on any ggplots
    tmpHist = tmpHist[!is.na(finBin)]

      ## Monthly ## - not doing monthly right now -

      ## Quarterly ##
        # fiscal years by quarters
          # restrict data based on current quarter
          tmpHist = tmpHist[ Quarter <= currentQuarter]
          # convert quarter to factor for ggplot graphing
          tmpHist[, Quarter := factor(Quarter)]
          # if number of rows is less than 1, skip quarter graphs
          if(nrow(tmpHist)>0) {
           # Financial Length of Stay
            # fiscal years by quarters - financial
            p.fyByQtrs = ggplot(data = tmpHist, aes(x = finBin) )+
                            geom_bar(stat = "bin", position = "stack", width = 1,
                                     fill = um_colors[1], color = um_colors[4])+
                            facet_wrap( ~ Quarter, scales = "fixed")+
                            my_theme+theme( axis.title.x = element_text(colour = "black", size=8),
                                            axis.title.y = element_text(colour = "black", size=8),
                                            axis.text.x = element_text(angle=90, vjust = 0.3, size=8 ),
                                            plot.title = element_text(size=8))+
                            labs(x = "Length of Stay in Days",
                                 y = "Number of Admissions",
                                 title = paste("FY", allFys[i],
                                  "Community Hospitalizations\nFinancial Length of Stay by Quarter"))
            # save plot - fiscal years by quarters
            ggsave(plot=p.fyByQtrs,
                   filename= paste(baseResults, "Quarterly/Length of Stay/Histograms",
                              paste("quarterly finLOS histogram FY ", allFys[i], ".pdf", sep=""), sep = "/"),
                   width=2.5, height=5, dpi=600, units="in")
            # remove R objects
            rm(p.fyByQtrs)

           # Clinical Length of Stay
            # fiscal years by quarters - clinical
            p.fyByQtrs = ggplot(data = tmpHist, aes(x = clinBin) )+
              geom_bar(stat = "bin", position = "stack", width = 1,
                       fill = um_colors[1], color = um_colors[4])+
              facet_wrap( ~ Quarter, scales = "fixed")+
              my_theme+theme( axis.title.x = element_text(colour = "black", size=8),
                              axis.title.y = element_text(colour = "black", size=8),
                              axis.text.x = element_text(angle=90, vjust = 0.3, size=8 ),
                              plot.title=element_text(size=8))+
              labs(x = "Length of Stay in Days",
                   y = "Number of Admissions",
                   title = paste("FY", allFys[i],
                                 "Community Hospitalizations\nClinical Length of Stay by Quarter"))
            # save plot - fiscal years by quarters
            ggsave(plot=p.fyByQtrs,
                   filename= paste(baseResults, "Quarterly/Length of Stay/Histograms",
                                   paste("quarterly clinLOS histogram FY ", allFys[i], ".pdf", sep=""), sep = "/"),
                   width=2.5, height=4.5, dpi=600, units="in")
            # remove R objects
            rm(p.fyByQtrs)
          } # end of if statement

      ## Yearly ##
        # each fiscal year by quarters
        tmpHist = hosp_data[fiscalyr==allFys[i]]
        # remove NAs from data.table so that it doesnt show up on any ggplots
        tmpHist = tmpHist[!is.na(finBin)]

        # financial LOS - fiscal years
          p.fy = ggplot(data = tmpHist, aes(x = finBin) )+
            geom_bar(stat = "bin", position = "stack", width = 1,
                     fill = um_colors[1], color = um_colors[4])+
            my_theme+theme( axis.title.x = element_text(colour = "black", size=7),
                            axis.title.y = element_text(colour = "black", size=7),
                            axis.text.x = element_text(angle=90, vjust = 0.3, size=7 ),
                            axis.text.y = element_text(size=7),
                            plot.title = element_text(size=8))+
            labs(x = "Length of Stay in Days",
                 y = "Number of Admissions",
                 title = paste("Community Hospitalizations\nFinancial Length of Stay\n FY", allFys[i]))
          # save plot - fiscal years
          ggsave(plot=p.fy,
                 filename= paste(baseResults, "Yearly/Length of Stay/Histograms",
                                 paste("yearly finLOS histogram FY ", allFys[i], ".pdf", sep=""), sep = "/"),
                 width=2, height=3.5, dpi=600, units="in")
          # remove R objects
          rm(p.fy)

        # clinical LOS - fiscal years
          p.fy = ggplot(data = tmpHist, aes(x = clinBin) )+
            geom_bar(stat = "bin", position = "stack", width = 1,
                     fill = um_colors[1], color = um_colors[4])+
            my_theme+theme( axis.title.x = element_text(colour = "black", size=7),
                            axis.title.y = element_text(colour = "black", size=7),
                            axis.text.x = element_text(angle=90, vjust = 0.3, size=7 ),
                            axis.text.y = element_text(size=7),
                            plot.title = element_text(size=8))+
            labs(x = "Length of Stay in Days",
                 y = "Number of Admissions",
                 title = paste("Community Hospitalizations\nClinical Length of Stay\n FY",
                               allFys[i]))
          # save plot - fiscal years
          ggsave(plot=p.fy,
                 filename= paste(baseResults, "Yearly/Length of Stay/Histograms",
                                 paste("yearly clinLOS histogram FY ", allFys[i], ".pdf", sep=""), sep = "/"),
                 width=2, height=3.5, dpi=600, units="in")
          # remove R objects
          rm(p.fy)
          }
          # remove un-wanted R objects
          rm(qtrByFy, tmpHist, tmpFy, tmpMon, tmpFySummary)

#### LOS Boxplots ####
  # make a copy of hospital data
    losData = copy(hosp_data)
  # remove records prior to 10/1/2010
    losData = losData[!(Auth_eff<as.Date("2010-10-01"))]
  # remove records without a discharge date
    losData = losData[!is.na(Hosp_disc)]
  # keep only columns that are needed
    keepNames = c("case_no", "adult", "UnitsUsed", "Auth_ID", "Program",
                  "month_year", "fiscalyr", "Quarter", "fund", "clinLOS")
    losData[, setdiff(colnames(losData), keepNames) := NULL ]
  # convert units used to numeric
    losData[, UnitsUsed := as.numeric(UnitsUsed)]
  # convert year to character
    losData[, fiscalyr := as.character(fiscalyr) ]
  # convert quarter to character
    losData[, Quarter := as.character(Quarter)]
  # make finLOS column
    losData[, finLOS := my_min( c(units=UnitsUsed, clinLOS)), by = "Auth_ID" ]
  # remove records where clinLOS is 0
    losData = losData[!clinLOS==0]
  ### yearly LOS graphics ###
    ### overall ###
      ### financial ###
        ## Regular boxplot - not log transformed ##
          p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="finLOS", group=NULL, jitter.x=0.2,
                        title.plot="Community Hospitalizations \nOverall Financial Length of Stay by FY",
                        title.x.axis="Fiscal Years", title.y.axis="Financial Length of Stay in Days",
                        logBase=NULL)
          # save plot
          ggsave(plot=p.boxplot,
                 filename= paste(baseResults,
                  "Yearly/Length of Stay/Boxplots/Jittered/Overall/ch overall finLOS by fy.pdf", sep = "/"),
                 width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="finLOS", group=NULL, jitter.x=0.2,
                       title.plot="Community Hospitalizations \nOverall Log Financial Length of Stay by FY",
                       title.x.axis="Fiscal Years", title.y.axis="Financial Length of Stay in Days", logBase=2)
          # save plot
          ggsave(plot=p.boxplot,
                 filename= paste(baseResults,
                    "Yearly/Length of Stay/Boxplots/Jittered/Overall/ch log overall finLOS by fy.pdf", sep = "/"),
                 width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
      ### clinical ###
        ## Regular boxplot - not log transformed ##
          p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="clinLOS", group=NULL, jitter.x=0.2,
                               title.plot="Community Hospitalizations \nOverall Clinical Length of Stay by FY",
                               title.x.axis="Fiscal Years", title.y.axis="Clinical Length of Stay in Days",
                               logBase=NULL)
          # save plot
          ggsave(plot=p.boxplot,
                 filename= paste(baseResults,
                    "Yearly/Length of Stay/Boxplots/Jittered/Overall/ch overall clinLOS by fy.pdf", sep = "/"),
                 width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="clinLOS", group=NULL, jitter.x=0.2,
                        title.plot="Community Hospitalizations \nOverall Log Clinical Length of Stay by FY",
                        title.x.axis="Fiscal Years", title.y.axis="Clinical Length of Stay in Days", logBase=2)
          # save plot
          ggsave(plot=p.boxplot,
                 filename= paste(baseResults,
                   "Yearly/Length of Stay/Boxplots/Jittered/Overall/ch log overall clinLOS by fy.pdf", sep = "/"),
                 width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
    ### program ###
      ### financial ###
        ## Regular boxplot - not log transformed ##
          p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="finLOS", group="Program", jitter.x=0.2,
                        title.plot="Community Hospitalizations \nProgram Financial Length of Stay by FY",
                        title.x.axis="Fiscal Years", title.y.axis="Financial Length of Stay in Days",
                        logBase=NULL)
          # save plot
          ggsave(plot=p.boxplot,
                 filename= paste(baseResults,
                    "Yearly/Length of Stay/Boxplots/Jittered/Program/ch program finLOS by fy.pdf", sep = "/"),
                 width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="finLOS", group="Program", jitter.x=0.2,
                        title.plot="Community Hospitalizations \nProgram Log Financial Length of Stay by FY",
                        title.x.axis="Fiscal Years", title.y.axis="Financial Length of Stay in Days", logBase=2)
          # save plot
          ggsave(plot=p.boxplot,
                 filename= paste(baseResults,
                    "Yearly/Length of Stay/Boxplots/Jittered/Program/ch log program finLOS by fy.pdf", sep = "/"),
                 width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
      ### clinical ###
        ## Regular boxplot - not log transformed ##
          p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="clinLOS", group="Program", jitter.x=0.2,
                        title.plot="Community Hospitalizations \nProgram Clinical Length of Stay by FY",
                        title.x.axis="Fiscal Years", title.y.axis="Clinical Length of Stay in Days",
                        logBase=NULL)
          # save plot
          ggsave(plot=p.boxplot,
                 filename= paste(baseResults,
                  "Yearly/Length of Stay/Boxplots/Jittered/Program/ch program clinLOS by fy.pdf", sep = "/"),
                 width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="clinLOS", group="Program", jitter.x=0.2,
                        title.plot="Community Hospitalizations \nProgram Log Clinical Length of Stay by FY",
                        title.x.axis="Fiscal Years", title.y.axis="Clinical Length of Stay in Days", logBase=2)
          # save plot
          ggsave(plot=p.boxplot,
                 filename= paste(baseResults,
                  "Yearly/Length of Stay/Boxplots/Jittered/Program/ch log program clinLOS by fy.pdf", sep = "/"),
                 width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
    ### fund ###
      ### financial ###
        ## Regular boxplot - not log transformed ##
        p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="finLOS", group="fund", jitter.x=0.2,
                      title.plot="Community Hospitalizations \nFund Financial Length of Stay by FY",
                      title.x.axis="Fiscal Years", title.y.axis="Financial Length of Stay in Days", logBase=NULL)
        # save plot
        ggsave(plot=p.boxplot,
               filename= paste(baseResults,
                "Yearly/Length of Stay/Boxplots/Jittered/Fund/ch fund finLOS by fy.pdf", sep = "/"),
               width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="finLOS", group="fund", jitter.x=0.2,
                       title.plot="Community Hospitalizations \nFund Log Financial Length of Stay by FY",
                       title.x.axis="Fiscal Years", title.y.axis="Financial Length of Stay in Days", logBase=2)
          # save plot
          ggsave(plot=p.boxplot,
                 filename= paste(baseResults,
                  "Yearly/Length of Stay/Boxplots/Jittered/Fund/ch log fund finLOS by fy.pdf", sep = "/"),
                 width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
      ### clinical ###
        ## Regular boxplot - not log transformed ##
          p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="clinLOS", group="fund", jitter.x=0.2,
                       title.plot="Community Hospitalizations \nFund Clinical Length of Stay by FY",
                       title.x.axis="Fiscal Years", title.y.axis="Clinical Length of Stay in Days",
                       logBase=NULL)
          # save plot
          ggsave(plot=p.boxplot,
                 filename= paste(baseResults,
                                 "Yearly/Length of Stay/Boxplots/Jittered/Fund/ch fund clinLOS by fy.pdf", sep = "/"),
                 width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="fiscalyr", yVar="clinLOS", group="fund", jitter.x=0.2,
                       title.plot="Community Hospitalizations \nFund Log Clinical Length of Stay by FY",
                       title.x.axis="Fiscal Years", title.y.axis="Clinical Length of Stay in Days", logBase=2)
          # save plot
          ggsave(plot=p.boxplot,
                 filename= paste(baseResults,
                  "Yearly/Length of Stay/Boxplots/Jittered/Fund/ch log fund clinLOS by fy.pdf", sep = "/"),
                 width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
  ### Quarterly ###
    ### overall ###
      ### financial ###
        ## Regular boxplot - not log transformed ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="finLOS", group=NULL, jitter.x=0.2,
            title.plot="Community Hospitalizations \nOverall Financial Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Financial Length of Stay in Days", logBase=NULL)+
          coord_flip()
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
              "Quarterly/Length of Stay/Boxplots/Jittered/Overall/ch overall finLOS by qtr.pdf", sep = "/"),
            width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="finLOS", group=NULL, jitter.x=0.2,
            title.plot="Community Hospitalizations \nOverall Log Financial Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Financial Length of Stay in Days", logBase=2)+
          coord_flip()
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
              "Quarterly/Length of Stay/Boxplots/Jittered/Overall/ch log overall finLOS by qtr.pdf", sep = "/"),
            width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
      ### clinical ###
        ## Regular boxplot - not log transformed ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="clinLOS", group=NULL, jitter.x=0.2,
            title.plot="Community Hospitalizations \nOverall Clinical Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Clinical Length of Stay in Days", logBase=NULL)+
          coord_flip()
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
              "Quarterly/Length of Stay/Boxplots/Jittered/Overall/ch overall clinLOS by qtr.pdf", sep = "/"),
            width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="clinLOS", group=NULL, jitter.x=0.2,
            title.plot="Community Hospitalizations \nOverall Log Clinical Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Clinical Length of Stay in Days", logBase=2)+
          coord_flip()
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
              "Quarterly/Length of Stay/Boxplots/Jittered/Overall/ch log overall clinLOS by qtr.pdf", sep = "/"),
            width=4, height=losData[, length(unique(fiscalyr))]*.5+2.25, dpi=600, units="in")
    ### program ###
      ### financial ###
        ## Regular boxplot - not log transformed ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="finLOS", group="Program", jitter.x=0.2,
            title.plot="Community Hospitalizations \nProgram Financial Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Financial Length of Stay in Days", logBase=NULL,
            xlabAngle=90)
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Quarterly/Length of Stay/Boxplots/Jittered/Program/ch program finLOS by qtr.pdf", sep = "/"),
            width=losData[, length(unique(Quarter))]*.2+3, height=5, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="finLOS", group="Program", jitter.x=0.2,
            title.plot="Community Hospitalizations \nProgram Log Financial Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Financial Length of Stay in Days", logBase=2,
            xlabAngle=90)
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Quarterly/Length of Stay/Boxplots/Jittered/Program/ch log program finLOS by qtr.pdf", sep = "/"),
            width=losData[, length(unique(Quarter))]*.2+3, height=5, dpi=600, units="in")
      ### clinical ###
        ## Regular boxplot - not log transformed ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="clinLOS", group="Program", jitter.x=0.2,
            title.plot="Community Hospitalizations \nProgram Clinical Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Clinical Length of Stay in Days", logBase=NULL,
            xlabAngle=90)
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Quarterly/Length of Stay/Boxplots/Jittered/Program/ch program clinLOS by qtr.pdf", sep = "/"),
            width=losData[, length(unique(Quarter))]*.2+3, height=5, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="clinLOS", group="Program", jitter.x=0.2,
            title.plot="Community Hospitalizations \nProgram Log Clinical Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Clinical Length of Stay in Days", logBase=2,
            xlabAngle=90)
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Quarterly/Length of Stay/Boxplots/Jittered/Program/ch log program clinLOS by qtr.pdf", sep = "/"),
            width=losData[, length(unique(Quarter))]*.2+3, height=5, dpi=600, units="in")
    ### fund ###
      ### financial ###
        ## Regular boxplot - not log transformed ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="finLOS", group="fund", jitter.x=0.2,
            title.plot="Community Hospitalizations \nFund Financial Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Financial Length of Stay in Days", logBase=NULL,
            xlabAngle=90)
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Quarterly/Length of Stay/Boxplots/Jittered/Fund/ch fund finLOS by qtr.pdf", sep = "/"),
            width=losData[, length(unique(Quarter))]*.2+3, height=5, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="finLOS", group="fund", jitter.x=0.2,
            title.plot="Community Hospitalizations \nFund Log Financial Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Financial Length of Stay in Days", logBase=2,
            xlabAngle=90)
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
              "Quarterly/Length of Stay/Boxplots/Jittered/Fund/ch log fund finLOS by qtr.pdf", sep = "/"),
            width=losData[, length(unique(Quarter))]*.2+3, height=5, dpi=600, units="in")
      ### clinical ###
        ## Regular boxplot - not log transformed ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="clinLOS", group="fund", jitter.x=0.2,
            title.plot="Community Hospitalizations \nFund Clinical Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Clinical Length of Stay in Days", logBase=NULL,
            xlabAngle=90)
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Quarterly/Length of Stay/Boxplots/Jittered/Fund/ch fund clinLOS by qtr.pdf", sep = "/"),
            width=losData[, length(unique(Quarter))]*.2+3, height=5, dpi=600, units="in")
        ## Log2 boxplot ##
          p.boxplot = bpJitter(dataset=losData, xVar="Quarter", yVar="clinLOS", group="fund", jitter.x=0.2,
            title.plot="Community Hospitalizations \nFund Log Clinical Length of Stay by Quarters",
            title.x.axis="Quarters", title.y.axis="Clinical Length of Stay in Days", logBase=2,
            xlabAngle=90)
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Quarterly/Length of Stay/Boxplots/Jittered/Fund/ch log fund clinLOS by qtr.pdf", sep = "/"),
            width=losData[, length(unique(Quarter))]*.2+3, height=5, dpi=600, units="in")
  ### Monthly ###
    ### overall ###
      # cycle through 'log base 2' and 'no log transformation'
      for(m in 1:2) {
        logBase=list(NULL, 2)[[m]]
        if(!is.null(logBase)) {
          logLabel="Log 2"
          logSaveLabel=tolower(substr(logLabel, 1,4))} else {logLabel=NULL; logSaveLabel=NULL}
        # cycle through financial and clinical
        for(k in 1:2) {
          yVar = c("finLOS", "clinLOS")[k]
          # cycle through fiscal years by months
          for( i in seq(losData[, unique(fiscalyr)])) {
            if(yVar=="finLOS") {yLabel="Financial"} else {yLabel="Clinical"}
            fy=losData[, unique(fiscalyr)][i]
            tmpMonLength = length(losData[fiscalyr==fy][, unique(month_year)])
            p.boxplot = monBpJitter(fy=fy, dataset=losData, xVar="month_year", yVar=yVar,
              group=NULL, jitter.x=0.2,
              title.plot=paste("Community Hospitalizations: FY", fy, "\n", logLabel,
                               "Overall", yLabel,"Length of Stay by Months"),
              title.x.axis="Months", title.y.axis=paste(yLabel, "Length of Stay in Days"),
              logBase=logBase, flip=TRUE)
            # save plot
            ggsave(plot=p.boxplot,
                   filename= paste(baseResults,
                              paste("Monthly/Length of Stay/Boxplots/Jittered/Overall/ch ",
                                    logSaveLabel, "overall ", yVar," FY ",
                                   fy, " by mon.pdf", sep=""), sep = "/"),
                   width=4, height=tmpMonLength/3, dpi=600, units="in")
        } } }
        # remove R objects
        rm(fy, tmpMonLength, p.boxplot, yVar, logBase, logLabel, logSaveLabel)
    ### program and fund ###
      # cycle through facet groups
      for ( g in 1:2 ) {
        my_group = c("Program", "fund")[g]
      # cycle through 'log base 2' and 'no log transformation'
      for(m in 1:2) {
        logBase=list(NULL, 2)[[m]]
        if(!is.null(logBase)) {
          logLabel="Log 2"
          logSaveLabel=tolower(substr(logLabel, 1,4))} else {logLabel=NULL; logSaveLabel=NULL}
        # cycle through financial and clinical
        for(k in 1:2) {
          yVar = c("finLOS", "clinLOS")[k]
          # cycle through fiscal years by months
          for( i in seq(losData[, unique(fiscalyr)])) {
            if(yVar=="finLOS") {yLabel="Financial"} else {yLabel="Clinical"}
            fy=losData[, unique(fiscalyr)][i]
            tmpMonLength = length(losData[fiscalyr==fy][, unique(month_year)])
            p.boxplot = monBpJitter(fy=fy, dataset=losData, xVar="month_year", yVar=yVar,
                                    group=my_group, jitter.x=0.2,
                                    title.plot=paste("Community Hospitalizations: FY", fy, "\n", logLabel,
                                                     simpleCap(my_group), yLabel,"Length of Stay by Months"),
                                    title.x.axis="Months", title.y.axis=paste(yLabel, "Length of Stay in Days"),
                                    logBase=logBase, flip=FALSE, xlabAngle=90)
            # save plot
            ggsave(plot=p.boxplot,
                   filename= paste(baseResults,
                              paste("Monthly/Length of Stay/Boxplots/Jittered/", simpleCap(my_group), "/ch ",
                                logSaveLabel, my_group, " ", yVar," FY ",
                                fy, " by mon.pdf", sep=""), sep = "/"),
                   height=5, width=tmpMonLength/3+2.5, dpi=600, units="in")
          } } } }
      # remove R objects
      rm(fy, tmpMonLength, p.boxplot, yVar, logBase, logLabel,
         logSaveLabel, yLabel, k, m, i, g, levelOrder)

  ### non-jittered yearly boxplots with 15% top of data removed
    ### Yearly ###
      ## overall ##
        # financial
          p.boxplot = trimBp(data=losData, xVar="fiscalyr", yVar="finLOS",
            title="Financial LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
            x.axis.title="Fiscal Years", y.axis.title="Length of Stay in Days", trim=0.15, group=NULL)
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Yearly/Length of Stay/Boxplots/Trimmed/Overall/ch overall finLOS by fy.pdf", sep = "/"),
            width=losData[, length(unique(fiscalyr))]*.25+2.5, height=5.5, dpi=600, units="in")
        # clinical
          p.boxplot = trimBp(data=losData, xVar="fiscalyr", yVar="clinLOS",
           title="Clinical LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
           x.axis.title="Fiscal Years", y.axis.title="Length of Stay in Days", trim=0.15, group=NULL)
          # save plot
          ggsave(plot=p.boxplot,
           filename= paste(baseResults,
            "Yearly/Length of Stay/Boxplots/Trimmed/Overall/ch overall clinLOS by fy.pdf", sep = "/"),
           width=losData[, length(unique(fiscalyr))]*.25+2.5, height=5.5, dpi=600, units="in")
      ## fund ##
        # financial
          p.boxplot = trimBp(data=losData, xVar="fiscalyr", yVar="finLOS",
           title="Financial LOS by Fund: 15% Trimmed Mean\n w/ 15% Trimmed Data", scales = "free_x",
           x.axis.title="Fiscal Years", y.axis.title="Length of Stay in Days", trim=0.15, group="fund")
          # save plot
          ggsave(plot=p.boxplot,
           filename= file.path(baseResults,
            "Yearly/Length of Stay/Boxplots/Trimmed/Fund/ch fund finLOS by fy.pdf"),
           width=losData[, length(unique(fiscalyr))]*.25+2.5, height=5.5, dpi=600, units="in")
        # clinical
          p.boxplot = trimBp(data=losData, xVar="fiscalyr", yVar="clinLOS",
           title="Clinical LOS by Fund: 15% Trimmed Mean\n w/ 15% Trimmed Data", scales = "free_x",
           x.axis.title="Fiscal Years", y.axis.title="Length of Stay in Days", trim=0.15, group="fund")
          # save plot
          ggsave(plot=p.boxplot,
           filename= paste(baseResults,
            "Yearly/Length of Stay/Boxplots/Trimmed/Fund/ch fund clinLOS by fy.pdf", sep = "/"),
           width=losData[, length(unique(fiscalyr))]*.25+2.5, height=5.5, dpi=600, units="in")
      ## Program ##
        # financial
        p.boxplot = trimBp(data=losData, xVar="fiscalyr", yVar="finLOS",
          title="Financial LOS by Program: 15% Trimmed Mean\n w/ 15% Trimmed Data",
          x.axis.title="Fiscal Years", y.axis.title="Length of Stay in Days", trim=0.15, group="Program")
        # save plot
        ggsave(plot=p.boxplot,
          filename= paste(baseResults,
           "Yearly/Length of Stay/Boxplots/Trimmed/Program/ch program finLOS by fy.pdf", sep = "/"),
          width=losData[, length(unique(fiscalyr))]*.25+2.5, height=5.5, dpi=600, units="in")
        # clinical
        p.boxplot = trimBp(data=losData, xVar="fiscalyr", yVar="clinLOS",
          title="Clinical LOS by Program: 15% Trimmed Mean\n w/ 15% Trimmed Data",
          x.axis.title="Fiscal Years", y.axis.title="Length of Stay in Days", trim=0.15, group="Program")
        # save plot
        ggsave(plot=p.boxplot,
          filename= paste(baseResults,
           "Yearly/Length of Stay/Boxplots/Trimmed/Program/ch program clinLOS by fy.pdf", sep = "/"),
          width=losData[, length(unique(fiscalyr))]*.25+2.5, height=5.5, dpi=600, units="in")
    ### Quarterly ###
      ## Overall ##
        # financial
          p.boxplot = trimBp(data=losData, xVar="Quarter", yVar="finLOS",
            title="Financial LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
            x.axis.title="Quarters", y.axis.title="Length of Stay in Days", trim=0.15, group=NULL)+coord_flip()
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Quarterly/Length of Stay/Boxplots/Trimmed/Overall/ch overall finLOS by qtr.pdf", sep = "/"),
            width=4, height=losData[, length(unique(fiscalyr))]*.25+2.5, dpi=600, units="in")
        # clinical
          p.boxplot = trimBp(data=losData, xVar="Quarter", yVar="clinLOS",
            title="Clinical LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
            x.axis.title="Quarters", y.axis.title="Length of Stay in Days", trim=0.15, group=NULL)+coord_flip()
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Quarterly/Length of Stay/Boxplots/Trimmed/Overall/ch overall clinLOS by qtr.pdf", sep = "/"),
            width=4, height=losData[, length(unique(fiscalyr))]*.25+2.5, dpi=600, units="in")
      ## Fund ##
        # financial
        p.boxplot = trimBp(data=losData, xVar="Quarter", yVar="finLOS",
          title="Financial LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
          x.axis.title="Quarters", y.axis.title="Length of Stay in Days",
          trim=0.15, group="fund", xLabAngle=90, scales = "free_x")
        # save plot
        ggsave(plot=p.boxplot,
          filename= paste(baseResults,
           "Quarterly/Length of Stay/Boxplots/Trimmed/Fund/ch fund finLOS by qtr.pdf", sep = "/"),
          width=losData[, length(unique(fiscalyr))]*.25+4.5, height=5.5, dpi=600, units="in")
        # clinical
        p.boxplot = trimBp(data=losData, xVar="Quarter", yVar="clinLOS",
          title="Clinical LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
          x.axis.title="Quarters", y.axis.title="Length of Stay in Days",
          trim=0.15, group="fund", xLabAngle=90, scales = "free_x")
        # save plot
        ggsave(plot=p.boxplot,
          filename= paste(baseResults,
           "Quarterly/Length of Stay/Boxplots/Trimmed/Fund/ch fund clinLOS by qtr.pdf", sep = "/"),
          width=losData[, length(unique(fiscalyr))]*.25+4.5, height=5.5, dpi=600, units="in")
      ## Program ##
        # financial
        p.boxplot = trimBp(data=losData, xVar="Quarter", yVar="finLOS",
          title="Financial LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
          x.axis.title="Quarters", y.axis.title="Length of Stay in Days",
          trim=0.15, group="Program", xLabAngle=90)
        # save plot
        ggsave(plot=p.boxplot,
          filename= paste(baseResults,
           "Quarterly/Length of Stay/Boxplots/Trimmed/Program/ch program finLOS by qtr.pdf", sep = "/"),
          width=losData[, length(unique(fiscalyr))]*.25+4.5, height=5.5, dpi=600, units="in")
        # clinical
        p.boxplot = trimBp(data=losData, xVar="Quarter", yVar="clinLOS",
          title="Clinical LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
          x.axis.title="Quarters", y.axis.title="Length of Stay in Days",
          trim=0.15, group="Program", xLabAngle=90)
        # save plot
        ggsave(plot=p.boxplot,
          filename= paste(baseResults,
           "Quarterly/Length of Stay/Boxplots/Trimmed/Program/ch program clinLOS by qtr.pdf", sep = "/"),
          width=losData[, length(unique(fiscalyr))]*.25+4.5, height=5.5, dpi=600, units="in")
    ### Monthly ###
      losData[, month_year :=
        factor(month_year, levels=c(unique(as.character(month_year)[order(month_year)])) ) ]
      ## Overall ##
        # financial
          p.boxplot = trimBp(data=losData, xVar="month_year", yVar="finLOS",
            title="Financial LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
            x.axis.title="Months", y.axis.title="Length of Stay in Days",
            trim=0.15, group="fiscalyr", xLabAngle=90)
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Monthly/Length of Stay/Boxplots/Trimmed/Overall/ch overall finLOS by mon.pdf", sep = "/"),
            width=losData[, length(unique(fiscalyr))]*.25+5.5, height=4, dpi=600, units="in")
        # clinical
          p.boxplot = trimBp(data=losData, xVar="month_year", yVar="clinLOS",
            title="Clinical LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
            x.axis.title="Months", y.axis.title="Length of Stay in Days",
            trim=0.15, group="fiscalyr", xLabAngle=90)
          # save plot
          ggsave(plot=p.boxplot,
            filename= paste(baseResults,
             "Monthly/Length of Stay/Boxplots/Trimmed/Overall/ch overall clinLOS by mon.pdf", sep = "/"),
            width=losData[, length(unique(fiscalyr))]*.25+5.5, height=4, dpi=600, units="in")

      ## Program ##
        # financial
          for(i in seq(allFys) ) {
            p.boxplot = trimBp(data=losData[fiscalyr==as.character(allFys[i])], xVar="month_year", yVar="finLOS",
                               title="Financial LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
                               x.axis.title="Months", y.axis.title="Length of Stay in Days",
                               trim=0.15, group="Program", xLabAngle=90, scales="fixed")
            # save plot
            ggsave(plot=p.boxplot,
             filename= paste(baseResults,
              paste("Monthly/Length of Stay/Boxplots/Trimmed/Program/ch program finLOS by mon FY ",
               as.character(allFys[i]), ".pdf", sep=""),
                sep = "/"), width=5, height=4, dpi=600, units="in")
          }
        # clinical
          for(i in seq(allFys) ) {
            p.boxplot = trimBp(data=losData[fiscalyr==as.character(allFys[i])], xVar="month_year", yVar="clinLOS",
                               title="Clinical LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
                               x.axis.title="Months", y.axis.title="Length of Stay in Days",
                               trim=0.15, group="Program", xLabAngle=90, scales="fixed")
            # save plot
            ggsave(plot=p.boxplot,
             filename= paste(baseResults,
              paste("Monthly/Length of Stay/Boxplots/Trimmed/Program/ch program clinLOS by mon FY ",
               as.character(allFys[i]), ".pdf", sep=""), sep = "/"), width=5, height=4, dpi=600, units="in")
          }

      ### Fund ###
        # financial
          for(i in seq(allFys) ) {
            p.boxplot = trimBp(data=losData[fiscalyr==as.character(allFys[i])], xVar="month_year", yVar="finLOS",
             title="Financial LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
             x.axis.title="Months", y.axis.title="Length of Stay in Days",
             trim=0.15, group="fund", xLabAngle=90, scales="fixed")
            # save plot
            ggsave(plot=p.boxplot,
             filename= paste(baseResults,
              paste("Monthly/Length of Stay/Boxplots/Trimmed/Fund/ch fund finLOS by mon FY ",
               as.character(allFys[i]), ".pdf", sep=""), sep = "/"), width=5, height=4, dpi=600, units="in")
          }
        # clinical
          for(i in seq(allFys) ) {
            p.boxplot = trimBp(data=losData[fiscalyr==as.character(allFys[i])],
                               xVar="month_year", yVar="clinLOS",
             title="Clinical LOS: 15% Trimmed Mean\n w/ 15% Trimmed Data",
             x.axis.title="Months", y.axis.title="Length of Stay in Days",
             trim=0.15, group="fund", xLabAngle=90, scales="fixed")
            # save plot
            ggsave(plot=p.boxplot,
             filename= paste(baseResults,
              paste("Monthly/Length of Stay/Boxplots/Trimmed/Fund/ch fund clinLOS by mon FY ",
               as.character(allFys[i]), ".pdf", sep=""), sep = "/"), width=5, height=4, dpi=600, units="in")
          }

  ### Funding ###
    # add cost label
    fyFundSummary[, costLab := sapply(cost, moneyAdd) ]
    qtrFundSummary[, costLab := sapply(cost, moneyAdd) ]
    monFundSummary[, costLab := sapply(cost, moneyAdd) ]
    # remove 'no funding' Fund
    fyFundSummary = fyFundSummary[!fund=="no funding"]
    qtrFundSummary = qtrFundSummary[!fund=="no funding"]
    monFundSummary = monFundSummary[!fund=="no funding"]

    ## year funding ##
      # year fund labels
      if( nrow(fyFundSummary[fy==as.numeric(fiscalYear)])<1 ) {
        stop("No current fiscal year data to label fund - check to see if all data is loaded into RAM")
      }
      # fix if NAs exist - replace with 0
      fyFundSummary[is.na(admissions), admissions := 0]
      fyFundSummary[is.na(consumerAdmit), consumerAdmit := 0]

        fyFundSummary[fy==as.numeric(fiscalYear), fundLab := fund]
        fyFundSummary[fy==as.numeric(fiscalYear) & fund=="ABW",
                      costLab := paste(paste(rep(" ", 4), collapse=""), costLab) ]
        fyFundSummary[, conCostLab := paste(consumers, " (", trim(costLab), ")", sep="")]
      # fix fyFundSummary fy from numeric to character for ggplot
        fyFundSummary[, fy := as.character(fy)]
    ## quarter funding ##

      # quarter fund fund labels
      # qtrFundSummary[, qtr := as.numeric(qtr)]
      qtrFundSummary[qtr==as.character(currentQuarter), fundLab := fund]
      qtrFundSummary[qtr==as.character(currentQuarter) & fund=="ABW",
                     costLab := paste(paste(rep(" ", 4), collapse=""), costLab) ]
      # fix qtrFundSummary qtr from date class to character for ggplot
        qtrFundSummary[, qtr := as.character(qtr) ]
      # number of years
      tmpFyLength = fyFundSummary[, length(unique(fy))]
      # create plot
      p.fyFund = ggplot(data=fyFundSummary, aes(x=fy, fill=fund, y=cost, ymax=1.5*cost))+
        geom_bar(stat="identity", width = 0.6, position = position_dodge(.6) )+
        coord_flip()+
        my_theme+
        labs(y="", x="",
             title="Yearly Community Hospitalizations\nCost by Fund: All Fiscal Years")+
        geom_text(data=fyFundSummary, hjust= -.1 , vjust= .5, stat="identity", position = position_dodge(.6),
                  aes( label = costLab, x = fy, fill = fund, y = cost ), size = 2.25 )+
        geom_text(data=fyFundSummary, hjust= -.1 , vjust= .5, stat="identity", position = position_dodge(.6),
                  aes( label = fundLab, x = fy, fill = fund, y = 0 ), size = 2.25, fontface="bold" )+
        theme(axis.ticks=element_blank(),
              axis.text.x = element_blank() )+
        scale_fill_manual( values = um_colors[c(1:3, 5)] )
      # save plot
      ggsave(plot=p.fyFund,
             filename= paste(baseResults, "Yearly/Fund",
                             "yearly cost by funding all years.pdf", sep = "/"),
             width=5.5, height=tmpFyLength*.4+1.75, dpi=600, units="in")
      # remove R object
      rm(p.fyFund, tmpFyLength)

    # number of quarters
    tmpQtrLength = qtrFundSummary[, length(unique(qtr))]
    # create plot - quarterly funding
      p.qtrFund = ggplot(data=qtrFundSummary, aes(x=qtr, fill=fund, y=cost, ymax=1.2*cost))+
        geom_bar(stat="identity", width = 0.6, position = position_dodge(.6) )+
        coord_flip()+
        my_theme+
        labs(y="", x="",
             title="Quarterly Community Hospitalizations\nCost by Fund: All Fiscal Years")+
        geom_text(data=qtrFundSummary, hjust= -.1 , vjust= .5, stat="identity", position = position_dodge(.6),
                  aes( label = costLab, x = qtr, fill = fund, y = cost ), size = 2.5 )+
        geom_text(data=qtrFundSummary, hjust= -.1 , vjust= .5, stat="identity", position = position_dodge(.6),
                  aes( label = fundLab, x = qtr, fill = fund, y = 0 ), size = 2.5, fontface="bold" )+
        theme(axis.ticks=element_blank(),
              axis.text.x = element_blank() )+
        scale_fill_manual( values = um_colors[c(1:3, 5)] )
      # save plot
      ggsave(plot=p.qtrFund,
             filename= paste(baseResults, "Quarterly/Fund",
                             "quarterly cost by funding all years.pdf", sep = "/"),
             width=5.5, height=tmpQtrLength/4+2.2, dpi=600, units="in")
      # remove R objects
      rm(p.qtrFund)

    ### monthly funding ###
      # change date class to factors so that graphs show months in sequential order
      for( i in seq(allFys)) {
        # each fiscal year
        tmpFy = allFys[i]
        # dataset based on tmpFy
        tmpMon = monFundSummary[ allFys[i]==as.numeric(my_fy(month_year)) ]
        # fund label for latest month
          # latest month chronologically
          tmpLastMon = tmpMon[, max(month_year)]
          # creating fund label
          tmpMon[as.character(month_year)==as.character(tmpLastMon), fundLab := fund]
        # convert month to factor
        tmpMon[, month_year := as.factor(month_year) ]
        # re-order levels chronologically
        levelOrder = tmpMon[, as.character(as.yearmon(unique(month_year)))]
        # re-level months without changing data
        tmpMon$month_year = with(tmpMon, factor(tmpMon$month_year, levels= levelOrder ))

        # label for extra space between DD label and cost label at the top of the plot
        setkey(tmpMon, fundLab)
        tmpMon[ J("ABW"), costLab :=
          paste(paste( rep(" ", 5), collapse=""), costLab) ]
        # number of months
        tmpMonLength = tmpMon[, length(unique(month_year))]
        # create ggplot - monthly funding
        p.tmpMon = ggplot(data=tmpMon, aes(x=month_year, y=cost, fill=fund, ymax=cost*1.1) )+
          geom_bar(stat="identity", width=.6, position=position_dodge(width=.6))+
          my_theme+
          coord_flip()+
          geom_text(data=tmpMon, hjust = -.05, vjust=.5,
            stat="identity", position = position_dodge(.6),
            aes( label = fundLab, x = month_year, fill = fund, y = 0),
            size = 2.2, fontface="bold")+
          geom_text(data=tmpMon, hjust = -0.1, vjust=.5, stat="identity",
            aes(label=costLab, x=month_year, fill=fund, y=cost),
            position=position_dodge(width=0.6), size = 2.2)+
          theme(axis.ticks=element_blank(),
                axis.text.x = element_blank() )+
          scale_fill_manual( values = um_colors[c(1:3,5)] )+
          labs(title=paste("Monthly Cost by Funding: FY", allFys[i]))
        # save plot
          ggsave(plot=p.tmpMon,
                 filename= paste(baseResults, "Monthly/Fund",
                                 paste("monthly cost by funding FY ",
                                       allFys[i], ".pdf", sep=""), sep = "/"),
                 width=5.5, height=tmpMonLength*.25+1.5, dpi=600, units="in")
        # remove R objects
          rm(p.tmpMon)
      }

  ### Program Summary ###
    ## Labels for ggplot ##
      # Month
        # cost and admissions
          monProgSummary[, costCon :=
            paste(sapply(cost, moneyAdd), " (", consumers, ")", sep="") ]
        # labels to prevent a legend - only needed for top bar
         # use shortProg to make program label for start of fiscal years
         setkey(monProgSummary, month_year)
         monProgSummary[ J(as.yearmon(paste("Oct", allFys-1))),
                        progLab := sapply(Program, shortProg) ]
        # fix NAs to 0s so that they show up as 0 and not NA
          monProgSummary[is.na(consumerAdmit), consumerAdmit := 0]
        # label for extra space between DD label and cost label at the top of the plot
          setkey(monProgSummary, progLab)
          monProgSummary[ J("DD"),
            costCon := paste(paste( rep(" ", 10), collapse=""), costCon) ]
        # label for cost and consumers for top bar on each plot
          setkey(monProgSummary, progLab)
          monProgSummary[ J("Y&F"),
            costCon :=  paste(sapply(cost, moneyAdd),
            " (", consumers, " consumers)", sep="") ]
        # consumers admitted and admissions
          monProgSummary[, conAdm :=
            paste(consumerAdmit, " (", admissions, ")", sep=""),
            by = c("consumerAdmit", "admissions") ]
          # label for top bar of plot
            setkey(monProgSummary, progLab)
            monProgSummary[ J("Y&F"),
              conAdm := paste(consumerAdmit, " consumers (", admissions, " admissions)", sep="" )]
          # label for DD Adult at the top of the plot
            setkey(monProgSummary, progLab)
            monProgSummary[ J("DD"),
              conAdm := paste(paste( rep(" ", 5), collapse=""), conAdm)]
        # create fiscal year
          monProgSummary[, fy := my_fy(as.Date((as.yearmon(month_year))), format="%Y-%m-%d") ]

      # Quarter
        # cost and consumers
        qtrProgSummary[, costCon :=
          paste(sapply(cost, moneyAdd), " (", consumers, ")", sep="") ]
        # consumers admitted and admissions
        qtrProgSummary[, conAdm :=
          paste(consumerAdmit, " (", admissions, ")", sep=""),
          by = c("consumerAdmit", "admissions") ]
      # Year
        # cost and consumers
        fyProgSummary[, costCon :=
          paste(sapply(cost, moneyAdd), " (", consumers, ")", sep="") ]
        # consumers admitted and admissions
        fyProgSummary[, conAdm :=
          paste(consumerAdmit, " (", admissions, ")", sep=""),
          by = c("consumerAdmit", "admissions") ]
      # setkey to NULL to prevent errors
        setkey(monProgSummary, NULL)

#### ggplots and results ####
  ### csv results ###
  write.csv(fyFundSummary,
            paste(baseResults, "Yearly/Fund/ch fund consumers by fy.csv", sep="/"),
            row.names=FALSE)
  ### ggplots ###

    ## Monthly ggplots by fiscal year ##
      # monthly - for loop to go through each fiscal year
      for( i in seq(allFys) ) {
        # select each fiscal year for each iteration
          tmpMoPrSummary = monProgSummary[as.numeric(fy)==allFys[i]]
        # change month_year to factor that is ordered sequentially
          # convert month year to character
          tmpMoPrSummary[, month_year := as.character(month_year) ]
          # tmpMoPrSummary month to factor
          tmpMoPrSummary[, month_year := as.factor(month_year) ]
          # re-order levels chronologically
          levelOrder = tmpMoPrSummary[, as.character(as.yearmon(
            unique(month_year))[rev(order(as.yearmon(unique(month_year))))]) ]
          # change levels
          tmpMoPrSummary$month_year = with(tmpMoPrSummary,
                                 factor(tmpMoPrSummary$month_year, levels= levelOrder ))

      ## cost and consumers ##
        # number of months
          tmpMonLength = tmpMoPrSummary[, length(unique(month_year))]
        # create plot - quarterly funding
          p.monProg = ggplot(data=tmpMoPrSummary, aes(x=month_year, fill=Program, y=cost, ymax=1.15*cost))+
            geom_bar(stat="identity", width = 0.7, position = position_dodge(.7) )+
            coord_flip()+
            my_theme+
            labs(y="", x="",
                 title= paste("Monthly Community Hospitalizations\nCost and Consumers by Program: FY", allFys[i]) )+
            geom_text(data=tmpMoPrSummary, hjust= -.05 , vjust= .5, stat="identity", position = position_dodge(.7),
                      aes( label = costCon, x = month_year, fill = Program, y = cost ), size = 2 )+
            geom_text(data=tmpMoPrSummary, hjust = -.05, vjust=.5, stat="identity", position = position_dodge(.7),
                      aes( label = progLab, x = month_year, fill = Program, y = 0 ), size = 2, fontface="bold")+
            theme(axis.ticks=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=7),
                  plot.title = element_text(size=9))+
            scale_fill_manual( values = um_colors[c(1:3,5)] )
          # save plot
          ggsave(plot=p.monProg,
                 filename= paste(baseResults, "Monthly/Costs",
                            paste("monthly cost and consumers by program FY ",
                                  allFys[i], ".pdf", sep=""), sep = "/"),
                 width=5.5, height=tmpMonLength*.35+1.25, dpi=600, units="in")
          # remove R objects
          rm(p.monProg)

          ## consumers admitted and admissions ##
            # ggplot - monthly
              p.monProg = ggplot(data=tmpMoPrSummary,
                aes(x=month_year, fill=Program, y=consumerAdmit, ymax=1.3*consumerAdmit))+
                geom_bar(stat="identity", width = 0.6, position = position_dodge(.6) )+
                coord_flip()+
                my_theme+
                labs(y="", x="",
                     title= paste("Monthly Community Hospitalizations
                                  Consumers Admitted and Admissions by Program
                                  FY", allFys[i]))+
                geom_text(data=tmpMoPrSummary, hjust= -.1 , vjust= .5, stat="identity", position = position_dodge(.6),
                          aes( label = conAdm, x = month_year, fill = Program, y = consumerAdmit ), size = 2.2 )+
                geom_text(data=tmpMoPrSummary, hjust = -.05, vjust=.5, stat="identity", position = position_dodge(.6),
                          aes( label = progLab, x = month_year, fill = Program, y = 0 ), size = 2.2, fontface="bold")+
                theme(axis.ticks=element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_text(size=7),
                      plot.title = element_text(size=9))+
                scale_fill_manual( values = um_colors[c(1:3,5)] )
              # save plot
              ggsave(plot=p.monProg,
                     filename= paste(baseResults, "Monthly/Admissions",
                                     paste("monthly consumers admitted and admissions by program FY ",
                                           allFys[i], ".pdf", sep=""), sep = "/"),
                     width=5, height=tmpMonLength*.4+1.5, dpi=600, units="in")
              # remove R object
              rm(p.monProg)
      }

      ## quarterly ##
        # convert qtr to character
          qtrProgSummary[, qtr := as.character(qtr) ]
        # label program for last quarter of each fiscal year
          setkey(qtrProgSummary, qtr)
          qtrProgSummary[ J(paste(allFys, "Q4")),
                          progLab := sapply(Program, shortProg)]
          # if current fiscal year, then make last quarter the labeled quarter
            qtrProgSummary[ my_fy(as.Date(as.yearqtr(qtr)-.25))==fiscalYear &
                              max(qtr)==qtr, progLab := sapply(Program, shortProg)]
        # extra space for DD label at the top of the plot
          setkey(qtrProgSummary, progLab)
          qtrProgSummary[ J("DD"),
            conAdm := paste(paste( rep(" ", 5), collapse=""), conAdm)]
        # extra space for DD label at the top of the plot
          setkey(qtrProgSummary, progLab)
          qtrProgSummary[ J("DD"),
            costCon := paste(paste( rep(" ", 5), collapse=""), costCon)]
        # label for top bar of plot (consumers admitted and admissions)
          setkey(qtrProgSummary, progLab)
          qtrProgSummary[ J("Y&F"),
            conAdm := paste(consumerAdmit, " consumers (", admissions, " admissions)", sep="" )]
        # label for cost and consumers for top bar on each plot
          setkey(qtrProgSummary, progLab)
          qtrProgSummary[ J("Y&F"),
            costCon :=  paste(sapply(cost, moneyAdd),
            " (", consumers, " consumers)", sep="") ]

        ## Quarterly ggplots by fiscal year ##
        # monthly - for loop to go through each fiscal year
        for( i in seq(allFys) ) {
          # select each fiscal year for each iteration
          tmpQtrPrSummary = qtrProgSummary[ my_fy(as.Date(as.yearqtr(qtr)-.25))==allFys[i] ]
          # if dataset is empty, do not run the following code
          if(nrow(tmpQtrPrSummary)>0) {
          ## cost and consumers ##
            # number of quarters
            tmpQtrLength = tmpQtrPrSummary[, length(unique(qtr))]
            # create plot - quarterly funding
              p.qtrProg = ggplot(data=tmpQtrPrSummary, aes(x=qtr, fill=Program, y=cost, ymax=1.3*cost))+
                geom_bar(stat="identity", width = 0.6, position = position_dodge(.6) )+
                coord_flip()+
                my_theme+
                labs(y="", x="",
                     title= paste("Quarterly Community Hospitalizations\nCost and Consumers by Program\nFY",
                                  allFys[i]) )+
                geom_text(data=tmpQtrPrSummary, hjust= -.05 , vjust= .5,
                          stat="identity", position = position_dodge(.6),
                          aes( label = costCon, x = qtr, fill = Program, y = cost ), size = 2.2 )+
                geom_text(data=tmpQtrPrSummary, hjust = -.05, vjust=.5,
                          stat="identity", position = position_dodge(.6),
                          aes( label = progLab, x = qtr, fill = Program, y = 0 ),
                          size = 2.2, fontface="bold")+
                theme(axis.ticks=element_blank(),
                      axis.text.x = element_blank() )+
                scale_fill_manual( values = um_colors[c(1:3,5)] )
              # save plot
              ggsave(plot=p.qtrProg,
                     filename= paste(baseResults, "Quarterly/Costs",
                                     paste("quarterly cost and consumers by program FY ",
                                           allFys[i], ".pdf", sep=""), sep = "/"),
                     width=6, height=tmpQtrLength/4+2, dpi=600, units="in")
              # remove R objects
              rm(p.qtrProg)

          ## consumers admitted and admissions ##
            # ggplot
              p.qtrProg = ggplot(data=tmpQtrPrSummary,
                aes(x=qtr, fill=Program, y=consumerAdmit, ymax=1.3*consumerAdmit))+
                geom_bar(stat="identity", width = 0.6, position = position_dodge(.6) )+
                coord_flip()+
                my_theme+
                labs(y="", x="",
                     title= paste("Quarterly Community Hospitalizations
                                  Consumers Admitted and Admissions by Program
                                  FY", allFys[i]))+
                geom_text(data=tmpQtrPrSummary, hjust= -.1 , vjust= .5, stat="identity", position = position_dodge(.6),
                          aes( label = conAdm, x = qtr, fill = Program, y = consumerAdmit ), size = 2.3 )+
                geom_text(data=tmpQtrPrSummary, hjust = -.05, vjust=.5, stat="identity", position = position_dodge(.6),
                          aes( label = progLab, x = qtr, fill = Program, y = 0 ), size = 2.3, fontface="bold")+
                theme(axis.ticks=element_blank(),
                      axis.text.x = element_blank() )+
                scale_fill_manual( values = um_colors[c(1:3,5)] )
              # save plot
              ggsave(plot=p.qtrProg,
                     filename= paste(baseResults, "Quarterly/Costs",
                                     paste("quarterly consumers admitted and admissions by program FY ",
                                           allFys[i], ".pdf", sep=""), sep = "/"),
                     width=6, height=tmpQtrLength/4+2.25, dpi=600, units="in")
              # remove R objects
              rm(p.qtrProg)
          } # end of if statement
        }

      ## yearly ##
        # make program labels
        fyProgSummary[as.numeric(fiscalYear)==fy,
          progLab := sapply(Program, shortProg)]
        # extra space for DD label at the top of the plot
        setkey(fyProgSummary, progLab)
        fyProgSummary[ J("DD"),
          conAdm := paste(paste( rep(" ", 5), collapse=""), conAdm)]
        # extra space for DD label at the top of the plot
        setkey(fyProgSummary, progLab)
        fyProgSummary[ J("DD"),
          costCon := paste(paste( rep(" ", 5), collapse=""), costCon)]
        # convert fiscal year to character
        fyProgSummary[, fy := as.character(fy) ]
        # label for top bar of plot (consumers admitted and admissions)
          setkey(fyProgSummary, progLab)
          fyProgSummary[ J("Y&F"),
            conAdm := paste(consumerAdmit, " consumers (", admissions, " admissions)", sep="" )]
        # label for cost and admissions for top bar on each plot
          setkey(fyProgSummary, progLab)
          fyProgSummary[ J("Y&F"),
            costCon :=  paste(sapply(cost, moneyAdd),
            " (", consumers, " consumers)", sep="") ]

        # Yearly ggplots by fiscal year
          # number of fiscal years
          tmpFyLength = fyProgSummary[, length(unique(fy))]
          # create plot - cost and consumers
            p.fyProg = ggplot(data=fyProgSummary, aes(x=fy, fill=Program, y=cost, ymax=1.15*cost))+
              geom_bar(stat="identity", width = 0.6, position = position_dodge(.6) )+
              coord_flip()+
              my_theme+
              labs(y="", x="",
                   title= paste("Yearly Community Hospitalizations\nCost and Consumers by Program"))+
              geom_text(data=fyProgSummary, hjust= -.05 , vjust= .5, stat="identity", position = position_dodge(.6),
                        aes( label = costCon, x = fy, fill = Program, y = cost ), size = 2.2 )+
              geom_text(data=fyProgSummary, hjust = -.05, vjust=.5, stat="identity", position = position_dodge(.6),
                        aes( label = progLab, x = fy, fill = Program, y = 0 ), size = 2.2, fontface="bold")+
              theme(axis.ticks=element_blank(),
                    axis.text.x = element_blank() )+
              scale_fill_manual( values = um_colors[c(1:3,5)] )
            # save plot
            ggsave(plot=p.fyProg,
                   filename= paste(baseResults, "Yearly/Costs",
                              "yearly cost and consumers by program.pdf", sep = "/"),
                   width=5.5, height=tmpFyLength/4+2, dpi=600, units="in")
            # remove R objects
            rm(p.fyProg)

          # ggplot - consumers admitted and admissions
            p.fyProg = ggplot(data=fyProgSummary, aes(x=fy, fill=Program, y=consumerAdmit, ymax=1.2*consumerAdmit))+
              geom_bar(stat="identity", width = 0.6, position = position_dodge(.6) )+
              coord_flip()+
              my_theme+
              labs(y="", x="",
                   title= paste("Yearly Community Hospitalizations\nConsumers Admitted and Admissions by Program"))+
              geom_text(data=fyProgSummary, hjust= -.1 , vjust= .5, stat="identity", position = position_dodge(.6),
                        aes( label = conAdm, x = fy, fill = Program, y = consumerAdmit ), size = 2 )+
              geom_text(data=fyProgSummary, hjust = -.05, vjust=.5, stat="identity", position = position_dodge(.6),
                        aes( label = progLab, x = fy, fill = Program, y = 0 ), size = 2.2, fontface="bold")+
              theme(axis.ticks=element_blank(),
                    axis.text.x = element_blank() )+
              scale_fill_manual( values = um_colors[c(1:3, 5)] )
            # save plot
            ggsave(plot=p.fyProg,
                   filename= paste(baseResults,
                    "Yearly/Admissions/yearly consumers admitted and admissions by program.pdf", sep = "/"),
                   width=5.5, height=tmpFyLength/4+2, dpi=600, units="in")
            # remove R object
            rm(p.fyProg)

### program only by months, quarters, fiscal years ###
  ## Monthly ##
    # list of Programs
    tmpProgList = monProgSummary[, unique(Program) ]
    for (k in seq(allFys)) {
     # temporary fiscal year
     tmpFy = as.character(allFys[k])
      for (i in seq(tmpProgList)) {
       # temporary program
       tmpProg = tmpProgList[i]
      # select data for one program and year at a time
        setkey(monProgSummary, fy, Program)
        tmpMoPrSum = monProgSummary[ J(tmpFy, tmpProg)]

      # change month_year to factor that is ordered sequentially
        # convert month year to character
        tmpMoPrSum[, month_year := as.character(month_year) ]
        # tmpMoPrSum month to factor
        tmpMoPrSum[, month_year := as.factor(month_year) ]
        # re-order levels chronologically
        levelOrder = tmpMoPrSum[, as.character(as.yearmon(
          unique(month_year))[rev(order(as.yearmon(unique(month_year))))]) ]
        # change levels
        tmpMoPrSum$month_year = with(tmpMoPrSum,
          factor(tmpMoPrSum$month_year, levels= levelOrder ))
        # cost and admission label
          # re-write labels that are for previous ggplots
          tmpMoPrSum[, costCon := paste(sapply(cost, moneyAdd),
            " (", consumers, ")", sep="") ]
          # re-write labels for the top bar of the plot
          tmpMoPrSum[month_year==paste("Oct", as.numeric(tmpFy)-1), costCon :=
            paste(sapply(cost, moneyAdd), " (", consumers, " consumers)", sep="") ]
        # consumer and admission label
          # re-write labels that are for previous ggplots
            tmpMoPrSum[, conAdm := paste(consumerAdmit, " (", admissions, ")", sep="") ]
          # re-write labels for the top bar of the plot
            tmpMoPrSum[month_year==paste("Oct", as.numeric(tmpFy)-1), conAdm :=
              paste(consumerAdmit, " consumers (", admissions, " admissions)", sep="") ]
        # number of months
          tmpMonLength = tmpMoPrSum[, length(unique(month_year))]
        # create plot - cost and consumers
          p.monPrSum = ggplot(data = tmpMoPrSum, aes(x=month_year, y=cost, ymax=1.35*cost))+
            geom_bar(stat="identity", width = 0.6, fill = um_colors[1], position = position_dodge(.6) )+
            coord_flip()+
            my_theme+
            labs(y="", x="",
                 title= paste("Monthly Community Hospitalizations\n", tmpProg, "FY", tmpFy))+
            geom_text(data=tmpMoPrSum, hjust= -.01 , vjust= .5,
                      stat="identity", position = position_dodge(.6),
                      aes( label = costCon, x = month_year, fill = Program, y = cost ), size = 3 )+
            theme(axis.ticks=element_blank(),
                  axis.text.x = element_blank() )+
            scale_fill_manual( values = um_colors[c(1:3,5)] )
          # save plot
          ggsave(plot=p.monPrSum,
                 filename= paste(baseResults, "Monthly/Program",
                            paste("monthly cost and consumers ",
                             tmpProg, " FY ", tmpFy, ".pdf", sep=""), sep = "/"),
                 width=7.5, height=tmpMonLength/4+1, dpi=600, units="in")
          # remove R object
          rm(p.monPrSum)

        # consumers admitted and admissions - ggplot
          p.monPrSum = ggplot(data = tmpMoPrSum, aes(x=month_year, y=consumerAdmit, ymax=1.35*consumerAdmit))+
            geom_bar(stat="identity", width = 0.6, fill = um_colors[1], position = position_dodge(.6) )+
            coord_flip()+
            my_theme+
            labs(y="", x="",
                 title= paste("Monthly Community Hospitalizations\n", tmpProg, "Admissions FY", tmpFy))+
            geom_text(data=tmpMoPrSum, hjust= -.01 , vjust= .5,
                      stat="identity", position = position_dodge(.6),
                      aes( label = conAdm, x = month_year, fill = Program, y = consumerAdmit), size = 3 )+
            theme(axis.ticks=element_blank(),
                  axis.text.x = element_blank() )+
            scale_fill_manual( values = um_colors[c(1:3,5)] )
          # save plot
          ggsave(plot=p.monPrSum,
                 filename= paste(baseResults, "Monthly/Program",
                            paste("monthly consumers admitted and admissions ",
                             tmpProg, " FY ", tmpFy, ".pdf", sep=""), sep = "/"),
                 width=7.5, height=tmpMonLength/4+1, dpi=600, units="in")
          # remove R objects
          rm(p.monPrSum)
      }
    }

  ## Quarterly ##
    # list of Programs
      tmpProg = qtrProgSummary[, unique(Program) ]
    # for loop to go through Programs
    for( i in seq(tmpProg) ) {
      # select data for one program at a time
      setkey(qtrProgSummary, Program)
      tmpQtrPrSum = qtrProgSummary[ J(tmpProg[i])]
      # cost and consumers label
        # re-write labels that are for previous ggplots
        tmpQtrPrSum[, costCon := paste(sapply(cost, moneyAdd),
          " (", consumers, ")", sep="") ]
        # re-write labels for the top bar of the plot
        tmpQtrPrSum[tmpProg[i]==Program & qtr==as.character(currentQuarter), costCon :=
          paste(sapply(cost, moneyAdd), " (", consumers, " consumers)", sep="") ]
      # consumers admitted and admission label
        # re-write labels that are for previous ggplots
        tmpQtrPrSum[, conAdm := paste(consumerAdmit, " (",
          admissions, ")", sep="") ]
        # re-write labels for the top bar of the plot
        tmpQtrPrSum[tmpProg[i]==Program & qtr==as.character(currentQuarter), conAdm :=
          paste(consumerAdmit, " consumers (", admissions, " admissions)", sep="") ]
      # number of quarters
      tmpQtrLength = tmpQtrPrSum[, length(unique(qtr))]
      # create plot - cost and consumers
        p.qtrPrSum = ggplot(data = tmpQtrPrSum, aes(x=qtr, y=cost, ymax=1.25*cost))+
          geom_bar(stat="identity", width = 0.6, fill = um_colors[1], position = position_dodge(.6) )+
          coord_flip()+
          my_theme+
          labs(y="", x="",
               title= paste("Quarterly Community Hospitalizations\n", tmpProg[i]))+
          geom_text(data=tmpQtrPrSum, hjust= -.01 , vjust= .5,
                    stat="identity", position = position_dodge(.6),
                    aes( label = costCon, x = qtr, fill = Program, y = cost ), size = 3 )+
          theme(axis.ticks=element_blank(),
                axis.text.x = element_blank() )+
          scale_fill_manual( values = um_colors[c(1:3,5)] )
        # save plot
        ggsave(plot=p.qtrPrSum,
               filename= paste(baseResults, "Quarterly/Costs",
                               paste("quarterly cost and consumers ",
                                     tmpProg[i], ".pdf", sep=""), sep = "/"),
               width=7, height=tmpQtrLength/4+1, dpi=600, units="in")
        # remove R objects
        rm(p.qtrPrSum)

      # consumers admitted and admissions - ggplot
        p.qtrPrSum = ggplot(data = tmpQtrPrSum, aes(x=qtr, y=consumerAdmit, ymax=1.3*consumerAdmit))+
          geom_bar(stat="identity", width = 0.6, fill = um_colors[1], position = position_dodge(.6) )+
          coord_flip()+
          my_theme+
          labs(y="", x="",
               title= paste("Quarterly Community Hospitalizations\n", tmpProg[i], "Admissions"))+
          geom_text(data=tmpQtrPrSum, hjust= -.01 , vjust= .5,
                    stat="identity", position = position_dodge(.6),
                    aes( label = conAdm, x = qtr, fill = Program, y = consumerAdmit ), size = 3 )+
          theme(axis.ticks=element_blank(),
                axis.text.x = element_blank() )+
          scale_fill_manual( values = um_colors[c(1:3,5)] )
        # save plot
        ggsave(plot=p.qtrPrSum,
               filename= paste(baseResults, "Quarterly/Admissions",
                               paste("quarterly consumers admitted and admissions ",
                                     tmpProg[i], ".pdf", sep=""), sep = "/"),
               width=7, height=tmpQtrLength/4+1, dpi=600, units="in")
        # remove R objects
        rm(p.qtrPrSum)
    }

  ## Yearly ##
    # list of Programs
    tmpProg = fyProgSummary[, unique(Program) ]
    # for loop to go through Programs
      for( i in seq(tmpProg) ) {
      # select data for one program at a time
      setkey(fyProgSummary, Program)
      tmpFyPrSum = fyProgSummary[ J(tmpProg[i])]
      # cost and consumers label
      tmpFyPrSum[, costLab := paste(sapply(cost, moneyAdd), " (", consumers, ")", sep="") ]
      tmpFyPrSum[tmpProg[i]==Program & fy==as.numeric(fiscalYear), costLab :=
        paste(sapply(cost, moneyAdd), " (", consumers, " consumers)", sep="") ]
      # consumers admitted and admission label
      tmpFyPrSum[, conAdm := paste(consumerAdmit, " (", admissions, ")", sep="") ]
      tmpFyPrSum[tmpProg[i]==Program & fy==as.numeric(fiscalYear), conAdm :=
        paste(consumerAdmit, " consumers (", admissions, " admissions)", sep="") ]
      # number of fiscal years
      tmpFyLength = tmpFyPrSum[, length(unique(fy))]
      # cost and consumers - ggplot
        p.fyPrSum = ggplot(data = tmpFyPrSum, aes(x=fy, y=cost, ymax=1.3*cost))+
                      geom_bar(stat="identity", width = 0.6, fill = um_colors[1], position = position_dodge(.6) )+
                      coord_flip()+
                      my_theme+
                      labs(y="", x="",
                           title= paste("Yearly", tmpProg[i]))+
                      geom_text(data=tmpFyPrSum, hjust= -.05 , vjust= .5,
                                stat="identity", position = position_dodge(.6),
                                aes(label = costLab, x = fy, fill = Program, y = cost), size = 2.2 )+
                      theme(axis.ticks=element_blank(),
                            axis.text.x = element_blank() )+
                      scale_fill_manual( values = um_colors[c(1:3,5)] )
        # save plot
        ggsave(plot=p.fyPrSum,
               filename= paste(baseResults, "Yearly/Costs",
                          paste("yearly cost and consumers ",
                                tmpProg[i], ".pdf", sep=""), sep = "/"),
               width=5, height=tmpFyLength/4+.75, dpi=600, units="in")
        # remove R objects
        rm(p.fyPrSum)

      # consumers admitted and admissions - ggplot
        p.fyPrSum = ggplot(data = tmpFyPrSum, aes(x=fy, y=consumerAdmit, ymax=1.3*consumerAdmit))+
          geom_bar(stat="identity", width = 0.6, fill = um_colors[1], position = position_dodge(.6) )+
          coord_flip()+
          my_theme+
          labs(y="", x="",
               title= paste("Yearly", tmpProg[i], "\nFY", allFys[i], "Admissions"))+
          geom_text(data=tmpFyPrSum, hjust= -.05 , vjust= .5,
                    stat="identity", position = position_dodge(.6),
                    aes( label = conAdm, x = fy, fill = Program, y = consumerAdmit ), size = 2.2 )+
          theme(axis.ticks=element_blank(),
                axis.text.x = element_blank() )+
          scale_fill_manual( values = um_colors[c(1:3,5)] )
        # save plot
        ggsave(plot=p.fyPrSum,
               filename= paste(baseResults, "Yearly/Admissions",
                               paste("yearly consumers admitted and admissions ",
                                     tmpProg[i], ".pdf", sep=""), sep = "/"),
               width=5, height=tmpFyLength/4+.75, dpi=600, units="in")
        # remove R objects
        rm(p.fyPrSum)
      }

  ### Fiscal Year Budget by Fund ###
    # join fyFundSummary and budget data
    setkey(budgetData, fund, fy)
    setkey(fyFundSummary, fund, fy)
    fyFundSummary = merge(fyFundSummary, budgetData, all.x=TRUE, by=c("fund", "fy"))

    # calculate the number of months completed
      endFy = as.yearmon(paste("Oct", as.numeric(fiscalYear)-1 ))
      months_completed = (currentMonth-endFy+1/12)*12

    # we do not want projections on graph if the current month is september
    if(months_completed<12) {
      # projections for fiscal year 2013
        fyFundSummary[fy==fiscalYear, projection := cost/months_completed*12]
        # projection label
        fyFundSummary[fy==fiscalYear, projLab := sapply(projection, moneyAdd) ]

    ## ggplots ##
      # faceted ggplots
        p.facetBudget = ggplot(data=fyFundSummary, aes(x= fy, fill= fund, y= cost, ymax = cost*1.5 ) )+
                          geom_bar(position = position_dodge(width=.6), stat="identity", width=.6, alpha=.8 )+
                          geom_bar( aes(y=projection), position = position_dodge(width=.6),
                                    stat="identity", width=.6, alpha=.8 )+
                          geom_errorbar(stat="identity", aes(ymin=budget, ymax=budget), width=.65,
                                        position=position_dodge(width=0.6), color="red", alpha=1)+
                          labs(title = "Community Hospitalization Budget Projection",
                               x="",
                               y="Number of Hospitalizations")+
                          my_theme+
                          theme(axis.text.x = element_text(size=7))+
                          facet_wrap (~fund, scales = "free_y")+
                          geom_text(data=fyFundSummary, hjust= -.1 , vjust= -0.4 , stat="identity",
                                    aes( label = costLab, x = fy, fill= fund, y = cost, angle=90 ),
                                    size = 2.5, color="black")+
                          geom_text(data=fyFundSummary[fy==allFys[length(allFys)]],
                                    hjust= -.1 , vjust= 1.2 , stat="identity",
                                    aes( label = projLab, x = fy, fill= fund, y = projection, angle=90 ),
                                    size = 2.5, color="black")+
                          geom_text(data=fyFundSummary[fy==allFys[length(allFys)]],
                                    hjust= -.05 , vjust= 0 , stat="identity",
                                    aes( label = fundLab, x = fy, fill= fund, y = 0, angle=90),
                                    size = 2.5, fontface="bold", color="black")+
                          scale_fill_manual( values = um_colors[c(1:3, 5)] )+
                          theme(axis.ticks=element_blank(),
                                axis.text.y = element_blank())
          # save plot
          ggsave(plot=p.facetBudget,
                 filename= paste(baseResults, "Yearly/Budget",
                            paste("yearly faceted budget by funding.pdf", sep=""), sep = "/"),
                 width=4.5, height=4, dpi=600, units="in")

        # budget all ggplot
          p.budgetAll = ggplot(data=fyFundSummary, aes(x= fy, fill= fund, y= cost, ymax = projection*1.25 ) )+
            geom_bar(position = position_dodge(width=.6), stat="identity", width=.6, alpha=.8 )+
            geom_bar( aes(y=projection), position = position_dodge(width=.6), stat="identity", width=.6, alpha=.8 )+
            geom_errorbar(stat="identity", aes(ymin=budget, ymax=budget), width=.65,
                          position=position_dodge(width=0.6), color="red", alpha=1)+
            labs(title="Community Hospitalization Budget Projection",
                 x="",
                 y="Number of Hospitalizations")+
            my_theme+
            theme(axis.text.x = element_text(size=7))+
            scale_y_discrete(breaks=NULL, labels=NULL)+
            geom_text(data=fyFundSummary, hjust= -.1 , vjust= 0.8 ,
                      stat="identity", position = position_dodge(width=.6),
                      aes( label = costLab, x = fy, fill= fund, y = cost, angle=90 ),
                      size = 2.5, color="black")+
            geom_text(data=fyFundSummary, hjust= -.1 , vjust= -.8 ,
                      stat="identity", position = position_dodge(width=.6),
                      aes( label = projLab, x = fy, fill= fund, y = projection, angle=90 ),
                      size = 2.5, color="black")+
            geom_text(data=fyFundSummary, hjust= -.05 , vjust= .5,
                      stat="identity", position = position_dodge(width=.6),
                      aes( label = fundLab, x = fy, fill= fund, y = 0, angle=90),
                      size = 2.5, fontface="bold", color="black")+
            scale_fill_manual( values = um_colors[c(1:3, 5)] )
          # save plot
          ggsave(plot=p.budgetAll,
                 filename= paste(baseResults, "Yearly/Budget",
                                 paste("yearly budget by funding.pdf", sep=""), sep = "/"),
                 width=4, height=4.5, dpi=600, units="in")

      # save both budget plots in one pdf
        pdf( file = paste(baseResults, "Yearly/Budget",
                          paste("yearly budget - facets & All.pdf", sep=""), sep = "/"),
             width = 5, height = 8, onefile = TRUE, bg = "white")
        all_plots = multiplot(p.budgetAll, p.facetBudget)
        dev.off()
      # remove R objects
        rm(p.budgetAll, p.facetBudget)

    } else {
      ## ggplots ##
        # faceted ggplots
          p.facetBudget = ggplot(data=fyFundSummary, aes(x= fy, fill= fund, y= cost, ymax = cost*1.2 ) )+
            geom_bar(position = position_dodge(width=.6), stat="identity", width=.6, alpha=.8 )+
            geom_errorbar(stat="identity", aes(ymin=budget, ymax=budget), width=.65,
                          position=position_dodge(width=0.6), color="red", alpha=1)+
            labs(title = "Community Hospitalization Budget FY End",
                 x="",
                 y="Number of Hospitalizations")+
            my_theme+
            facet_wrap (~fund, scales = "free_y")+
            geom_text(data=fyFundSummary, hjust= -.1 , vjust= -0.4 , stat="identity",
                      aes( label = costLab, x = fy, fill= fund, y = cost, angle=90 ),
                      size = 2.5, color="black")+
            # geom_text(data=fyFundSummary, hjust= -.05 , vjust= 0 , stat="identity",
            #           aes( label = fundLab, x = fy, fill= fund, y = 0), angle=90, fontface="bold",
            #           size = 2.5, color="black")+
            scale_fill_manual( values = um_colors[c(1:3, 5)] )+
            theme(axis.ticks=element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_text(size=7))
          # save plot
          ggsave(plot=p.facetBudget,
                 filename= paste(baseResults, "Yearly/Budget",
                                 paste("yearly faceted budget by funding.pdf", sep=""), sep = "/"),
                 width=5.5, height=4, dpi=600, units="in")

      # budget all ggplot
        p.budgetAll = ggplot(data=fyFundSummary, aes(x= fy, fill= fund, y= cost, ymax = cost*1.15 ) )+
          geom_bar(position = position_dodge(width=.6), stat="identity", width=.6, alpha=.8 )+
          geom_errorbar(stat="identity", aes(ymin=budget, ymax=budget), width=.65,
                        position=position_dodge(width=0.6), color="red", alpha=1)+
          labs(title="Community Hospitalization Budget FY End",
               x="",
               y="Number of Hospitalizations")+
          my_theme+
          theme(axis.text.x = element_text(size=7))+
          scale_y_discrete(breaks=NULL, labels=NULL)+
          geom_text(data=fyFundSummary, hjust= -.1 , vjust= 0.5 , stat="identity", position = position_dodge(width=.6),
                    aes( label = costLab, x = fy, fill= fund, y = cost, angle=90 ),
                    size = 2.5, color="black")+
          geom_text(data=fyFundSummary, hjust= -.05 , vjust= .5 , stat="identity", position = position_dodge(width=.6),
                    aes( label = fundLab, x = fy, fill= fund, y = 0, angle=90, fontface="bold" ),
                    size = 2.5, color="black")+
          scale_fill_manual( values = um_colors[c(1:3, 5)] )
        # save plot
        ggsave(plot=p.budgetAll,
               filename= paste(baseResults, "Yearly/Budget",
                               paste("yearly budget by funding.pdf", sep=""), sep = "/"),
               width=4, height=4, dpi=600, units="in")

      # save both budget plots in one pdf
        pdf( file = paste(baseResults, "Yearly/Budget",
                          paste("yearly budget - facets & All.pdf", sep=""), sep = "/"),
             width = 5, height = 8, onefile = TRUE, bg = "white")
        all_plots = multiplot(p.budgetAll, p.facetBudget)
        dev.off()
      # remove R objects
        rm(p.budgetAll, p.facetBudget)
    }

  ### overall consumers by fund ###
    # number of fiscal years
    tmpFyLength = fyFundSummary[, length(unique(fy))]
    # create plot
    p.ovrFund = ggplot(data=fyFundSummary, aes(x=fy, y=consumers, fill=fund, ymax=consumers*1.2))+
      geom_bar(stat="identity", width=0.7, position=position_dodge(0.7))+
      coord_flip()+
      my_theme+
      labs(y="", x="", title="CSTS Yearly Consumers by Fund")+
      geom_text(data=fyFundSummary, position=position_dodge(width = 0.7),
                hjust= -.1 , vjust= .5, stat="identity",
                aes( label = conCostLab, x = fy, y=consumers, fill=fund),
                size = 2.1, fontface="bold" )+
      geom_text(data=fyFundSummary, position=position_dodge(width = 0.7),
                hjust= -.1 , vjust= .5, stat="identity",
                aes( label = fundLab, x = fy, y=0, fill=fund),
                size = 2.1, fontface="bold" )+
      scale_fill_manual(values = um_colors[c(1:3, 5)] )+
      theme(axis.ticks.x=element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=7))
    # save plot
    ggsave(plot=p.ovrFund,
           filename= paste(
             paste(baseResults, sep = ""),
             "Yearly/Fund/ch fund consumers by fy.pdf", sep="/"),
           width = 5.5, height=0.4*tmpFyLength+.7, units="in", dpi=600 )
    # remove R objects
    rm(p.ovrFund, tmpFyLength)

  # for loop to indicate when month is completed
    print(paste(month, "completed")); flush.console()
} # end of for loop for monthList

# how long it takes for entire for loop to finish
  endTime = Sys.time()-startTime
  endTime