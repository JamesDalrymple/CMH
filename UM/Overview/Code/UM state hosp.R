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
    source(paste(baseWD,
                 "Dropbox/WCCMH/R/begin script R code.r",
                 sep = "/") )

  # current input parameters - manual updating is required
    # fiscal year
    fiscalYear = "2016"
    # current month list for which months need to be ran
    # monthList = c("October", "November", "December", "January", "February")
    monthList = c("October", "November")

  # setup for working directories - data and results
    project = "State Hospitalizations" # user input required
    initialWD = "Dropbox/Utilization Management/UM Monthly Reports"
    dataWD = file.path("Data", project)

  # start time
    startTime = Sys.time()

#### Data Upload ####
  # set working directory for data upload - same no matter the month
    setwd( paste(baseWD, initialWD, dataWD, sep="/") )

  # reading in data and converting it to data.table format
    fy2011 = read.dtable("FY 2011 2056.csv")
    fy2012 = read.dtable("FY 2012 2056.csv")
    fy2013 = read.dtable("FY 2013 2056.csv")
    fy2014 = read.dtable("FY 2014 2056.csv")
    fy2015 = read.dtable("FY 2015 2056.csv")
    stateRates = read.dtable("state_hosp_fy_rates.csv")
    ngriData = read.dtable("NGRI prison monthly numbers.csv")
    stateBudget = read.dtable("state hosp budget.csv")

  # combining datasets the data.table way
    hosp = rbindlist( list(fy2011, fy2012, fy2013, fy2014, fy2015))
    rm(fy2011, fy2012, fy2013, fy2014, fy2015)

#### Data Restriction ####
  # get only state facilities
    setkey(hosp, Hosp_Type)
    hosp = hosp[ J("State Facility")]
  # get rid of forensic centers
    setkey(hosp, Hosp)
    hosp = hosp [ !J("Forensic Center") ]

#### Data Manipulation  - Pre For-Loop ####
  # keep only these columns from hosp dataset
    keepNames = c("Hosp", "Auh_ID", "case_no1", "adult", "Auth_eff", "Auth_Exp", "Hosp_disc")
  # remove column names that are not needed
    hosp[, c(setdiff(names(hosp), keepNames)) := NULL]
  # change names
    setnames(hosp, old = "case_no1", new = "case_no")

  ### Fix Records Manually ###
    # manually adjusting the discharge date ----
      hosp[case_no=="141137" & Auth_eff=="10/1/2010" & Auth_Exp=="7/11/2011", Hosp_disc:="7/11/2011"]
      hosp[case_no=="214057" & Auth_eff=="4/20/2012" & Auth_Exp=="4/1/2013", Hosp_disc:="4/1/2013"]
      hosp[case_no=="214555" & Auth_eff=="5/9/2012" & Auth_Exp=="2/28/2013", Hosp_disc:="2/28/2013"]

  # Date conversion from character to date class
    hosp[, Auth_eff:= dateConvert(Auth_eff) ]
    hosp[, Hosp_disc:= dateConvert(Hosp_disc) ]
    hosp[, Auth_Exp:= dateConvert(Auth_Exp) ]

  # re-naming state hospitals
    hosp[, Hosp := gsub(x=Hosp, pattern="-State Facility", replacement="") ]
    hosp[, Hosp := gsub(x=Hosp, pattern=" & Pheasant Ridge", replacement="") ]
    hosp = trim(hosp)
    # change Kalamazoo to a shorter name
    setkey(hosp, Hosp)
    hosp[J("Kalamazoo Psychiatric Hospital"), Hosp := "Kalamazoo Psychiatric"]
    setkey(hosp, NULL)

  # when Auh_ID is the same, combine record
    hosp[, Auth_eff := min(Auth_eff, na.rm=TRUE), by = "Auh_ID" ]
    hosp[, Auth_Exp := max(Auth_Exp, na.rm=TRUE), by = "Auh_ID" ]
    hosp[, Hosp_disc := max(Hosp_disc, na.rm=TRUE) , by = "Auh_ID" ]

  # remove duplicate records that occurred from removing unwanted columns
    setkey(hosp, NULL)
    hosp = unique(hosp)

  ### state hospitalization rates ###
    stateRates[, fy := as.character(fy)]

  ### ngriData ###
    # convert month_year to date class
    ngriData[, month_year := dateConvert(month_year, format="%m/%d/%Y")]
    # convert month_year to as.yearmon class
    ngriData[, month_year := as.yearmon(month_year)]
    # add fiscal year
    ngriData[, fiscalyr := my_fy(month_year)]
    # convert month year
      ngriData[, month_year := as.character(month_year)]
    # change names
      setnames(ngriData, old="month_year", new="month")

  ### state budget ###
    stateBudget[, fy := as.character(fy)]

#### Data Manipulation and Calculation - For Loops ####
  # For Loop that goes throughout each for loop
  for( m in seq_along(monthList)) {
    # select month
    month_m = monthList[m]

    # end date of month_m
    month_end = as.Date(as.yearmon(paste(month_m, fiscalYear))+1/12)-1

    # make a copy of hosp so that hosp can be reused each month
    hosp_m = copy(hosp)

    ### Restrict data based on current month --- month_m ###
      # exclude hospital admissions after the last day in the current month
      hosp_m = hosp_m[Auth_eff <= month_end]
      # set blank discharges to be month_end
      hosp_m[is.na(Hosp_disc), Hosp_disc := month_end ]
      # set hospital discharge to be no later than month_end
      hosp_m[, Hosp_disc := as.Date(as.character(min(Hosp_disc, month_end))), by = "Auh_ID"]
      # set Auth_Exp to be no later than month_end
      hosp_m[, Auth_Exp := as.Date(as.character(min(Auth_Exp, month_end))), by = "Auh_ID"]
      # remove duplicates
      hosp_m = unique(hosp_m)

    # create all months that need to be cycled through
      # if statement to make the fiscal year correct for Oct, Nov, and Dec
      if( month_m %in% c("October", "November", "December") ) {
        all_months = as.yearmon("2010-10")+
          0:((as.yearmon(paste(month_m, as.numeric(fiscalYear)-1))-as.yearmon("2010-10"))*12)/12
      } else {
        all_months = as.yearmon("2010-10")+0:((as.yearmon(paste(month_m, fiscalYear))-as.yearmon("2010-10"))*12)/12
      }

    # create all quarters that need to be cycled through
      all_qtrs = unique(as.yearqtr(all_months)+.25)
        # remove last quarter if the last day in the quarter is not in the current month
        if(month_end < dateConvert(all_qtrs[length(all_qtrs)])-1) {
          all_qtrs = all_qtrs[-length(all_qtrs)]
        }
    # create all fiscal years
      all_fys = unique(my_fy(all_months))

  ### Monthly ###
    # initialize variables
    AggrMon = NULL
    AgeMon = NULL
    hospNameMon = NULL

    # for loop - months
    for(i in seq_along(all_months) ) {
     # set up current month
     currentMonth = all_months[i]

     # first and last date in the month
      firstDay = as.Date(currentMonth)
      lastDay = as.Date(currentMonth, frac = 1)

     # temp hosp month - select only days in the temp month
      tmpHospMon = hosp_m[Auth_eff <= lastDay & (Hosp_disc >= firstDay | is.na(Hosp_disc)) ]

      # function - state hospitalization financial days
       los.PdUnit = function(Start, End, admit, exp, disc) {

            # the last day of the month - the first day in the month
            paidUnits = suppressMessages( # supress messages to prevent non-missing arguments to min warning messages
              as.numeric(min(exp, disc, End, na.rm=TRUE)-max(Start, admit, na.rm = TRUE))+1
          )
          return( paidUnits )
        }

      # use los.PdUnit function to determine how many days each consumer spends
      ## in the hospital that month, per admission record
        tmpHospMon[, monPaidUnits :=
          los.PdUnit(Start = firstDay,
                       End = lastDay,
                       admit = Auth_eff,
                       exp = Auth_Exp,
                       disc = Hosp_disc),
          by = c("Auh_ID", "case_no")]

      # create fiscal year
        tmpHospMon[, fy := my_fy(currentMonth) ]

      # join with stateRates to add rate
        setkey(tmpHospMon, Hosp, fy); setkey(stateRates, Hosp, fy)
        tmpHospMon = merge(tmpHospMon, stateRates, all.x = TRUE, by = c("Hosp", "fy"))

      # create monCost column
        tmpHospMon[, monCost := monPaidUnits*rate]

      # cost and consumers by Hospital name
       tmpNameMon = tmpHospMon[, list(cost = sum(monCost, na.rm = TRUE),
                                      month = currentMonth,
                                      consumers = length(unique(case_no)),
                                      fy = unique(fy) ), by = "Hosp"]

      # temp month Age
       tmpAgeMon = tmpHospMon[, list(consumers = length(unique(case_no)),
                         cost = sum(monCost, na.rm = TRUE),
                         month = currentMonth), by = "adult" ]
     # temp month age
      setkey(tmpAgeMon, adult)
      tmpAgeMon = tmpAgeMon[ J(c("Y", "N")) ]
      tmpAgeMon[, month := currentMonth]
      # change 'Y' to 'adult' and 'N' to 'child'
      adultChild = function(x) switch(x,
        'Y' = 'adult',
        'N' = 'child')
      tmpAgeMon[, adult := sapply(adult, adultChild)]

      # aggregated temp Month summary
        tmpAggrMo = tmpHospMon[, list(month = as.character(currentMonth),
          cost = sum(monCost, na.rm = TRUE),
          consumers = length(unique(case_no)),
          fy = unique(fy)) ]

      # add admissions to tmpAggrMo
        tmpAggrMo[, admissions := tmpHospMon[ Auth_eff %between% c(firstDay, lastDay),
                                    length(unique(case_no))] ]

        # add discharges to tmpAggrMo
        tmpAggrMo[, discharges :=
                    tmpHospMon[Hosp_disc %between% c(firstDay, lastDay),
                               length(unique(case_no))]]

        # combine all aggregated months together
          AggrMon = rbindlist( list(AggrMon, tmpAggrMo))
        # combine all tmpAgeMon (age results)
          AgeMon = rbindlist( list(AgeMon, tmpAgeMon))
        # combine all tmpNameMon (hospital names by month)
          hospNameMon = rbindlist( list(hospNameMon, tmpNameMon))
    }
    # remove R objects
    rm(tmpAggrMo, tmpAgeMon, tmpHospMon, tmpNameMon, firstDay, lastDay)

  ### Quarterly ###
    # initialize variables
    AggrQtr = NULL
    AgeQtr = NULL
    hospNameQtr = NULL

    # for loop - quarterly
    for(i in seq_along(all_qtrs) ) {
      # set up current month
      currentQtr = all_qtrs[i]

      # first and last date in the month
      firstDay = as.Date(currentQtr-.25)
      lastDay = as.Date(currentQtr-.25, frac = 1)

      # temp hosp month - select only days in the temp month
      tmpHospQtr = hosp_m[Auth_eff <= lastDay & (Hosp_disc >= firstDay | is.na(Hosp_disc)) ]

      # use los.PdUnit function to determine how many days each consumer spends
      ## in the hospital that month, per admission record
      tmpHospQtr[, qtrPaidUnits :=
                   los.PdUnit(Start = firstDay,
                              End = lastDay,
                              admit = Auth_eff,
                              exp = Auth_Exp,
                              disc = Hosp_disc),
                 by = c("Auh_ID", "case_no")]

      # create fiscal year
      tmpHospQtr[, fy := my_fy(as.Date(currentQtr-.25, frac=1)) ]
      # create quarter
      tmpHospQtr[, qtr := currentQtr ]

      # join with stateRates to add rate
      setkey(tmpHospQtr, Hosp, fy); setkey(stateRates, Hosp, fy)
      tmpHospQtr = merge(tmpHospQtr, stateRates,
                         all.x = TRUE, by = c("Hosp", "fy"))

      # create qtrCost column
      tmpHospQtr[, qtrCost := qtrPaidUnits*rate]

      # cost and consumers by Hospital name
      tmpNameQtr = tmpHospQtr[, list(cost = sum(qtrCost, na.rm = TRUE),
                                     qtr = currentQtr,
                                     consumers = length(unique(case_no)),
                                     fy = unique(fy) ), by = "Hosp"]

      # temp month Age
      tmpAgeQtr = tmpHospQtr[, list(consumers = length(unique(case_no)),
                                    cost = sum(qtrCost, na.rm = TRUE),
                                    qtr = currentQtr), by = "adult" ]
      # temp month age
        setkey(tmpAgeQtr, adult)
        tmpAgeQtr = tmpAgeQtr[ J(c("Y", "N")) ]
        tmpAgeQtr[, month := currentQtr]
        # change 'Y' to 'adult' and 'N' to 'child'
        tmpAgeQtr[, adult := sapply(adult, adultChild)]

      # aggregated temp quarter summary
      tmpAggrQtr = tmpHospQtr[, list(qtr = currentQtr,
                                    cost = sum(qtrCost, na.rm = TRUE),
                                    consumers = length(unique(case_no)))]

      # add admissions to tmpAggrQtr
      tmpAggrQtr[, admissions :=
        tmpHospQtr[ Auth_eff %between% c(firstDay, lastDay),
                   length(unique(case_no)) ] ]

      # add discharges to tmpAggrQtr
      tmpAggrQtr[, discharges :=
        tmpHospQtr[Hosp_disc %between% c(firstDay, lastDay),
                   length(unique(case_no))] ]

      # combine all quarters together
        AggrQtr = rbindlist( list(AggrQtr, tmpAggrQtr))
      # combine all tmpAgeMon (age results)
        AgeQtr = rbindlist( list(AgeQtr, tmpAgeQtr))
      # combine all tmpNameQtr
        hospNameQtr = rbindlist( list(hospNameQtr, tmpNameQtr) )
    }
    # remove R objects
    rm(tmpAggrQtr, tmpHospQtr, tmpAgeQtr, firstDay, lastDay)

  ### Yearly ###
    # initialize variables
    AggrFy = NULL
    AgeFy = NULL
    hospNameFy = NULL

    # for loop - quarterly
    for(i in seq_along(all_fys) ) {
      # set up current month
      currentFy = all_fys[i]

      # first and last date in the month
      firstDay = as.Date(as.yearmon(paste("Oct", as.numeric(currentFy)-1 )))
      lastDay = as.Date(as.yearmon(paste("Sep", as.numeric(currentFy) )), frac=1)

      # temp hosp month - select only days in the temp month
      tmpHospFy = hosp_m[Auth_eff <= lastDay & (Hosp_disc >= firstDay | is.na(Hosp_disc)) ]

      # use los.PdUnit function to determine how many days each consumer spends
      ## in the hospital that month, per admission record
      tmpHospFy[, fyPaidUnits :=
                   los.PdUnit(Start = firstDay,
                              End = lastDay,
                              admit = Auth_eff,
                              exp = Auth_Exp,
                              disc = Hosp_disc),
                 by = c("Auh_ID", "case_no")]

      # create fiscal year
      tmpHospFy[, fy := currentFy ]

      # join with stateRates to add rate
      setkey(tmpHospFy, Hosp, fy); setkey(stateRates, Hosp, fy)
      tmpHospFy = merge(tmpHospFy, stateRates, all.x = TRUE, by = c("Hosp", "fy"))

      # create fyCost column
      tmpHospFy[, fyCost := fyPaidUnits*rate]

      # cost and consumers by Hospital name
      tmpNameFy = tmpHospFy[, list(days = sum(fyPaidUnits, na.rm=TRUE),
                                   cost = sum(fyCost, na.rm = TRUE),
                                   fy = currentFy,
                                   consumers = length(unique(case_no))), by = "Hosp"]


      # temp fiscal year Age
      tmpAgeFy = tmpHospFy[, list(days = sum(fyPaidUnits, na.rm=TRUE),
                                  consumers = length(unique(case_no)),
                                  cost = sum(fyCost, na.rm = TRUE),
                                  fy = currentFy), by = "adult" ]
      # temp fiscal year age
        setkey(tmpAgeFy, adult)
        tmpAgeFy = tmpAgeFy[ J(c("Y", "N")) ]
        tmpAgeFy[, fy := currentFy]
        # change 'Y' to 'adult' and 'N' to 'child'
        tmpAgeFy[, adult := sapply(adult, adultChild)]

      # aggregated temp fiscal year summary
      tmpAggrFy = tmpHospFy[, list(fy = currentFy,
                                   days = sum(fyPaidUnits, na.rm=TRUE),
                                   cost = sum(fyCost, na.rm = TRUE),
                                   consumers = length(unique(case_no)))]

      # add admissions to tmpAggrFy
      tmpAggrFy[, admissions :=
        tmpHospFy[ Auth_eff %between% c(firstDay, lastDay),
                   length(unique(case_no)) ]]

      # add discharges to tmpAggrFy
      tmpAggrFy[, discharges :=
                   tmpHospFy[ Hosp_disc %between% c(firstDay, lastDay),
                               length(unique(case_no)) ]]

      # combine all fiscal years together
      AggrFy = rbindlist( list(AggrFy, tmpAggrFy) )
      # combine all tmpAgeMon (age results)
      AgeFy = rbindlist( list(AgeFy, tmpAgeFy) )
      # combine all tmpNameFy (hosp names)
      hospNameFy = rbindlist( list(hospNameFy, tmpNameFy) )
    }
    # remove R objects
    rm(tmpAggrFy, tmpHospFy, tmpAgeFy, firstDay, lastDay)

#### Budget and Projection ####
  ## join budget numbers ##
    setkey(stateBudget, fy)
    setkey(AggrFy, fy)
    AggrFy = merge(AggrFy, stateBudget, all.x=TRUE, by="fy")
    # keep budget values only for current fiscal year
      AggrFy[, budget := as.numeric(budget)] # convert to numeric
      setkey(AggrFy, fy)
      AggrFy[!J(currentFy), budget := NA_real_]

  ## calculate current fiscal year projection ##
    setkey(AggrMon, fy)
    months_completed = AggrMon[J(currentFy)][, length(unique(month))]
    currentProjection =  AggrMon[J(currentFy)][, sum(cost, na.rm=TRUE)]/months_completed*12
    setkey(AggrMon, NULL)
    # create current fiscal year projection
      setkey(AggrFy, fy)
      AggrFy[J(fiscalYear), project := currentProjection]

#### labeling for ggplots ####
  ### Fiscal Year ###
    ## Aggregate ##
      # label for admissions and discharges per fiscal year
        AggrFy[, changeLab := paste( "(+",admissions, " / -", discharges, ")", sep="") ]
      # cost label
        AggrFy[, costLab := sapply(cost, moneyAdd) ]
      # consumer label
        AggrFy[, conLab := as.character(consumers) ]
      # cost and consumer label
        # all labels
        AggrFy[, costConLab := paste(costLab, " (", consumers, ")", sep="" )]
        # top ggplot bar label
        setkey(AggrFy, fy)
        AggrFy[ J(fiscalYear), costConLab :=
          paste(costLab, " (", consumers, " consumers)", sep="" )]
      # projection label
        AggrFy[, projLab := sapply(project, moneyAdd) ]
    ## Hospital Names ##
      hospNameFy[!is.na(consumers), costConLab :=
        paste(sapply(cost, moneyAdd), " (", consumers, ")", sep="") ]
    ## Age ##
      # consumer label
      AgeFy[, conLab := as.character(consumers) ]
      # cost label
      AgeFy[, costLab := sapply(cost, moneyAdd) ]
      # cost and consumer label
        # all labels
        AgeFy[, costConLab := paste(costLab, " (", consumers, ")", sep="" )]
        # top ggplot bar label
        setkey(AgeFy, fy, adult)
        AgeFy[ J(fiscalYear, "N"), costConLab :=
          paste(costLab, " (", consumers, " consumers)", sep="" )]
  ### Quarter ###
    ## Aggregate ##
      # label for admissions and discharges per fiscal year
      AggrQtr[, changeLab := paste( "(+",admissions, " / -", discharges, ")", sep="") ]
      # consumer label
      AggrQtr[, conLab := as.character(consumers) ]
      # cost label
      AggrQtr[, costLab := sapply(cost, moneyAdd) ]
      # cost and consumer label
        # all labels
        AggrQtr[, costConLab := paste(costLab, " (", consumers, ")", sep="" )]
        # top ggplot bar label
          # cost consumer label
          setkey(AggrQtr, qtr)
          AggrQtr[ J(AggrQtr[, max(qtr)]), costConLab :=
            paste(costLab, " (", consumers, " consumers)", sep="" )]
          # consumer label
          setkey(AggrQtr, qtr)
          AggrQtr[J( max(as.yearqtr(qtr)) ), conLab := paste(consumers, "consumers") ]
    ## Hospital Names ##
      hospNameQtr[!is.na(consumers), costConLab :=
        paste(sapply(cost, moneyAdd), " (", consumers, ")", sep="") ]
    ## Age ##
      # cost label
      AgeQtr[, costLab := sapply(cost, moneyAdd) ]
      # consumer label
      AgeQtr[, conLab := as.character(consumers) ]
      # cost and consumer label
        # all labels
        AgeQtr[, costConLab := paste(costLab, " (", consumers, ")", sep="" )]
        # top ggplot bar label
          # cost & consumer label
          setkey(AgeQtr, qtr, adult)
          AgeQtr[ J(AgeQtr[, max(qtr)], "Y"), costConLab :=
                   paste(costLab, " (", consumers, " consumers)", sep="" )]
          # consumer label
          setkey(AgeQtr, qtr, adult)
          AgeQtr[ J(AgeQtr[, max(qtr)], "Y"),
            conLab := paste(consumers, "consumers")]
  ### Month ###
    ## Aggregate ##
      # label for admissions and discharges per fiscal year
      AggrMon[, changeLab := paste( "(+",admissions, " / -", discharges, ")", sep="") ]
      # cost label
      AggrMon[, costLab := sapply(cost, moneyAdd) ]
      # consumer label
      AggrMon[, conLab := as.character(consumers) ]
      # cost and consumer label
        # all labels
        AggrMon[, costConLab := paste(costLab, " (", consumers, ")", sep="" )]
        # top ggplot bar label
          # cost Consumer Lab
          setkey(AggrMon, month)
          AggrMon[ J(c(paste("Sep", all_fys), AggrMon[, as.character(max(as.yearmon(month)))] )), costConLab :=
            paste(costLab, " (", consumers, " consumers)", sep="" )]
          # consumer label
          setkey(AggrMon, month)
          AggrMon[ J(c(paste("Sep", all_fys), AggrMon[, as.character(max(as.yearmon(month)))] )),
                   conLab := paste( consumers, " consumers", sep="" ) ]
    ## Hospital Names ##
      hospNameMon[!is.na(consumers), costConLab := paste(sapply(cost, moneyAdd), " (", consumers, ")", sep="") ]
    ## Age ##
      # cost label
      AgeMon[, costLab := sapply(cost, moneyAdd) ]
      # cost and consumer label
        # all labels
        AgeMon[!is.na(cost), costConLab := paste(costLab, " (", consumers, ")", sep="" )]
        # top ggplot bar label
          # convert AgeMon to character
          AgeMon[, month := as.character(month)]
          # cost consumer labeling for top bars
          setkey(AgeMon, month, adult)
          AgeMon[ J(c(paste("Sep", all_fys), AggrMon[, as.character(max(as.yearmon(month)))] ), "Y"), costConLab :=
                    paste(costLab, " (", consumers, " consumers)", sep="" )]

#### Results ####
  # Working directory
    # results working directory
    resultsWD = paste("Results",
                      paste("fy", fiscalYear), month_m, project, sep = "/")

  ### Monthly ###
    # Admissions, discharges and costs, consumers
    for( i in seq_along(all_fys)) {
      # each fiscal year
      tmpFy = all_fys[i]
      # dataset based on tmpFy
      tmpMon = AggrMon[ all_fys[i]==fy ]
      # convert month to factor
      tmpMon[, month := as.factor(month) ]
      # re-order levels chronologically
      levelOrder = tmpMon[, as.character(as.yearmon(unique(month)))][
                    tmpMon[, as.numeric(order(as.yearmon(unique(month))))]]
      # re-level months without changing data
      tmpMon$month = with(tmpMon, factor(tmpMon$month, levels= levelOrder ))
      # number of months
      tmpMonLength = tmpMon[, length(unique(month))]
      # ggplot - admissions/discharges
      p.tmpMon = ggplot( data = tmpMon, aes(x = month, y = consumers, ymax = consumers*1.4))+
        geom_bar(stat="identity", width=.8, alpha=1, fill=um_colors[2] )+
        labs(title = paste("State Hospitalization Costs: FY", tmpFy))+
        geom_text(data=tmpMon, hjust= 0 , vjust= .5, stat="identity",
                  aes( label = conLab, x = month, y = consumers ), size = 3, fontface="bold" )+
        geom_text(data=tmpMon, hjust= 0 , vjust= .5, stat="identity",
                  aes( label = changeLab, x = month, y = 0 ), size = 3, fontface="italic" )+
        coord_flip()+
        my_theme+
        theme(axis.ticks=element_blank(),
              axis.text.x = element_blank())
      # save plot - admissions/discharges
      ggsave(plot=p.tmpMon,
             filename= paste(
              paste(baseWD, initialWD, resultsWD,
              "Monthly/Admissions/monthly admit & discharge FY ",
              sep="/"), tmpFy, ".pdf", sep=""),
             width=6, height=tmpMonLength/4+1, dpi=600, units="in")
      # remove R objects
        rm(p.tmpMon)

      # ggplot - costs/consumers
      p.tmpMon = ggplot( data = tmpMon, aes(x = month, y = cost, ymax = cost*1.4))+
        geom_bar(stat="identity", width=.8, alpha=1, fill=um_colors[2] )+
        labs(title = paste("State Hospitalization Consumers & Costs: FY", tmpFy))+
        geom_text(data=tmpMon, hjust= 0 , vjust= .5, stat="identity",
                  aes( label = costConLab, x = month, y = cost ), size = 3, fontface="bold" )+
        coord_flip()+
        my_theme+
        theme(axis.ticks=element_blank(),
              axis.text.x = element_blank())
      # save plot - admissions/discharges
      ggsave(plot=p.tmpMon,
             filename= paste(
               paste(baseWD, initialWD, resultsWD,
               "Monthly/Admissions/monthly consumers & cost FY ",
               sep="/"), tmpFy, ".pdf", sep=""),
             width=6, height=tmpMonLength/4+1.25, dpi=600, units="in")
      # remove R object
      rm(p.tmpMon)
    }

    ### NGRI & Prison plot - FY 2013 and forward - adult only and cost ###
      setkey(ngriData, month); setkey(AggrMon, month)
      ngriCost = merge(ngriData, AggrMon, by = "month")
      # remove columns that are not needed
      ngriCost[, c("conLab", "costLab", "admissions", "discharges", "changeLab",
                   "IST", "costConLab") := NULL ]
      # remaining consumers
      ngriCost[, other := consumers-NGRI-Prison]
      # label for consumers and cost
      ngriCost[, conCost := paste(consumers, " (", sapply(cost, moneyAdd), ")", sep="")]

      # ngri stacked barplot data
      ngriStacked = melt(ngriCost, id=c("month", "fy", "conCost", "consumers"),
                         measure=c("NGRI", "Prison", "other"))
      ngriStacked = data.table(ngriStacked)
      ngriStacked[, value := as.numeric(value)]

      tmpAllfy = ngriStacked[, unique(fy)]
      for( i in seq(tmpAllfy)) {
      # tmp fiscal year
        tmpFy = tmpAllfy[i]
      # select fiscal year
        tmpNgri = ngriStacked[fy==tmpAllfy[i] ]
      # number of months in dataset
        tmpMonLength = tmpNgri[, length(unique(month))]
      # fix order of months via factors
      tmpNgri[, month := factor(month,
        levels = as.character(tmpNgri[, as.yearmon(unique(month))[order(as.yearmon(unique(month)))]]) )]

      # create ggplot
      p.tmpMon = ggplot(data=tmpNgri, aes(x=month, y=value, group=month, fill=variable, ymax=1.5*value+5) )+
        geom_bar(stat="identity", position="stack", width=.7)+
        coord_flip()+
        my_theme+
        theme(legend.position = "top",
               panel.background = element_rect(fill = "white", colour = NA),
               panel.border = element_rect(fill = NA,
                                           colour = "grey60"),
               panel.grid.major = element_line(colour = "grey70",size = 0.2),
               panel.grid.minor = element_line(colour = "grey85",
                                               size = .5 ),
               strip.background = element_rect(fill = "white", colour = "white"),
               axis.title.x = element_text(colour = "grey40", size=8),
               axis.title.y = element_text(colour = "grey40", size=8),
               axis.text.y = element_text(size=7) )+
        labs(title="State Hospitalization NGRI & Prison Consumers/Costs",
             x="month", y="consumers", fill="")+
        scale_y_continuous(minor_breaks = seq(1, 20, 1))+
        geom_text(data=tmpNgri, stat="identity", hjust=-0.05, vjust=0.5,
              aes(x=month, y=consumers, group=month, label=conCost), size=2.7,
              width=.7)+
        scale_fill_manual(values=um_colors[ c(1,2,5)])
      # save plot - NGRI & prison costs and consumers
      ggsave(plot=p.tmpMon,
             filename= paste(
               paste(baseWD, initialWD, resultsWD,
                     "Monthly/Admissions/monthly NGRI & prison con & cost FY ",
                     sep="/"), tmpFy, ".pdf", sep=""),
             width=4.25, height=max(0.2*tmpMonLength+0.8, 1.75), dpi=600, units="in")
      # remove R object
      rm(p.tmpMon, tmpNgri, tmpFy)
      }
      rm(tmpAllfy)

    # State Hospitals by Name
    for( i in seq_along(all_fys)) {
      # each fiscal year
      tmpFy = all_fys[i]
      # dataset based on tmpFy
      tmpMon = hospNameMon[ all_fys[i]==fy ]
      # convert month to factor
      tmpMon[, month := as.factor(month) ]
      # re-order levels chronologically
      levelOrder = tmpMon[, as.character(as.yearmon(unique(month)))][
        tmpMon[, as.numeric(order(as.yearmon(unique(month))))]]
      # re-level months without changing data
      tmpMon$month = with(tmpMon, factor(tmpMon$month, levels = levelOrder))
      # number of months
      tmpMonLength = tmpMon[, length(unique(month))]

      # ggplot - hospital names by months
      p.tmpMon = ggplot( data = tmpMon, aes(x = month, y = cost, fill = Hosp, ymax = cost*1.4))+
        geom_bar(stat="identity", width=.8, alpha=1 )+
        labs(title = "State Hospitalization Costs")+
        facet_wrap(~Hosp, scale="free_y") +
        my_theme+
        theme(axis.ticks=element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(angle = 90, size=8, vjust=.5),
              plot.title = element_text(size=12))+
        scale_fill_manual( values = um_colors[1:4] )+
        geom_text(data=tmpMon, hjust= -0.05 , vjust= 0.5, stat="identity",
                  aes( label = costConLab,
                       x=month, y=cost ), size = 3, angle = 90 )
      # save plot - hospital names by months
      ggsave(plot=p.tmpMon,
             filename=paste(
               paste(baseWD, initialWD, resultsWD,
               "Monthly/Costs & Consumers/state hosp costs & consumers FY ",
               sep="/"), tmpFy, ".pdf", sep=""),
             width=max(tmpMonLength*.3+3, 3.5), height=7, dpi=600, units="in")
      }

      # State Hospitals by Age
      for( i in seq_along(all_fys)) {
        # each fiscal year
        tmpFy = all_fys[i]
        # dataset based on tmpFy
        tmpMon = AgeMon[ all_fys[i]==my_fy(as.yearmon(month)) ]
        # convert month to factor
        tmpMon[, month := as.factor(month) ]
        # re-order levels chronologically
        levelOrder = tmpMon[, as.character(as.yearmon(unique(month)))][
          tmpMon[, as.numeric(order(as.yearmon(unique(month))))]]
        # re-level months without changing data
          tmpMon$month = with(tmpMon, factor(tmpMon$month, levels = levelOrder))
        # consumer labeling
          tmpMon[, conLab := as.character(consumers) ]
          # for top/first bars
          setkey(tmpMon, month, adult)
          tmpMon[ J( paste("Oct", as.numeric(all_fys)-1), "adult" ),
                  conLab := paste(consumers, "consumers") ]
        # number of months
        tmpMonLength = tmpMon[, length(unique(month))]

        # ggplot - costs and consumers by age
        p.tmpMon = ggplot( data = tmpMon, aes(x = month, y = cost, fill = adult, ymax = cost*1.3))+
          geom_bar(stat="identity", width=.8, alpha=1 )+
          labs(title = paste("State Hospitalization FY ", tmpFy, "\n Costs & Consumers by Age", sep=""))+
          facet_wrap(~adult, scale="free_y")+
          my_theme+
          theme(axis.ticks=element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size=7, angle = 90, vjust=.5),
                plot.title = element_text(size=8))+
          scale_fill_manual( values = um_colors[1:4] )+
          geom_text(data=tmpMon, hjust= -0.05 , vjust= 0.5, stat="identity",
                    aes( label = costConLab,
                         x=month, y=cost ), size = 2.5, angle = 90 )
        # save plot - hospital names by months
        ggsave(plot=p.tmpMon,
               filename=paste(
                 paste(baseWD, initialWD, resultsWD,
                 "Monthly/Cost & Consumers by Age/state hosp cost & consumers by age FY ",
                 sep="/"), tmpFy, ".pdf", sep=""),
               width=tmpMonLength/4+1.25, height=4, dpi=600, units="in")

        # ggplot - consumers by age
        p.tmpMon = ggplot( data = tmpMon, aes(x = month, y = consumers, fill = adult, ymax = consumers*1.3))+
          geom_bar(stat="identity", width=.8, alpha=1 )+
          labs(title = paste("State Hospitalization FY ", tmpFy, "\nConsumers by Age", sep=""))+
          facet_wrap(~adult, scale="free_y") +
          my_theme+
          theme(axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(angle = 90, size=7, vjust=.5),
                plot.title = element_text(size=7))+
          scale_fill_manual( values = um_colors[1:2] )+
          geom_text(data=tmpMon, hjust= -0.1 , vjust= 0.5, stat="identity",
                    aes( label = conLab,
                         x=month, y=consumers ), size = 2.5, angle = 90 )
        # save plot - hospital names by months
        ggsave(plot=p.tmpMon,
               filename=paste(
                 paste(baseWD, initialWD, resultsWD,
                 "Monthly/Consumers by Age/state hosp consumers by age FY ",
                 sep="/"), tmpFy, ".pdf", sep=""),
               width=tmpMonLength/4+1.25, height=4.5, dpi=600, units="in")
      }
      rm(p.tmpMon, tmpFy, tmpMon, tmpMonLength)

  ### Quarterly ###
    # set working directory
    setwd( paste(baseWD, initialWD, resultsWD, "Quarterly", sep="/") )
    # convert month to factor
      AggrQtr[, qtr := as.character(qtr) ]
    # number of quarters
      tmpQtrLength = AggrQtr[, length(unique(qtr))]
    # ggplot - admissions/discharges
      p.tmpQtr = ggplot( data = AggrQtr, aes(x=qtr, y=consumers, ymax=consumers*1.3))+
        geom_bar(stat="identity", width=.8, alpha=1, fill=um_colors[2] )+
        labs(title = "State Hospitalization\n Quarterly Consumers" )+
        geom_text(data=AggrQtr, hjust= 0 , vjust= .5, stat="identity",
                  aes( label = conLab, x = qtr, y = consumers ), size = 2.5, fontface="bold" )+
        geom_text(data=AggrQtr, hjust= 0 , vjust= .5, stat="identity",
                  aes( label = changeLab, x = qtr, y = 0 ), size = 2.5, fontface="italic" )+
        coord_flip()+
        my_theme+
        theme(axis.ticks=element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size=7))
    # save plot - admissions/discharges
      ggsave(plot=p.tmpQtr,
             filename= paste(baseWD, initialWD, resultsWD,
              "Quarterly/quarterly admit & discharge.pdf", sep="/"),
             width=3.75, height=tmpQtrLength/10+1.5, dpi=600, units="in")
    # remove R objects
      rm(p.tmpQtr)

    # ggplot - costs/consumers
      p.tmpQtr = ggplot( data = AggrQtr, aes(x = qtr, y = cost, ymax = cost*1.45))+
        geom_bar(stat="identity", width=.8, alpha=1, fill=um_colors[2] )+
        labs(title = paste("State Hospitalization Costs & Consumers"))+
        geom_text(data=AggrQtr, hjust= 0 , vjust= .5, stat="identity",
                  aes( label = costConLab, x = qtr, y = cost ), size = 3, fontface="bold" )+
        coord_flip()+
        my_theme+
        theme(axis.ticks=element_blank(),
              axis.text.x = element_blank())
      # save plot - admissions/discharges
        ggsave(plot=p.tmpQtr,
               filename= paste(baseWD, initialWD, resultsWD,
                "Quarterly/quarterly costs & consumers.pdf", sep="/"),
               width=5.5, height=tmpQtrLength/10+2, dpi=600, units="in")
      # remove R objects
        rm(p.tmpQtr)

    # State Hospitals by Name
      # convert quarter to character
        hospNameQtr[, qtr := as.character(qtr) ]

      # ggplot - hospital names by months
        p.tmpQtr = ggplot( data = hospNameQtr, aes(x = qtr, y = cost, fill = Hosp, ymax = cost*1.4))+
          geom_bar(stat="identity", width=.8, alpha=1 )+
          labs(title = "State Hospitalization Costs")+
          facet_wrap(~Hosp, scale="free_y") +
          my_theme+
          theme(axis.ticks=element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(angle = 90, size=7, vjust=.5) )+
          scale_fill_manual( values = um_colors[1:4] )+
          geom_text(data=hospNameQtr, hjust= -0.05 , vjust= 0.5, stat="identity",
                    aes( label = costConLab,
                         x=qtr, y=cost ), size = 2.2, angle = 90 )
        # save plot - hospital names by months
          ggsave(plot=p.tmpQtr,
                 filename = paste(baseWD, initialWD, resultsWD,
                  "Quarterly/state hosp costs & consumers.pdf", sep="/"),
                 width=tmpQtrLength/8+2.875, height=5, dpi=600, units="in")
        # remove R objects
          rm(p.tmpQtr)

    # State Hospitals by Age
      # convert qtr to character
        AgeQtr[, qtr := as.character(qtr)]

      # ggplot - costs and consumers by age
        p.tmpQtr = ggplot( data = AgeQtr, aes(x = qtr, y = cost, fill = adult, ymax = cost*1.4))+
          geom_bar(stat="identity", width=.7, alpha=1 )+
          labs(title = "State Hospitalization\n Costs & Consumers by Age")+
          facet_wrap(~adult, scale="free_y")+
          my_theme+
          theme(axis.ticks=element_blank(),
                axis.text.y=element_blank(),
                axis.text.x=element_text(angle=90, size=7, vjust=.5))+
          scale_fill_manual( values = um_colors[1:4] )+
          geom_text(data=AgeQtr, hjust= -0.05 , vjust=0.5, stat="identity",
                    aes( label=costConLab, x=qtr, y=cost), size=2.5, angle=90, width=.7)
        # save plot - hospital names by months
          ggsave(plot=p.tmpQtr,
                 filename=paste(baseWD, initialWD, resultsWD,
                  "Quarterly/state hosp cost & consumers by age.pdf", sep="/"),
                 width=tmpQtrLength/4+1, height=4, dpi=600, units="in")
        # remove R objects
          rm(p.tmpQtr)

      # ggplot - consumers by age
        p.tmpQtr = ggplot( data = AgeQtr, aes(x = qtr, y = consumers, fill = adult, ymax = consumers*1.3))+
          geom_bar(stat="identity", width=.8, alpha=1 )+
          labs(title = paste("State Hospitalization\n Consumers by Age", sep=""))+
          facet_wrap(~adult, scale="free_y") +
          my_theme+
          theme(axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(angle = 90, size=6, vjust=.5))+
          scale_fill_manual( values = um_colors[1:2] )+
          geom_text(data=AgeQtr, hjust= -0.1 , vjust= 0.5, stat="identity",
                    aes( label = conLab,
                         x=qtr, y=consumers ), size = 2.5, angle = 90 )
        # save plot - hospital names by months
          ggsave(plot=p.tmpQtr,
                 filename= paste(baseWD, initialWD, resultsWD,
                  "Quarterly/state hosp consumers by age.pdf", sep="/"),
                 width=tmpQtrLength/4+1, height=3.25, dpi=600, units="in")
        # remove R objects
          rm(p.tmpQtr, tmpQtrLength)

  ### Yearly ###
    # number of colors
      tmpFyLength = AggrFy[, length(unique(fy))]
    # graph projections only if month is not September
    if(month_m!="September") {
    # budget projection - cost by year
      p.budget = ggplot( data = AggrFy, aes(x = fy, fill = fy, y = cost, ymax = 1.15*max(cost, project, na.rm=TRUE) ))+
        geom_bar(stat="identity", width =.6, alpha=.7, position = position_dodge(width=.6) )+
        geom_bar( aes(x=fy, fill=fy, y=project), stat="identity", width =.6,
                  alpha=.7, position = position_dodge(width=.6) )+
        geom_errorbar(stat="identity", aes(ymin=budget, ymax=budget),
                      width=.65, position=position_dodge(width=0.6), color="red", alpha=1)+
        labs(title = "State Hospitalization Budget Projection")+
        geom_text(data = AggrFy, hjust= 0, vjust= -.2, stat="identity",
                  aes( label = costLab, x = fy , y = cost ), size = 3, fontface="bold" )+
        geom_text(data = AggrFy, hjust= 0, vjust= .9, stat="identity",
                  aes( label = projLab, x = fy , y = project ), size = 3, fontface="bold" )+
        coord_flip()+
        my_theme+
        theme(axis.ticks=element_blank(),
              axis.text.x = element_blank())+
        scale_fill_manual(values = um_colors[rep(1, tmpFyLength)] )
    # save plot
      ggsave(plot=p.budget,
             filename= paste(baseWD, initialWD, resultsWD,
                             "Yearly/budget projection.pdf", sep="/"),
             width=5, height=tmpFyLength/8+1.3, dpi=600, units="in")
    # remove R objects
      rm(p.budget)
    } else {

    # budget projection - cost by year
      p.budget = ggplot( data = AggrFy, aes(x = fy, fill = fy, y = cost, ymax = 1.15*max(cost, project, na.rm=TRUE) ))+
        geom_bar(stat="identity", width =.6, alpha=.7, position = position_dodge(width=.6) )+
        geom_errorbar(stat="identity", aes(ymin=budget, ymax=budget),
                      width=.65, position=position_dodge(width=0.6), color="red", alpha=1)+
        labs(title = "State Hospitalization Budget Year End")+
        geom_text(data = AggrFy, hjust= 0 , vjust= 0.5, stat="identity",
                  aes( label = costLab, x = fy , y = cost ), size = 3, fontface="bold" )+
        coord_flip()+
        my_theme+
        theme(axis.ticks=element_blank(),
              axis.text.x = element_blank())+
        scale_fill_manual(values = um_colors[ rep(1, tmpFyLength) ] )
      # save plot
        ggsave(plot=p.budget,
               filename= paste(baseWD, initialWD, resultsWD,
                "Yearly/budget projection.pdf", sep="/"),
               width=5, height=tmpFyLength/8+1.3, dpi=600, units="in")
      # remove R objects
        rm(p.budget)
      }

    # ggplot - admissions/discharges
      p.tmpFy = ggplot( data=AggrFy, aes(x=fy, y=consumers, ymax=consumers*1.3))+
        geom_bar(stat="identity", width=.8, alpha=1, fill=um_colors[2] )+
        labs(title="State Hospitalization\n Fiscal Year Costs")+
        geom_text(data=AggrFy, hjust=0, vjust=.5, stat="identity",
                  aes( label = conLab, x=fy, y = consumers ), size=3, fontface="bold" )+
        geom_text(data=AggrFy, hjust=0, vjust=.5, stat="identity",
                  aes(label=changeLab, x=fy, y=0), size=3, fontface="italic" )+
        coord_flip()+
        my_theme+
        theme(axis.ticks=element_blank(),
              axis.text.x = element_blank())
      # save plot - admissions/discharges
        ggsave(plot=p.tmpFy,
               filename= paste(baseWD, initialWD, resultsWD,
                "Yearly/yearly admit & discharge.pdf", sep="/"),
               width=5, height=tmpFyLength/5+1.4, dpi=600, units="in")
      # remove R objects
        rm(p.tmpFy)

    # ggplot - costs/consumers
      p.tmpFy = ggplot( data=AggrFy, aes(x=fy, y=cost, ymax=cost*1.45))+
        geom_bar(stat="identity", width=.8, alpha=1, fill=um_colors[2] )+
        labs(title=paste("State Hospitalization Consumers & Costs"))+
        geom_text(data=AggrFy, hjust=0, vjust= .5, stat="identity",
                  aes( label=costConLab, x=fy, y=cost), size=3, fontface="bold" )+
        coord_flip()+
        my_theme+
        theme(axis.ticks=element_blank(),
              axis.text.x = element_blank())
      # save plot - admissions/discharges
        ggsave(plot=p.tmpFy,
               filename=paste(baseWD, initialWD, resultsWD,
                "Yearly/yearly consumers & cost.pdf", sep="/"),
               width=5.5, height=tmpFyLength/5+1.4, dpi=600, units="in")
      # remove R objects
        rm(p.tmpFy)

    # State Hospitals by Name
      # ggplot - hospital names by months
      p.tmpFy = ggplot( data=hospNameFy, aes(x=fy, y=cost, fill=Hosp, ymax=cost*1.4))+
        geom_bar(stat="identity", width=.8, alpha=1 )+
        labs(title="State Hospitalization Costs")+
        facet_wrap(~Hosp, scale="free_y") +
        my_theme+
        theme(axis.ticks=element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(angle=90, vjust=.5) )+
        scale_fill_manual( values = um_colors[1:4] )+
        geom_text(data=hospNameFy, hjust=-0.05 , vjust=0.5, stat="identity",
                  aes( label = costConLab,
                       x=fy, y=cost), size=2.5, angle=90)
      # save plot - hospital names by months
        ggsave(plot=p.tmpFy,
               filename = paste(baseWD, initialWD, resultsWD,
                "Yearly/state hosp costs & consumers.pdf", sep="/"),
               width=3.5, height=tmpFyLength/8+5, dpi=600, units="in")
      # remove R objects
        rm(p.tmpFy)

    # State Hospitals by Age
      p.tmpFy = ggplot( data = AgeFy, aes(x = fy, y = cost, fill = adult, ymax = cost*1.3))+
        geom_bar(stat="identity", width=.8, alpha=1 )+
        labs(title = "State Hospitalization\n Costs & Consumers by Age")+
        facet_wrap(~adult, scale="free_y")+
        my_theme+
        theme(axis.ticks=element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(angle = 90, size=7, vjust=.5) )+
        scale_fill_manual( values = um_colors[1:4] )+
        geom_text(data=AgeFy, hjust= -0.05 , vjust= 0.5, stat="identity",
                  aes( label = costConLab,
                       x=fy, y=cost ), size = 2.5, angle = 90 )
      # save plot - hospital names by months
        ggsave(plot=p.tmpFy,
               filename= paste(baseWD, initialWD, resultsWD,
                "Yearly/state hosp cost & consumers by age.pdf", sep="/"),
               width=tmpFyLength/4+1.5, height=4, dpi=600, units="in")
      # remove R objects
        rm(p.tmpFy)

    # ggplot - consumers by age
      p.tmpFy = ggplot( data = AgeFy, aes(x = fy, y = consumers, fill = adult, ymax = consumers*1.3))+
        geom_bar(stat="identity", width=.8, alpha=1 )+
        labs(title = paste("State Hospitalization\n Consumers by Age", sep=""))+
        facet_wrap(~adult, scale="free_y") +
        my_theme+
        theme(axis.ticks = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(angle = 90, vjust= 0.5) )+
        scale_fill_manual( values = um_colors[1:2] )+
        geom_text(data=AgeFy, hjust= -0.25 , vjust= 0.5, stat="identity",
                  aes( label = conLab,
                       x=fy, y=consumers ), size = 2.5, angle = 90 )
      # save plot - hospital names by months
        ggsave(plot=p.tmpFy,
               filename= paste(baseWD, initialWD, resultsWD,
                               "Yearly/state hosp consumers by age.pdf", sep="/"),
               width=tmpFyLength/4+1.5, height=3.5, dpi=600, units="in")
      # remove R objects
        rm(p.tmpFy)
  # notification of month completion
    print(paste(currentMonth, "completed"))
    flush.console()
}

# end time
  endTime = Sys.time()
  paste(round(endTime-startTime, 2), "duration of run time in minutes")

