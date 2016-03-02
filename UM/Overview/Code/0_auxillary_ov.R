pkg_loader(packages = c("ggplot2", "RODBC", "ReporteRs", "xlsx",
                        "stringi", "sqldf"))
aux <- new.env(parent  = .GlobalEnv )

aux$count_overlap <- function(fy_start, fy_end, h_start, h_exp, h_end) {
  dt_count <- data.table(fy_start, fy_end, h_start, h_exp, h_end)
  dt_count[is.na(h_end), h_end := input$max_end]
  dt_count[, count_start := pmax(fy_start, h_start, na.rm = TRUE)]
  dt_count[, count_end := pmin(fy_end,
                               pmax(h_exp, h_end, na.rm = TRUE), na.rm = TRUE)]
  dt_count[, days_overlap := as.num(count_end - count_start) + 1]
  return(dt_count[, days_overlap])
}

input$calendar_year <-
  as.chr(ifelse(input$months %in% c("October", "November", "December"),
                as.num(input$fy)-1, input$fy))
input$end_dates <-
  as.Date(as.yearmon(paste(input$months, input$calendar_year)), frac = 1)
input$start_dates <- rep(
  as.Date(as.yearmon(paste("Oct", as.num(input$fy)-3))),
  length(input$end_dates))
input$min_start <- min(input$start_dates)
input$max_end <- max(input$end_dates)

# cost and consumer function
aux$cost_con <- function(cost, consumers) {
  result <- paste(money_add(cost), " (", consumers, ")", sep="")
  return(result)
}

# cost and consumer function
aux$cost_con2 <- function(cost, consumers) {
  result <- paste(money_add(cost), " (", consumers, " consumers)", sep="")
  return(result)
}

# function to add spaces before ABW cost labels
aux$abw_money <- function(cost, consumers) {
  result <- paste(paste(rep(" ", 6), collapse=""),
                  money_add(cost), " (", consumers, ")", sep="")
  return(result)
}

aux$dist_cases <- function(x) {
  x <- unique(x)
  x <- x[!is.na(x)]
  result <- length(x) # NA only becomes 0
  return(result)
}

### monthly Overall CMH Program graphs ----
# function to create and save ggplots for CMH programs by fiscal year
aux$plotPrMoCMH <- function(dataInput) {
  # change date class to factors so that graphs show months in sequential order
  dataInput[, monthDate := as.factor(monthDate) ]
  levelOrder <- dataInput[, as.character(as.yearmon(unique(monthDate)))]
  # change levels
  dataInput$monthDate = with(dataInput, factor(dataInput$monthDate, levels= levelOrder ))
  # input fiscal year
  inputFy = dataInput[, unique(fy) ]
  # number of completed months
  tmpMonLength = dataInput[, length(unique(monthDate))]

  # Monthly ggplot
  myPlot <- ggplot( data=dataInput, aes(x=monthDate,y=consumers,fill=Program, ymax=max(consumers*1.2)) )+
    geom_bar(stat="identity", width=0.6, position=position_dodge(0.7), alpha=1, color="black", size=.1 )+
    coord_flip()+
    my_theme+
    labs(y="", x="",
         title= paste("Monthly Overall Consumers Served: Fiscal Year ", inputFy, "\n CSTS Programs", sep=""))+
    geom_text(data=dataInput, position=position_dodge(width = 0.7),
              hjust= -.1 , vjust= .5, stat="identity",
              aes( label = consumers, x = monthDate, y = consumers, fill=Program ), size = 2.5 )+
    geom_text(data=dataInput, position=position_dodge(width = 0.7),
              hjust= 0 , vjust= .5, stat="identity",
              aes( label = progLabel, x = monthDate, y = 0, fill=Program ),
              size = 3, fontface="bold" )+
    scale_fill_manual(values = um_colors[1:3] )+
    theme(axis.ticks=element_blank(),
          axis.text.x = element_blank() )
  # save plot
  ggsave(plot=myPlot,
         filename= paste(
           paste(baseWD, initialWD, resultsWD, sep=""), "Monthly/Consumers",
           paste("ovr monthly Overall CMH consumers served ", inputFy, ".pdf"), sep="/"),
         width=5.25, height=1/3*tmpMonLength+1.5, dpi=600, units="in")
  return(myPlot)
}


### function to create and save ggplots by groups with date columns
aux$groupPlot <- function(dataInput, dateColumn, dateColType=NULL, title=NULL, x_title=NULL,
                      y_title=NULL, y, group=NULL, filename=NULL, width=5.5, height=NULL) {
  if(dateColType=="month") {
    # change date class to factors so that graphs show months in sequential order
    eval(parse(text =
      paste("levelOrder = dataInput[, as.character(as.yearmon(unique(",
            dateColumn, ")))]", sep="")))
    eval(parse(text =
      paste("dataInput[, ", dateColumn, " := factor(", dateColumn,
            ", levels = levelOrder) ]", sep="")))
  }
  # length of months
  tmpDateLength <- eval(parse(text = paste("dataInput[, length(unique(", dateColumn, ")) ]", sep="")))
  # height of graph
  if(is.null(height)) {height = tmpDateLength/3+1.5}
  # make a column for ymax
  eval(parse(text =
               paste("dataInput[, ymax := 1.4*", y, " ]", sep="")))
  # Monthly ggplot
  myPlot <- ggplot( data=dataInput, aes_string(x=dateColumn,y=y, fill=group, ymax="ymax") )+
    geom_bar(stat="identity", width=0.7, position=position_dodge(0.7), alpha=1, color="black", size=.1 )+
    coord_flip()+
    my_theme+
    labs(y=y_title, x=x_title, title=title )+
    geom_text(data=dataInput, position=position_dodge(width = 0.7),
              hjust= -.1 , vjust= .5, stat="identity",
              aes_string( label = y, x = dateColumn, y = y, fill=group ), size = 2.1 )+
    theme(legend.position="top",
          legend.title = element_text(size=10, face="bold"),
          axis.ticks=element_blank(),
          axis.text.x = element_blank())
  if(group=="fund") {
    myPlot = myPlot+
      scale_fill_manual(values = um_colors[1:3] ) # +
    # geom_text(data=dataInput, position=position_dodge(width = 0.7),
    #  hjust= 0 , vjust= .5, stat="identity",
    #  aes( label = fundLab, x = monthDate, y = 0, fill=fund ), size = 2.1, fontface='bold' )
  } else if(group=="Program") {
    myPlot = myPlot+
      scale_fill_manual(values = um_colors[1:4] ) # +
    # geom_text(data=dataInput, position=position_dodge(width = 0.7),
    #          hjust= 0 , vjust= .5, stat="identity",
    #          aes( label = progLabel, x = monthDate, y = 0, fill=Program ), size = 2.1, fontface='bold' )
  }
  # save plot
  ggsave(plot=myPlot,
         filename= filename,
         width=width, height=height, dpi=600, units="in")
  return(myPlot)
}
# free standing hospitals (for hospital bed results)
aux$free_stand = c("BCA of Detroit, LLC DBA BCA StoneCrest Center",
                   "Forest View Hospital",
                   "Havenwyck Hospital",
                   "Harbor Oaks",
                   "Henry Ford Kingswood Hospital",
                   "Pine Rest Christian Mental Health Services",
                   "The Behavioral Center of Michigan")

# trimmed boxplot (outliers not jittered)
aux$trimBp <- function(data, xVar, yVar, title=NULL, x.axis.title=NULL, y.axis.title=NULL, trim=.15,
                  group=NULL, xLabAngle=0, scales="free") {
  # copy dataset to avoid changing original dataset
  dataset = copy(data)
  # if group exists, use it in the 'by' argument
  if(is.null(group)) {
    dataset[, keep := .SD[, quantile(x=get(yVar), probs=(1-trim), type=7, na.rm=TRUE)], by=c(xVar) ]
    dataset[, median := .SD[, median(x=get(yVar), na.rm=TRUE)], by=c(xVar)]
    dataset[, trimMean := .SD[, meanTrim(x=get(yVar), trim=trim)], by=c(xVar)]
  } else {
    dataset[, keep := .SD[, quantile(x=get(yVar), probs=(1-trim), type=7, na.rm=TRUE)], by=c(xVar, group) ]
    dataset[, median := .SD[, median(x=get(yVar), na.rm=TRUE)], by=c(xVar, group) ]
    dataset[, trimMean := .SD[, meanTrim(x=get(yVar), trim=trim)], by=c(xVar, group) ]
  }
  ## list of cases kept and retained
  # dataset[get(yVar)>keep, list(cases = length(case_no)), by=c(xVar)]
  # dataset[!(get(yVar)>keep), list(cases = length(case_no)), by=c(xVar)]

  # trim off the data based on 'trim'
  dataset = dataset[!(get(yVar)>keep)]

  # q1, q2, whiskers
  if(is.null(group)) {
    dataset[, q1 := .SD[, quantile(x=get(yVar), probs=0.25, type=7, na.rm=TRUE)], by=c(xVar)]
    dataset[, q3 := .SD[, quantile(x=get(yVar), probs=0.75, type=7, na.rm=TRUE)], by=c(xVar)]
    dataset[, whiskLow := .SD[, max(q1-1.5*(q3-q1), min(get(yVar))) ], by=c(xVar) ]
    dataset[, whiskHigh := .SD[, min(q3+1.5*(q3-q1), max(get(yVar))) ], by=c(xVar) ]
  } else {
    dataset[, q1 := .SD[, quantile(x=get(yVar), probs=0.25, type=7, na.rm=TRUE)], by=c(xVar, group)]
    dataset[, q3 := .SD[, quantile(x=get(yVar), probs=0.75, type=7, na.rm=TRUE)], by=c(xVar, group) ]
    dataset[, whiskLow := .SD[, max(q1-1.5*(q3-q1), min(get(yVar))) ], by=c(xVar, group) ]
    dataset[, whiskHigh := .SD[, min(q3+1.5*(q3-q1), max(get(yVar)) ) ], by=c(xVar, group) ]
  }

  # if only 1 data point, only draw the trimmed mean
  dataset[, nSamples := .N , by = c(xVar, group)]
  dataset[nSamples<3, c("q1", "q3", "whiskLow", "whiskHigh") := NA_real_ ]

  # outliers
  dataset[get(yVar)> q3+1.5*(q3-q1) | get(yVar)< q1-1.5*(q3-q1), outlier := "Y"]
  dataset[!(get(yVar)> q3+1.5*(q3-q1) | get(yVar)< q1-1.5*(q3-q1)), outlier := "N"]

  # create boxplot
  p.boxplot = ggplot(data=dataset, aes_string(x=xVar) )+
    geom_boxplot(stat="identity", aes(ymin=whiskLow, ymax=whiskHigh, lower=q1,
                                      upper=q3, middle=trimMean),
                 fill=um_colors[1], color=um_colors[4], outlier.colour=um_colors[4],
                 width=.6, outlier.size = 1)+
    #scale_x_discrete()+
    geom_point(data=dataset[outlier=="Y"], aes_string(x=xVar, y=yVar), color=um_colors[4], size=1.5)+
    my_theme+
    labs(title=title,
         x=x.axis.title, y=y.axis.title)+
    theme(legend.position = "topleft",
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey80"),
          panel.grid.major = element_line(colour = "grey70", size = 0.2),
          panel.grid.minor = element_line(colour = "grey85", size = .2),
          strip.background = element_rect(fill = "white", colour = "white"),
          axis.title.x = element_text(colour = "grey30", size=8),
          axis.title.y = element_text(colour = "grey30", size=8),
          axis.text.x = element_text(size=7, angle=xLabAngle),
          axis.text.y = element_text(size=7) )
  ### this code is not setup for properly dealing with group not null
  # geom_point(data=dataset,aes_string(x=xVar, y="trimMean"), color="darkred",
  #           shape=18, size=1.5,show_guide = FALSE)+
  # geom_text(data=dataset, aes(x=xVar, label=trimMean, y=trimMean+.05 ), size=2.25)
  # make a facet if group is not null
  if(!is.null(group)) {
    facets = facet_wrap(formula(paste("~ ", group)), scales=scales)
    p.boxplot = p.boxplot + facets
  }
  return(p.boxplot)
}

# monthly boxplot w/ jittered outliers using zoo package for months
aux$monBpJitter <- function(fy, dataset, xVar, yVar, group=NULL, jitter.x=0.2,
                       title.plot=NULL, title.x.axis=NULL, title.y.axis=NULL, logBase=NULL, flip=FALSE,
                       xlabAngle=0) {
  tmpDataset = copy(dataset[fiscalyr==fy])
  tmpDataset[, month_year :=  factor(month_year,
                                     levels=c(unique(as.character(month_year))[order(unique(as.yearmon(month_year)))])) ]
  p.boxplot = bpJitter(dataset=tmpDataset,
                       xVar=xVar, yVar=yVar, group=group, jitter.x=jitter.x,
                       title.plot=title.plot,
                       title.x.axis=title.x.axis, title.y.axis=title.y.axis, logBase=logBase,
                       xlabAngle=xlabAngle)
  if(flip==TRUE) {p.boxplot = p.boxplot+coord_flip()}
  return(p.boxplot)
}

# Clinical Days/timePeriod function to determine days a consumer stayed in the hospital ----
aux$los.clin <- function(Start, End, admit, expire, discharge) {
  # creating calculated end date
  if(is.na(as.character(discharge))) {calcEnd = min(End, expire+1) } else
    # note: as.character converts NA to NA_character_ so that is.na works (see previous line)
  {calcEnd = min(End, discharge) }

  # creating calculated start date
  if(admit <= Start) {calcStart = Start} else
    if(admit %between% c(Start, End)) { calcStart = admit}

  # creating number of hospital days the consumer spent in the current time period
  daysHosp = calcEnd-calcStart

  # return days Hosp
  return(as.numeric(daysHosp))
}

# Financial Days/timePeriod function to determine days a consumer stayed in the hospital----
aux$los.finUnits <- function(Start, End, admit, UnitsUsed) {
  # vector of dates of the hospital record
  # if UnitsUsed is NA, replace it with '0' for the next calculation
  if(is.na(UnitsUsed)) { UnitsUsed = 0 }
  daysPaid = as.Date(admit:(admit+UnitsUsed))
  daysPaid = daysPaid[-length(daysPaid)]
  # vector of dates in the temp time period
  daysMonth = as.Date(Start:End)
  # intersection of daysPaid and daysMonth (number of days in the time period that were paid)
  DaysPd = as.Date(intersect(daysMonth, daysPaid))
  # number of paid units in the time period
  monPaidUnits = length(DaysPd)
  # return the number of days paid in the time period
  return( monPaidUnits )
}

# clinical LOS function - if discharge is blank, use auth_exp
aux$clinicalStay = function(start, expiration, discharge) {
  if(!is.na(discharge)) {
    clinStay = discharge-start
  } else {
    clinStay = expiration-start
  }
  return(as.numeric(clinStay))
}

aux$ask <- function (msg = "Please choose an option 1-3:")
  {
    cat(msg)
    y <- readLines(con = stdin(), n = 1)
    if (y %nin% c(1, 2, 3)) {
      y <- aux$ask()
    }
    return(y)
  }
