#### auxillary code for community hospitalizations ####


### testing code
  # data=losData[fiscalyr=="2013"]; xVar="month_year"; yVar="finLOS"; title="test title"
  # x.axis.title="test x"; y.axis.title="test y"; trim=0.15; group="Program"; xLabAngle=90; scales="fixed"
  #
  # rm(data,xVar,yVar,title,x.axis.title,y.axis.title,trim,group,xLabAngle)

# trimmed boxplot (outliers not jittered)
trimBp = function(data, xVar, yVar, title=NULL, x.axis.title=NULL, y.axis.title=NULL, trim=.15,
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
monBpJitter = function(fy, dataset, xVar, yVar, group=NULL, jitter.x=0.2,
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
  los.clin = function(Start, End, admit, expire, discharge) {
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
  los.finUnits = function(Start, End, admit, UnitsUsed) {
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

## if team is missing, needs to be assigned to Non-CMH (was 'Other' per Mike, but this cause problems since
 # too many new teams were created without my knowning resulting in TONS of 'Others')
  teamSwitch = function(x) switch(x,
                                  "Non-CMH" = "Non-CMH",
                                  "WSH - PATH/PORT" = "Non-CMH",
                                  "WSH - PORT" = "Non-CMH",
                                  "WSH - Access/Engagement" = "Non-CMH",
                                  "Community Support and Treatment Services - CSTS" = "Non-CMH",
                                  "Crisis Residential Services" = "Non-CMH",
                                  "WSH - MI - Adult" = "MI Adult",
                                  "WSH - ACT" = "MI Adult",
                                  "WSH - ATO" = "MI Adult",
                                  "WSH - Children's Services" = "Youth & Family",
                                  "WSH - Children's Services - Home Based" = "Youth & Family",
                                  "WSH - DD Adult" = "DD Adult",
                                  "Non-CMH")

# function for fund
  my_fund <- function(x) {result <- switch(x,
                                 "ABW-Acute Services" = "ABW",
                                 "Medicaid - Acute Services" = "Medicaid",
                                 "General Fund - Acute Services" = "GF",
                                 "General Fund" = "GF",
                                 "Dual - Medicare + Medicaid" = "Medicaid",
                                 "Medicaid - State Plan" = "Medicaid",
                                 "Adult Benefit Waiver" = "ABW",
                                 "MIChild" = "Medicaid",
                                 "HMP - Acute Services" = "HMP",
                                 "HMP-Acute Services" = "HMP",
                                 "SED Waiver" = "Medicaid",
                                 x
                                 )
                          if(nchar(result)<1) {result <- "no funding"}
                          return(result)
  }
my_fund <- Vectorize(my_fund)

# clinical LOS function - if discharge is blank, use auth_exp
  clinicalStay = function(start, expiration, discharge) {
    if(!is.na(discharge)) {
      clinStay = discharge-start
    } else {
      clinStay = expiration-start
    }
    return(as.numeric(clinStay))
  }

# function to convert Program to shorter name
  shortProg = function(x) {
    switch(x,
           "DD Adult" = "DD",
           "MI Adult" = "MI",
           "Youth & Family" = "Y&F",
           "Non-CMH" = "Non-CMH",
           "Other")
  }