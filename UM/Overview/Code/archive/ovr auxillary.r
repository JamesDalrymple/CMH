usePackage(packages=c("RODBC", "ReporteRs"))
# narrow down team assignment to one per person per day
  teamCMH <- c("WSH - Access/Engagement",
              "Community Support and Treatment Services - CSTS",
              "WSH - ACT",
              "WSH - ATO",
              "WSH - Children's Services",
              "WSH - Children's Services - Home Based",
              "WSH - DD Adult",
              "WSH - ICSS team",
              "WSH - MI - Adult",
              "WSH - OBRA")

# case function to determine team priority - output is numerical
## this function will help reduce rows that have the same consumer,
## cpt code, date, and yet more than one team, into having only one team
### I have gone over this with Kelly (early November 2013)
  team.priority <- function(x) { y <- switch(x,
                                           "WSH - OBRA" = 1,
                                           "WSH - DD Adult" = 2,
                                           "WSH - ACT" = 3,
                                           "WSH - MI - Adult" = 4,
                                           "WSH - ATO" = 4,
                                           "WSH - Children\'s Services - Home Based" = 5,
                                           "WSH - Children\'s Services" = 6,
                                           "Community Support and Treatment Services - CSTS" = 7,
                                           "WSH - Access/Engagement" = 7,
                                           "Non-CMH" = 10)
                                return(as.numeric(y))
  }

# change team name
## note that here OBRA consumers are removed/assigned to Non-CMH
  team.change <- function(x) { y <- switch(x,
                                         "WSH - OBRA" = "Non-CMH",
                                         "WSH - Children\'s Services - Home Based" = "Child Services",
                                         "WSH - Children\'s Services" = "Child Home Based",
                                         "WSH - DD Adult" = "DD Adult",
                                         "WSH - ACT" = "ACT",
                                         "WSH - MI - Adult" = "MI Adult",
                                         "WSH - ATO" = "MI Adult",
                                         "Community Support and Treatment Services - CSTS" = "Non-CMH",
                                         "WSH - Access/Engagement" = "Non-CMH",
                                         "Non-CMH" = "Non-CMH")
  }

# cost and consumer function
  costCon <- function(cost, consumers) {
    result <- paste(moneyAdd(cost), " (", consumers, ")", sep="")
    return(result)
  }

# cost and consumer function
  costCon2 <- function(cost, consumers) {
    result <- paste(moneyAdd(cost), " (", consumers, " consumers)", sep="")
    return(result)
  }

# function to add spaces before ABW cost labels
  abwMoney <- function(cost, consumers) {
    result <- paste(paste(rep(" ", 6), collapse=""),
                   moneyAdd(cost), " (", consumers, ")", sep="")
    return(result)
  }

### monthly Overall CMH Program graphs ----
  # function to create and save ggplots for CMH programs by fiscal year
  plotPrMoCMH <- function(dataInput) {
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
  groupPlot <- function(dataInput, dateColumn, dateColType=NULL, title=NULL, x_title=NULL,
                       y_title=NULL, y, group=NULL, filename=NULL, width=5.5, height=NULL) {
    if(dateColType=="month") {
      # change date class to factors so that graphs show months in sequential order
      eval(parse(text =
        paste("levelOrder = dataInput[, as.character(as.yearmon(unique(", dateColumn, ")))]", sep="")
      ))
      eval(parse(text =
        paste("dataInput[, ", dateColumn, " := factor(", dateColumn, ", levels = levelOrder) ]", sep="")
      ))
    }

    # length of months
    tmpDateLength <- eval(parse(text = paste("dataInput[, length(unique(", dateColumn, ")) ]", sep="")))
    # height of graph
    if(is.null(height)) {height = tmpDateLength/3+1.5}

    # make a column for ymax
    eval(parse(text =
                 paste("dataInput[, ymax := 1.4*", y, " ]", sep="")
    ))

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