#### INITIALIZATION ####
# clear RAM
rm(list = ls())

# which computer results in correct base working directory
baseWD <- switch(Sys.info()["nodename"],
                 "JAMES" = "B:",
                 "JAMES-PC" = "D:",
                 "CMHJ3DTVZ1-LT" = "C:/Users/dalrymplej",
                 "WSHSQLGP" = "C:/Users/dalrymplej")

# current date parameters
# current fiscal year
fiscal_year <- "2016"
# current month list for which months need to be ran
month_list = c("November") # , "November", "December")
time_frame = "initial"
# error object initialization
errorList <- NULL

# start time
# overall time
startTime <- Sys.time()
# before for loop time
preTime <- Sys.time()

# set working directory for data upload and aux code - same no matter the month
project <- "Overall Picture" # user input required
initialWD <- "Dropbox/Utilization Management/UM Monthly Reports"
dataWD <- file.path("Data", project) # working directory for data upload

## source outside files ##
# read in Lsource file - personal library
source(file.path(baseWD, "Dropbox/WCCMH/R/begin script R code.r"))
source(file.path(baseWD, initialWD, "R Code/Overall Picture/ovr auxillary.r"))

# m=1
for(m in seq_along(month_list)) {
  resultsWD <- file.path(baseWD, initialWD, "Results", paste("fy", fiscal_year), month_list[m],
                         "Overall Picture")
  doc <- docx(title = paste("Overall Picture", fiscal_year, month_list[m]))
  doc <- addTitle(doc, "Overall Picture Graphs", level=1)
  doc <- addParagraph(doc, "This Word document is created using R software and ReporteRs package. More will be added later, this is just the beginning. If we use MS Word 2010 or newer, then the graphs will be of the highest quality. We can use a MS Word template if that is preferred. This comment will be edited/removed at some point. Everything format-wise and word-wise is subject to change.")
  read_files <- list.files(pattern = "rds", path=file.path(baseWD, initialWD, dataWD,
                                     paste("fy", fiscal_year), month_list[m], time_frame), full.names=TRUE)
  # load fund data
  prog_fund_data <- readRDS(grep(x=read_files, pattern="fund_data", value=TRUE, ignore.case=TRUE))
  # load fund data
  prog_fb_served <- readRDS(grep(x=read_files, pattern="prog_fb_served", value=TRUE, ignore.case=TRUE))
  # load program summary (admission, discharge, active_between)
  prog_summary <- readRDS(grep(x=read_files, pattern="prog_summary", value=TRUE, ignore.case=TRUE))
  prog_fy <- prog_summary$fy
  prog_qtr <- prog_summary$qtr
  prog_mon <- prog_summary$mon

#### Manipulate Data, Create Plots, Save Plots ####
### CMH program consumer movement: plot program admissions, discharge, active ###
prog_fy[, p_label := paste0("+", num_new_program, "/-", num_disc_program)]
prog_qtr[, p_label := paste0("+", num_new_program, "/-", num_disc_program)]
prog_mon[, p_label := paste0("+", num_new_program, "/-", num_disc_program)]

prog_fy[, time := gsub(time, pattern="20", replace=" ", fixed=TRUE)]
## fiscal year ##
p_prog_fy <- ggplot(data=prog_fy, aes(x=time, y=num_adm, ymax=1.2*num_adm, fill=program, group=program)) +
  geom_bar(stat="identity", width=0.6, position=position_dodge(0.6), color="black", size=0.1) +
  geom_text(data=prog_fy, hjust= -.2 , vjust= 0.5 , stat="identity", position=position_dodge(width=0.6),
            aes( label=num_adm, x=time, fill=program, group=program, y=num_adm), size = 3, angle= 90) +
  geom_text(data=prog_fy, hjust= -.05 , vjust= 0.5 , stat="identity", position=position_dodge(width=0.6),
            aes(label=p_label, x=time, fill=program, group=program, y=0), size = 3, angle= 90) +
  my_theme + labs(title="Program: FY Admissions\n(+ new adms / - discharges)") +
  theme(legend.key.size = unit(0.5, "cm"),
        axis.ticks=element_blank(), legend.position = "top",
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle=0, size=10),
        plot.title = element_text(face="bold", size=10)) +
  scale_fill_manual(values = um_colors[ c(1:3) ] )
ggsave(plot=p_prog_fy,
       filename= file.path(baseWD, initialWD, "Results", paste("fy", fiscal_year), month_list[m],
                           "Overall Picture", "Yearly/Program", "ovr fy adm by prog.pdf"),
       width=5, height=5, dpi=600, units="in")
doc <- addTitle(doc, "Admissions", level = 2)
doc <- addParagraph(doc, "Plus indicates a new admission during the time span, and minus indicates a discharge during the time span. The numbers at the end of the bars indicate how many people were open to the program at least one day during the date range. While it is possible to have a person in two programs at once, the truth is such an even is extremely rare, and can be ignored completely. A person that has a gap of 90 or more days theoretically should have been discharged and re-admitted, but we do not account for that because it is similarly extremely rare. We do not discharge people if there is a gap in their admission record, as this would result in a lot of misleading admissions and discharges. Such gaps are staff error usually, but not always.")
doc <- addTitle(doc, "Fiscal Year", level = 3)
doc <- addPlot(doc, fun=print, x=p_prog_fy, height=4, width=3)
rm(p_prog_fy)
## fiscal quarter ##
doc <- addTitle(doc, "Fiscal Quarter", level = 3)
for(i in 1:prog_qtr[, length(unique(program))]) {
  # establish tmp data sets
  tmp_prog <- prog_qtr[, unique(program)][i]
  tmp_prog_data <- prog_qtr[program==tmp_prog]
  # quarterly plot
  p_prog_qtr <- ggplot(data=tmp_prog_data, aes(x=time, y=num_adm, ymax=1.2*num_adm, fill=program, group=program)) +
    geom_bar(stat="identity", width=0.6, position=position_dodge(0.6), color="black", size=0.1) +
    geom_text(data=tmp_prog_data, hjust= -.1 , vjust= 0.5 , stat="identity", position=position_dodge(width=0.6),
              aes( label=num_adm, x=time, fill=program, group=program, y=num_adm), size = 3, angle= 0) +
    geom_text(data=tmp_prog_data, hjust= -.1 , vjust= 0.5 , stat="identity", position=position_dodge(width=0.6),
              aes(label=p_label, x=time, fill=program, group=program, y=0), size = 3, angle= 0) +
    my_theme + labs(title=paste0(tmp_prog, ": Quarterly Admissions\n(+ new adms / - discharges)")) +
    theme(axis.ticks=element_blank(), # legend.position = "top",
          axis.text.x = element_blank(),
          axis.text.y = element_text(angle=0, size=10),
          plot.title = element_text(face="bold", size=10)) +
    scale_fill_manual(values = um_colors[i] )+
    coord_flip()
  ggsave(plot=p_prog_qtr,
         filename= file.path(baseWD, initialWD, "Results", paste("fy", fiscal_year), month_list[m],
                             "Overall Picture", "Quarterly/Program", paste0("ovr qtr adm", tmp_prog, ".pdf")),
         width=5, height=5, dpi=600, units="in")
  doc <- addPlot(doc, fun=print, x=p_prog_qtr, height=3.5, width=3.5)
  rm(p_prog_qtr, tmp_prog, tmp_prog_data)
}

doc <- addTitle(doc, "Months", level = 3)
## monthly ##
for(i in 1:prog_mon[, length(unique(program))]) {
  # establish tmp data sets
  tmp_prog <- prog_mon[, unique(program)][i]
  tmp_prog_data <- prog_mon[program==tmp_prog]
  tmp_prog_data[, time := factor(time, levels=tmp_prog_data[, unique(time)])]
  # monthly plot
  p_prog_mon <- ggplot(data=tmp_prog_data, aes(x=time, y=num_adm, ymax=1.2*num_adm, fill=program, group=program)) +
    geom_bar(stat="identity", width=0.6, position=position_dodge(0.6), color="black", size=0.1) +
    geom_text(data=tmp_prog_data, hjust= -.1 , vjust= 0.5 , stat="identity", position=position_dodge(width=0.6),
              aes(label=num_adm, x=time, fill=program, group=program, y=num_adm), size = 3, angle= 0) +
    geom_text(data=tmp_prog_data, hjust= -.1 , vjust= 0.5 , stat="identity", position=position_dodge(width=0.6),
              aes(label=p_label,
                  x=time, fill=program, group=program, y=0), size = 3, angle= 0) +
    my_theme + labs(title=paste0(tmp_prog, ": Monthly Admissions\n(+ new adms / - discharges)")) +
    theme(axis.ticks=element_blank(), # legend.position = "top",
          axis.text.x = element_blank(),
          axis.text.y = element_text(angle=0, size=10),
          plot.title = element_text(face="bold", size=10)) +
    scale_fill_manual(values = um_colors[i] )+
    coord_flip()
  ggsave(plot=p_prog_mon,
         filename= file.path(baseWD, initialWD, "Results", paste("fy", fiscal_year), month_list[m],
                             "Overall Picture", "Monthly/Program", paste0("ovr mon adm", tmp_prog, ".pdf")),
         width=5, height=5, dpi=600, units="in")
  doc <- addPlot(doc, fun=print, x=p_prog_mon, height=3.5, width=3.5)
  rm(p_prog_mon, tmp_prog, tmp_prog_data)
}

### Funding Bucket Served ###
doc <- addTitle(doc, "Consumers Served (Funding Bucket)", level = 2)
doc <- addParagraph(doc, "Consumers served is defined as a consumer having at least one service in the funding bucket during the date range. Whether the consumer has admission or not determines whether they show up in a program or in Non-CMH. Please note that a person is only allowed to be on one program at a time, decided by a priority algorithm. This is a different rule and pool when compared with how we count admissions in section 1.")
## fiscal year ##
prog_fy_srv <- prog_fb_served[span=="fy"]
p_prog_fy_srv <- ggplot(data=prog_fy_srv, aes(x=cat_time, y=num_cases, ymax=1.2*num_cases, fill=program))+
  geom_bar(stat="identity", width=0.6, position=position_dodge(0.6), color="black", size=0.1)+
  geom_text(data=prog_fy_srv, hjust= -.1 , vjust= 0.5 , stat="identity", position=position_dodge(width=0.6),
            aes( label=num_cases, x=cat_time, fill=program , y=num_cases), size = 3, angle=0)+
  geom_text(data = prog_fy_srv[cat_time==as.character(prog_fy_srv[, max(as.numeric(cat_time))])],
            hjust=0 , vjust=0.5 , stat="identity", position=position_dodge(width=0.6),
            aes(label=program, x=cat_time, fill=program , y=0), size=3, angle=0, fontface="bold")+
  my_theme+
  theme(axis.ticks=element_blank(), # legend.position = "top",
        legend.position = "topleft",
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle=0, size=10))+
  scale_fill_manual(values = um_colors[ c(1:3, 5) ] )+
  coord_flip()+labs(title="Consumers Served by Program: Years")
ggsave(plot=p_prog_fy_srv,
       filename= file.path(baseWD, initialWD, "Results", paste("fy", fiscal_year), month_list[m],
                           "Overall Picture", "Yearly/Program", "ovr fy con srv by prog.pdf"),
       width=5, height=3.75, dpi=600, units="in")
doc <- addTitle(doc, "Fiscal Year", level = 3)
doc <- addPlot(doc, fun=print, x=p_prog_fy_srv, height=3.5, width=5.5)

## fiscal quarter ##
prog_qtr_srv <- setkey(prog_fb_served, span)[J("qtr")]
# plot program quarterly
p_prog_qtr_srv <- ggplot(data=prog_qtr_srv, aes(x=cat_time, y=num_cases, ymax=1.3*num_cases, fill=program))+
  geom_bar(stat="identity", width=0.6, position=position_dodge(0.6), color="black", size=0.1)+
  geom_text(data=prog_qtr_srv, aes(x=cat_time, y=num_cases, fill=program, label=num_cases), angle=90, hjust=-0.05, size=2.25)+
  facet_wrap(~program, scale="free", ncol=2)+
  my_theme+
  theme(axis.text.x = element_text(angle=90, size=6))+
  scale_fill_manual(values = um_colors[ c(1:3, 5) ] )+
  labs(title="Consumers Served by Program: Quarterly")
ggsave(plot=p_prog_qtr_srv,
       filename= file.path(baseWD, initialWD, "Results", paste("fy", fiscal_year), month_list[m],
                           "Overall Picture", "Quarterly/Program", "ovr qtr con srv by prog.pdf"),
       width=5, height=4, dpi=600, units="in")
doc <- addTitle(doc, "Fiscal Quarter", level = 3)
doc <- addPlot(doc, fun=print, x=p_prog_qtr_srv, height=5.5, width=5.5)

## fiscal month ##
prog_mon_srv <- setkey(prog_fb_served, span)[J("mon")]
prog_mon_srv[, cat_time := as.yearmon(gsub(x=cat_time, pattern="-", replace=" 20"))]

# factor by order of zoo months
prog_mon_srv[, cat_time := factor(cat_time, levels = prog_mon_srv[, as.character(sort(unique(cat_time)))]) ]
# plot CMH adm mon
p_prog_mon <- ggplot(data=prog_mon_srv, aes(x=cat_time, y=num_cases, ymax=1.3*num_cases, fill=program))+
  geom_bar(stat="identity", width=0.6, position=position_dodge(0.6), color="black", size=0.1)+
  geom_text(data=prog_mon_srv, aes(x=cat_time, y=num_cases, fill=program, label=num_cases),
            position=position_dodge(0.6), angle=90, hjust=-0.05, size=3)+
  facet_wrap(~program, scale="free", ncol=2)+
  my_theme+
  theme(axis.text.x = element_text(angle=90))+
  scale_fill_manual(values = um_colors[ c(1:3, 5) ] ) +
  labs(title="Consumers Served by Program: Monthly")
ggsave(plot=p_prog_mon,
       filename= file.path(baseWD, initialWD, "Results", paste("fy", fiscal_year), month_list[m],
                           "Overall Picture", "Monthly/Program", "ovr mon con srv by prog.pdf"),
       width=5, height=5, dpi=600, units="in")
doc <- addTitle(doc, "Months", level = 3)
doc <- addPlot(doc, fun=print, x=p_prog_mon, height=5.5, width=5.5)

### Fund Data ###
doc <- addTitle(doc, "Funding Sources", level = 2)
doc <- addParagraph(doc, "We are separating spend-down medicaid consumers from full medicaid consumers. Spend-down is abbreviated 'SD' in the graphs. All funding status is determined via the buckets listed in the funding bucket, except for spend-down consumers which we also have to look at the Medicaid coverage category. There should be four categories: Medicaid, Medicaid spend-down, HMP, and GF. Whether and when a person meets their spend-down is not relevant. ABW will stop showing up after three years has passed. We could force all quarters to show up for all funds. It may be better to use another source to determine funding. Setting costs to spend-down will require very careful discussion and planning, and may or may not be possible with any degree of accuracy.")


## fiscal year ##
fund_fy <- prog_fund_data[span=="fy"]
fund_fy[, cat_time := as.character(cat_time)]
# plot prog fy
p_fund_fy <- ggplot(data=fund_fy, aes(x=cat_time, y=num_cases, ymax=1.2*num_cases, fill=fund))+
  geom_bar(stat="identity", width=0.6, position=position_dodge(0.6), color="black", size=0.1)+
  geom_text(data=fund_fy, hjust= -.1 , vjust= 0.5 , stat="identity", position=position_dodge(width=0.6),
            aes( label=num_cases, x=cat_time, fill=fund , y=num_cases), size = 3, angle=0)+
  geom_text(data = fund_fy[cat_time==as.character(fund_fy[, max(as.numeric(cat_time))])], hjust=0 ,
            vjust=0.5 , stat="identity", position=position_dodge(width=0.6),
            aes(label=fund, x=cat_time, fill=fund , y=0), size=3, angle=0, fontface="bold")+
  my_theme+
  theme(axis.ticks=element_blank(), # legend.position = "top",
        legend.position = "topleft",
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle=0, size=10))+
  scale_fill_manual(values = um_colors[ c(1:5) ] )+
  coord_flip()+labs(title="Consumers Served by Fund: Years")
ggsave(plot=p_fund_fy,
       filename= file.path(baseWD, initialWD, "Results", paste("fy", fiscal_year), month_list[m],
                           "Overall Picture", "Yearly/Fund", "ovr fy con srv by fund.pdf"),
       width=5, height=3.75, dpi=600, units="in")
doc <- addTitle(doc, "Fiscal Year", level = 3)
doc <- addPlot(doc, fun=print, x=p_fund_fy, height=3.5, width=5.5)

## fiscal quarter ##
fund_qtr <- setkey(prog_fund_data, span)[J("qtr")]
# plot fund quarterly
p_fund_qtr <- ggplot(data=fund_qtr, aes(x=cat_time, y=num_cases, ymax=1.5*num_cases, fill=fund))+
  geom_bar(stat="identity", width=0.6, position=position_dodge(0.6), color="black", size=0.1)+
  geom_text(data=fund_qtr, aes(x=cat_time, y=num_cases, fill=fund, label=num_cases), angle=90, hjust=-0.05, size=2.25)+
  facet_wrap(~fund, scale="free", ncol=2)+
  my_theme+
  theme(axis.text.x = element_text(angle=90, size=6))+
  scale_fill_manual(values = um_colors[ c(1:5) ] )+
  labs(title="Consumers Served by Fund: Quarterly")
doc <- addTitle(doc, "Fiscal Quarter", level = 3)
ggsave(plot=p_fund_qtr,
       filename= file.path(baseWD, initialWD, "Results", paste("fy", fiscal_year), month_list[m],
                           "Overall Picture", "Quarterly/Fund", "ovr qtr con srv by fund.pdf"),
       width=5, height=5.5, dpi=600, units="in")
doc <- addPlot(doc, fun=print, x=p_fund_qtr, height=5.5, width=5.5)

## fiscal month ##
fund_mon <- setkey(prog_fund_data, span)[J("mon")]
fund_mon[, cat_time := as.yearmon(gsub(x=cat_time, pattern="-", replace=" 20"))]

# factor by order of zoo months
fund_mon[, cat_time := factor(cat_time, levels = fund_mon[, as.character(sort(unique(cat_time)))]) ]
# plot fund monthly
p_fund_mon <- ggplot(data=fund_mon, aes(x=cat_time, y=num_cases, ymax=1.3*num_cases, fill=fund))+
  geom_bar(stat="identity", width=0.6, position=position_dodge(0.6), color="black", size=0.1)+
  geom_text(data=fund_mon, aes(x=cat_time, y=num_cases, fill=fund, label=num_cases),
            position=position_dodge(0.6), angle=90, hjust=-0.05, size=3)+
  facet_wrap(~fund, scale="free", ncol=2)+
  my_theme+
  theme(axis.text.x = element_text(angle=90))+
  scale_fill_manual(values = um_colors[ c(1:5) ] ) +
  labs(title="Consumers Served by Fund: Monthly")
ggsave(plot=p_fund_mon,
       filename= file.path(baseWD, initialWD, "Results", paste("fy", fiscal_year), month_list[m],
                           "Overall Picture", "Monthly/Fund", "ovr mon con srv by fund.pdf"),
       width=5, height=5, dpi=600, units="in")
doc <- addTitle(doc, "Months", level = 3)
doc <- addPlot(doc, fun=print, x=p_fund_mon, height=5.5, width=5.5)
# save document
writeDoc(doc, file = file.path(baseWD, initialWD, "Results", paste("fy", fiscal_year), month_list[m],
  "Overall Picture", paste0("overall picture ", month_list[m], " ", fiscal_year, ".docx")))
print(paste("Monthly ggplot results complete: fy", fiscal_year, month_list[m]))
}