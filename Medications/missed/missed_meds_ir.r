#23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789#
#### [Initializing working directory and input parameters] ####
# clear RAM
rm(list = ls())

# which computer results in correct base working directory
baseWD <- switch(Sys.info()["nodename"],
                 "DESKTOP-45K7RRN" = "B:",
                 "JAMES" = "B:",
                 "JAMES-PC" = "D:",
                 "CMHJ3DTVZ1-LT" = "C:/Users/dalrymplej",
                 "WSHSQLGP" = "C:/Users/dalrymplej")
results <- "Dropbox/Medications/Results/Missed Meds"
r_code <- "Dropbox/Medications/R Code/Missed Meds"

# read in Lsource file - personal library
source(file.path(baseWD, "Dropbox/WCCMH/R/begin script R code.r"))
source(file.path(baseWD, r_code, "mm_ir_auxillary.r"))

channel <- odbcConnect("wshsqlgp")
odbcQuery(channel = channel, query = "use James_CSTS")
qtr_dt_list <- dt_fy_qtrs(as.Date("12/31/2015", format = "%m/%d/%Y"))
current_fy <- paste0("FY", substr(qtr_dt_list[, unique(my_fy(start_date))], 3, 4))
# intialize datasets to hold entire fiscal year data
fy_summary_red <- fy_summary_full <-
  data.table(vendor_short = character(), num_authorized = integer(),
             num_ir_cases = integer(), num_IRs = integer(),
             pct_con_IRs = numeric(), qtr = character())
for_review <- data.table(auth_case_no = numeric(), vendor_short = character(),
                         mm_ir_cases = integer(), num_IRs = integer(),
                         qtr = character(), vendor_name = character())
mm_plots <- NULL
v_names <- data.table(vendor_name = character(), vendor_short = character())
# cycle through quarters and make quarterly graphs/csvs and combine quarterly data for fy graphs
for (i in 1:nrow(qtr_dt_list)) {
current_qtr <- gsub(x = qtr_dt_list[i, qtr], pattern = " ", replace = "_")
startdate <- as.character(format(qtr_dt_list[i, start_date], "%m/%d/%Y"))
enddate <- as.character(format(qtr_dt_list[i, end_date], "%m/%d/%Y"))

#### [SQL] ####
### [SQL]:[query setup] ###
# details for boxplots and drilldown
sql_ir_detail <-
  paste0("select * from dbo.jd_ir_detail('", startdate, "', '", enddate, "')")
# full detail for personal review and drilldown
sql_full_detail <-
  paste0("select * from dbo.jd_ir_full_detail('", startdate, "', '", enddate, "')")

### [SQL]:[run queries] ###
full_detail <- sqlQuery(channel = channel, query = sql_full_detail,
                        stringsAsFactors = FALSE)
ir_detail <- sqlQuery(channel = channel, query = sql_ir_detail,
                      stringsAsFactors = FALSE)
### data.table conversion ###
full_detail <- data.table(full_detail)
full_detail <- full_detail[order(vendor, case_no, IR_number, begintime)]
ir_detail <- data.table(ir_detail)
ir_detail[, qtr := current_qtr] # add current fy qtr

ir_detail[, vendor_name := vendor]
ir_detail[, vendor_short := shortVendor(vendor)]
ir_detail[, vendor := NULL]
tmp_v_names <- unique(ir_detail[, c("vendor_name", "vendor_short"),
  with = FALSE])[vendor_short!=vendor_name]
v_names <- rbindlist(list(v_names, tmp_v_names))
v_names <- unique(v_names)
rm(tmp_v_names)

### fixing data errors ###
# fixing manually per Pat Cowan 7/14/2015
ir_detail[auth_case_no == 212189 & qtr == "2015_Q2", num_IRs := 0]
# removing Turning Leaf vendor per Pat Cown 7/20/2015
for_review <- rbindlist(list(for_review,
  ir_detail[vendor_name == "TURNING LEAF REHABILITATION SERVICES, INC"]),
  use.names = TRUE)
ir_detail <- ir_detail[!vendor_name ==
  "TURNING LEAF REHABILITATION SERVICES, INC"]

for_review <- rbindlist(list(for_review,
  ir_detail[num_IRs > 30]), use.names = TRUE)
ir_detail <- ir_detail[num_IRs <= 30]

# aggregate graph data
ir_summary <- ir_detail[,
  list(num_authorized =length(unique(auth_case_no)),
       mm_ir_cases = sum(mm_ir_cases),
       num_IRs = sum(num_IRs, na.rm = TRUE)),
  by=list(vendor_short)]
ir_summary_red <- ir_summary[mm_ir_cases>0]
rm(ir_summary, sql_full_detail, sql_ir_detail)

# prepare quarterly data for fy summary
ir_summary_red[, pct_con_IRs := mm_ir_cases/num_authorized]
ir_summary_red[, qtr := current_qtr]

#### [Graphing] ###
# scatterplot
scatter_dt <- ir_detail[vendor_short %in% ir_summary_red[,
  vendor_short]][mm_ir_cases > 0]

p_scatter <- ggplot(data = scatter_dt, aes(x = vendor_short, y = num_IRs)) +
  geom_point(position = position_jitter(height = 0.2, width = 0.2), alpha = 0.6) +
  coord_flip() + my_theme +
  theme(plot.title = element_text(size = 12, face = "bold", vjust = 0.05),
        panel.grid.major = element_line(color = "grey60", size = 0.1),
        panel.grid.minor = element_line(color = "grey80", size = 0.1),
        axis.title.x = element_text(colour = "grey30"),
        axis.text.x = element_text(size = 6)) +
  labs(title = paste0(qtr_dt_list[i, qtr], ": IRs - Missed Meds"),
       x = "", y = "# missed medication IRs per consumer") +
  scale_y_continuous(minor_breaks = seq(0 , scatter_dt[, max(num_IRs)], 1),
                     breaks = seq(0, scatter_dt[, max(num_IRs)], 2))
mm_plots[[i]] <- p_scatter
# ggsave(filename=file.path(baseWD, results, paste0("scatter", current_qtr, ".pdf")),
#       plot=p_scatter, height=6, width=7.5, units="in", dpi=600)

# save csv files
full_detail <- full_detail[!is.na(IR_number)]
write.csv(full_detail, file.path(baseWD, results,
  paste0("full_ir_details_", current_qtr, ".csv")), row.names = FALSE)

# combine all fy data
fy_summary_red <- rbindlist(list(fy_summary_red, ir_summary_red))
}

### fy summary ###

# reduced vendors
red_vendors <- expand.grid(
  vendor_short=fy_summary_red[, unique(vendor_short)],
  qtr=qtr_dt_list[, gsub(qtr, pattern=" ", replace="_")]
)
fy_summary_red <- merge(
  x=fy_summary_red,
  y=red_vendors,
  all=TRUE, by=c("qtr", "vendor_short")
)

### all fy data plots ###
# labeling prep
fy_summary_red <- fy_summary_red[order(pct_con_IRs)]
fy_summary_red[, pct_con_IRs := pct_con_IRs*100]
fy_summary_red[!is.na(pct_con_IRs), pct_label :=
  paste0(round(pct_con_IRs, 0), "% (", num_ir_cases, "/", num_authorized, ")")]
setcolorder(fy_summary_red,
  c("vendor_short", "qtr", "num_authorized", "num_ir_cases", "num_IRs", "pct_con_IRs", "pct_label"))
fy_summary_red[, qtr := factor(qtr, levels =rev(sort(as.character(unique(qtr)))))]
fy_summary_red[, label_length := nchar(pct_label)/max(pct_con_IRs, na.rm=TRUE)*10+pct_con_IRs+6]
fy_summary_red[is.na(pct_con_IRs), label_length := NA]

one_plot_dt <- copy(fy_summary_red)
fy_one_plot <- ggplot(data=one_plot_dt, position=position_dodge(0.5),
       aes(x = vendor_short, y=pct_con_IRs, ymax=pct_con_IRs*1.1, fill = qtr))+
  geom_bar(stat="identity", width=0.5, size=0.1, color="black", position=position_dodge(0.85))+
  geom_point(stat="identity", color="black", size=5.5, position=position_dodge(0.5),
             aes(x=vendor_short, y=label_length), show.legend=FALSE)+
  geom_point(stat="identity", color="white", size=4.5, position=position_dodge(0.5),
             aes(x=vendor_short, y=label_length), show.legend=FALSE)+
  coord_flip()+
  my_theme+theme(plot.title = element_text(size=10, face="bold", vjust=-0.05),
                 legend.margin=unit(-0.6,"cm"),
                 legend.position="", legend.key.size = unit(0.1, "inch"),
                 legend.text=element_text(size=6), legend.title = element_blank(),
                 axis.text.x = element_text(size=6),
                 axis.title.x = element_text(colour = "grey30", size=8),
                 axis.text.y = element_text(size=6),
                 axis.title.y = element_blank(),
                 plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))+
  geom_text(data=one_plot_dt, position=position_dodge(0.5),
    aes(x=vendor_short, y=pct_con_IRs, fill=qtr, label=pct_label), hjust=0, size=2.5)+
  geom_text(data=one_plot_dt, position=position_dodge(0.5),
            aes(x=vendor_short, y=label_length, label=num_IRs, fill=qtr), color="black", size=2.5)+
  labs(title=paste0(qtr_dt_list[i, qtr],
    ": IRs - Missed Medications\n (# IR missed meds consumers/# authorized), total IRs circled"),
       x="Providers", y="% consumers w/ missed medication IRs")+
  scale_fill_manual(values=um_colors[1:fy_summary_red[, length(unique(qtr))]])
mm_plots[[length(mm_plots)+1]] <- fy_one_plot

fy_summary_red[, label_length := nchar(pct_label)*170/max(pct_con_IRs, na.rm=TRUE)+pct_con_IRs]
fy_facets <- ggplot(data=fy_summary_red, position=position_dodge(0.5),
       aes(x=vendor_short, y=pct_con_IRs, ymax=pct_con_IRs*2.5, fill=qtr))+
  geom_bar(stat="identity", width=0.8, size=0.1, color="black", position=position_dodge(0.5))+
  geom_point(stat="identity", color="black", size=5.5, position=position_dodge(0.5),
             aes(x=vendor_short, y=0), show.legend=FALSE)+
  geom_point(stat="identity", color="white", size=4.5, position=position_dodge(0.5),
             aes(x=vendor_short, y=0), show.legend=FALSE)+
  facet_wrap(~vendor_short, scales="free", ncol=6)+
  my_theme+theme(plot.title = element_text(size=11, face="bold", vjust=-0.2),
                 legend.margin=unit(-0.6,"cm"),
                 legend.position="", legend.key.size = unit(0.1, "inch"),
                 legend.text=element_text(size=6), legend.title = element_blank(),
                 axis.text.x = element_text(size=6),
                 axis.title.x = element_text(colour = "grey30", size=8),
                 axis.text.y = element_text(size=6),
                 axis.title.y = element_blank(),
                 plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))+
  geom_text(data=fy_summary_red, position=position_dodge(0.5),
            aes(x=vendor_short, y=pct_con_IRs, fill=qtr, label=pct_label), hjust=0, size=2.5, angle=90)+
  geom_text(data=fy_summary_red, position=position_dodge(0.5),
            aes(x=vendor_short, y=0, label=num_IRs, fill=qtr), color="black", size=2.5, angle=90)+
  labs(title=paste0(qtr_dt_list[i, qtr],
                    ": IRs - Missed Meds: total IRs circled\n (# IR missed meds consumers/# authorized)"),
       x="Providers", y="% consumers w/ missed medication IRs")+
  scale_fill_manual(values=um_colors[1:fy_summary_red[, length(unique(qtr))]])
mm_plots[[length(mm_plots)+1]] <- fy_facets

write.csv(for_review, file.path(baseWD, results, "cases_omitted_for_review.csv"), row.names=FALSE)
write.csv(fy_summary_red, "fy_data.csv", row.names=FALSE)

# Creation of mydoc, a mydocx object
mydoc <- docx(title="Missed Medications: FY 2015" )

# add table v_names into mydoc
mydoc <- addFlexTable( mydoc,  FlexTable(v_names[order(vendor_short)]))

# add a page break
# mydoc = addPageBreak( mydoc )

# add text with stylename "Normal" into mydoc
# mydoc = addParagraph( mydoc, value = "Hello World!", stylename = "Normal" )

# add a plot into mydoc
for(i in 1:nrow(qtr_dt_list)) {
  mydoc <- addPlot( mydoc, fun=print, x=mm_plots[[i]], width=6, height=3.5 )
}
mydoc <- addPlot( mydoc, fun=print, x=mm_plots[[{nrow(qtr_dt_list)+1}]], width=6, height=5.5)
mydoc <- addPlot( mydoc, fun=print, x=mm_plots[[{nrow(qtr_dt_list)+2}]], width=6, height=8.5 )

# write the doc
writeDoc( mydoc, file = file.path(baseWD, results, paste("Missed_Meds_FY15.docx")))
