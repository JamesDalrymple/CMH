#23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789#
#### [Initializing working directory and input parameters] ####
# clear RAM
rm(list = ls())

# which computer results in correct base working directory
baseWD <- switch(Sys.info()["nodename"],
                 "JAMES" = "B:",
                 "JAMES-PC" = "D:",
                 "CMHJ3DTVZ1-LT" = "C:/Users/dalrymplej",
                 "WSHSQLGP" = "C:/Users/dalrymplej")
results <- "Dropbox/Medications/Results/Missed Meds"
r_code <- "Dropbox/Medications/R Code/Missed Meds"

# read in Lsource file - personal library
source(file.path(baseWD, "Dropbox/WCHO work/R source/begin script R code.r"))
source(file.path(baseWD, r_code, "mm_ir_auxillary.r"))

channel <- odbcConnect("wshsqlgp")
odbcQuery(channel=channel, query="use James_CSTS")
qtr_dt_list <- dt_fy_qtrs()
current_fy <- paste0("FY", substr(qtr_dt_list[, unique(my_fy(start_date))], 3, 4))
# intialize datasets to hold entire fiscal year data
fy_summary_red <- fy_summary_full <-
  data.table(vendor=character(), num_authorized=integer(),
             num_ir_cases=integer(), num_IRs=integer(), pct_con_IRs=numeric(), qtr = character())
for_review <- data.table(auth_case_no=numeric(), vendor=character(), mm_ir_cases=integer(), num_IRs=integer(), qtr=character())
# cycle through quarters and make quarterly graphs/csvs and combine quarterly data for fy graphs
for(i in 1:nrow(qtr_dt_list)) {
current_qtr <- gsub(x=qtr_dt_list[i, qtr], pattern=" ", replace="_")
startdate <- as.character(format(qtr_dt_list[i, start_date], "%m/%d/%Y"))
enddate <- as.character(format(qtr_dt_list[i, end_date], "%m/%d/%Y"))

#### [SQL ####
### [SQL: query setup] ###
# details for boxplots and drilldown
sql_ir_detail <- paste0("select * from dbo.jd_ir_detail('", startdate, "', '", enddate, "')")
# full detail for personal review and drilldown
sql_full_detail <- paste0("select * from dbo.jd_ir_full_detail('", startdate, "', '", enddate, "')")

### [SQL]:[run queries] ###
full_detail <- sqlQuery(channel = channel, query=sql_full_detail, stringsAsFactors=FALSE)
ir_detail <- sqlQuery(channel = channel, query=sql_ir_detail, stringsAsFactors=FALSE)
### data.table conversion ###
full_detail <- data.table(full_detail)
full_detail <- full_detail[order(vendor, case_no, IR_number, begintime)]
ir_detail <- data.table(ir_detail)
ir_detail[, qtr := current_qtr] # add current fy qtr
### fixing data errors ###
ir_detail[auth_case_no==212189 & qtr=="2015_Q2", num_IRs := 0] # fixing manually per Pat Cowan 7/14/2015

# aggregate graph data
ir_summary <- ir_detail[,
  list(num_authorized =length(unique(auth_case_no)),
       mm_ir_cases = sum(mm_ir_cases),
       num_IRs = sum(num_IRs, na.rm=TRUE)),
  by=list(vendor)]
ir_summary_red <- ir_summary[mm_ir_cases>0]
ir_summary_full <- ir_summary
rm(ir_summary, sql_missed_meds, sql_ir_detail)

#### [Graphing Preparation] ####
# create percentage columns
ir_summary_red[, pct_con_IRs := mm_ir_cases/num_authorized]
ir_summary_full[, pct_con_IRs := mm_ir_cases/num_authorized]
# re-level for ggplot2
ir_summary_red[, vendor := factor(vendor, levels=ir_summary_red[order(pct_con_IRs), vendor])]
ir_summary_full[, vendor := factor(vendor, levels=ir_summary_full[order(pct_con_IRs), vendor])]
# add current quarter
ir_summary_red[, qtr := current_qtr]
ir_summary_full[, qtr := current_qtr]

### [Graphing]:[ggplot2] ###
#### [Graphing] ###

# all-information-in-one graph
distance_add <- ir_summary_red[, max(pct_con_IRs)*35]
p_one_graph <- ggplot(data=ir_summary_red, aes(x=vendor, y=pct_con_IRs*100, ymax=pct_con_IRs*130))+
  geom_bar(stat="identity", width=0.5, fill="lightblue", color="black")+
  geom_point(stat="identity", width=0.5, color="black", size=10,
             aes(x=vendor, y=pct_con_IRs*100+distance_add))+
  geom_point(stat="identity", width=0.5, color="white", size=9,
             aes(x=vendor, y=pct_con_IRs*100+distance_add))+
  coord_flip()+
  my_theme+theme(plot.title = element_text(size=10, face="bold"),
                 axis.title.x = element_text(colour = "grey30"),
                 axis.title.y = element_text(colour = "white"))+
  geom_text(data=ir_summary_red, aes(x=vendor, y=pct_con_IRs*100,
                             label=paste0(round(pct_con_IRs*100, 1),
                                          "% (", mm_ir_cases, "/", num_authorized, ")" )), hjust=-0.2, size=2.5)+
  geom_text(data=ir_summary_red, aes(x=vendor, y=pct_con_IRs*100+distance_add, label=num_IRs), color="black", size=2.5)+
  labs(title=paste0(qtr_dt_list[i, qtr], ": IRs - Missed Meds\n (# IR missed meds consumers/# authorized)\n# IRs circled"),
       x="Providers", y="% consumers w/ missed medication IRs")
ggsave(filename=file.path(baseWD, results, paste0("missed_med_IR_one_graph_", current_qtr, ".pdf")),
       plot=p_one_graph, height=6, width=7.5, units="in", dpi=600)

# 'consumers with missed med IRs' and 'consumers/consumers authorized'
p_pct_mm <- ggplot(data=ir_summary_red, aes(x=vendor, y=pct_con_IRs*100, ymax=pct_con_IRs*120))+
  geom_bar(stat="identity", width=0.5, fill="lightblue", color="black")+
  coord_flip()+
  my_theme+theme(plot.title = element_text(size=12, face="bold"),
                 axis.title.x = element_text(colour = "grey30"),
                 axis.title.y = element_text(colour = "grey30"))+
  geom_text(data=ir_summary_red, aes(x=vendor, y=pct_con_IRs*100,
    label=paste0(round(pct_con_IRs*100, 1),
    "% (", mm_ir_cases, "/", num_authorized, ")" )), hjust=-0.2, size=2.5)+
  labs(title=paste0(qtr_dt_list[i, qtr], ": IRs - Missed Meds\n (unique consumers/consumers authorized for service)"),
                    x="Providers", y="% consumers w/ missed medication IRs")

# numer of missed med IRs by vendor ... skipping this for now
ir_summary_red[, vendor := factor(vendor, levels=ir_summary_red[order(num_IRs), vendor])]
p_num_irs <- ggplot(data=ir_summary_red, aes(x=vendor, y=num_IRs, ymax=num_IRs*1.20))+
  geom_bar(stat="identity", width=0.5, fill="lightblue", color="black")+
  coord_flip()+
  my_theme+theme(plot.title = element_text(size=12, face="bold"),
                 axis.title.x = element_text(colour = "grey30"),
                 axis.title.y = element_text(colour = "grey30"))+
  geom_text(data=ir_summary_red, aes(x=vendor, y=num_IRs, label=num_IRs), hjust=-0.2, size=2.5)+
  labs(title=paste0(qtr_dt_list[i, qtr], ": IRs - Missed Meds"),
       x="Providers", y="number of missed medication IRs")

# jittered boxplots
box_dt <- ir_detail[vendor %in% ir_summary_red[, vendor]]

# removing outliers greater than 30 for the boxplots, will have a separate file for these cases
tmp_for_review <- box_dt[num_IRs>30]
box_dt <- box_dt[!num_IRs>30]

bp_red <- bpJitter(dataset = box_dt[mm_ir_cases>0], shape_inside=3, shape_outlier=16, alpha=0.4,
         xVar = "vendor", yVar="num_IRs", jitter.x=0.2, jitter.y=0.2, title.plot = "Zero MM IR Consumers Excluded")+coord_flip()
bp_full <- bpJitter(dataset = box_dt,
         xVar = "vendor", yVar="num_IRs", jitter.x=0.2, jitter.y=0.2, title.plot = "Zero MM IR Consumers Included")+coord_flip()


# comb_plots <- arrangeGrob(p_one_graph, p_pct_mm)
# if(!dir.exists(file.path(baseWD, results))) {
#   dir.create(file.path(baseWD, results))
# }
# ggsave(filename=file.path(baseWD, results, paste0("all_plots_", current_qtr, ".pdf")),
#        plot=comb_plots, height=6, width=7.5, units="in", dpi=600)

ggsave(filename=file.path(baseWD, results, paste0("reduced_bp_", current_qtr, ".pdf")),
       plot=bp_red, height=6, width=7.5, units="in", dpi=600)
ggsave(filename=file.path(baseWD, results, paste0("full_bp_", current_qtr, ".pdf")),
       plot=bp_full, height=6, width=7.5, units="in", dpi=600)

# save csv files
write.csv(full_detail, file.path(baseWD, results, paste0("full_ir_details_", current_qtr, ".csv")), row.names = FALSE)

# re-level for rbindlist
ir_summary_red[, vendor := as.character(vendor)]
ir_summary_full[, vendor := as.character(vendor)]

# combine all fy data
fy_summary_red <- rbindlist(list(fy_summary_red, ir_summary_red))
fy_summary_full <- rbindlist(list(fy_summary_full, ir_summary_full))
for_review <- rbindlist(list(for_review, tmp_for_review))
}

### make all vendors show up for all quarters ###
# reduced vendors
red_vendors <- expand.grid(
  vendor=fy_summary_red[, unique(vendor)],
  qtr=qtr_dt_list[, gsub(qtr, pattern=" ", replace="_")]
)
fy_summary_red <- merge(
  x=fy_summary_red,
  y=red_vendors,
  all=TRUE, by=c("qtr", "vendor")
)
# full vendors
full_vendors <- expand.grid(
  vendor=fy_summary_full[, unique(vendor)],
  qtr=qtr_dt_list[, gsub(qtr, pattern=" ", replace="_")]
)
fy_summary_full <- merge(
  x=fy_summary_full,
  y=full_vendors,
  all=TRUE, by=c("qtr", "vendor")
)

### all fy data plots ###
# labeling prep
fy_summary_red <- fy_summary_red[order(pct_con_IRs)]
fy_summary_red[, pct_con_IRs := pct_con_IRs*100]
fy_summary_red[!is.na(pct_con_IRs), pct_label :=
  paste0(round(pct_con_IRs, 1), "% (", num_ir_cases, "/", num_authorized, ")")]

vendor_list <- fy_summary_red[, unique(vendor)]
vendors_l1 <- vendor_list[1:floor(length(vendor_list)/2)]
vendors_l2 <- vendor_list[(floor(length(vendor_list)/2)+1):length(vendor_list)]

fy_a_dt <- copy(fy_summary_red[vendor %in% vendors_l1])
fy_a_dt[, label_length := nchar(pct_label)/max(pct_con_IRs, na.rm=TRUE)*4+pct_con_IRs]
fy_b_dt <- copy(fy_summary_red[vendor %in% vendors_l2])
fy_b_dt[, label_length := nchar(pct_label)*400/max(pct_con_IRs, na.rm=TRUE)+pct_con_IRs]

fy_one_plot_a <- ggplot(data=fy_a_dt, position=position_dodge(0.7),
       aes(x=vendor, y=pct_con_IRs, ymax=pct_con_IRs*1.25, fill=qtr))+
  geom_bar(stat="identity", width=0.5, color="black", position=position_dodge(0.7))+
  geom_point(stat="identity", width=0.5, color="black", size=6, position=position_dodge(0.7),
             aes(x=vendor, y=label_length), show_guide=FALSE)+
  geom_point(stat="identity", width=0.5, color="white", size=5, position=position_dodge(0.7),
             aes(x=vendor, y=label_length), show_guide=FALSE)+
  coord_flip()+
  my_theme+theme(plot.title = element_text(size=10, face="bold"), legend.position="right",
                 axis.title.x = element_text(colour = "grey30"),
                 axis.title.y = element_text(colour = "white"))+
  geom_text(data=fy_a_dt, position=position_dodge(0.7),
    aes(x=vendor, y=pct_con_IRs, fill=qtr, label=pct_label), hjust=0, size=2.75)+
  geom_text(data=fy_a_dt, position=position_dodge(0.7),
            aes(x=vendor, y=label_length, label=num_IRs, fill=qtr), color="black", size=2)+
  labs(title=paste0(qtr_dt_list[i, qtr], ": IRs - Missed Meds - Group 1\n (# IR missed meds consumers/# authorized)\n# IRs circled"),
       x="Providers", y="% consumers w/ missed medication IRs")+
  scale_fill_manual(values=um_colors[1:fy_a_dt[, length(unique(qtr))]])
ggsave(filename=file.path(baseWD, results, paste("fy_reduced_group1.pdf")),
       plot=fy_one_plot_a, height=8, width=8, units="in", dpi=600)

fy_one_plot_b <- ggplot(data=fy_b_dt, position=position_dodge(0.7),
                        aes(x=vendor, y=pct_con_IRs, ymax=pct_con_IRs*1.25, fill=qtr))+
  geom_bar(stat="identity", width=0.5, color="black", position=position_dodge(0.7))+
  geom_point(stat="identity", width=0.5, color="black", size=6, position=position_dodge(0.7),
             aes(x=vendor, y=label_length), show_guide=FALSE)+
  geom_point(stat="identity", width=0.5, color="white", size=5, position=position_dodge(0.7),
             aes(x=vendor, y=label_length), show_guide=FALSE)+
  coord_flip()+
  my_theme+theme(plot.title = element_text(size=10, face="bold"), legend.position="right",
                 axis.title.x = element_text(colour = "grey30"),
                 axis.title.y = element_text(colour = "white"))+
  geom_text(data=fy_b_dt, position=position_dodge(0.7),
            aes(x=vendor, y=pct_con_IRs, fill=qtr, label=pct_label), hjust=-0.2, size=2.75)+
  geom_text(data=fy_b_dt, position=position_dodge(0.7),
            aes(x=vendor, y=label_length, label=num_IRs, fill=qtr), color="black", size=2)+
  labs(title=paste0(qtr_dt_list[i, qtr], ": IRs - Missed Meds - Group 2\n (# IR missed meds consumers/# authorized)\n# IRs circled"),
       x="Providers", y="% consumers w/ missed medication IRs")+
  scale_fill_manual(values=um_colors[1:fy_b_dt[, length(unique(qtr))]])
ggsave(filename=file.path(baseWD, results, paste("fy_reduced_group2.pdf")),
       plot=fy_one_plot_b, height=8, width=8, units="in", dpi=600)

write.csv(for_review, file.path(baseWD, results, "cases_omitted_for_review.csv"), row.names=FALSE)
