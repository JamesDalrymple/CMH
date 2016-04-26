#### initializing working directory and input parameters ####
library(wccmh)
scrub()
# which computer results in correct base working directory
baseWD <- switch(Sys.info()["nodename"],
                 "DESKTOP-45K7RRN" = "B:",
                 "JAMES-PC" = "D:",
                 "WSHSQLGP" = "C:/Users/dalrymplej")
# read in source file - personal library
source(file.path(baseWD,
                 "Dropbox/WCCMH/R/begin script R code.r") )
# setup for working directories - data and results
dataWD <- "Dropbox/Medications/Data"
resultsWD <- "Dropbox/Medications/Results/Medication Incidents"
codeWD <- "Dropbox/Medications/R Code"
# load auxillary file ----
source(file.path(baseWD, codeWD, "med auxillary.r"))
require(ggplot2)
library(RColorBrewer)
library(ReporteRs) # install.packages("ReporteRs")
#### load data ####
report_date <- "4/26/2016"
date_underscore <- gsub(report_date, pattern = "/", replace = "_")
ir <- fread(list.files(file.path(baseWD, dataWD, date_underscore),
                 pattern = "IR_details 2076 fy 2016 q2.csv",
                 full.names = TRUE))
ir[, qtr := my_qtr(`Discovery Date`)]
med_cat <- grep(x = ir[, unique(Classification)],
                pattern = "dose|med$|meds$|pharmacy",
                ignore.case = TRUE, value = TRUE)
setkey(ir, Classification)
ir_red <- ir[J(med_cat), .SD,
             .SDcols = c("Classification", "Case no", "qtr")]
ir_summary <- ir_red[, list(num_consumers = length(`Case no`)),
              by = list(Classification, qtr)]

new_lvls <- ir_summary[qtr == max(qtr)][order(-num_consumers), Classification]
ir_summary[, Classification :=  factor(Classification, levels = new_lvls)]


ir_p <- ggplot(data = ir_summary, aes(x = Classification,
          ymax = 1.2*num_consumers, y = num_consumers, fill = qtr)) +
  geom_bar(stat = "identity", position = position_dodge(0.5),
           width = 0.5, color = "black", size = 0.2) +
  my_theme +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(colour = "grey30",size = 0.2),
    panel.grid.minor = element_line(colour = "grey80",
                                    size = .2),
    axis.text.x = element_text(angle = 45, vjust = 0.7),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  labs(title = "Medication Incidents by Quarter\nFiscal Year 2016") +
  scale_fill_manual(values = brewer.pal(4, "Set1"), name = "") +
  geom_text(data = ir_summary, position = position_dodge(0.5),
    vjust = -0.5, size = 2.5, aes(x = Classification, y = num_consumers,
        fill = qtr, label = num_consumers))

mydoc <- ReporteRs::docx(title = "Medication Incidents By Quarter")
mydoc <- addPlot( mydoc, fun = print, x = ir_p, width = 5.5, height = 3.5 )
writeDoc( mydoc, file = file.path(baseWD, resultsWD, date_underscore,
                                  paste("Med_IRs_Qtr_FY16 Q2.docx")))