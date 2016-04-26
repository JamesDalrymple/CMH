library(wccmh)
scrub()
baseWD <- switch(Sys.info()["nodename"],
                 WSHSQLGP = "C:/Users/dalrymplej")
proj_wd <- list(
  git_base = file.path(baseWD, "Documents/GitHub/CMH/Medications"),
  dropbox = file.path(baseWD, "Dropbox/Medications/Results")
) ; rm(baseWD)
proj_wd$data <- file.path(proj_wd$git_base, "Data")
proj_wd$code <- file.path(proj_wd$git_base, "Code")
proj_wd$cur_res <- file.path(proj_wd$dropbox, "Current")
proj_wd$miss_res <- file.path(proj_wd$dropbox, "Missed")
proj_wd$inc_res <- file.path(proj_wd$dropbox, "Incidents")

input <-list(
  report_dt = "1/8/2016",
  start_dt = "10/1/2015",
  end_dt = "3/31/2015"
  )
input$run_dt <- gsub(input$report_dt, pattern = "/", replace = "_")

source(file.path(proj_wd$code, "0 med auxillary.R"))
source(file.path(proj_wd$code, "1 med sql.R"))
source(file.path(proj_wd$code, "2 med prep.R"))


  currentMeds <- fread(file.path(baseWD, dataWD,
    date_underscore, paste0("E2 report 2158 ran ", date_underscore, ".csv")))
  openCMH <- fread(file.path(baseWD, dataWD,
    date_underscore, paste0("E2 report 2181 sheet1 ", date_underscore, ".csv")))


p_benzo <- ggplot(data=agg_Provider_Benzo, aes(x=Provider, y=num_Benzo_prescribed, ymax=1.1*num_Benzo_prescribed))+
  geom_bar(stat="identity", fill="lightblue", color="black", width=1)+
  my_theme+theme(axis.title.x = element_text(colour = "grey30"),
                 axis.title.y = element_text(colour = "grey30") )+
  coord_flip()+
  labs(title="Number of Benzos Currently Prescribed", x="staff", y="number of prescriptions")+
  geom_text(data=agg_Provider_Benzo, aes(x=Provider, y=num_Benzo_prescribed, label=num_Benzo_prescribed), hjust=-.1, size=3)
  ggsave(filename="benzo barplot.png", plot=p_benzo, path=file.path(baseWD, resultsWD), width=5, height=5, units="in")

p_stim <- ggplot(data=agg_Provider_Stim, aes(x=Provider, y=num_Stimulants_prescribed, ymax=1.1*num_Stimulants_prescribed))+
  geom_bar(stat="identity", fill="lightblue", color="black", width=1)+
  my_theme+theme(axis.title.x = element_text(colour = "grey30"),
                 axis.title.y = element_text(colour = "grey30") )+
  coord_flip()+
  labs(title="Number of Stimulants Currently Prescribed", x="staff", y="number of prescriptions")+
  geom_text(data=agg_Provider_Stim, aes(x=Provider, y=num_Stimulants_prescribed, label=num_Stimulants_prescribed), hjust=-.1, size=3)
  ggsave(filename="stimulants barplot.png", plot=p_stim, path=file.path(baseWD, resultsWD), width=5, height=4, units="in")

p_benzo_stim <- ggplot(data=agg_benzo_stim, aes(x=Provider, y=num_benzo_stim_prescribed, ymax=1.1*num_benzo_stim_prescribed))+
  geom_bar(stat="identity", fill="lightblue", color="black", width=1)+
  my_theme+theme(axis.title.x = element_text(colour = "grey30"),
                 axis.title.y = element_text(colour = "grey30") )+
  coord_flip()+
  labs(title="Number of Benzos and Stimulants Currently Prescribed", x="staff", y="number of prescriptions")+
  geom_text(data=agg_benzo_stim, aes(x=Provider, y=num_benzo_stim_prescribed, label=num_benzo_stim_prescribed), hjust=-.1, size=3)
  ggsave(filename="benzo_stim barplot.png", plot=p_benzo_stim, path=file.path(baseWD, resultsWD), width=5, height=3, units="in")

## combine aggregated results
  # sleeping AAPs per Pat Cowan on 6/19/2014
  results = data.table(group = c("Benzos", "Stimulants", "Benzo & Stimulants",
                                 "antipysch 2 or more", "antidepress 2 or more"),
             consumers = c(length(benzoCons), length(stimCons), numBenzoStim,
                           length(antiPyschCon), length(antidepressCon)),
             CSTSconsumers = numCon,
             pct = c(pctBenzo, pctStims, pctbenzoStim, pctAntiPych, pctAntidepress))
  results[, pct := round(pct*100,2) ]

#### save results ####
  ### create information about the file to share with end-users ###
  aboutFile = data.table(Report_Date = report_date,
                         Last_Updated = as.character(Sys.time()),
                         Data_Source_1 = c("E2 report 2158"),
                         Data_Source_2 = c("E2 report 2181"),
                         id=1)
  aboutFile = melt(aboutFile, id="id", variables=colnames(aboutFile)[-1])
  aboutFile = data.table(aboutFile)
  aboutFile[, id := NULL]
  rownames(aboutFile) = aboutFile[, variable]
  aboutFile[, variable := NULL]

  #### create workbook ####
    wb = createWorkbook()
    # bold option and underline
    cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()
  ### create familyDrugs_sheet ###
    med_sheet = createSheet(wb, sheetName="classed drugs")
    # add familyDrugs to med_sheet
    addDataFrame(x=currentMeds, sheet=med_sheet, showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
                 colnamesStyle=cs3)
  ### create aggregated data ###
    aggregate_sheet = createSheet(wb, sheetName="aggregated")
    # add CSTS total results to aggregate_sheet
    addDataFrame(x=results, sheet=aggregate_sheet, showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
                 colnamesStyle=cs3)
    # add benzos only to aggregate_sheet
    addDataFrame(x=agg_Provider_Benzo, sheet=aggregate_sheet, showNA=FALSE, row.names = FALSE, startRow=nrow(results)+3, startColumn=1,
                 colnamesStyle=cs3)
    # add stimulants only to aggregate_sheet
    addDataFrame(x=agg_Provider_Stim, sheet=aggregate_sheet, showNA=FALSE, row.names = FALSE, startRow=nrow(agg_Provider_Benzo)+nrow(results)+5, startColumn=1,
                 colnamesStyle=cs3)

  ### create benzo_sheet ###
    benzo_sheet = createSheet(wb, sheetName="benzo meds")
    # add benzoList to atypical_sheet
    addDataFrame(x=data.table(benzos = benzoList), sheet=benzo_sheet,
                 showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
                 colnamesStyle=cs3)
  ### create stim_sheet ###
    stim_sheet = createSheet(wb, sheetName="stimulant meds")
    # add stimList to stim_sheet
    addDataFrame(x=data.table(stimulants = stimList), sheet=stim_sheet,
                 showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
                 colnamesStyle=cs3)
  ### create antiPys_sheet ###
    antiPys_sheet = createSheet(wb, sheetName="anti-psych meds")
    # add stimList to stim_sheet
    addDataFrame(x=data.table(anti_pyschotic = antiPysList), sheet=antiPys_sheet,
                 showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
                 colnamesStyle=cs3)
  ### create antiPys_sheet ###
    antidepress_sheet = createSheet(wb, sheetName="anti-depress meds")
    # add stimList to stim_sheet
    addDataFrame(x=data.table(anti_depress = antidepressList), sheet=antidepress_sheet,
                 showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
                 colnamesStyle=cs3)
  ### create atypical_sheet ### - sleeping this code per Pat Cowan 6/19/2014
  #  atypical_sheet = createSheet(wb, sheetName="atypical meds")
  #  # add atypicalList to atypical_sheet
  #  addDataFrame(x=atypicalList, sheet=atypical_sheet, showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
  #               colnamesStyle=cs3)

  ### create cstsBenzo_sheet ###
    cstsBenzo_sheet = createSheet(wb, sheetName="CSTS Benzos")
    # add cstsBenzos to cstsBenzo_sheet
    addDataFrame(x=cstsBenzos, sheet=cstsBenzo_sheet,
                 showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
                 colnamesStyle=cs3)
  ### create cstsStim_sheet ###
    cstsStim_sheet = createSheet(wb, sheetName="CSTS Stimulants")
    # add cstsStim to cstsStim_sheet
    addDataFrame(x=cstsStim, sheet=cstsStim_sheet,
                 showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
                 colnamesStyle=cs3)
  ### create cstsAntipsych_sheet ###
    cstsAntipsych_sheet = createSheet(wb, sheetName="CSTS Antipsych")
    # add cstsAntipsych to cstsAntipsych_sheet
    addDataFrame(x=cstsAntipsych, sheet=cstsAntipsych_sheet,
                 showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
                 colnamesStyle=cs3)
  ### create cstsAntipsych_sheet ###
  cstsAntidepress_sheet = createSheet(wb, sheetName="CSTS Antidepress")
  # add cstsAntidepress to cstsAntidepress_sheet
  addDataFrame(x=cstsAntidepress, sheet=cstsAntidepress_sheet,
               showNA=FALSE, row.names = FALSE, startRow=1, startColumn=1,
               colnamesStyle=cs3)
  ### create sheet data information ###
    sheet_info = createSheet(wb, sheetName="data info")
    # add data information
    addDataFrame(x=aboutFile, sheet=sheet_info, showNA=FALSE, row.names=TRUE, col.names=FALSE, startRow=1,
                 startColumn = 1, rownamesStyle=cs3)
  ### save workbook ###
  saveWorkbook(wb=wb, file=file.path(baseWD, resultsWD,
    paste("medicines ", gsub(report_date, pattern="/", replace="_"), ".xlsx", sep="")))
