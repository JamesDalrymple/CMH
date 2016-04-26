#### Goal ####
  # Percentage of CSTS consumers prescribed:
  #   1. Benzodiazepines
  #   2. Stimulants
  #   3. Combination of benzodiazepines and stimulants
  #   4. Combination of 2 atypical antipsychotic medications

#### initializing working directory and input parameters ####
  # clear RAM
  rm(list = ls())

  # which computer results in correct base working directory
  baseWD <- switch(Sys.info()["nodename"],
           "DESKTOP-45K7RRN" = "B:",
           "JAMES-PC" = "D:",
           "CMHJ3DTVZ1-LT" =, WSHSQLGP = "C:/Users/dalrymplej")
  # read in source file - personal library
  source(file.path(baseWD,
    "Dropbox/WCCMH/R/begin script R code.r"))
  # setup for working directories - data and results
  dataWD <- "Dropbox/Medications/Data"
  resultsWD <- "Dropbox/Medications/Results"
  codeWD <- "Dropbox/Medications/R Code"

#### load auxillary file ####
  library(wccmh)
  source(file.path(baseWD, codeWD, "med auxillary.r"))
#### load data ####
  report_date <- "1/8/2016"
  date_underscore <- gsub(report_date, pattern = "/", replace = "_")

  currentMeds <- fread(file.path(baseWD, dataWD,
    date_underscore, paste0("E2 report 2158 ran ", date_underscore, ".csv")))
  openCMH <- fread(file.path(baseWD, dataWD,
    date_underscore, paste0("E2 report 2181 sheet1 ", date_underscore, ".csv")))

#### manipulate data ####
  # force all medicines to be lowercase
  currentMeds[, Drug := tolower(Drug)]
  ### currentMeds ###
    setkey(currentMeds, Drug)
    currentMeds = currentMeds[!J("")]
    invisible(setkey(currentMeds, NULL))
    # remove client name from current meds (if it is still there)
    invisible(suppressWarnings(currentMeds[, Client_Name := NULL]))
    # make a list of medicines that are 'typical family doctor medicines' and remove them
    allMeds = currentMeds[, unique(Drug)]

    ### create benzo column
    currentBenzo = mySearch(x = allMeds, pattern = benzoList)
    # classify current benzo's as benzo
    setkey(currentMeds, Drug)
    currentMeds[J(currentBenzo), benzo := "Y"]
    setkey(currentMeds, NULL)
    ### create stimulant column
    currentStim = mySearch(x = allMeds, pattern = stimList)
    # classify current stimulant's as stimulant
    setkey(currentMeds, Drug)
    currentMeds[J(currentStim), stimulant := "Y"]
    invisible(setkey(currentMeds, NULL))
    ### create atypical antipsychotic column
    aapDT = aTypical(data = allMeds)
    setkey(currentMeds, Drug); setkey(aapDT, Drug)
    currentMeds = aapDT[currentMeds, nomatch = NA]
    ### create antipsychotic column
    currentAntiPysch = mySearch(x = allMeds, pattern = antiPysList)
    setkey(currentMeds, Drug)
    currentMeds[J(currentAntiPysch), antiPysch := "Y"]
    invisible(setkey(currentMeds, NULL))
    ### create antidepressant column
    currentAntidepress = mySearch(x = allMeds, pattern = antidepressList)
    setkey(currentMeds, Drug)
    currentMeds[J(currentAntidepress), antidepress := "Y"]
    invisible(setkey(currentMeds, NULL))

  ### open consumers ###
    # change names automatically
    setnames(openCMH, old = colnames(openCMH), new = tolower(colnames(openCMH)))
    # keep only wanted columns
    keepCols = c("case_no", "cmh_effdt", "cmh_expdt")
    openCMH[, setdiff(colnames(openCMH), keepCols) := NULL]
    # remove duplicates
    openCMH = unique(openCMH)
    # number of consumers open 5/29/2014
    numCon = openCMH[, length(unique(case_no))]

#### aggregate results ####
  ## number/pct of benzos
  setkey(currentMeds, benzo)
  benzoCons = currentMeds[J("Y")][, unique(Case_no)]
  pctBenzo = length(benzoCons)/numCon
  cstsBenzos = currentMeds[J("Y"), list(number_prescribed = .N), by = Drug]
  ## number/pct of stimulants
  setkey(currentMeds, stimulant)
  stimCons = currentMeds[J("Y")][, unique(Case_no)]
  pctStims = length(stimCons)/numCon
  cstsStim = currentMeds[J("Y"), list(number_prescribed = .N), by = Drug]

  # number of benzos and stimulants
  numBenzoStim = length(intersect(benzoCons, stimCons))
  pctbenzoStim = numBenzoStim/numCon
  ## number of consumers with 2 or more AAPs
  aapCon = currentMeds[, list(AAPs = length_noNA(AAPclass)),
                       by = list(Case_no)][AAPs > 1, unique(Case_no)]
  pctAAPcon = length(aapCon)/numCon
  # numer of consumers with 2 or more antipyschotics
  antiPyschCon = currentMeds[, list(antiPysch = length_noNA(antiPysch)),
                             by = list(Case_no)][antiPysch > 1, unique(Case_no)]
  pctAntiPych = length(antiPyschCon)/numCon
  cstsAntipsych = currentMeds[antiPysch == "Y", list(number_prescribed = .N), by=Drug]

  # number of consumers with 2 more more antidepressants
  antidepressCon = currentMeds[antidepress == "Y", list(antidepress = .N),
                               by = list(Case_no)][antidepress > 1, unique(Case_no)]
  pctAntidepress = length(antidepressCon)/numCon
  cstsAntidepress = currentMeds[antidepress == "Y", list(number_prescribed = .N), by=Drug]

  ### summary by prescriber ###
    ### number of benzo's per prescriber ###
      agg_Provider_Benzo <- currentMeds[Provider!="" & (benzo=="Y"), list(num_Benzo_prescribed = length(Case_no)), by=list(Provider)]
      # factor levels
      agg_Provider_Benzo[, Provider := factor(Provider, levels=agg_Provider_Benzo[order(num_Benzo_prescribed), Provider] )]
      # order dataset
      agg_Provider_Benzo <- agg_Provider_Benzo[order(-num_Benzo_prescribed)]
    ### number of stimulants per prescriber ###
      agg_Provider_Stim <- currentMeds[Provider!="" & (stimulant=="Y"), list(num_Stimulants_prescribed = length(Case_no)), by=list(Provider)]
      # factor levels
      agg_Provider_Stim[, Provider := factor(Provider, levels=agg_Provider_Stim[order(num_Stimulants_prescribed), Provider] )]
      # order dataset
      agg_Provider_Stim <- agg_Provider_Stim[order(-num_Stimulants_prescribed)]
    ### number of benzo's and stimulants per prescriber ###
      agg_benzo_stim <- setkey(currentMeds, Case_no)[J(intersect(benzoCons, stimCons)),  list(num_benzo_stim_prescribed = length(Case_no)), by=list(Provider)]
      agg_benzo_stim <- agg_benzo_stim[Provider!=""]
      # factor levels
      agg_benzo_stim[, Provider := factor(Provider, levels=agg_benzo_stim[order(num_benzo_stim_prescribed), Provider] )]
      # order dataset
      agg_benzo_stim <- agg_benzo_stim[order(-num_benzo_stim_prescribed)]

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
