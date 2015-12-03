#### initializing working directory and input parameters ####
# clear RAM
rm(list = ls())
# which computer results in correct base working directory
myDirectory = function() {
  switch(
    Sys.info()["nodename"],
    "CMHJ3DTVZ1-LT" = "C:/Users/dalrymplej",
    "JAMES" = "B:", # laptop,
    "DESKTOP-45K7RRN" = "B:",
    "JAMES-PC" = "D:", # homePC
    "WSHSQLGP" = "C:/Users/dalrymplej",
    Sys.info()["nodename"]
  )
}
baseWD <- myDirectory()

# read in source file - personal library
source(file.path(baseWD,
                 "Dropbox/WCCMH/R/begin script R code.r"))

# working directories
dataWD <- "Dropbox/PI Projects/Hospitalizations/Data"
resultsWD <- "Dropbox/PI Projects/Hospitalizations/Results"
codeWD <- "Dropbox/PI Projects/Hospitalizations/R Code"

#### load data and auxillary file ####
files_qtr <-
  list.files(file.path(baseWD, dataWD, "served qtr"), full.names = FALSE)
files_fy  <-
  list.files(file.path(baseWD, dataWD, "served fy"), full.names = FALSE)

  # load consumers served by fiscal year
  fy_served <- NULL
  for(i in seq_along(files_fy)) {
    tmp_served <- read.dtable(file.path(baseWD, dataWD, "served fy", files_fy[i]))
    tmp_served[, fy := substr(files_fy[i], 25, 28)]
    fy_served <- rbindlist(list(fy_served, tmp_served))
  }

  # load consumers served by fiscal quarter
  qtr_served <- tmp_served <- NULL
  for(i in seq_along(files_qtr)) {
    tmp_served <- read.dtable(file.path(baseWD, dataWD, "served qtr", files_qtr[i]))
    tmp_served[, qtr := substr(files_qtr[i], 25, 31)]
    qtr_served <- rbindlist(list(qtr_served, tmp_served))
  }
  rm(tmp_served)

  # load hospital data
  hosp <- read.dtable(file.path(baseWD, dataWD, "E2 hosp report 2152 10_1_2010 to 8_24_2015.csv"))

#### load auxillary file ####
  source(file.path(baseWD, codeWD, "comm_hosp_auxillary.r"))

#### manipulate data ####
  ### consumers served qtr data ###
  setnames(qtr_served, old=colnames(qtr_served), new=tolower(colnames(qtr_served)))
  qtr_served[, team := my_team(team)]
  qtr_con <- qtr_served[, list(con_served = length(unique(case_no))), by=list(qtr, team)]

  ### consumers served fy data ###
  setnames(fy_served, old=colnames(fy_served), new=tolower(colnames(fy_served)))
  fy_served[, team := my_team(team)]
  fy_con <- fy_served[, list(con_served = length(unique(case_no))), by=list(fy, team)]

  ### hospital data ###
    # change column names automatically
    setnames(hosp, old=colnames(hosp), new=gsub(x=tolower(colnames(hosp)), pattern="[0-9]", replace="") )
    setnames(hosp, old="team_at_admit", new="team")
    # keep only wanted columns
    # keepCols <- c("case_no", "auth_eff", "auth_exp", "hosp_disc", "team")
    # hosp[, setdiff(colnames(hosp), keepCols) := NULL]
    # make hosp start date as Date class
    hosp[, auth_eff := dateConvert(auth_eff)]
    # restrict hospital records to admissions during the date range
    hosp <- hosp[auth_eff >= dateConvert("10/1/2010")] # we dont want any from FY2010
    # fix teams
    hosp[, team := my_team(team)]
    # make admission ID used to count the number of hospital admissions
    hosp[, adm_ID := .GRP, by=list(case_no, auth_eff)]
    # make fiscal year column
    hosp[, fy := my_fy(auth_eff)]
    # make quarter column
    hosp[, qtr := my_qtr(auth_eff)]
    hosp <- hosp[team %in% cmh_teams]

#### aggregation results ####

  # fiscal year summary
  fy_adm <- hosp[, list(con_hosp = length(unique(case_no)),
              admissions = length(unique(adm_ID))), keyby=list(fy, team)]
  fy_adm[, multiple_adm := admissions - con_hosp]
  fy_rates <- merge(fy_adm, fy_con, by=c("fy", "team"), all=TRUE)
  fy_rates[, pct := round(con_hosp/con_served*100, 1)]
  fy_rates <- fy_rates[team %in% cmh_teams]

  # fiscal quarter summary
  qtr_adm <- hosp[, list(con_hosp = length(unique(case_no)),
              admissions = length(unique(adm_ID))), keyby=list(qtr, team)]
  qtr_adm[, multiple_adm := admissions - con_hosp]
  qtr_rates <- merge(qtr_adm, qtr_con, by=c("qtr", "team"), all=TRUE)
  qtr_rates[, pct := round(con_hosp/con_served*100, 1)]
  qtr_rates <- qtr_rates[team %in% cmh_teams]
  # filter out unfinished quarters
  qtr_rates <- qtr_rates[!is.na(con_served)]

#### results ####
  # fiscal year ggplot: version 1
  p_fy_v1 <- ggplot(data=fy_adm, aes(x=fy, y=admissions, fill=team) )+
    geom_bar(stat="identity", position=position_dodge(0.5), width=0.5, color="black")+
    my_theme+
    theme(legend.position="top",
          axis.title.x = element_text(color="grey30"),
          axis.title.y = element_text(color="grey30"),
          plot.title = element_text(size=12))+
    coord_flip()+
    labs(title="CSTS Community Hospital Admissions", x="number of admissions")+
    geom_text(data=fy_adm, hjust= -.1, vjust= 0.5 , stat="identity", position = position_dodge(width=.5),
              aes( label = admissions, x = fy, fill= team, y = admissions, angle=0 ),
              size = 2.5, color="black")
  # ggsave(filename=file.path(baseWD, resultsWD, "faceted_team_by_fy_v1.pdf"),
  #        plot=p_fy_v1, width=7, height=7, units="in")

  # fiscal year ggplot: version 2
  p_fy_v2 <- ggplot(data=fy_adm, aes(x=fy, y=admissions, fill=team, ymax=1.2*admissions) )+
    geom_bar(stat="identity", position=position_dodge(0.5), width=0.5, color="black")+
    my_theme+
    theme(axis.title.x = element_text(color="grey30"),
          axis.title.y = element_text(color="grey30"),
          plot.title = element_text(size=12))+
    facet_wrap(~team, scale="free_y")+
    labs(title="CSTS Community Hospital Admissions", y="number of admissions", x="fiscal year")+
    geom_text(data=fy_adm, hjust= 0.5, vjust= -0.5 , stat="identity", position = position_dodge(width=.5),
              aes( label = admissions, x = fy, fill= team, y = admissions, angle=0 ),
              size = 2.5, color="black")
  ggsave(filename=file.path(baseWD, resultsWD, "faceted_CMH_w_NonCMH_by_fy.pdf"),
         plot=p_fy_v2, width=7, height=5, units="in")

  # fiscal year ggplot: version 3
  p_fy_v3 <- ggplot(data=fy_adm[team!="Non-CMH"],
                    aes(x=fy, y=admissions, fill=team, ymax=1.2*admissions) )+
    geom_bar(stat="identity", position=position_dodge(0.5), width=0.5, color="black")+
    my_theme+
    theme(axis.title.x = element_text(color="grey30"),
          axis.title.y = element_text(color="grey30"),
          plot.title = element_text(size=12))+
    facet_wrap(~team, scale="free_y")+
    labs(title="CSTS Community Hospital Admissions", y="number of admissions", x="fiscal year")+
    geom_text(data=fy_adm[team!="Non-CMH"], hjust= 0.5, vjust= -0.5 , stat="identity", position = position_dodge(width=.5),
              aes( label = admissions, x = fy, fill= team, y = admissions, angle=0 ),
              size = 2.5, color="black")
  ggsave(filename=file.path(baseWD, resultsWD, "faceted_CMH_by_fy.pdf"),
         plot=p_fy_v3, width=8, height=4, units="in")

  ### percentage served in the hospital ###
    # fiscal year ggplot: version 3
    p_fy_rates <- ggplot(data=fy_rates,
                      aes(x=fy, y=pct, fill=team, ymax=1.2*pct) )+
      geom_bar(stat="identity", position=position_dodge(0.5),
               width=0.5, color="black", fill="lightblue")+
      my_theme+
      theme(axis.title.x = element_text(color="grey30"),
            axis.title.y = element_text(color="grey30"),
            axis.text.x = element_text(angle=90, vjust=0.5),
            plot.title = element_text(size=12))+
      facet_wrap(~team, scale="free_y")+
      labs(title="CSTS Community Hospitalization Rates", y="percent hospitalized", x="fiscal year")+
      geom_text(data=fy_rates, hjust= 0.5, vjust= -0.5 , stat="identity",
                position = position_dodge(width=0.5),
                aes(label = paste0(pct, "%"), x = fy, y = pct, fill=team), angle=0,
                size = 2.5, color="black")
    ggsave(filename=file.path(baseWD, resultsWD, "faceted_CMH_rates_fy.pdf"),
           plot=p_fy_rates, width=6, height=4, units="in")

#### save results ####
  ### create information about the file to share with end-users ###
  aboutFile = data.table(
    Report_Date = report_date,
    Last_Updated = as.character(Sys.time()),
    Data_Sources = c(list.files(file.path(baseWD, dataWD), pattern="[.]"),
                                files_fy, files_qtr))

#### create workbook ####
  wb = createWorkbook()
  # bold option and underline
  cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()
### create hospital admissions by fiscal year ###
  fy_sheet = createSheet(wb, sheetName="fiscal_year")
  addDataFrame(x=fy_adm, sheet=fy_sheet, showNA=FALSE,
               row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
### create hospital rates by fiscal year ###
  fy_rates_sheet = createSheet(wb, sheetName="fy_rates")
  addDataFrame(x=fy_rates, sheet=fy_rates_sheet, showNA=FALSE,
               row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)

### create hospital admissions by fiscal quarter ###
  qtr_sheet = createSheet(wb, sheetName="fiscal_qtr")
  addDataFrame(x=qtr_adm, sheet=qtr_sheet, showNA=FALSE,
               row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
### create hospital rates by fiscal year ###
  qtr_rates_sheet = createSheet(wb, sheetName="qtr_rates")
  addDataFrame(x=qtr_rates, sheet=qtr_rates_sheet, showNA=FALSE,
               row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)


### create sheet data information ###
  sheet_info = createSheet(wb, sheetName="Data Info")
  # add data information
  addDataFrame(x=aboutFile, sheet=sheet_info, showNA=FALSE, row.names=FALSE, col.names=TRUE, startRow=1,
               startColumn = 1, colnamesStyle=cs3)

### save workbook ###
saveWorkbook(wb=wb, file=file.path(baseWD, resultsWD, # result_folder,
                                   paste0("hosp_fy_results_",
                                          report_date,
                                          ".xlsx")))
