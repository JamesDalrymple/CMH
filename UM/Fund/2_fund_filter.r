## to do:: remove state hospital consumers from all spreadsheets

# initializing working directory and input parameters -------------------------
rm(list = ls()) # clear RAM
# which computer results in correct base working directory
wd <- list()
wd <- switch (Sys.info()["nodename"],
  "WSHSQLGP" = {# PCE 107
  wd['dropbox_base'] <- "C:/Users/dalrymplej/Dropbox"
  wd['github_base'] <- "C:/Users/dalrymplej/Documents/GitHub"
  },
  "DESKTOP-45K7RRN" = {# county laptop
  wd['dropbox_base'] <- "filler"
  wd['github_base'] <- "filler"
  },
  "JAMES-2" = {# main computer
  wd['dropbox_base'] <- "filler"
  wd['github_base'] <- "filler"
  }
)

  # user input required
  run_date <- "9/22/2015" # for folder
  end_date <- "7/31/2015" # data parameter end
  # keep_funds <- c("GF")
  # keep_funds <- c("spend-down")
  # keep_funds <- c("Medicaid", "HMP")
  # keep_funds <- c("Medicare/TPP")

  # working folders and subfolders
  sub_folder <- gsub(x = run_date, pattern = "/", replace = "_")
  dataWD <- file.path("Dropbox/Utilization Management/Fund Only/Data",
                      sub_folder)
  codeWD <- "Dropbox/Utilization Management/Fund Only/R Code"
  resultsWD <- file.path("Dropbox/Utilization Management/Fund Only/Results",
                         sub_folder)
  rm(sub_folder)

  ## read in source files ##
   # global auxillary file (personal library)
   source(file.path(baseWD, "Dropbox/WCCMH/R/begin script R code.r"))
   # local auxillary file
   source(file.path(baseWD, codeWD, "service array", "service auxillary.r"))
   # base service array
   source(file.path(baseWD, codeWD, "service array", "base_service_array.r"))

#### filtering ####
  ## CSTS open consumers without a fund/insurance layer ##
  # tmp_services[is.na(fund) & is.na(cmh_expdt) & !is.na(cmh_effdt)]
  all_funds <- services[, unique(fund)]



#   data.table(services[fund!="closed_CMH" & fund!="non_CMH"
#            & team!="Non-CMH", table(team)]
#   )
#
  data.table(services[fund != "closed_CMH" & fund != "non_CMH"
                      & team != "Non-CMH",
                      table(fund)])[, sum(N)]
#
#   nrow(services)
#   services[, length(unique(case_no))]

  # no insurance:
  # 10441
  # insurance - (I assume) retroactively got Medicaid:
  # 262589, 260438, 225892, 12098, 12716, 252800
  # 216351, 12810, 1145860, 13827
  # only did not have medicaid June 2015
  # 12186,
  # 207181 - died in June 2015, but they wrote the wrong year... 2016

#   ncol(services)
#   hospitalized <- services[case_no %in%
#     c(14156, 10947, 103106, 219859, 95710, 246762, 1152586,
#       1132623, 1134948, 100015, 12646)]
#   ncol(hospitalized)
#   write.csv(hospitalized, file.path(baseWD, resultsWD, "hospitalized.csv"), row.names=FALSE)

#### create fund xlsx files ####
for(i in seq_along(all_funds)) {
  keep_funds <- all_funds[i]
  tmp_services <- services[fund %in% keep_funds]
  tmp_mi_services <- mi_services[fund %in% keep_funds]
  tmp_yf_services <- yf_services[fund %in% keep_funds]

#### create workbook ####
  wb <- createWorkbook()
  # bold option and underline
  cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()
  ### create DD ###
    dd_sheet <- createSheet(wb, sheetName="DD")
    dd_services <- copy(tmp_services[team=="DD"])
    addDataFrame(x=dd_services, sheet=dd_sheet, showNA=FALSE,
                 row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
  ### Youth and Family ###
    yf_sheet <- createSheet(wb, sheetName="Y&F")
    addDataFrame(x=tmp_yf_services, sheet=yf_sheet, showNA=FALSE,
                 row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
  ### MI Adult ###
    mi_sheet <- createSheet(wb, sheetName="MI")
    addDataFrame(x=tmp_mi_services, sheet=mi_sheet, showNA=FALSE,
                 row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
  ### level 0 - no TCM ###
    l0_sheet <- createSheet(wb, sheetName="MI_L0")
    l0_noTCM <- copy(tmp_mi_services[level=="L0_No_TCM"])
    addDataFrame(x=l0_noTCM, sheet=l0_sheet, showNA=FALSE,
                 row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
  ### level 1 ###
    l1_sheet <- createSheet(wb, sheetName="MI_L1")
    l1_mi <- copy(tmp_mi_services[level=="L1"])
    addDataFrame(x=l1_mi, sheet=l1_sheet, showNA=FALSE,
                 row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
  ### level 2 ###
    l2_sheet <- createSheet(wb, sheetName="MI_L2")
    l2_mi <- copy(tmp_mi_services[level=="L2"])
    addDataFrame(x=l2_mi, sheet=l2_sheet, showNA=FALSE,
                 row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
  ### level 3 ###
    l3_sheet <- createSheet(wb, sheetName="MI_L3")
    l3_mi = copy(tmp_mi_services[level=="L3"])
    addDataFrame(x=l3_mi, sheet=l3_sheet, showNA=FALSE,
                 row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
  ### ACT ###
    act_sheet <- createSheet(wb, sheetName="MI_L4_ACT")
    act_services <- copy(tmp_mi_services[team=="ACT"])
    addDataFrame(x=act_services, sheet=act_sheet, showNA=FALSE,
                 row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
  ### MI Residential ###
    res_sheet <- createSheet(wb, sheetName="MI_L5_Res")
    res_services <- copy(tmp_mi_services[level=="Residential"])
    addDataFrame(x=res_services, sheet=res_sheet, showNA=FALSE,
                 row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
    ### CLS H2015 (CLS | Community) ###
    cls_h2015_sheet <- createSheet(wb, sheetName="CLS H2015")
    # filter out cls community blanks
    cls_h2015_save <- copy(tmp_services)
    if(length(grep(x=colnames(cls_h2015_save), pattern="CLS | Community", fixed=TRUE))==1) {
      setnames(cls_h2015_save, "CLS | Community", "cls_community")
      cls_h2015_save <- cls_h2015_save[!is.na(cls_community)]
      setnames(cls_h2015_save, "cls_community", "CLS | Community")
      addDataFrame(x=cls_h2015_save, sheet=cls_h2015_sheet, showNA=FALSE,
                   row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
    } else {
      addDataFrame(x=services[case_no==-1], sheet=cls_h2015_sheet, showNA=FALSE,
                   row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
    }
    ### CLS H2016 ###
    cls_h2016_sheet <- createSheet(wb, sheetName="CLS H2016")
    # filter out cls h2016
    cls_h2016_save <- copy(tmp_services)
    if(length(grep(x=colnames(cls_h2016_save),
                   pattern= "CLS | Specialized Residential", fixed=TRUE))>0) {
      setnames(cls_h2016_save, "CLS | Specialized Residential", "cls_h2016")
      cls_h2016_save <- cls_h2016_save[!is.na(cls_h2016)]
      setnames(cls_h2016_save, "cls_h2016", "CLS | Specialized Residential")
      addDataFrame(x=cls_h2016_save, sheet=cls_h2016_sheet, showNA=FALSE,
                   row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
    } else {
      addDataFrame(x=services[case_no==-1], sheet=cls_h2016_sheet, showNA=FALSE,
                   row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
    }
    ### CLS T2038 ###
    cls_t2038_sheet <- createSheet(wb, sheetName="CLS T2038")
    # filter out cls t2038
    cls_t2038_save <- copy(tmp_services)

    if(length(grep(x=colnames(cls_t2038_save),
                   pattern="Home Modification | Children/Housing Assistance", fixed=TRUE))>0) {
      setnames(cls_t2038_save, "Home Modification | Children/Housing Assistance", "cls_t2038")
      cls_t2038_save <- cls_t2038_save[!is.na(cls_t2038)]
      setnames(cls_t2038_save, "cls_t2038", "Home Modification | Children/Housing Assistance")
      # add cls_t2038_save to cls_t2038_sheet
      addDataFrame(x=cls_t2038_save, sheet=cls_t2038_sheet, showNA=FALSE,
                   row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
    } else {
      addDataFrame(x=services[case_no==-1], sheet=cls_t2038_sheet, showNA=FALSE,
                   row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
    }
    ### CLS H0043 (CLS | independent living) ###
    cls_h0043_sheet <- createSheet(wb, sheetName="CLS H0043")
    # filter out cls independent living
    cls_h0043_save <- copy(tmp_services)
    if(length(grep(x=colnames(cls_h0043_save),
                   pattern="CLS | independent living", fixed=TRUE))>0) {
      setnames(cls_h0043_save, "CLS | independent living", "cls_ind_living")
      cls_h0043_save <- cls_h0043_save[!is.na(cls_ind_living)]
      setnames(cls_h0043_save, "cls_ind_living", "CLS | independent living")
      addDataFrame(x=cls_h0043_save, sheet=cls_h0043_sheet, showNA=FALSE,
                   row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
    } else {
      addDataFrame(x=services[case_no==-1], sheet=cls_h0043_sheet, showNA=FALSE,
                   row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
    }
  ### create all_fund_sheet ###
  all_fund_sheet <- createSheet(wb,
    sheetName=gsub(x=paste(keep_funds, collapse=""), pattern="/", replace="_") )
  addDataFrame(x=tmp_services, sheet=all_fund_sheet, showNA=FALSE, row.names=FALSE,
               startRow=1, startColumn=1, colnamesStyle=cs3)
  ### create HAB_sheet ###
    if(nrow(tmp_services[fund=="HAB spend-down"])>0 ) {
      hab_sheet <- createSheet(wb, sheetName="HAB spenddown")
      addDataFrame(x=tmp_services[fund=="HAB spend-down"], sheet=hab_sheet, showNA=FALSE, row.names=FALSE,
                   startRow=1, startColumn=1, colnamesStyle=cs3)
    }
  ### create sheet data information ###
    sheet_info = createSheet(wb, sheetName="Data Info")
    addDataFrame(x=aboutFile, sheet=sheet_info, showNA=FALSE,
                 row.names=FALSE, col.names=TRUE, startRow=1,
                 startColumn = 1, colnamesStyle=cs3)

### save workbook ###
  # create result folder
  ### R 3.2.0 approach
  if(!dir.exists(file.path(baseWD, resultsWD))) {
    dir.create(file.path(baseWD, resultsWD))
    paste("folder created", file.path(baseWD, resultsWD), sep="...")
  }

  # file name
  export_file <- paste0(
    fund_name(keep_funds),
    "_service_array_", run_par, ".xlsx")

  gc(reset = TRUE)
  saveWorkbook(wb=wb, file=file.path(baseWD, resultsWD,
                                     export_file))
  print(paste(keep_funds, "completed"))
  rm(wb)
}
