### fund only auxillary ###
pkg_loader(packages = c("data.table", "zoo", "xlsx"))

# convert locus scores to numbers
word_to_num <- function(x) {
  switch(x,
         "Level Zero" = 1,
         "Level One" = 1,
         "Level Two" = 2,
         "Level Three" = 3,
         "Level Four" = 4,
         "Level Five" = 5,
         "Level Six" = 3, # per Kelly B. 5/7/2015
         2)
}
word_to_num <- Vectorize(word_to_num)

# function to convert TCM to levels
levelTCM <- function(x) {
  result <- cut(x, breaks = c(-Inf, 1, 3, 13, Inf),
                labels = c(0, 1, 2, 3), right=FALSE)
  result <- as.numeric(result)
  return(result)
}

# find the priority team for consumers
team.priority <- function(x) { y = switch(x,
                                         "WSH - OBRA" = 1,
                                         "WSH - ACT" = 2,
                                         "WSH - DD Adult" = 3,
                                         "WSH - MI - Adult" = 4,
                                         "WSH - ATO" = 4,
                                         "WSH - Children\'s Services - Home Based" = 5,
                                         "WSH - Children\'s Services" = 5,
                                         "WSH - PATH/PORT" = 6,
                                         "Community Support and Treatment Services - CSTS" = 7,
                                         "WSH - Access/Engagement" = 7,
                                         "Non-CMH" = 9,
                                         10)
                              return(as.numeric(y))
}

# change team name
teamFix <- function(x) {
  y <- switch(x,
             "WSH - OBRA" = "Non-CMH",
             "WSH - Children\'s Services - Home Based" = "Child HB",
             "WSH - Children\'s Services" = "Child",
             "WSH - DD Adult" = "DD",
             "WSH - ACT" = "ACT",
             "WSH - MI - Adult" = "MI",
             "WSH - ATO" = "MI",
             "Community Support and Treatment Services - CSTS" = "Access/Engagement",
             "WSH - Access/Engagement" = "Access/Engagement",
             "Non-CMH" = "Non-CMH",
             "WSH - PATH/PORT" = "PATH/PORT",
             "WSH - ICSS team" = "Non-CMH",
             x)
}
teamFix <- Vectorize(teamFix)

# change fund
fundFix <- function(x) {
  switch(x,
         "Medicaid - State Plan" = "Medicaid",
         "Medicaid - b3" = "Medicaid",
         "Medicaid - Hab Sup Waiver" = "Medicaid",
         "General Fund" = "GF",
         "General Fund - Acute Services" = "GF",
         "Healthy Michigan Plan" = "HMP",
         "Adult Benefit Waiver" = "Adult Benefit Waiver",
         "Child Waiver" = "Child Waiver",
         "MIChild" = "MIChild",
         "Medicaid - Acute Services" = "Medicaid",
         "ABW-Acute Services" = "ABW",
         "HMP-Acute Services" = "HMP",
         "Medicaid - Partial Services" = "Medicaid",
         "General Fund - Partial Services" = "GF",
         "unknown fund"
         )
}
fundFix <- Vectorize(fundFix)

# narrow down team assignment to one per person per day
teamCMH <- c("WSH - Access/Engagement",
            "Community Support and Treatment Services - CSTS",
            "WSH - ACT",
            "WSH - ATO",
            "WSH - Children's Services",
            "WSH - Children's Services - Home Based",
            "WSH - DD Adult",
            "WSH - MI - Adult")

# check if vector is empty
checkEmpty <- function(x) {
  if(class(x)=="Date") {
    x <- as.character(x)
  }
  result <- (unique(x)=="" | is.na(unique(x)) )
  result <- sum(result)/length(result)
  if(result<1) {result <- "non-empty"} else {result <- "empty"}
  return(result)
}

# # fix a dt with empty columns via checkEmpty and mapply - retired 9/8/2015 per Kelly Bellus
# export_dt <- function(x) {
#   if (nrow(x)>0) {
#     if (length(names(which(mapply(checkEmpty, x)=="empty")))>0) {
#       x[, names(which(mapply(checkEmpty, x)=="empty")) := NULL]
#     }
#   }
#   return(x)
# }

# convert csv to rds
csv_to_rds <- function(csv_file, keep_cols, rename_cols=rename_cols, rm.original) {
  csv_data <- read.dtable(csv_file)
  rds_file <- gsub(x=csv_file, pattern=".csv", replace=".rds")

  if(length(csv_data[, intersect(colnames(csv_data), keep_cols)])!=length(keep_cols)) {
    stop("keep_cols are not in original file! Check original file!")}
  suppressWarnings(csv_data[, setdiff(colnames(csv_data), keep_cols) := NULL])
  setnames(csv_data, colnames(csv_data), rename_cols)
  saveRDS(object=csv_data, file=rds_file)
  if(rm.original) {unlink(x=csv_file); print("original file deleted")}
  result <- readRDS(file=rds_file)
}

# load, save, compress data and delete original
load_data <- function(file_pattern, keep_cols, rename_cols=rename_cols, rm.original=FALSE) {
  data_csv <- grep(x=csv_files, pattern=file_pattern,
                   ignore.case=TRUE, value=TRUE, perl=TRUE)
  data_rds <- grep(x=rds_files, pattern=file_pattern,
                   ignore.case=TRUE, value=TRUE)
  if(length(data_csv)==0 && length(data_rds)==0 ) {stop("No file found with given pattern (csv or rds).")}
  if(length(data_csv) > 1 ) {
    stop(paste("too many csv", file_pattern, "files!", sep=" - "))
  }
  if(length(data_csv)==1) {
    # load data_csv, keep only wanted columns, save as rds file,
    # then load rds file, then delete data_csv file
    result <- csv_to_rds(csv_file=data_csv,
               keep_cols = keep_cols,
               rename_cols = rename_cols,
               rm.original = rm.original)
  }  else if(length(data_rds)==1) {
    # load rds file
    result <- readRDS(file=data_rds)
    if( length(colnames(result)) > length(rename_cols) ) {
      result[, setdiff(colnames(result), rename_cols) := NULL]
      saveRDS(object=result, file=data_rds)
    }
  }
  return(result)
}

# helper function for making file name for export
fund_name <- function(x) {
  x <- paste(tolower(x), collapse="_")
  x <- gsub(x=x, pattern="/", replace="_")
  x <- gsub(x=x, pattern="-", replace="")
  return(x)
}

my_paste <- function(..., collapse="|") {
  L <- c(...)
  L <- L[!is.na(L)]
  L <- paste(L, collapse=collapse)
return(unique(L))
}
my_paste(c(1, 2, NA))
