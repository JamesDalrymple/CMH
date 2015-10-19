# read in all FB files, and move them to archived folder and remove them from
# downloads folder

require(data.table)

# read in all FB files in the download folder ---------------------------------
download_location <- "C:/Users/dalrymplej/Downloads"
move_location <- "G:/CSTS Data Analyst Archives/FB_archives"

d00_list <- list.files(download_location, pattern="D00",
           full.names = TRUE)

run_date <- format(Sys.Date(), "%m_%d_%y")

keep_cols <- c(
  "CASE #",
  "PRI PROCEDURE CODE",
  "UNIT TYPE",
  "FROM DATE",
  "THRU DATE",
  "UNITS",
  "ALLOWED AMOUNT")
new_cols <- c("case_no",
              "cpt",
              "unit_type",
              "from_date",
              "thru_date",
              "units",
              "cost")

for(i in seq_along(d00_list)) {
  first_6_rows <- data.table(read.table(
    d00_list[i],
    nrows = 6,
    sep = ",",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

  dt_type <- tolower(gsub(
    x = first_6_rows[Report. == "Direct/Contracted Services:",
                     Funding.Source.Bucket.Report],
    pattern = " Services",
    fixed = TRUE,
    replace = ""
  ))
  dt_date <- unlist(strsplit(x = first_6_rows[Report. == "Date Range:",
                                              Funding.Source.Bucket.Report],
                             split = " - "))
  dt_date <- format(as.Date(dt_date), "%m_%d_%y")
  new_name <- paste("fb", dt_type, paste(dt_date, collapse = "_to_"), "run",
                    run_date)
  # move file in downloads to archived FB folder
  file.copy(from=d00_list[i], to=file.path(move_location,
                                           paste0(new_name, ".csv")))
  new_dt <- fread(d00_list[i],
                  select = keep_cols)
  setnames(new_dt, old=keep_cols, new=new_cols)
  saveRDS(object = new_dt, file = file.path(move_location, "rds",
                                            paste0(new_name, ".rds")))
}