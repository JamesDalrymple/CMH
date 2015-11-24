# read in all FB files, and move them to archived folder and remove them from
# downloads folder
rm(list = ls()) # scrub()
# source local helper file ----------------------------------------------------
source(file.path("~", "Github/CMH/FB storage", "1_FB_auxillary.R"))
# read in all FB files in the download folder ---------------------------------
download_location <- "C:/Users/dalrymplej/Downloads"
move_location <- "G:/CSTS Data Analyst Archives/FB_archives"
d00_list <- list.files(download_location, pattern = "D00",
                       full.names = TRUE)

# fb creation date (same as download date)

for (i in seq_along(d00_list)) {
  # i=1
  cur_dt <- read.fb(d00_list[i]) # current data.table

  create_date <-
    format(as.Date(file.info(d00_list[i])$ctime), "%m_%d_%y")
  attr_names <- names(attributes(cur_dt))
  prov_type <- grep(
    x = attr_names,
    pattern = "contracted|direct",
    ignore.case = TRUE,
    value = TRUE
  )
  prov_type <- switch(
    attr(cur_dt, prov_type),
    "(all)" = "all",
    "Direct Services" = "direct",
    "Contract Services" = "contract",
    p_stop("didnt anticipate this prov_type")
  )
  rate_set <- attr(cur_dt,
                   grep(
                     x = attr_names,
                     pattern = "rate",
                     ignore.case = TRUE,
                     value = TRUE
                   ))
  rate_set <- aux$alter_rates(rate_set)

  date_range <- attr(cur_dt, "Date Range")
  date_range <- rapply(strsplit(date_range, split = " - "),
                       function(x) {
                         format(as.Date(x), "%m_%d_%y")
                       })
  date_range <-
    gsub(x = date_range,
         pattern = "0(\\d)",
         replace = "\\1")
  date_range <- paste(date_range, collapse = "_to_")
  # the new file name we are assigning to cur_dt
  new_name <- paste("fb", rate_set, prov_type, date_range, "ran", create_date)
  saveRDS(object = cur_dt,
          file = file.path(move_location, "rds",
                           paste0(new_name, ".rds")))
  file.remove(d00_list[i])
}