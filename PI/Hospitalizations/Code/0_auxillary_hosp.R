### fund only auxillary ###
pkg_loader(packages = c("gdata", "data.table", "zoo", "xlsx", "RODBC", "sqldf"))

aux <- new.env(parent = .GlobalEnv)

aux$fy_combn <- function(start_date, end_date) {
  qtrs <- seq(from = as.yearqtr(date_convert(start_date))+.25,
      to = as.yearqtr(date_convert(end_date))+.25,
      by = 0.25
      )
  dt_qtrs <- data.table(span_label = qtrs)
  dt_qtrs[, span_start := as.Date(qtrs-.25, frac = 0)]
  dt_qtrs[, span_end := as.Date(qtrs-.25, frac = 1)]
  dt_qtrs[, span_type := "qtr"]
  fys <- unique(my_fy(as.Date(qtrs-.25, frac = 0)))
  dt_fys <- copy(dt_qtrs)
  dt_fys[, span_label := my_fy(span_start)]
  dt_fys[, span_start := min(span_start), by = span_label]
  dt_fys[, span_end := max(span_end), by = span_label]
  dt_fys[, span_type := "fy"]
  dt_fys <- unique(dt_fys)
  dt_qtrs[, span_label := as.chr(span_label)]
  dt_combn <- rbindlist(list(dt_qtrs, dt_fys), use.names = TRUE)
  return(dt_combn)
}

project_wd$results <- file.path(project_wd$results, input$report_date)
if (!dir.exists(project_wd$data)) {
  dir.create(project_wd$data, recursive = TRUE)
}


# cmh vs non-cmh classification
aux$is_cmh <- function(x) {
  recode_string(x,
                recode_key = list(
                  CMH = c("MI", "DD", "Child", "Child HB",
                          "ACT", "UM"),
                  Access = "Access",
                  `non-CMH` = c(NA, "unknown", "non-CMH", "OBRA", "PORT")
                ))
}

aux$recode_cmh_key <- list(
  ACT = c("ACT", "WSH - ACT"),
  DD = c("DD", "WSH - DD Adult",
         "DD Adult"),
  MI = c("WSH - MI - Adult", "WSH - ATO", "MI", "MI Adult"),
  Child = c("Child", "WSH - Children's Services", "Children's Services"),
  `Child HB` = c("WSH - Children's Services - Home Based", "Child HB",
                 "Home Based"),
  PORT = "WSH - PATH/PORT",
  Access = c("Community Support and Treatment Services - CSTS", "CSTS",
    "WSH - Access/Engagement", "Access/Engagement", "Access",
    "Washtenaw County Community Mental Health"),
  OBRA = c("WSH - OBRA", "OBRA"),
  UM = c("WSH - Utilization Management",
         "UM"),
  `non-CMH` = c(
    "non-CMH", "Non-CMH", "WSH - MH Court", "WSH - ICSS team",
    "WSH - Sobriety Court", "Crisis Residential Services", "PORT", "PATH/PORT",
    "WSH - PATH/PORT", "WSH - PORT", "WSH - PATH", "PATH")
  )
aux$cmh_recode <-
  function(x, missing_key = "non-CMH") {
    if (class(x) == "factor") x <- as.chr(x)
    if (any(is.na(x))) x[is.na(x)] <- missing_key
    recode_key <- aux$recode_cmh_key
    unknown <- setdiff(x, unlist(recode_key, use.names = FALSE))
    recode_key$unknown <- unknown
    recode_string(x = x, recode_key = recode_key)
  }
# aux$cmh_recode(x = c("PORT", "Access"))