### fund only auxillary ###
pkg_loader(packages = c("gdata", "data.table", "zoo", "xlsx", "RODBC"))

aux <- new.env(parent = .GlobalEnv)

aux$fy_combn <- function(start_date, end_date) {
  qtrs <- seq(from = as.yearqtr(date_convert(start_date)),
      to = as.yearqtr(date_convert(end_date)),
      by = 0.25
      )
  dt_qtrs <- data.table(span_label = qtrs)
  dt_qtrs[, span_start := as.Date(qtrs, frac = 0)]
  dt_qtrs[, span_end := as.Date(qtrs, frac = 1)]
  dt_qtrs[, span_type := "qtr"]
  fys <- unique(my_fy(as.Date(qtrs)))
  dt_fys <- copy(dt_qtrs)
  dt_fys[, span_label := my_fy(span_start)]
  dt_fys[, span_start := min(span_start), by = span_label]
  dt_fys[, span_end := min(span_end), by = span_label]
  dt_fys[, span_type := "fy"]
  dt_fys <- unique(dt_fys)
  dt_qtrs[, span_label := as.chr(span_label)]
  dt_combn <- rbindlist(list(dt_qtrs, dt_fys), use.names = TRUE)
  return(dt_combn)
}

project_wd$results <- file.path(project_wd$results, input$report_date)

# cmh vs non-cmh classification
is_cmh <- function(x) {
  recode_string(x,
                recode_key = list(
                  cmh = c("MI", "DD", "Child", "Child HB", "ACT"),
                  non_cmh = c(NA, "unknown")
                ))
}




