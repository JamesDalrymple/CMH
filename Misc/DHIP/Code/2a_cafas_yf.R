caf <- new.env(parent = .GlobalEnv)

if (is.l1(grep(x = names(cafas), pattern = "ClientPrimaryID", value = TRUE))) {
  setnames(cafas, "ClientPrimaryID", "case_no")
}
# grab caregiver columns and reshape with case_no as primary key --------------
caf$cg_max_col <-
  max(grep(x = names(cafas), pattern = "caregiver", ignore.case = TRUE))
caf$cafas_cg <-
  cafas[, .SD, .SDc = c(1:caf$cg_max_col), with = TRUE]
# remove empty data.table column
caf$rm_cols <- lapply(caf$cafas_cg,
  FUN = function(x) identical(length(x), sum(is.na(x))))
caf$cafas_cg[,
  names(caf$rm_cols[isTRUE(caf$rm_cols)]) := NULL]
caf$cafas_cg <- melt(caf$cafas_cg, id = "case_no")
# retain consumerw with only non-empty values ---
# caf$cafas_cg[value == "Great Aunt"] # 1139767
caf$cafas_cg <- caf$cafas_cg[value != ""]
caf$cafas_cg <- caf$cafas_cg[value == input$fy_value]
# main cafas data set ---------------------------------------------------------
caf$cafas_dates <-
  cafas[, .SD, .SDcols = c(1,
    {caf$cg_max_col+1}:{length(names(cafas))})]

caf$cafas_measures_l <-
  list(
    access_date = grep(x = names(caf$cafas_dates),
      pattern = "assessdate", value = TRUE, ignore.case = TRUE),
    admin_desc = grep(x = names(caf$cafas_dates),
      pattern = "adminDesc", value = TRUE, ignore.case = TRUE),
    total_score = grep(x = names(caf$cafas_dates),
         pattern = "totalscore$", value = TRUE, ignore.case = TRUE),
    total_score_diff = grep(x = names(caf$cafas_dates),
         pattern = "totalscorediff$", value = TRUE, ignore.case = TRUE)
  )
# change non-character classes to character to help melt combine 'like' cols.
for (j in seq_along(names(caf$cafas_dates)[-1])) {
  set(caf$cafas_dates, j = j,
      value = as.chr(caf$cafas_dates[[j]]))
}
# I get a warning message if I join NA cols to char columns
caf$cafas_dates[is.na(caf$cafas_dates)] <- "" # avoids warn msg
caf$cafas_dates <-
  melt(data = caf$cafas_dates, id.vars = c("case_no"),
       measure.vars = caf$cafas_measures_l,
       variable.factor = FALSE,
       value.name = c("assess_dt", "desc", "total_score", "total_diff"))
# column class correction
caf$cafas_dates[, variable := NULL]
caf$cafas_dates[caf$cafas_dates==""] <- NA # undoes a prev. line
caf$cafas_dates[, total_score := as.num(total_score)]
caf$cafas_dates[, total_diff := as.num(total_diff)]
caf$cafas_dates <- caf$cafas_dates[!is.na(assess_dt)]
caf$cafas_dates[, assess_dt := date_convert(assess_dt)]

caf$cafas_dates[, max_assess_dt := max(assess_dt), by = case_no]
caf$cafas_dates[, min_assess_dt := min(assess_dt), by = case_no]

caf$cafas_analyze <- mmerge(l = list(
  caf$cafas_dates[min_assess_dt == assess_dt,
    list(min_total_score = total_score), by = list(case_no)],
  caf$cafas_dates[max_assess_dt == assess_dt,
    list(max_total_score = total_score), by = list(case_no)]
), all = TRUE, by = c("case_no"))
caf$cafas_analyze[, case_no := as.int(case_no)]


# join to case_no's that are eligible for current fiscal year
caf$cafas_analyze <-
  caf$cafas_analyze[caf$cafas_cg, on = "case_no"]
# calculate summary statistics
cafas_results <-
  caf$cafas_analyze[, list(
    `total cases` = length(unique(case_no)),
    `avg initial total score` = mean(min_total_score, na.rm = TRUE),
    `avg last total score` = mean(max_total_score, na.rm = TRUE),
    `avg total differences` = mean(min_total_score, na.rm = TRUE) -
      mean(max_total_score, na.rm = TRUE))]