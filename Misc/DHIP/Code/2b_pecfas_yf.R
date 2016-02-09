pec <- new.env(parent = .GlobalEnv)

if (is.l1(grep(x = names(pecfas), pattern = "ClientPrimaryID", value = TRUE))) {
  setnames(pecfas, "ClientPrimaryID", "case_no")
}
# grab caregiver columns and reshape with case_no as primary key --------------
pec$cg_max_col <-
  max(grep(x = names(pecfas), pattern = "caregiver", ignore.case = TRUE))
pec$pecfas_cg <-
  pecfas[, .SD, .SDc = c(1:pec$cg_max_col), with = TRUE]
# remove empty data.table column
pec$rm_cols <- lapply(pec$pecfas_cg,
  FUN = function(x) identical(length(x), sum(is.na(x))))
if (!is.l0(pec$rm_cols[isTRUE(pec$rm_cols)])) {
  pec$pecfas_cg[,
    names(pec$rm_cols[isTRUE(pec$rm_cols)]) := NULL]
}
pec$pecfas_cg <- melt(pec$pecfas_cg, id = "case_no")
# retain consumerw with only non-empty values ---
# pec$pecfas_cg[value == "Great Aunt"] # 1139767
pec$pecfas_cg <- pec$pecfas_cg[value != ""]
pec$pecfas_cg <- pec$pecfas_cg[value == input$fy_value]
# main pecfas data set ---------------------------------------------------------
pec$pecfas_dates <-
  pecfas[, .SD, .SDcols = c(1,
    {pec$cg_max_col+1}:{length(names(pecfas))})]

pec$pecfas_measures_l <-
  list(
    access_date = grep(x = names(pec$pecfas_dates),
      pattern = "assessdate", value = TRUE, ignore.case = TRUE),
    admin_desc = grep(x = names(pec$pecfas_dates),
      pattern = "adminDesc", value = TRUE, ignore.case = TRUE),
    total_score = grep(x = names(pec$pecfas_dates),
         pattern = "totalscore$", value = TRUE, ignore.case = TRUE),
    total_score_diff = grep(x = names(pec$pecfas_dates),
         pattern = "totalscorediff$", value = TRUE, ignore.case = TRUE)
  )
# change non-character classes to character to help melt combine 'like' cols.
for (j in seq_along(names(pec$pecfas_dates)[-1])) {
  set(pec$pecfas_dates, j = j,
      value = as.chr(pec$pecfas_dates[[j]]))
}
# I get a warning message if I join NA cols to char columns
pec$pecfas_dates[is.na(pec$pecfas_dates)] <- "" # avoids warn msg
pec$pecfas_dates <-
  melt(data = pec$pecfas_dates, id.vars = c("case_no"),
       measure.vars = pec$pecfas_measures_l,
       variable.factor = FALSE,
       value.name = c("assess_dt", "desc", "total_score", "total_diff"))
# column class correction
pec$pecfas_dates[, variable := NULL]
pec$pecfas_dates[pec$pecfas_dates==""] <- NA # undoes a prev. line
pec$pecfas_dates[, total_score := as.num(total_score)]
pec$pecfas_dates[, total_diff := as.num(total_diff)]
pec$pecfas_dates[, case_no := as.int(case_no)]
pec$pecfas_dates <- pec$pecfas_dates[!is.na(assess_dt)]
pec$pecfas_dates[, assess_dt := date_convert(assess_dt)]

pec$pecfas_dates[, max_assess_dt := max(assess_dt), by = case_no]
pec$pecfas_dates[, min_assess_dt := min(assess_dt), by = case_no]

pec$pecfas_analyze <- mmerge(l = list(
  pec$pecfas_dates[min_assess_dt == assess_dt,
    list(min_total_score = total_score), by = list(case_no)],
  pec$pecfas_dates[max_assess_dt == assess_dt,
    list(max_total_score = total_score), by = list(case_no)]
), all = TRUE, by = c("case_no"))
pec$pecfas_analyze[, case_no := as.int(case_no)]

# join to case_no's that are eligible for current fiscal year
pec$pecfas_analyze <-
  pec$pecfas_analyze[pec$pecfas_cg, on = "case_no"]
# calculate summary statistics
pecfas_results <-
  pec$pecfas_analyze[, list(
    `total cases` = length(unique(case_no)),
    `avg initial total score` = mean(min_total_score, na.rm = TRUE),
    `avg last total score` = mean(max_total_score, na.rm = TRUE),
    `avg total differences` = mean(min_total_score, na.rm = TRUE) -
      mean(max_total_score, na.rm = TRUE))]