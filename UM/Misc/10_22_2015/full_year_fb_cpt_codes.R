rm(list = ls())

require(EquaPac)
fb_dt <- readRDS(file.path("G:/CSTS Data Analyst Archives/FB_archives",
  "rds/fb fye14 blnd all 9_1_14_to_9_30_15 ran 10_22_15.rds"))

setnames(
  fb_dt,
  old = c("UNITS", "PRI PROCEDURE CODE", "UNIT TYPE"),
  new = c("units", "CPT", "unit_type")
)

summary_dt <-
  fb_dt[, list(total_units = sum(units)), by = c("CPT", "unit_type")]
um_desc <- fread(file.path("C:/Users/dalrymplej/Dropbox",
  "Utilization Management/UM_Desc_MDCH_2012.csv"))
um_desc[nchar(`CPT CD`)==3, `CPT CD` := paste0("0", `CPT CD`)]
um_desc <- um_desc[, .SD, .SDcols = c("CPT CD", "UM Desc")]

# comb_dt1a <- summary_dt[um_desc, nomatch=NA, on=c(CPT="CPT CD")]
# comb_dt1b <- merge(summary_dt, um_desc, by.x="CPT", by.y="CPT CD", all.y=TRUE)
# # these are equal
# all.equal(comb_dt1a[order(CPT), CPT], comb_dt1b[order(CPT), CPT])

# comb_dt2a <-  um_desc[summary_dt, nomatch=NA, on=c(`CPT CD`="CPT")]
# comb_dt2b <- merge(summary_dt, um_desc, by.x="CPT", by.y="CPT CD", all.x=TRUE)
# # these are equal
# all.equal(
#   comb_dt2a[order(`CPT CD`), `CPT CD`],
#   comb_dt2b[order(CPT), CPT]
#   )

summary_dt[, CPT := gsub(x=CPT, pattern=" ", replace="")]

output <- merge(summary_dt, um_desc, by.x="CPT", by.y="CPT CD", all=TRUE)
write.csv(output,
          file.path(
          "C:/Users/dalrymplej/Documents/GitHub/CMH/UM/Misc/10_22_2015",
          "fb_9_1_2014_to_9_30_2015_CPT_summary.csv"),
          row.names = FALSE)
