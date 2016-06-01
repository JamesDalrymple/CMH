# exporting data with pipe delimited text files -------------------------------
write.table(modify$hh_detail,
            file = file.path(project_wd$results, "hh_detail.txt"),
            row.names = FALSE, sep = "|", na = "")
write.table(modify$hh_services,
            file = file.path(project_wd$results, "hh_services.txt"),
            row.names = FALSE, sep = "|", na = "")
write.table(modify$hh_bucket,
            file = file.path(project_wd$results, "hh_bucket.txt"),
            row.names = FALSE, sep = "|", na = "")
write.table(modify$tiers,
            file = file.path(project_wd$results, "hh_tiers.txt"),
            row.names = FALSE, sep = "|", na = "")
write.table(modify$cc360_main,
            file = file.path(project_wd$results, "cc360_main.txt"),
            row.names = FALSE, sep = "|", na = "")
# sas

short_cols <- paste0("col_", seq(names(modify$cc360_main)))
cc360_main_short <- copy(modify$cc360_main)
setnames(cc360_main_short, names(cc360_main_short), short_cols)

write.xport(cc360_main_short,
  file = file.path(project_wd$results, "cc360_main.dat"))
# read.xport(file = file.path(project_wd$results, "cc360_main.dat"))
# toSAS(modify$cc360_main[1:2])
pkg_loader("foreign")
write.foreign(df=modify$cc360_main,
              datafile="cc360_main_tmp.csv",
              codefile="cc360_main.sas", package="SAS")


write.table(modify$cc360_main,
            file = file.path(project_wd$results, "cc360_main.csv"),
            row.names = FALSE, sep = ",", na = "")
write.table(modify$cc360_main,
            file = file.path(project_wd$results, "cc360_main.txt"),
            row.names = FALSE, sep = "|", na = "")

if (FALSE) {
  lapply(modify$tiers, unique)
  modify$cc360_main[, unique(.SD), .SDcols = Cs(case_no, HH_adm_date, HH_disc_date)][order(case_no)][,  length(unique(case_no))]

  t(modify$hh_detail[1, ])
  t(modify$hh_services[1, ])
  t(modify$hh_bucket[1, ])
  t(modify$tiers[1, ])
  t(modify$cc360_main[1, ])
  t(modify$cc360_med[1, ])
}
