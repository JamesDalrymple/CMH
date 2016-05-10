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
write.table(modify$cc360_med,
            file = file.path(project_wd$results, "cc360_med.txt"),
            row.names = FALSE, sep = "|", na = "")