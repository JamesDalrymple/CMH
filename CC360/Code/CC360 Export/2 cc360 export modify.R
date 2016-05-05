

# sql$output$cc360_main[is.na(sql$output$cc360_main)] <- ""
write.table(sql$output$cc360_main, file = "export_cc360_main.txt",
          row.names = FALSE, sep = "|", na = "")
write.table(sql$output$cc360_med, file = "export_cc360_med.txt",
            row.names = FALSE, sep = "|", na = "")

# # fread("export_cc360_main.csv", sep="|")
# test_read <- read.table("export_cc360_main.txt", sep="|", header = TRUE)
# data.table(test_read)
#
# sql$output$cc360_main[is.na(Home_Health_adm_date)]
#
# str(sql$output$cc360_main)
