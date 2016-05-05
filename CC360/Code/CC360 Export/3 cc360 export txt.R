

write.table(sql$output$cc360_main, file = "export_cc360_main.txt",
            row.names = FALSE, sep = "|", na = "")
write.table(sql$output$cc360_med, file = "export_cc360_med.txt",
            row.names = FALSE, sep = "|", na = "")