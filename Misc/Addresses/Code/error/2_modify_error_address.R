modify <- new.env(parent = .GlobalEnv)

address <- copy(sql$output$address)
guardian <- copy(sql$output$guardian)


address[, length(unique(case_no))]
guardian[, length(unique(case_no))]


# address ---------------------------------------------------------------------
setnames(address, names(address), tolower(names(address)))

