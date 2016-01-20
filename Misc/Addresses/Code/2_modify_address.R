modify <- new.env(parent = .GlobalEnv)

address <- copy(sql$output$address)
guardian <- copy(sql$output$guardian)

# address ---------------------------------------------------------------------
setnames(address, names(address), tolower(names(address)))
setnames(address,
         grep(x = names(address), pattern = "_x_", value = TRUE),
         paste0("service_in_", input$service_days, "_days"))
# fixing 1-time errors
address[, addr1_fixed := gsub(address_line1,
                              pattern = " NSN 09/23/09", replacement = "")]
address[addr1_fixed %like% "992 Rabbit Run Circle Apt 101",
        addr1_fixed := "992 Rabbit Run Circle Apt 101"]
# fixing addresses for mailing and other address in case mailing address is NA
address[, mail1_fixed := aux$fix_address(mailaddress_line1)]
address[, mail2_fixed := aux$fix_address(mailaddress_line2)]
address[, addr2_fixed := aux$fix_address(address_line1)]
address[, addr2_fixed := aux$fix_address(address_line2)]
address[grep(x = addr2_fixed, pattern = "[0-9]"),
        addr1_fixed := paste(addr1_fixed, addr2_fixed)]
address[grep(x = mail2_fixed, pattern = "[0-9]"),
        mail1_fixed := paste(mail1_fixed, mail2_fixed)]
address[, c("addr2_fixed", "mail2_fixed") := NULL]
address[, city := aux$fix_city(city)]
address[, mailcity := aux$fix_city(mailcity)]
address[city == "NANA", city := NA]
address[mailcity == "NANA", mailcity := NA]
address[, unique(mailcity)]
address[,  state := toupper(state)]
address[,  mailstate := toupper(mailstate)]
address[, fixed_mailing_address :=
          ifelse(!is.na(mail1_fixed), mail1_fixed, addr1_fixed)]
address[, c("addr1_fixed", "mail1_fixed") := NULL]

# these cases need guardians
modify$guard_cases <- address[guardian_in_sheet2 == "Y", unique(case_no)]

# guardian --------------------------------------------------------------------
setnames(guardian, names(guardian), tolower(names(guardian)))
guardian[case_no %in% modify$guard_cases]
