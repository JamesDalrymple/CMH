modify <- new.env(parent = .GlobalEnv)

address <- copy(sql$output$address)
guardian <- copy(sql$output$guardian)


address[, length(unique(case_no))]
guardian[, length(unique(case_no))]


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
address[is.na(mailcity), mailcity := city]
address[is.na(mailstate), mailstate := state]
address[is.na(mailzipcode), mailzipcode := zipcode]
address[,  state := toupper(state)]
address[,  mailstate := toupper(mailstate)]
address[, fixed_mailing_address :=
          ifelse(!is.na(mail1_fixed), mail1_fixed, addr1_fixed)]
address[, c("addr1_fixed", "mail1_fixed") := NULL]
# fixing capitalization
address[, fixed_mailing_address := cap_word(fixed_mailing_address)]
address[, client_first := cap_word(client_first)]
address[, client_last := cap_word(client_last)]

# these cases need guardians
modify$guard_cases <- address[guardian_in_sheet2 == "Y", unique(case_no)]

# guardian --------------------------------------------------------------------
setnames(guardian, names(guardian), tolower(names(guardian)))
# restrict to vocational guardians
guardian <- guardian[case_no %in% modify$guard_cases]
# fixing addresses for mailing and other address in case mailing address is NA
guardian[, mail1_fixed := aux$fix_address(mailaddress_line1)]
guardian[, mail2_fixed := aux$fix_address(mailaddress_line2)]
guardian[grep(x = mail2_fixed, pattern = "[0-9]"),
        mail1_fixed := paste(mail1_fixed, mail2_fixed)]
guardian[, c("mail2_fixed") := NULL]
guardian[, mailcity := aux$fix_city(mailcity)]
guardian[mailcity == "NANA", mailcity := NA]
guardian[,  mailstate := toupper(mailstate)]
setnames(guardian, "mail1_fixed", "fixed_mailing_address")
guardian[, fixed_mailing_address := cap_word(fixed_mailing_address)]
guardian[, client_first := cap_word(client_first)]
guardian[, client_last := cap_word(client_last)]
guardian[, guardian_first := cap_word(guardian_first)]
guardian[, guardian_last := cap_word(guardian_last)]