prep <- new.env(parent = .GlobalEnv)

prep$adm <- copy(sql$output$cmh_adm)
prep$ir <- copy(sql$output$ir)
prep$cur_meds <- copy(sql$output$cur_meds)
prep$ir_detail <- copy(sql$output$ir_detail)
prep$ir_full <- copy(sql$output$ir_full)

# current medications ---------------------------------------------------------
prep$cur_meds[, drug := tolower(drug)] #lowercase
prep$cur_meds <- prep$cur_meds[!is.na(drug)]
# make a list of medicines that are 'typical family doctor medicines' and remove them
prep$all_meds <- prep$cur_meds[, unique(drug)]
# benzos ---
prep$cur_benzo <- aux$mySearch(x = prep$all_meds, pattern = aux$benzoList)
setkey(prep$cur_meds, drug)[J(prep$cur_benzo), benzo := "Y"]
setkey(prep$cur_meds, NULL)
# stimulants ---
prep$cur_stim <- aux$mySearch(x = prep$all_meds, pattern = aux$stimList)
# classify current stimulant's as stimulant ---
setkey(prep$cur_meds, drug)[J(prep$cur_stim), stim := "Y"]
setkey(prep$cur_meds, NULL)
# create atypical antipsychotic column ---
prep$aapDT = aux$aTypical(data = prep$all_meds)
setkey(prep$cur_meds, drug); setkey(prep$aapDT, drug)
prep$cur_meds <- prep$aapDT[prep$cur_meds, nomatch = NA]
# create antipsychotic column ---
prep$cur_antipysch <- aux$mySearch(x = prep$all_meds,
                                   pattern = aux$antiPysList)
setkey(prep$cur_meds, drug)[J(prep$cur_antipysch ), antiPysch := "Y"]
# create antidepressant column ---
prep$cur_antidepress <- aux$mySearch(x = prep$all_meds,
                                     pattern = aux$antidepressList)
setkey(prep$cur_meds, drug)[J(prep$cur_antidepress), antidepress := "Y"]

# open consumers --------------------------------------------------------------
prep$adm[, setdiff(names(prep$adm), Cs(case_no, cmh_effdt, cmh_expdt)) := NULL]
prep$adm <- unique(prep$adm)



