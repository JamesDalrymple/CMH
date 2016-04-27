prep <- new.env(parent = .GlobalEnv)

prep$adm <- copy(sql$output$cmh_adm)
prep$ir <- copy(sql$output$ir)
prep$vendor_auth <- copy(sql$output$vendor_auth)
prep$cur_meds <- copy(sql$output$cur_meds)

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

# missed meds ir --------------------------------------------------------------
# missed med ir ---
prep$ir[, discovery_date := as.Date(discovery_date)]
prep$ir[, fy := my_fy(discovery_date)]
prep$ir[, qtr := my_qtr(discovery_date)]
prep$ir[, short_ven := aux$shortVendor(vendor)]
prep$ir_full <- copy(prep$ir)
prep$ir[, setdiff(names(prep$ir),
  Cs(case_no, short_ven, IR_number, discovery_date, fy, qtr)) := NULL]
setnames(prep$ir, "short_ven", "vendor")
# vendor authorizations ---
prep$vendor_auth[, vendor := aux$shortVendor(vendor)]
prep$vendor_auth <- setkey(prep$vendor_auth, vendor)[prep$ir[, unique(vendor)], nomatch = 0]
setf(prep$vendor_auth, j = Cs(auth_eff, auth_exp), as.Date)
setkey(prep$vendor_auth, auth_eff, auth_exp)
prep$vendor_auth <-
  foverlaps(aux$span_dt, prep$vendor_auth,
          by.x = Cs(span_start, span_end),
          by.y = Cs(auth_eff, auth_exp))