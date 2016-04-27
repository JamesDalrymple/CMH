agg <- new.env(parent = .GlobalEnv)

# aggregate number of total open CMH consumers
agg$num_con <- prep$adm[, length(unique(case_no))]

# aggregate current medication results ----------------------------------------
# number/pct of benzos ---
agg$benzo$cons <- setkey(prep$cur_meds, benzo)[J("Y")][, unique(case_no)]
agg$benzo$pct <- length(agg$benzo$cons)/agg$num_con
agg$benzo$drug_dt <-
  prep$cur_meds[J("Y"), list(number_prescribed = .N), by = drug]
## number/pct of stimulants
agg$stim$cons <- setkey(prep$cur_meds, stim)[J("Y")][, unique(case_no)]
agg$stim$pct <- length(agg$stim$cons)/agg$num_con
agg$stim$drug_dt =
  prep$cur_meds[J("Y"), list(number_prescribed = .N), by = drug]
# number of benzos and stimulants
agg$benzo_stim$cons <- intersect(agg$benzo$cons, agg$stim$cons)
agg$benzo_stim$pct <- length(agg$benzo_stim$cons)/agg$num_con
## number of consumers with 2 or more AAPs
agg$aap$cons <-
  prep$cur_meds[, list(AAPs = aux$length_noNA(AAPclass)),
                by = list(case_no)][AAPs > 1, unique(case_no)]
agg$aap$pct <- length(agg$aap$cons)/agg$num_con
# numer of consumers with 2 or more antipyschotics
agg$anti_psych$cons <-
  prep$cur_meds[, list(antiPysch = aux$length_noNA(antiPysch)),
                by = list(case_no)][antiPysch > 1, unique(case_no)]
agg$anti_psych$pct <- length(agg$anti_psych$cons)/agg$num_con
agg$anti_psych$drug_dt <-
  prep$cur_meds[antiPysch == "Y", list(number_prescribed = .N), by = drug]
# number of consumers with 2 more more antidepressants
agg$anti_depress$cons <-
  prep$cur_meds[antidepress == "Y", list(antidepress = .N),
                by = list(case_no)][antidepress > 1, unique(case_no)]
agg$anti_depress$pct <-
  length(agg$anti_depress$cons)/agg$num_con
agg$anti_depress$drug_dt <-
  prep$cur_meds[antidepress == "Y", list(number_prescribed = .N), by=drug]
# summary by prescriber ---
# benzo
agg$benzo$provider <-
  prep$cur_meds[!is.na(provider) & (benzo=="Y"), list(
    num_Benzo_prescribed = length(case_no)), by=list(provider)]
agg$benzo$provider[, provider := factor(provider,
  levels = agg$benzo$provider[order(num_Benzo_prescribed), provider] )]
setorder(agg$benzo$provider, -num_Benzo_prescribed)
# stimulant
agg$stim$provider <-
  prep$cur_meds[!is.na(provider) & (stim=="Y"), list(
    num_Stimulants_prescribed = length(case_no)), by=list(provider)]
agg$stim$provider[, provider := factor(provider,
  levels = agg$stim$provider[order(num_Stimulants_prescribed), provider] )]
setorder(agg$stim$provider, -num_Stimulants_prescribed)
# number of benzo's and stimulants per prescriber
agg$benzo_stim$provider <-
  setkey(prep$cur_meds, case_no)[J(intersect(agg$benzo$cons, agg$stim$cons)),
    list(num_benzo_stim_prescribed = length(case_no)), by=list(provider)]
agg$benzo_stim$provider <- agg$benzo_stim$provider[!is.na(provider)]
agg$benzo_stim$provider[, provider := factor(provider,
  levels = agg$benzo_stim$provider[order(num_benzo_stim_prescribed), provider])]
setorder(agg$benzo_stim$provider, -num_benzo_stim_prescribed)

# combine aggregated results
# sleeping AAPs per Pat Cowan on 6/19/2014
agg$cur_med$results <- data.table(group = c("Benzos", "Stimulants", "Benzo & Stimulants",
                               "antipysch 2 or more", "antidepress 2 or more"),
                     consumers = c(length(agg$benzo$cons), length(agg$stim$cons), length(agg$benzo_stim$cons),
                                   length(agg$anti_psych$con), length(agg$anti_depress$con)),
                     CMH_consumers = agg$num_con,
                     pct = c(agg$benzo$pct, agg$stim$pct, agg$benzo_stim$pct, agg$anti_psych$pct, agg$anti_depress$pct))
agg$cur_med$results[, pct := round(pct*100, 2) ]
