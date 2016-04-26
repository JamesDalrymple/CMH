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



### summary by prescriber ###
### number of benzo's per prescriber ###
agg_Provider_Benzo <- prep$cur_meds[Provider!="" & (benzo=="Y"), list(num_Benzo_prescribed = length(Case_no)), by=list(Provider)]
# factor levels
agg_Provider_Benzo[, Provider := factor(Provider, levels=agg_Provider_Benzo[order(num_Benzo_prescribed), Provider] )]
# order dataset
agg_Provider_Benzo <- agg_Provider_Benzo[order(-num_Benzo_prescribed)]
### number of stimulants per prescriber ###
agg_Provider_Stim <- prep$cur_meds[Provider!="" & (stimulant=="Y"), list(num_Stimulants_prescribed = length(Case_no)), by=list(Provider)]
# factor levels
agg_Provider_Stim[, Provider := factor(Provider, levels=agg_Provider_Stim[order(num_Stimulants_prescribed), Provider] )]
# order dataset
agg_Provider_Stim <- agg_Provider_Stim[order(-num_Stimulants_prescribed)]
### number of benzo's and stimulants per prescriber ###
agg_benzo_stim <- setkey(prep$cur_meds, Case_no)[J(intersect(benzoCons, stimCons)),  list(num_benzo_stim_prescribed = length(Case_no)), by=list(Provider)]
agg_benzo_stim <- agg_benzo_stim[Provider!=""]
# factor levels
agg_benzo_stim[, Provider := factor(Provider, levels=agg_benzo_stim[order(num_benzo_stim_prescribed), Provider] )]
# order dataset
agg_benzo_stim <- agg_benzo_stim[order(-num_benzo_stim_prescribed)]
