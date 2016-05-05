agg <- new.env(parent = .GlobalEnv)

# aggregate number of total open CMH consumers
agg$num_con <- prep$adm[, list(consumers = length(unique(case_no))),
                        by = list(span_label, span_type)]

# aggregate current medication results ----------------------------------------
# number/pct of benzos ---
agg$benzo$cons <- setkey(prep$cur_meds, benzo)[J("Y")][, unique(case_no)]
agg$benzo$pct <-
  length(agg$benzo$cons)/agg$num_con[span_type == "fy", consumers]
agg$benzo$drug_dt <-
  prep$cur_meds[J("Y"), list(number_prescribed = .N), by = drug]
## number/pct of stimulants
agg$stim$cons <- setkey(prep$cur_meds, stim)[J("Y")][, unique(case_no)]
agg$stim$pct <- length(agg$stim$cons)/agg$num_con[span_type == "fy", consumers]
agg$stim$drug_dt =
  prep$cur_meds[J("Y"), list(number_prescribed = .N), by = drug]
# number of benzos and stimulants
agg$benzo_stim$cons <- intersect(agg$benzo$cons, agg$stim$cons)
agg$benzo_stim$pct <-
  length(agg$benzo_stim$cons)/agg$num_con[span_type == "fy", consumers]
## number of consumers with 2 or more AAPs
agg$aap$cons <-
  prep$cur_meds[, list(AAPs = aux$length_noNA(AAPclass)),
                by = list(case_no)][AAPs > 1, unique(case_no)]
agg$aap$pct <- length(agg$aap$cons)/agg$num_con[span_type == "fy", consumers]
# numer of consumers with 2 or more antipyschotics
agg$anti_psych$cons <-
  prep$cur_meds[, list(antiPysch = aux$length_noNA(antiPysch)),
                by = list(case_no)][antiPysch > 1, unique(case_no)]
agg$anti_psych$pct <-
  length(agg$anti_psych$cons)/agg$num_con[span_type == "fy", consumers]
agg$anti_psych$drug_dt <-
  prep$cur_meds[antiPysch == "Y", list(number_prescribed = .N), by = drug]
# number of consumers with 2 more more antidepressants
agg$anti_depress$cons <-
  prep$cur_meds[antidepress == "Y", list(antidepress = .N),
                by = list(case_no)][antidepress > 1, unique(case_no)]
agg$anti_depress$pct <-
  length(agg$anti_depress$cons)/agg$num_con[span_type == "fy", consumers]
agg$anti_depress$drug_dt <-
  prep$cur_meds[antidepress == "Y", list(number_prescribed = .N), by = drug]
# summary by prescriber ---
# benzo
agg$benzo$provider <-
  prep$cur_meds[!is.na(provider) & (benzo == "Y"), list(
    num_Benzo_prescribed = length(case_no)), by=list(provider)]
agg$benzo$provider[, provider := factor(provider,
  levels = agg$benzo$provider[order(num_Benzo_prescribed), provider] )]
setorder(agg$benzo$provider, -num_Benzo_prescribed)
# stimulant
agg$stim$provider <-
  prep$cur_meds[!is.na(provider) & (stim == "Y"), list(
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
agg$cur_med$results <-
  data.table(group = c("Benzos", "Stimulants", "Benzo & Stimulants",
      "antipysch 2 or more", "antidepress 2 or more"),
    consumers = c(length(agg$benzo$cons), length(agg$stim$cons),
                  length(agg$benzo_stim$cons), length(agg$anti_psych$con),
                  length(agg$anti_depress$con)),
    CMH_consumers = agg$num_con[span_type == "fy", consumers],
    pct = c(agg$benzo$pct, agg$stim$pct, agg$benzo_stim$pct,
            agg$anti_psych$pct, agg$anti_depress$pct))
agg$cur_med$results[, pct := round(pct * 100, 2)]

# missed medicaition IRs ------------------------------------------------------
agg$mm_ir$ven_auth <- prep$vendor_auth[, list(con_auth = length(unique(case_no))),
                                    by = list(vendor, span_label, span_type)]
agg$mm_ir$ven_fy <- prep$ir[, list(consumers = length(unique(case_no)),
                               num_IRs = length(unique(IR_number))),
                        by = list(vendor, fy)]
agg$mm_ir$ven_qtr <- prep$ir[, list(consumers = length(unique(case_no)),
                                num_IRs = length(unique(IR_number))),
                         by = list(vendor, qtr)]
agg$mm_ir$ven_comb <- rbindlist(list(
  agg$mm_ir$ven_fy[agg$mm_ir$ven_auth[span_type == "fy"],
               on = c("vendor" = "vendor", "fy" = "span_label")],
  agg$mm_ir$ven_qtr[agg$mm_ir$ven_auth[span_type == "qtr"],
                on = c("vendor" = "vendor", "qtr" = "span_label")]))
setnames(agg$mm_ir$ven_comb, "fy", "span_label")
agg$mm_ir$ven_comb[, span_type := ifelse(grepl("Q", span_label), "qtr", "fy")]
agg$mm_ir$ven_comb[is.na(consumers), consumers := 0]
agg$mm_ir$ven_comb[, pct_mm_ir := round(consumers/con_auth*100, 0)]
agg$mm_ir$ven_comb[, gg_lab :=
  sprintf("%1$s%% (%2$s/%3$s)", pct_mm_ir, consumers, con_auth)]
agg$mm_ir$ven_comb[is.na(num_IRs), num_IRs := 0]

agg$mm_ir$con_fy <-prep$ir[, list(num_IRs = length(unique(IR_number))),
                           by = list(case_no, vendor, fy)]
agg$mm_ir$con_qtr <- prep$ir[, list(num_IRs = length(unique(IR_number))),
                             by = list(case_no, vendor, qtr)]
agg$mm_ir$con_comb <- rbindlist(list(agg$mm_ir$con_fy, agg$mm_ir$con_qtr))
setnames(agg$mm_ir$con_comb, "fy", "span_label")
agg$mm_ir$con_comb[, span_type := ifelse(grepl("Q", span_label), "qtr", "fy")]

# medication incidents (medication related IRs) -------------------------------


agg$med_inc$comb <- rbindlist(list(
  prep$ir[, list(cases = length(unique(case_no)),
                 incidents = length(unique(IR_number))),
          by = .(qtr, classification)],
  prep$ir[, list(cases = length(unique(case_no)),
                 incidents = length(unique(IR_number))),
          by = .(fy, classification)]))
setnames(agg$med_inc$comb, "qtr", "span_label")
agg$med_inc$comb <- agg$med_inc$comb[agg$num_con, on = c("span_label")]
agg$med_inc$comb[, pct_incidents := cases/consumers]
agg$med_inc$qtr <- agg$med_inc$comb[span_type == "qtr"]
agg$med_inc$fy <- agg$med_inc$comb[span_type == "fy"]