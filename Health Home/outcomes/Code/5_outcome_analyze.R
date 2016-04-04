analyze <- new.env(parent = .GlobalEnv)

analyze$bmi_dt <- copy(modify$output$bmi)
analyze$bp_dt <- copy(modify$output$bp)

# bmi -------------------------------------------------------------------------
# we would expect a decrease in BMI to be a good thing,
# except for special cases.
analyze$bmi_dt[first_bmi >= 18.5 & last_bmi >= 18.5,
       outcome_num := as.chr(factor(sign(floor(last_bmi-first_bmi)),
              levels = c("-1", "0", "1"),
              labels = c("improved", "maintained", "regressed")))]
analyze$bmi_dt[, outcome_num := as.chr(outcome_num)]
analyze$bmi_dt[first_bmi >= 18.5 & last_bmi < 18.5,
       outcome_num := "regressed"]
analyze$bmi_dt[first_bmi < 18.5, outcome_num :=
         as.chr(factor(sign(last_bmi-first_bmi),
                levels = c("1", "0", "-1"),
                labels = c("improved", "maintained", "regressed")))]
# results: bmi by team --------------------------------------------------------
analyze$bmi_long_team <-
  analyze$bmi_dt[, list(num_cases = length(unique(case_no))),
                 by = list(team, outcome_num)]
# presentation table
analyze$bmi_team$table <-
  merge(dcast(analyze$bmi_long_team, fill = 0, drop = FALSE,
      team ~ outcome_num, value.var = "num_cases"),
      output[, list(total = length(unique(case_no))),
             by = team], all.x = TRUE, by = "team")
analyze$bmi_team$table[, total_not_missing :=
                         psum(improved, maintained, regressed)]
analyze$bmi_team$table[, unknown :=
  total-psum(improved, maintained, regressed)]
analyze$bmi_team$table <-
  analyze$bmi_team$table[team %in% c("Access",  "ACT", "DD", "MI")]
# pct for graphing
analyze$bmi_team$pct <- analyze$bmi_team$table[,
  list(improved = round(improved/total_not_missing*100, 1),
       maintained = round(maintained/total_not_missing*100, 1),
       regressed = round(regressed/total_not_missing*100, 1)
       # unknown = round(unknown/total*100, 1)
       ), by = team]
analyze$bmi_team$pct <-
  melt(analyze$bmi_team$pct, id = "team",
  measure.vars = c("improved", "maintained", "regressed"), # "unknown"
  variable.name = "status", value.name = "pct_con")
# graph
analyze$bmi_team$pct <- analyze$bmi_team$pct[team %nin% c("UM", "PORT")]
analyze$bmi_team$plot <-
  ggplot(data = analyze$bmi_team$pct,
         aes(x = status, fill = team, y = pct_con, ymax = 1.2*pct_con))+
  geom_bar(stat = "identity", width = 0.75, color = "black", size = 0.1,
           position = position_dodge(0.75))+coord_flip() +
  geom_text(data = analyze$bmi_team$pct,
            position = position_dodge(0.75), hjust = -0.1, size = 3,
            aes(x = status, fill = team,
                y = pct_con, label = paste0(pct_con, "%"))) +
  labs(y = "percent consumers", x = "status") +
  aux$my_theme+scale_fill_manual(values =
    RColorBrewer::brewer.pal(
      analyze$bmi_team$pct[, length(unique(team))], "Blues"))
# results: bmi by hh_team -----------------------------------------------------
# presentation table
analyze$bmi_hh$table <- merge(dcast(analyze$bmi_dt, hh_team ~ outcome_num,
      fun.aggregate = length, value.var = "case_no"),
      output[, list(total = length(unique(case_no))),
             by = hh_team], all.x = TRUE, by = "hh_team")
analyze$bmi_hh$table[, `NA` := NULL]
analyze$bmi_hh$table[, unknown :=
  total-psum(improved, maintained, regressed)]
analyze$bmi_hh$table[,
  total_not_missing := psum(improved, maintained, regressed)]
# pct for graphing
analyze$bmi_hh$pct <- analyze$bmi_hh$table[,
  list(improved = round(improved/total_not_missing*100, 1),
       maintained = round(maintained/total_not_missing*100, 1),
       regressed = round(regressed/total_not_missing*100, 1)
       # unknown = round(unknown/total*100, 1)
       ), by = hh_team]
analyze$bmi_hh$pct <-
  melt(analyze$bmi_hh$pct, id = "hh_team",
       measure.vars = c("improved", "maintained", "regressed"), # "unknown"
       variable.name = "status", value.name = "pct_con")
# graph
analyze$bmi_hh$plot <-
  ggplot(data = analyze$bmi_hh$pct,
         aes(x = status, fill = hh_team, y = pct_con, ymax = 1.2*pct_con))+
  geom_bar(stat = "identity", width = 0.75, color = "black", size = 0.1,
           position = position_dodge(0.75))+coord_flip() +
  geom_text(data = analyze$bmi_hh$pct,
            position = position_dodge(0.75), hjust = -0.1, size = 3,
            aes(x = status, fill = hh_team,
                y = pct_con, label = paste0(pct_con, "%"))) +
  labs(y = "percent consumers", x = "status") +
  aux$my_theme+scale_fill_manual(values = aux$outcome_colors[1:2])
# results: bmi by individual staff member -------------------------------------
analyze$bmi_hh$staff_ind$table <-
  merge(dcast(analyze$bmi_dt, hh_team + samhsa_staff ~ outcome_num,
        subset = .(hh_team == "Y"),
        fun.aggregate = length, value.var = "case_no"),
        output[hh_team == "Y", list(total = length(unique(case_no))),
               by = list(hh_team, samhsa_staff)], all.x = TRUE,
        by = c("hh_team", "samhsa_staff"))
analyze$bmi_hh$staff_ind$table[, unknown :=
  total-psum(improved, maintained, regressed)]
analyze$bmi_hh$staff_ind$table[, total_not_missing :=
                                 psum(improved, maintained, regressed)]
analyze$bmi_hh$staff_ind$pct <-
  analyze$bmi_hh$staff_ind$table[,
   list(improved = round(improved/total_not_missing*100, 1),
        maintained = round(maintained/total_not_missing*100, 1),
        regressed = round(regressed/total_not_missing*100, 1)
        # unknown = round(unknown/total*100, 1)
        ),
   by = list(hh_team, samhsa_staff)]
# blood pressure --------------------------------------------------------------
analyze$bp$sys$table <-
  dcast(modify$output$bp, hh_team ~ jama_sys_status,
        fun.aggregate = function(x) length(unique(x)),
        value.var = "case_no")
analyze$bp$dia$table <-
  dcast(modify$output$bp, hh_team ~ jama_dia_status,
        fun.aggregate = function(x) length(unique(x)),
        value.var = "case_no")
analyze$bp$hh$table <- output[age >= 18,
  list(total = length(unique(case_no))), by = hh_team]
# add hh_team totals
analyze$bp$sys$table <- merge(analyze$bp$sys$table,
                             analyze$bp$hh$table,
                             by = "hh_team",
                             all = TRUE)
analyze$bp$dia$table <- merge(analyze$bp$dia$table,
                             analyze$bp$hh$table,
                             by = "hh_team",
                             all = TRUE)
analyze$bp$sys$table[, unknown :=
  total - psum(improved, maintained, regressed)]
analyze$bp$sys$table[,
  total_not_missing := psum(improved, maintained, regressed)]
analyze$bp$dia$table[, unknown :=
  total - psum(improved, maintained, regressed)]
analyze$bp$dia$table[,
  total_not_missing := psum(improved, maintained, regressed)]

# pct for graphing
analyze$bp$sys$pct <-
  analyze$bp$sys$table[,
    list(improved = round(improved/total_not_missing*100, 1),
         maintained = round(maintained/total_not_missing*100, 1),
         regressed = round(regressed/total_not_missing*100, 1)
         # unknown = round(unknown/total*100, 1)
         ), by = hh_team]
analyze$bp$dia$pct <-
  analyze$bp$dia$table[,
   list(improved = round(improved/total_not_missing*100, 1),
        maintained = round(maintained/total_not_missing*100, 1),
        regressed = round(regressed/total_not_missing*100, 1)
        # unknown = round(unknown/total*100, 1)
        ), by = hh_team]
analyze$bp$sys$pct <- melt(analyze$bp$sys$pct, id = "hh_team",
  measure.vars = c("improved", "maintained", "regressed"), # "unknown"
  variable.name = "status", value.name = "pct_con")
analyze$bp$dia$pct <- melt(analyze$bp$dia$pct, id = "hh_team",
  measure.vars = c("improved", "maintained", "regressed"), # "unknown"
  variable.name = "status", value.name = "pct_con")
# graph
analyze$bp$sys$plot <-
  ggplot(data = analyze$bp$sys$pct,
         aes(x = status, fill = hh_team, y = pct_con, ymax = 1.2*pct_con))+
  geom_bar(stat = "identity", width = 0.75, color = "black", size = 0.1,
           position = position_dodge(0.75))+coord_flip() +
  geom_text(data = analyze$bp$sys$pct,
            position = position_dodge(0.75), hjust = -0.1, size = 3,
            aes(x = status, fill = hh_team,
                y = pct_con, label = paste0(pct_con, "%"))) +
  labs(y = "percent consumers", x = "status") +
  aux$my_theme+scale_fill_manual(values = aux$outcome_colors[1:2])
analyze$bp$dia$plot <-
  ggplot(data = analyze$bp$dia$pct,
         aes(x = status, fill = hh_team, y = pct_con, ymax = 1.2*pct_con))+
  geom_bar(stat = "identity", width = 0.75, color = "black", size = 0.1,
           position = position_dodge(0.75))+coord_flip() +
  geom_text(data = analyze$bp$dia$pct,
            position = position_dodge(0.75), hjust = -0.1, size = 3,
            aes(x = status, fill = hh_team,
                y = pct_con, label = paste0(pct_con, "%"))) +
  labs(y = "percent consumers", x = "status") +
  aux$my_theme+scale_fill_manual(values = aux$outcome_colors[1:2])
# wellness note ---------------------------------------------------------------
modify$wellness[, team := cmh_recode(team)]
modify$wellness <- modify$wellness[team %nin% c("UM", "PORT")]
# wellness note: team ---------------------------------------------------------
# presentation table
analyze$wn$team$table <-
  modify$wellness[!is.na(overall_health_rating) &
   wellnessnote_date == max_wn_date,
   list(num_consumers = length(unique(case_no))),
   by = list(team, overall_health_rating)]
analyze$wn$team$table <-
  merge(dcast(analyze$wn$team$table, team ~ overall_health_rating,
            variable.names = "status", value.var = "num_consumers",
            value.name = "con", fill = 0, drop = FALSE),
        modify$wellness[, list(total = length(unique(case_no))),
             by = team], all.x = TRUE, by = "team")
analyze$wn$team$table[, unknown :=
  total - psum(Excellent, Fair, Good, Poor)]
analyze$wn$team$table[, total_not_missing := psum(Excellent, Fair, Good, Poor)]
analyze$wn$team$table <-
  analyze$wn$team$table[team %in% c("Access", "ACT", "DD", "MI")]
# pct for graphing
analyze$wn$team$pct <-
  analyze$wn$team$table[,
   list(excellent = round(Excellent/total*100, 1),
    good = round(Good/total*100, 1),
    fair = round(Fair/total*100, 1),
    poor = round(Poor/total*100, 1)
    # unknown = round(unknown/total*100, 1)
    ), by = team]
analyze$wn$team$pct <-
  melt(analyze$wn$team$pct, id = "team",
       measure.vars = c("excellent", "good", "fair", "poor"), # "unknown"
       variable.name = "status", value.name = "pct_con")
# graph
analyze$wn$team$plot <-
ggplot(data = analyze$wn$team$pct,
       aes(x = status, fill = team,
           y = pct_con, ymax = 1.2*pct_con))+
  geom_bar(stat = "identity", width = 0.75, color = "black", size = 0.1,
           position = position_dodge(0.75))+coord_flip() +
  geom_text(data = analyze$wn$team$pct,
            position = position_dodge(0.75), hjust = -0.1, size = 3,
            aes(x = status, fill = team,
                y = pct_con, label = paste0(pct_con, "%"))) +
  labs(y = "percent consumers", x = "status") +
  aux$my_theme+scale_fill_manual(values = RColorBrewer::brewer.pal(
    analyze$bmi_team$pct[, length(unique(team))], "Blues"))
# wellness note: hh_team ------------------------------------------------------
# presentation table
analyze$wn$hh_team$table <-
  modify$wellness[!is.na(overall_health_rating) &
                    wellnessnote_date == max_wn_date,
                  list(num_consumers = length(unique(case_no))),
                  by = list(hh_team, overall_health_rating)]
analyze$wn$hh_team$table <-
  merge(dcast(analyze$wn$hh_team$table, hh_team ~ overall_health_rating,
              variable.names = "status", value.var = "num_consumers",
              value.name = "con", fill = 0, drop = FALSE),
        modify$wellness[, list(total = length(unique(case_no))),
                        by = hh_team], all.x = TRUE, by = "hh_team")
analyze$wn$hh_team$table[is.na(hh_team), hh_team := "N"]
analyze$wn$hh_team$table[, unknown :=
                        total - psum(Excellent, Fair, Good, Poor)]
analyze$wn$hh_team$table[, total_not_missing :=
                           psum(Excellent, Fair, Good, Poor)]
# pct for graphing
analyze$wn$hh_team$pct <-
  analyze$wn$hh_team$table[,
    list(excellent = round(Excellent/total*100, 1),
         good = round(Good/total*100, 1),
         fair = round(Fair/total*100, 1),
         poor = round(Poor/total*100, 1)
         # unknown = round(unknown/total*100, 1)
         ), by = hh_team]
analyze$wn$hh_team$pct <-
  melt(analyze$wn$hh_team$pct, id = "hh_team",
       measure.vars = c("excellent", "good", "fair", "poor"), # "unknown"
       variable.name = "status", value.name = "pct_con")
# graph
analyze$wn$hh_team$plot <-
  ggplot(data = analyze$wn$hh_team$pct,
         aes(x = status, fill = hh_team,
             y = pct_con, ymax = 1.2*pct_con))+
  geom_bar(stat = "identity", width = 0.75, color = "black", size = 0.1,
           position = position_dodge(0.75))+coord_flip() +
  geom_text(data = analyze$wn$hh_team$pct,
            position = position_dodge(0.75), hjust = -0.1, size = 3,
            aes(x = status, fill = hh_team,
                y = pct_con, label = paste0(pct_con, "%"))) +
  labs(y = "percent consumers", x = "status") +
  aux$my_theme+scale_fill_manual(values = aux$outcome_colors[1:2])