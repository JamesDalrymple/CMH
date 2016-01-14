# GRAPH RESULTS ---------------------------------------------------------------
graph <- new.env(parent = .GlobalEnv)
# make a list of all possible combinations
graph$all_combinations <- data.table(
  expand.grid(
    mon_fy = as.chr(as.yearmon(input$fy_start)+0:11/12),
    team = aux$teamsCMH))
# ipos results ----------------------------------------------------------------
ipos_graph <-
  modify$ipos_team[, list(
    team = team,
    pctCurrentIPOS = pctCurrentIPOS,
    pctMissingIPOS = pctMissingIPOS,
    pctExpireIPOS = pctExpireIPOS)]
ipos_graph[, mon_fy :=
  as.yearmon(paste(input$current_month, input$calendar_year))]
ipos_graph[, mon_fy := as.chr(mon_fy)]
# remove current month & add it back in to ensure current month is up-to-date
if (!file.exists(file.path(input$data_fy_wd,
  paste0("ipos fy ", input$current_fy, ".csv")))) {
write.csv(ipos_graph[order(team, mon_fy)],
  file.path(input$current_fy, paste0("ipos fy ",
    input$current_fy, ".csv") ), row.names=FALSE)
  warning(paste("A new file was created:",
                file.path(input$data_fy_wd,
                          paste0("ipos fy ", input$current_fy, ".csv"))))
}
iposAllMonths <- fread(file.path(input$data_fy_wd,
  paste0("ipos fy ", input$current_fy, ".csv")))
# iposAllMonths[, mon_fy :=
#   as.yearmon(gsub(x = mon_fy, pattern="-", replace=" 20"))]
iposAllMonths <-
  setkey(iposAllMonths, mon_fy)[
    !J(as.chr(as.yearmon(paste(input$current_month, input$calendar_year))))]
setkey(iposAllMonths, NULL)

sapply(iposAllMonths, class)
sapply(ipos_graph, class)
ipos_graph <- rbindlist(list(iposAllMonths, ipos_graph))
ipos_graph[, team := cmh_recode(team)]
rm(iposAllMonths)
ipos_graph <- unique(ipos_graph)
write.csv(ipos_graph[order(team, mon_fy)],
  file.path(input$data_fy_wd, paste0("ipos fy ",
    input$current_fy, ".csv")), row.names=FALSE)
# ipos_table1 ---
if(!file.exists(file.path(input$data_fy_wd,
    paste0("ipos t1 fy ", input$current_fy, ".csv")) )) {
  write.csv(ipos_table1, file.path(input$data_fy_wd,
    paste0("ipos t1 fy ", input$current_fy, ".csv")), row.names=FALSE)
  warning(paste("A new file was created:", file.path(input$current_fy,
    paste0("ipos t1 fy ", input$current_fy, ".csv")) ))
}
full_ipos_t1 <- fread(file.path(input$data_fy_wd,
  paste0("ipos t1 fy ", input$current_fy, ".csv")))
full_ipos_t1[, team := cmh_recode(team)]
# full_ipos_t1[, month :=
#   as.chr(as.yearmon(gsub(x = month, pattern = "-", replace = " 20")))]
full_ipos_t1 <- setkey(full_ipos_t1, month)[
  !J(as.chr(as.yearmon(paste(input$current_month, input$calendar_year))))]
full_ipos_t1 <- rbindlist(list(full_ipos_t1, ipos_table1))
full_ipos_t1 <- full_ipos_t1[!is.na(month)]
write.csv(full_ipos_t1[order(month, team)],
  file.path(input$data_fy_wd, paste0("ipos t1 fy ",
    input$current_fy, ".csv")), row.names = FALSE)
full_ipos_t1 <- fread(file.path(input$data_fy_wd,
  paste0("ipos t1 fy ", input$current_fy, ".csv")))
full_ipos_t1 <- full_ipos_t1[order(team, as.yearmon(month))]
setnames(full_ipos_t1, names(full_ipos_t1),
  gsub(x=names(full_ipos_t1), pattern="_", replace=" "))
# ipos_table2 ---
if (!file.exists(file.path(input$data_fy_wd,
  paste0("ipos t2 fy ", input$current_fy, ".csv")) )) {
  write.csv(ipos_table2, file.path(input$data_fy_wd,
    paste0("ipos t2 fy ", input$current_fy, ".csv")), row.names=FALSE)
  warning(paste("A new file was created:", file.path(input$data_fy_wd,
    paste0("ipos t2 fy ", input$current_fy, ".csv"))))
}
full_ipos_t2 <- fread(file.path(input$data_fy_wd,
  paste0("ipos t2 fy ", input$current_fy, ".csv")))
full_ipos_t2[, team := cmh_recode(team)]
full_ipos_t2 <- setkey(full_ipos_t2, month)[!J(as.chr(
  as.yearmon(paste(input$current_month, input$calendar_year))))]
full_ipos_t2 <- rbindlist(list(full_ipos_t2, ipos_table2))
write.csv(full_ipos_t2[order(team, month)], file.path(input$data_fy_wd,
  paste0("ipos t2 fy ", input$current_fy, ".csv")), row.names = FALSE)
full_ipos_t2 <- fread(file.path(input$data_fy_wd,
  paste0("ipos t2 fy ", input$current_fy, ".csv")))
full_ipos_t2 <- full_ipos_t2[order(team, as.yearmon(month))]
setnames(full_ipos_t2, old=c("blank_missing_IPOS", "currentIPOS", "expiredIPOS"),
  new=c("blank/missing IPOS", "current IPOS", "expired IPOS"))
### ipos_table3
full_ipos_t3 <- ipos_table3
full_ipos_t3[, supText := supervisor]
setkey(full_ipos_t3, primary_staff)[J(""),
  supText := paste0("\\textbf{", supText, "}")]
full_ipos_t3[, expired_missing_IPOS := as.chr(expired_missing_IPOS)]
setkey(full_ipos_t3, primary_staff)[J(""),
  expired_missing_IPOS := paste0("\\textbf{", expired_missing_IPOS, "}")]
setkey(full_ipos_t3, primary_staff)[J(""), expired_missing_IPOS :=
  paste0("\\hspace{2cm}\\textbf{", expired_missing_IPOS, "}")]
setkey(full_ipos_t3, NULL)
full_ipos_t3 <- full_ipos_t3[order(team, supervisor, primary_staff)]
full_ipos_t3[duplicated(supervisor), supervisor := ""]
full_ipos_t3[primary_staff != "", supText := ""] # testing to see if works
setnames(full_ipos_t3, old = c("primary_staff", "expired_missing_IPOS"),
  new = c("primary_staff", "expired/missing IPOS"))
setcolorder(full_ipos_t3,
  c("team", "supText", "supervisor", "primary_staff", 'expired/missing IPOS'))
# reshape/melt data for ggplot
ipos_graph <- data.table(
  reshape::melt(ipos_graph, id=c("team", "mon_fy"),
    variable.names=c("pctCurrentIPOS", "missingIPOS")))
# keep only pctCurrentIPOS per Laura H. 9/10/2014
ipos_graph <- setkey(ipos_graph, variable)["pctCurrentIPOS"]
ipos_graph[, mon_fy := as.chr(mon_fy)]
# merge with all possibilities
ipos_graph <- merge(graph$all_combinations,
  ipos_graph, all=TRUE, by=c("team", "mon_fy"))
# factor based on 12 consecutive months
ipos_graph[, mon_fy := factor(mon_fy,
  levels = as.chr(as.yearmon(input$fy_start)+0:11/12))]
# create graph
team_list <- ipos_graph[, unique(team)]
hist_ipos <- list(team_list = "")
for(i in seq_along(team_list)) {
  # start for loop
  hist_ipos[[i]] <-
    ggplot(data=ipos_graph[team==team_list[i]],
           aes(x=mon_fy, y=value, fill=variable, ymax=1.25*value))+
    geom_bar(width=0.3, stat="identity", color="black", size=1,
             position=position_dodge(0.3))+
    labs(title = paste0(team_list[i], " Graph ", i,
                        ".1.1 - % Current IPOS (Report 2003: ran ",
                        input$run_date, ")"),
         y="percent", fill="")+
    geom_text(data=ipos_graph[team==team_list[i]], position=position_dodge(0.3),
              aes(x=mon_fy, y=value, fill=variable, label=paste0(value, "%")),
              hjust=0.5, vjust=-0.5, size=3, na.rm=TRUE)+
    scale_fill_manual(values=c("#C282D6"), label=c("current IPOS"))+
    aux$my_theme+
    theme(panel.grid.major = element_line(colour = "grey80", size = 0.2),
          panel.grid.minor = element_line(colour = "grey80",
                                          size = .2 ),
          axis.title.y = element_text(colour = "grey30", size=10),
          axis.text.x = element_text(angle=45, hjust=1),
          plot.margin = unit( c(0, 0, -.2, 0) , "in" ))+
    scale_y_discrete(breaks = seq(0, 100, 25))+
    theme(panel.grid.major.x = element_blank())
}
names(hist_ipos) <- team_list
# unsigned/draft docs ---------------------------------------------------------
if(!file.exists( file.path(input$data_fy_wd,
  paste0("unsign_draft_docs t1 fy ", input$current_fy, ".csv")) )) {
  write.csv(unsignTeam, file.path(input$data_fy_wd,
    paste0("unsign_draft_docs t1 fy ",
           input$current_fy, ".csv")), row.names=FALSE)
  warning(paste("A new file was created:", file.path(input$data_fy_wd,
    paste0("unsign_draft_docs t1 fy ", input$current_fy, ".csv"))))
}
unsign_draft_t1 <- fread(file.path(input$data_fy_wd,
  paste0("unsign_draft_docs t1 fy ", input$current_fy, ".csv")))
unsign_draft_t1[, team := cmh_recode(team)]
# setnames(unsign_draft_t1, "month", "mon_fy")
unsign_draft_t1 <- setkey(unsign_draft_t1, mon_fy)[
  !J(as.chr(as.yearmon(paste(input$current_month, input$calendar_year))))]
unsign_draft_t1 <- rbindlist(list(unsign_draft_t1, unsignTeam))
write.csv(unsign_draft_t1[order(team, as.yearmon(mon_fy))],
  file.path(input$data_fy_wd, paste0("unsign_draft_docs t1 fy ",
    input$current_fy, ".csv")), row.names=FALSE)
unsign_draft_t1 <- fread(file.path(input$data_fy_wd,
  paste0("unsign_draft_docs t1 fy ", input$current_fy, ".csv")))
# merge with all possibilities
unsign_draft_t1 <- merge(graph$all_combinations, unsign_draft_t1,
                        all=TRUE, by=c("team", "mon_fy"))
# re-order - may not be necessary
unsign_draft_t1 <- unsign_draft_t1[order(team, as.yearmon(mon_fy))]
# factor based on 12 consecutive months
unsign_draft_t1[, mon_fy := factor(mon_fy, levels =
  as.chr(as.yearmon(input$fy_start)+0:11/12))]
p_unsign <- list()
for(i in seq_along(team_list)) {
  # create graph p_unsign
  p_unsign[[i]] =
    ggplot(data=unsign_draft_t1[team==team_list[i]],
      aes(x=mon_fy, fill=mon_fy, y=docs_unsigned, ymax=1.25*docs_unsigned))+
    geom_bar(width=0.5, stat="identity", color="black", size=1, fill="#C282D6",
             position=position_dodge(0.5))+
    labs(title = paste0(team_list[i], " Graph ", i,
      ".2.1 - # Unsigned/Draft Docs (Report 2004: ran ",
      input$run_date, ")"),
      y = "# unsigned/draft documents")+
    geom_text(vjust=0.5, hjust=-0.2, aes(x=mon_fy, fill=mon_fy,
      y=docs_unsigned, label=docs_unsigned),
      angle=90, position=position_dodge(0.5), size=2.75, na.rm=TRUE)+
    aux$my_theme+
    theme(legend.position="top",
          panel.grid.major = element_line(colour = "grey80", size = 0.2),
          panel.grid.minor = element_line(colour = "grey80", size = .2 ),
          axis.title.y = element_text(colour = "grey30", size=8),
          axis.text.x = element_text(angle=45, hjust=1),
          plot.margin = unit( c(0, 0, -.2, 0) , "in" ))+
    theme(panel.grid.major.x = element_blank())
}
names(p_unsign) <- team_list
setnames(unsign_draft_t1, old = "docs_unsigned", new = "unsigned/draft")
# demographic errors ----------------------------------------------------------
if(!file.exists( file.path(input$data_fy_wd,
  paste0("demo table fy ", input$current_fy, ".csv")) )) {
  write.csv(demo_team[order(team, month)], file.path(input$data_fy_wd,
    paste0("demo table fy ", input$current_fy, ".csv") ), row.names=FALSE)
  warning(paste("A new file was created:", file.path(input$data_fy_wd,
    paste0("demo table fy ", input$current_fy, ".csv"))))
}
demo_table <- fread(file.path(input$data_fy_wd,
  paste0("demo table fy ", input$current_fy, ".csv")))
setnames(demo_team, "month", "mon_fy")
setnames(demo_table, "month", "mon_fy")
demo_table[, team := cmh_recode(team)]
demo_table <- setkey(demo_table, mon_fy)[
  !J(as.chr(as.yearmon(paste(input$current_month, input$calendar_year))))]
setcolorder(demo_team, names(demo_table))
demo_table <- rbindlist(list(demo_table, demo_team), use.names = TRUE)
write.csv(demo_table[order(team, as.yearmon(mon_fy))], file.path(input$data_fy_wd,
  paste0("demo table fy ", input$current_fy, ".csv")), row.names=FALSE)
demo_table <- fread(file.path(input$data_fy_wd,
  paste0("demo table fy ", input$current_fy, ".csv")))
demo_table[is.na(con_errors), con_errors := 0]
# merge with all possibilities
demo_table <-
  merge(graph$all_combinations, demo_table, all=TRUE, by=c("team", "mon_fy"))
# re-order - may not be necessary
demo_table <- demo_table[order(team, as.yearmon(mon_fy))]
# factor based on 12 consecutive months
demo_table[, mon_fy :=
  factor(mon_fy, levels = as.chr(as.yearmon(input$fy_start)+0:11/12))]

p_demo <- list()
for(i in seq_along(team_list)) {
  # create graph p_demo
  p_demo[[i]] <-
    ggplot(data=demo_table[team==team_list[i]],
      aes(x=mon_fy, fill=mon_fy, y=con_errors, ymax=1.35*con_errors))+
    geom_bar(width=0.5, stat="identity", color="black", size=1,
      fill="#C282D6", position=position_dodge(0.5))+
    labs(title = paste0(team_list[i], " Graph ", i,
      ".3.1 - # Consumers with Demographic Errors\n (Report 2023: ran ",
      input$run_date, ")"),
      y="# consumer errors")+
    geom_text(vjust=0.5, hjust=-.5,
              aes(x=mon_fy, fill=mon_fy, y=con_errors, label=con_errors),
              angle=90, position=position_dodge(0.5), size=2.75, na.rm=TRUE)+
    aux$my_theme+
    theme(legend.position="top",
          panel.grid.major = element_line(colour = "grey80", size = 0.2),
          panel.grid.minor = element_line(colour = "grey80", size = .2 ),
          axis.title.y = element_text(colour = "grey30", size=8),
          axis.text.x = element_text(angle=45, hjust=1),
          plot.margin = unit( c(0, 0, -.2, 0) , "in" ),
          panel.grid.major.x = element_blank())
}
names(p_demo) <- team_list
# Health Errors ---------------------------------------------------------------
if (!file.exists( file.path(input$data_fy_wd,
  paste0("health table fy ", input$current_fy, ".csv")) )) {
  write.csv(health_team[order(team, month)], file.path(input$data_fy_wd,
    paste0("health table fy ", input$current_fy, ".csv") ), row.names=FALSE)
  warning(paste("A new file was created:", file.path(input$data_fy_wd,
    paste0("health table fy ", input$current_fy, ".csv"))))
}
health_table <- fread(file.path(input$data_fy_wd,
  paste0("health table fy ", input$current_fy, ".csv")))
health_table[, team := cmh_recode(team)]
health_team[, team := cmh_recode(team)]
setnames(health_table, "month", "mon_fy")
setnames(health_team, "month", "mon_fy")
health_table <- setkey(health_table, mon_fy)[
  !J(as.chr(as.yearmon(paste(input$current_month, input$calendar_year))))]
setcolorder(health_team, names(health_table))
health_table <- rbindlist(list(health_table, health_team), use.names = TRUE)
write.csv(health_table[order(team, as.yearmon(mon_fy))],
  file.path(input$data_fy_wd,  paste0("health table fy ",
    input$current_fy, ".csv")), row.names = FALSE)
health_table <- fread(file.path(input$data_fy_wd,
  paste0("health table fy ", input$current_fy, ".csv")))
health_table[is.na(con_errors), con_errors := 0]
# merge with all possibilities
health_table <- merge(graph$all_combinations, health_table,
                      all=TRUE, by=c("team", "mon_fy"))
# re-order - may not be necessary
health_table <- health_table[order(team, as.yearmon(mon_fy))]
# factor based on 12 consecutive months
health_table[, mon_fy := factor(mon_fy,
  levels = as.chr(as.yearmon(input$fy_start)+0:11/12))]

p_health <- list()
for(i in seq_along(team_list)) {
  # create graph p_health
  p_health[[i]] = ggplot(data=health_table[team==team_list[i]],
                         aes(x=mon_fy, fill=mon_fy, y=con_errors, ymax=1.3*con_errors, ymin=0))+
    geom_bar(width=0.5, stat="identity",
             color="black", size=1, fill="#C282D6", position=position_dodge(0.5))+
    labs(title = paste0(team_list[i], " Graph ", i,
                        ".4.1 - # Consumers w/ Health & Other Conditions Errors\n(Report 2145-2: ran ",
                        input$run_date, ")"),
         y="# health errors")+
    geom_text(vjust=0.5, hjust=-0.5,
              aes(x=mon_fy, fill=mon_fy, y=con_errors, label=con_errors),
              angle=90, position=position_dodge(0.5), size=3, na.rm=TRUE)+
    aux$my_theme+
    theme(legend.position="top",
          panel.grid.major = element_line(colour = "grey80", size = 0.2),
          panel.grid.minor = element_line(colour = "grey80", size = .2 ),
          axis.title.y = element_text(colour = "grey30", size=8),
          axis.text.x = element_text(angle=45, hjust=1),
          plot.margin = unit( c(0, 0, -.2, 0) , "in" ),
          panel.grid.major.x = element_blank() )+
    coord_cartesian(ylim=c(0, health_table[team==team_list[i], max(1.3*con_errors, 4, na.rm=TRUE)]))
}
names(p_health) <- team_list
# Minimum Wage Errors ---------------------------------------------------------
if(!file.exists( file.path(input$data_fy_wd,
                           paste0("wage table fy ", input$current_fy, ".csv")) )) {
  write.csv(wageTeam[order(team, mon_fy)], file.path(input$data_fy_wd,
    paste0("wage table fy ", input$current_fy, ".csv") ), row.names = FALSE)
  warning(paste("A new file was created:",
    file.path(input$data_fy_wd,  paste0("wage table fy ",
      input$current_fy, ".csv"))))
}
wage_table <- fread(file.path(input$data_fy_wd,
  paste0("wage table fy ", input$current_fy, ".csv")))
wage_table[, team := cmh_recode(team)]
# setnames(wage_table, "month", "mon_fy")
setnames(wageTeam, "month", "mon_fy")
wage_table <- setkey(wage_table, mon_fy)[!
  J(as.chr(as.yearmon(paste(input$current_month, input$calendar_year))))]
setcolorder(wageTeam, names(wage_table))
wage_table <- rbindlist(list(wage_table, wageTeam), use.names = TRUE)
write.csv(wage_table[order(team, as.yearmon(mon_fy))],
  file.path(input$data_fy_wd,  paste0("wage table fy ",
    input$current_fy, ".csv")), row.names=FALSE)
wage_table <- fread(file.path(input$data_fy_wd,
  paste0("wage table fy ", input$current_fy, ".csv")))
# merge with all possibilities
wage_table <- merge(graph$all_combinations, wage_table,
                    all=TRUE, by=c("team", "mon_fy"))
# re-order - may not be necessary
wage_table <- wage_table[order(team, as.yearmon(mon_fy))]
# factor based on 12 consecutive months
wage_table[, mon_fy := factor(mon_fy,
  levels = as.chr(as.yearmon(input$fy_start)+0:11/12))]

p_wage <- list()
for(i in seq_along(team_list)) {
  # create graph p_health
  p_wage[[i]] <-
    ggplot(data = wage_table[team==team_list[i]],
      aes(x = mon_fy, fill = mon_fy, y = con_errors,
          ymax = max(round(con_errors)+1, 1.25*con_errors)))+
    geom_bar(width = 0.5, stat = "identity", color = "black", size=1,
             fill = "#C282D6", position = position_dodge(0.5))+
    labs(title = paste0(team_list[i], " Graph ", i,
      ".5.1 - # Consumers w/ Minimum Wage Errors\n(Report 2205 ran ",
      input$run_date, ")"),
      y="# consumers with\n minimum wage errors")+
    geom_text(vjust = 0.5, hjust = -0.5, aes(x = mon_fy, fill = mon_fy,
      y = con_errors, label = con_errors), angle = 90,
      position = position_dodge(0.5), size = 3, na.rm = TRUE)+
    aux$my_theme+
    theme(legend.position="top",
          panel.grid.major = element_line(colour = "grey80", size = 0.2),
          panel.grid.minor = element_line(colour = "grey80", size = .2 ),
          axis.title.y = element_text(colour = "grey30", size=8),
          axis.text.x = element_text(angle=45, hjust=1),
          plot.margin = unit( c(0, 0, -.2, 0) , "in" ),
          panel.grid.major.x = element_blank() )+
    # integer only in breaks
    scale_y_discrete(breaks = aux$my_breaks(n = wage_table[team ==
      team_list[i]][, max(con_errors, na.rm=TRUE)]))
}
names(p_wage) <- team_list
# self sufficiency matrix -----------------------------------------------------
# team/supervisor
p_ssm_ts <- ggplot(data = ssm_ts, aes_(x = ~ts, y = ~`pct 2+`)) +
  geom_bar(stat = "identity", color = "black", width = 0.5,
           position = position_dodge(0.5), fill = "#C282D6") +
  ggtitle(expression(atop("At Least 2 SSMs since March 2014",
                          atop(italic("case load inside parentheses"), "")))) +
  labs(x = "team: supervisor", y = "percent") +
  geom_label(data=ssm_ts, aes(x = ts, y = 1,
    label = sprintf('%1$s%% (%2$s / %3$s)', round(`pct 2+`),
                    ssm_ts[, `2+ SSMs`], num_cases)),
    hjust = "inward", fontface= "bold", label.padding = unit(.25, "lines")) +
  coord_flip() +
  theme(panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(fill = NA,
                                    colour = "grey50"),
        panel.grid.major = element_line(colour = "grey70",size = 0.2),
        panel.grid.minor = element_line(colour = "grey85",
                                        size = .2 ),
        plot.title = element_text(size=10))
# team/supervisor/author
p_ssm_tsa <- ggplot(data = ssm_tsa, aes_(x = ~primary_staff, y = ~`pct 2+`, ymax = 120)) +
  geom_bar(stat = "identity", color = "black", width = 0.5,
           position = position_dodge(0.5), fill = "#C282D6") +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(data=ssm_tsa, position = position_dodge(0.5), angle = 0,
    aes(x = primary_staff, y = `pct 2+`, vjust = -0.1,
        label = round(`pct 2+`)), fontface= "bold") +
  facet_wrap(~ts, scales = "free_x", nrow = 2) +
  aux$my_theme +
  scale_y_discrete(breaks = seq(0, 100, 25))

# http://stackoverflow.com/questions/31572239/set-space-in-facet-wrap-like-in-facet-grid
