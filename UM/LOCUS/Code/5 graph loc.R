graph <- new.env(parent = .GlobalEnv)

# adm/disc/active w/ consumers admitted over 30 days --------------------------
# all #
graph$adm_status$all <- ggplot(data = agg$adm_status$all,
       aes(x = span_label, y = active)) +
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  geom_text(size = 3, stat = "identity", aes(x = span_label, y = 0,
    label = paste0("+", adm, "/", "-", disc) ), hjust = -.1)+
  theme_light() + coord_flip()+
  labs(title = "WCCMH: +Admissions, -Disc, Active consumers",
       y = "CMH core consumers active at least 30 days",
       x = NULL)
# MI program
graph$adm_status$mi <- ggplot(data = agg$adm_status$prog[program == "MI"],
  aes(x = span_label, y = active, ymax = 1.1*active)) +
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  geom_text(size = 3, stat = "identity", aes(x = span_label, y = 0,
    label = paste0("+", adm, "/", "-", disc) ), hjust = -.1)+
  geom_text(size = 3, stat = "identity", aes(x = span_label, y = active,
    label = active), hjust = -0.1)+
  theme_light() + coord_flip()+
  labs(title = "MI Program: +Admissions, -Disc, Active consumers",
       y = "core consumers active at least 30 days",
       x = NULL)
# DD program
graph$adm_status$dd <- ggplot(data = agg$adm_status$prog[program == "DD"],
  aes(x = span_label, y = active, ymax = 1.1*active)) +
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
    width = 1)+
  geom_text(size = 3, stat = "identity", aes(x = span_label, y = 0,
    label = paste0("+", adm, "/", "-", disc) ), hjust = -.1)+
  geom_text(size = 3, stat = "identity", aes(x = span_label, y = active,
    label = active), hjust = -0.1)+
  theme_light() + coord_flip()+
  labs(title = "DD Program: +Admissions, -Disc, Active consumers",
       y = "core consumers active at least 30 days",
       x = NULL)
# Y&F program
graph$adm_status$yf <- ggplot(data = agg$adm_status$prog[program == "Y&F"],
    aes(x = span_label, y = active, ymax = 1.1*active)) +
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
   width = 1)+
  geom_text(size = 3, stat = "identity", aes(x = span_label, y = 0,
    label = paste0("+", adm, "/", "-", disc) ), hjust = -.1)+
  geom_text(size = 3, stat = "identity", aes(x = span_label, y = active,
    label = active), hjust = -0.1)+
  theme_light() + coord_flip()+
  labs(title = "Y&F Program: +Admissions, -Disc, Active consumers",
       y = "core consumers active at least 30 days",
       x = NULL)
# Access program
graph$adm_status$yf <- ggplot(data = agg$adm_status$prog[program == "Access"],
                              aes(x = span_label, y = active, ymax = 1.1*active)) +
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  geom_text(size = 3, stat = "identity", aes(x = span_label, y = 0,
    label = paste0("+", adm, "/", "-", disc) ), hjust = -.1)+
  geom_text(size = 3, stat = "identity", aes(x = span_label, y = active,
    label = active), hjust = -0.1)+
  theme_light() + coord_flip()+
  labs(title = "Access Program: +Admissions, -Disc, Active consumers",
       y = "core consumers active at least 30 days",
       x = NULL)

# services for new consumers locus <= 2 ---------------------------------------
# locus - 3 months
graph$loc_3mon$all <-
  ggplot(data = agg$loc_3mon$all, aes(x = um_desc, y = total_units))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("WCCMH: Services First Six Months"),
    atop(italic(.("New Core CMH Consumers Locus 2 and Below")), ""))),
    y = "total number of units",
    x = "UM Code Description")
graph$loc_3mon$mi <-
  ggplot(data = agg$loc_3mon$prog[program == "MI"], aes(x = um_desc, y = total_units))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("MI Program: Services First Six Months"),
                           atop(italic(.("New Core CMH Consumers Locus 2 and Below")), ""))),
       y = "total number of units",
       x = "UM Code Description")
graph$loc_3mon$dd <-
  ggplot(data = agg$loc_3mon$prog[program == "DD"], aes(x = um_desc, y = total_units))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("DD Program: Services First Six Months"),
                           atop(italic(.("New Core CMH Consumers Locus 2 and Below")), ""))),
       y = "total number of units",
       x = "UM Code Description")
graph$loc_3mon$access <-
  ggplot(data = agg$loc_3mon$prog[program == "Access"], aes(x = um_desc, y = total_units))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("Y&F Program: Services First Six Months"),
                           atop(italic(.("New Core CMH Consumers Locus 2 and Below")), ""))),
       y = "total number of units",
       x = "UM Code Description")
# locus - 6 months
graph$loc_6mon$all <-
  ggplot(data = agg$loc_6mon$all, aes(x = um_desc, y = total_units))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("WCCMH: Services First Six Months"),
    atop(italic(.("New Core CMH Consumers Locus 2 and Below")), ""))),
       y = "total number of units",
       x = "UM Code Description")
graph$loc_6mon$mi <-
  ggplot(data = agg$loc_6mon$prog[program == "MI"],
         aes(x = um_desc, y = total_units))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("MI Program: Services First Six Months"),
                           atop(italic(.("New Core CMH Consumers Locus 2 and Below")), ""))),
       y = "total number of units",
       x = "UM Code Description")
graph$loc_6mon$dd <-
  ggplot(data = agg$loc_6mon$prog[program == "DD"],
         aes(x = um_desc, y = total_units))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("DD Program: Services First Six Months"),
                           atop(italic(.("New Core CMH Consumers Locus 2 and Below")), ""))),
       y = "total number of units",
       x = "UM Code Description")
graph$loc_6mon$access <-
  ggplot(data = agg$loc_6mon$prog[program == "Access"],
         aes(x = um_desc, y = total_units))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("Access Program: Services First Six Months"),
                           atop(italic(.("New Core CMH Consumers Locus 2 and Below")), ""))),
       y = "total number of units",
       x = "UM Code Description")
# New Admissions --------------------------------------------------------------
agg$new_adm$all


graph$new_adm$all <- ggplot(data = agg$new_adm$all,
       aes(x = span_label, y = num_adm, ymax = 1.1*num_adm))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  geom_text(aes(x = span_label, y = num_adm, label = num_adm), hjust = -.1)+
  labs(title = bquote(atop(.("WCCMH: New Core CMH Admissions"),
                           atop(italic(.("Program transfers do not count")), ""))),
       y = "Locus Levels",
       x = "number of new core CMH Admissions")
graph$new_adm$mi <- ggplot(data = agg$new_adm$prog[program == "MI"],
                            aes(x = span_label, y = num_adm, ymax = 1.1*num_adm))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  geom_text(aes(x = span_label, y = num_adm, label = num_adm), hjust = -.1)+
  labs(title = bquote(atop(.("MI Program: New Core CMH Admissions"),
                           atop(italic(.("Program transfers do not count")), ""))),
       y = "Locus Levels",
       x = "number of new core CMH Admissions")
graph$new_adm$dd <- ggplot(data = agg$new_adm$prog[program == "DD"],
                           aes(x = span_label, y = num_adm, ymax = 1.1*num_adm))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  geom_text(aes(x = span_label, y = num_adm, label = num_adm), hjust = -.1)+
  labs(title = bquote(atop(.("DD Program: New Core CMH Admissions"),
                           atop(italic(.("Program transfers do not count")), ""))),
       y = "Locus Levels",
       x = "number of new core CMH Admissions")
graph$new_adm$yf <- ggplot(data = agg$new_adm$prog[program == "Y&F"],
                           aes(x = span_label, y = num_adm, ymax = 1.1*num_adm))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  geom_text(aes(x = span_label, y = num_adm, label = num_adm), hjust = -.1)+
  labs(title = bquote(atop(.("Y&F Program: New Core CMH Admissions"),
                           atop(italic(.("Program transfers do not count")), ""))),
       y = "Locus Levels",
       x = "number of new core CMH Admissions")
graph$new_adm$access <- ggplot(data = agg$new_adm$prog[program == "Access"],
                           aes(x = span_label, y = num_adm, ymax = 1.1*num_adm))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  geom_text(aes(x = span_label, y = num_adm, label = num_adm), hjust = -.1)+
  labs(title = bquote(atop(.("Access Program: New Core CMH Admissions"),
                           atop(italic(.("Program transfers do not count")), ""))),
       y = "Locus Levels",
       x = "number of new core CMH Admissions")
