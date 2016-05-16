graph <- new.env(parent = .GlobalEnv)

# adm/disc/active w/ consumers admitted over 30 days
agg$adm_status[, span_label := factor(span_label,
  levels = agg$adm_status[, unique(span_label)])]
graph$adm_status <- ggplot(data = agg$adm_status,
       aes(x = span_label, y = active)) +
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  geom_text(size = 3, stat = "identity", aes(x = span_label, y = 0,
    label = paste0("+", adm, "/", "-", disc) ), hjust = -.1)+
  theme_light() + coord_flip()+
  labs(title = "+Admissions, -Disc, Active consumers",
       y = "CMH core consumers active at least 30 days",
       x = NULL)

# services for new consumers locus <= 2


graph$loc_3mon <-
  ggplot(data = agg$loc_3mon, aes(x = um_desc, y = total_units))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("Services First Six Months"),
    atop(italic(.("New Core CMH Consumers Locus 2 and Below")), ""))),
    y = "total number of units",
    x = "UM Code Description")

graph$loc_6mon <-
  ggplot(data = agg$loc_6mon, aes(x = um_desc, y = total_units))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("Services First Six Months"),
    atop(italic(.("New Core CMH Consumers Locus 2 and Below")), ""))),
       y = "total number of units",
       x = "UM Code Description")

graph$loc_6mon <- ggplot(data = agg$locus_levels,
       aes(x = factor(comb_disp), y = cases))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("Locus Score"),
    atop(italic(.("Override Replaces Calculated Score")), ""))),
       y = "Locus Levels",
       x = "number of consumers")

agg$new_adm[, span_label := factor(span_label, levels = agg$new_adm[, span_label])]
graph$new_adm <- ggplot(data = agg$new_adm,
       aes(x = span_label, y = num_adm))+
  geom_bar(stat = "identity", size = 0.1, color = "black", fill = "lightblue",
           width = 1)+
  theme_light() + coord_flip()+
  labs(title = bquote(atop(.("New Core CMH Admissions"),
                           atop(italic(.("Program transfers do not count")), ""))),
       y = "Locus Levels",
       x = "number of new core CMH Admissions")