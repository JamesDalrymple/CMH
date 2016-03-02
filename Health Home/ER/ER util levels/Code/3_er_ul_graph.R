graph <- new.env(parent = .GlobalEnv)
graph$theme_new <- theme_set(theme_light())
graph$theme_new <- theme_update(
  axis.text.x = element_text(angle = 90, color = "black"),
  axis.title.x = element_blank()
)
# ER graph
graph$util_er <- ggplot(data = modify$er_agg) +
  geom_point(stat = "identity",
             aes(x = mon_abr, y = er_rate, fill = fy, color = fy),
             show.legend = TRUE, na.rm = TRUE) +
  geom_path(stat = "identity",
            aes(x = mon_abr, y = er_rate, group = fy,
                fill = fy, color = fy),
            show.legend = FALSE, na.rm = TRUE) +
  geom_text(aes(x = mon_abr, y = 0, fill = fy, label = evidence_symbol),
            color = "black", angle = 90, size = 4) +
  facet_wrap(~ team + util_cat) +
  scale_color_manual(name = "Fiscal Year",
                     values = c("dodgerblue2", "orange2")) +
  scale_fill_manual(name = "Fiscal Year",
                    values = c("dodgerblue2", "orange2")) +
  labs(title = "Health Home Consumers ER Visits",
       y = "rate of ER Visits (ER Visits/consumers)") +
  aux$my_theme +
  scale_y_continuous(breaks = function(x) seq(0, max(x), by = .2))
# movement: all
graph$movement_a <- ggplot(modify$lvl_agg) +
  geom_point(stat = "identity",
    aes(x = mon, y = cases, color = level_change, group = level_change)) +
  geom_path(stat = "identity",
    aes(x = mon, y = cases, color = level_change, group = level_change)) +
  facet_wrap(~ team, scales = "free")  +
  labs(title = "Utilization Level Movement") +
  aux$my_theme
# movement: without maintained
graph$movement_b <- ggplot(modify$lvl_agg[level_change != "maintained"]) +
  geom_point(stat = "identity",
    aes(x = mon, y = cases, color = level_change, group = level_change)) +
  geom_path(stat = "identity",
    aes(x = mon, y = cases, color = level_change, group = level_change)) +
  facet_wrap(~ team, scales = "free")  +
  labs(title = "Utilization Level Movement w/o maintained") +
  aux$my_theme