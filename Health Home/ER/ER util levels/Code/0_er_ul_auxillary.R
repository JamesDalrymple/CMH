### fund only auxillary ###
pkg_loader(packages = c("data.table", "zoo", "xlsx", "RODBC", # "boot"
                        "ReporteRs", "ggplot2"))

aux <- new.env(parent = .GlobalEnv)

aux$evidence_p <- function(x) {
  cut(x, breaks = c(0, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("convincing", "strong", "moderate", "weak", "no"))
}

aux$evidence_symbol <- function(x) {
  cut(x, breaks = c(0, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", "+", "-"))
}


aux$util_cat <- function(x) {
  cut(x, breaks = c(-Inf, 0, 2, 3), labels = c("L0", "L1-L2", "L3"))
}

aux$colors <- c("mediumseagreen", "firebrick4", "orangered", "dodgerblue2",
                "royalblue2")

aux$outcome_colors <- c("lightcyan1", "lightskyblue")
aux$my_theme <-
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 12),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(fill = NA,
                                colour = "grey50"),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_line(colour = "grey80",
                                    size = .2)
  )

# function for categorizing util_level changes
aux$level_change <- function(x_cur, x_prev) {
  cut(x_cur-x_prev, breaks = c(-Inf, 0, 0.1, Inf), right = FALSE,
      labels = c("moved down", "maintained", "moved up"))
}
