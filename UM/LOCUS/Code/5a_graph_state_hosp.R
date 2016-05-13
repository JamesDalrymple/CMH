# MONTHLY/QUARTERLY/YEARLY ---
# cost (consumers)

state$graph$cost_consumers <- function(data) {
  data = state$agg$main[span_type == "mon"]
  ggplot(data) +
    geom_bar(stat = "identity", aes(x = span_label, y = cost),
             color = "black", size = 0.01, fill = cmh_colors$um[1]) +
    coord_flip() + theme_light() +
    scale_fill_manual(name=NULL, breaks = )

x <- data[, max(cost)-min(cost)]/5
x/(10^(nchar(x)-1))
    # theme_light()
    # theme_classic()
    # theme_minimal()

}


# (+ admissions, - discharges) enrolled
# adult vs child: enrolled
# adult vs child: cost (enrolled)
# cost & consumers grouped by state facility: cost (enrolled)
# FY ONLY: costs with budget projection