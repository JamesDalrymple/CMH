### fund only auxillary ###
options(java.parameters = "- Xmx1024m")
pkg_loader(packages = c("gdata", "data.table", "zoo", "xlsx", "RODBC",
  "SASxport", "ReporteRs", "ggplot2", "plyr", "RColorBrewer", "sqldf"),
  repos = "https://cran.mtu.edu/")
aux <- new.env(parent = .GlobalEnv)

seq.data.table <- function(x) {
  seq.int(nrow(x))
}