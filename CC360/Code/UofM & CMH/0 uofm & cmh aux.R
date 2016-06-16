### fund only auxillary ###
options(java.parameters = "- Xmx1024m")
pkg_loader(packages = c("gdata", "data.table", "zoo", "xlsx", "RODBC",
  "ReporteRs", "ggplot2", "RColorBrewer", "stringi"),
  repos = "https://cran.mtu.edu/")
aux <- new.env(parent = .GlobalEnv)
