### fund only auxillary ###
library(wccmh)
pkg_loader(packages = c("gdata", "data.table", "zoo", "xlsx", "RODBC"))

aux <- new.env(parent = .GlobalEnv)