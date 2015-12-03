### fund only auxillary ###
pkg_loader(packages = c("gdata", "data.table", "zoo", "xlsx", "RODBC"))

aux <- new.env(parent = .GlobalEnv)

aux$all_na <- function(x) all(is.na(x))