stat <- new.env(parent = .GlobalEnv)

stat$status_dt <-  data.table(
  status = Cs(regressed, maintained, improved), status_num = c(-1:1))
cmg$comb[stat$status_dt, status_num := i.status_num, on = c("status")]


cmg$comb[,
         wilcox.test(status_num, exact = FALSE)$p.value,
         keyby = list(var, cat, hh_cat)]