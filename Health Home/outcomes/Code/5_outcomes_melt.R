mlt <- new.env(parent = .GlobalEnv)

mlt$bmi <- list(
  hh = melt(cast$bmi$hh, id.vars = Cs(hh_cat)),
  hh_lev = melt(cast$bmi$hh_lev, id.vars = Cs(hh_cat)),
  hh_cmh = melt(cast$bmi$hh_cmh, id.vars = Cs(hh_cat, cmh_team)),
  hh_lev_cmh = melt(cast$bmi$hh_lev_cmh, id.vars = Cs(hh_cat, cmh_team)))

# for loop to cycle through possibilities?
expression(list(
  hh = melt(cast$bmi$hh, id.vars = Cs(hh_cat)),
  hh_lev = melt(cast$bmi$hh_lev, id.vars = Cs(hh_cat)),
  hh_cmh = melt(cast$bmi$hh_cmh, id.vars = Cs(hh_cat, cmh_team)),
  hh_lev_cmh = melt(cast$bmi$hh_lev_cmh, id.vars = Cs(hh_cat, cmh_team))))