# cast, melt, graph
cmg <- new.env(parent = .GlobalEnv)
# Eligibility summary ---------------------------------------------------------
# saved$eligible # may be overkill information
cmg$vars <- c("labs$gluc", "labs$ldl", "labs$a1c", "labs$trig", "wn$oh", "wn$pain", "bmi",
          "labs$chol", "labs$hdl")

aux$cast <- function(x) {
  eval(parse(text = sprintf(
    "cmg$cast$%1$s <- list(
  hh = dcast(pp$%1$s$hh, fill = 0, drop = FALSE, fun.aggregate = length,
             hh_cat ~ status, value.var = 'case_no'),
  hh_lev = dcast(pp$%1$s$hh_lev, fill = 0, drop = FALSE, fun.aggregate = length,
                 hh_cat  ~ status, value.var = 'case_no'),
  hh_cmh = dcast(pp$%1$s$hh_cmh, fill = 0, drop = FALSE, fun.aggregate = length,
                 hh_cat + cmh_team ~ status, value.var = 'case_no'),
  hh_lev_cmh = dcast(pp$%1$s$hh_lev_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = 'case_no')
  )", x)))
  invisible()
}
plyr::l_ply(cmg$vars, aux$cast)
cmg$vars <- c(cmg$vars, "dia", "sys")

# BP:diastolic summary
cmg$cast$dia <- list(
  hh = dcast(pp$bp$hh, fill = 0, drop = FALSE, fun.aggregate = length,
             hh_cat ~ dia_status, value.var = "case_no"),
  hh_lev = dcast(pp$bp$hh_lev, fill = 0, drop = FALSE, fun.aggregate = length,
                 hh_cat  ~ dia_status, value.var = "case_no"),
  hh_cmh = dcast(pp$bp$hh_cmh, fill = 0, drop = FALSE, fun.aggregate = length,
                 hh_cat + cmh_team ~ dia_status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$bp$hh_lev_cmh, fill = 0, drop = FALSE,
                     fun.aggregate = length, hh_cat + cmh_team ~ dia_status,
                     value.var = "case_no"))

# BP:systolic summary
cmg$cast$sys <- list(
  hh = dcast(pp$bp$hh, fill = 0, drop = FALSE, fun.aggregate = length,
             hh_cat ~ sys_status, value.var = "case_no"),
  hh_lev = dcast(pp$bp$hh_lev, fill = 0, drop = FALSE, fun.aggregate = length,
                 hh_cat  ~ sys_status, value.var = "case_no"),
  hh_cmh = dcast(pp$bp$hh_cmh, fill = 0, drop = FALSE, fun.aggregate = length,
                 hh_cat + cmh_team ~ sys_status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$bp$hh_lev_cmh, fill = 0, drop = FALSE,
                     fun.aggregate = length, hh_cat + cmh_team ~ sys_status,
                     value.var = "case_no"))

aux$melt <- function(x) {
  eval(parse(text = sprintf(
    "cmg$mlt$%1$s <- list(
    hh = melt(cmg$cast$%1$s$hh, id.vars = Cs(hh_cat)),
    hh_lev = melt(cmg$cast$%1$s$hh_lev, id.vars = Cs(hh_cat)),
    hh_cmh = melt(cmg$cast$%1$s$hh_cmh, id.vars = Cs(hh_cat, cmh_team)),
    hh_lev_cmh = melt(cmg$cast$%1$s$hh_lev_cmh, id.vars = Cs(hh_cat, cmh_team))
  )", x
)))
  invisible()
}
plyr::l_ply(cmg$vars, aux$melt)

attr(cmg$vars, "names") <- c("Glucose", "LDL", "A1C", "Triglycerides",
  "Overall Health", "Pain", "BMI", "Cholesterol", "HDL",
  "Diastolic Blood Pressure", "Systolic Blood Pressure")

invisible(Map(f = function(fmt, ...) {
    eval(parse(text = sprintf(fmt, ...)))
  },
  'cmg$plot$%1$s <-
  ggplot(data = cmg$mlt$%1$s$hh,
       aes(x = variable, y = value, fill = hh_cat, ymax = 1.2*value)) +
  geom_bar(stat = "identity", position = position_dodge(0.5), width = 0.5,
           color = "black")+
  theme_light()+
  geom_text(aes(x = variable, y = value, fill = hh_cat, label = value),
            position = position_dodge(0.5), hjust = -0.1)+
  coord_flip()+
  scale_fill_manual(name = NULL, values = aux$colors[c(1, 3)])+
  labs(x = NULL, y = "number of consumers", title = "%2$s")',
    cmg$vars, names(cmg$vars)))