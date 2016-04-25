analyze <- new.env(parent = .GlobalEnv)
# Eligibility summary ---------------------------------------------------------
# saved$eligible # may be overkill information

# BMI summary
analyze$bmi <- list(
  hh = dcast(pp$bmi$hh, fill = 0, drop = FALSE, fun.aggregate = length,
        hh_cat ~ status, value.var = "case_no"),
  hh_lev = dcast(pp$bmi$hh_lev, fill = 0, drop = FALSE, fun.aggregate = length,
                 hh_cat  ~ status, value.var = "case_no"),
  hh_cmh = dcast(pp$bmi$hh_cmh, fill = 0, drop = FALSE, fun.aggregate = length,
      hh_cat + cmh_team ~ status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$bmi$hh_lev_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"))

# oh summary
analyze$oh <- list(
  hh = dcast(pp$wn$oh$hh, fill = 0, drop = FALSE, fun.aggregate = length,
             hh_cat ~ status, value.var = "case_no"),
  hh_lev = dcast(pp$wn$oh$hh_lev, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat  ~ status, value.var = "case_no"),
  hh_cmh = dcast(pp$wn$oh$hh_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$wn$oh$hh_lev_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"))

# pain summary
analyze$pain <- list(
  hh = dcast(pp$wn$pain$hh, fill = 0, drop = FALSE, fun.aggregate = length,
             hh_cat ~ status, value.var = "case_no"),
  hh_lev = dcast(pp$wn$pain$hh_lev, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat  ~ status, value.var = "case_no"),
  hh_cmh = dcast(pp$wn$pain$hh_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$wn$pain$hh_lev_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"))

# BP:diastolic summary
analyze$dia <- list(
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
analyze$sys <- list(
  hh = dcast(pp$bp$hh, fill = 0, drop = FALSE, fun.aggregate = length,
             hh_cat ~ sys_status, value.var = "case_no"),
  hh_lev = dcast(pp$bp$hh_lev, fill = 0, drop = FALSE, fun.aggregate = length,
                 hh_cat  ~ sys_status, value.var = "case_no"),
  hh_cmh = dcast(pp$bp$hh_cmh, fill = 0, drop = FALSE, fun.aggregate = length,
                 hh_cat + cmh_team ~ sys_status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$bp$hh_lev_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ sys_status,
    value.var = "case_no"))

# lab:chol
analyze$chol <- list(
  hh = dcast(pp$labs$chol$hh, fill = 0, drop = FALSE, fun.aggregate = length,
    hh_cat ~ status, value.var = "case_no"),
  hh_lev = dcast(pp$labs$chol$hh_lev, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat  ~ status, value.var = "case_no"),
  hh_cmh = dcast(pp$labs$chol$hh_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$labs$chol$hh_lev_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"))
# lab:trig
analyze$trig <- list(hh = dcast(
  pp$labs$trig$hh, fill = 0, drop = FALSE, fun.aggregate = length,
    hh_cat ~ status, value.var = "case_no"),
  hh_lev = dcast(pp$labs$trig$hh_lev, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat  ~ status, value.var = "case_no"),
  hh_cmh = dcast(pp$labs$trig$hh_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$labs$trig$hh_lev_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"))
# lab:a1c
analyze$a1c <- list(
  hh = dcast(pp$labs$a1c$hh, fill = 0, drop = FALSE, fun.aggregate = length,
      hh_cat ~ status, value.var = "case_no"),
  hh_lev = dcast(pp$labs$a1c$hh_lev, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat  ~ status, value.var = "case_no"),
  hh_cmh = dcast(pp$labs$a1c$hh_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$labs$a1c$hh_lev_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"))
# lab:glucose
analyze$gluc <- list(
  hh = dcast(pp$labs$gluc$hh, fill = 0, drop = FALSE, fun.aggregate = length,
             hh_cat ~ status, value.var = "case_no"),
  hh_lev = dcast(pp$labs$gluc$hh_lev, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat  ~ status, value.var = "case_no"),
  hh_cmh = dcast(pp$labs$gluc$hh_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$labs$gluc$hh_lev_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"))
# lab:hdl
analyze$hdl <- list(
  hh = dcast(pp$labs$hdl$hh, fill = 0, drop = FALSE, fun.aggregate = length,
      hh_cat ~ status, value.var = "case_no"),
  hh_lev = dcast(pp$labs$hdl$hh_lev, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat  ~ status, value.var = "case_no"),
  hh_cmh = dcast(pp$labs$hdl$hh_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$labs$hdl$hh_lev_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"))
# lab:ldl
analyze$ldl <- list(
  hh = dcast(pp$labs$ldl$hh, fill = 0, drop = FALSE, fun.aggregate = length,
             hh_cat ~ status, value.var = "case_no"),
  hh_lev = dcast(pp$labs$ldl$hh_lev, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat  ~ status, value.var = "case_no"),
  hh_cmh = dcast(pp$labs$ldl$hh_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"),
  hh_lev_cmh = dcast(pp$labs$ldl$hh_lev_cmh, fill = 0, drop = FALSE,
    fun.aggregate = length, hh_cat + cmh_team ~ status, value.var = "case_no"))


