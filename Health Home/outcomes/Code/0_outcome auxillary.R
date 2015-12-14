### fund only auxillary ###
pkg_loader(packages = c("gdata", "data.table", "zoo", "xlsx", "RODBC",
                        "ReporteRs"))

aux <- new.env(parent = .GlobalEnv)

aux$all_na <- function(x) all(is.na(x))

# journal of american medical association (JAMA), plus hybrid for missing data
aux$bp_jama <-
  data.table(
    `systolic 60+` = c("0-50", "51-89", "90-149", "150-179", "180+"),
    `systolic >60` = c("0-50", "51-89", "90-139", "140-179", "180+"),
    diastolic = c("0-33", "34-59", "60-89", "90-100", "101+"),
    category = c("emergency hypotension", "hypotension", "normal",
                 "hypertension", "hypotension emergency"))
aux$sys_jama <- function(age, systolic) {
  breaks_under_60 <- c(0, 50, 89, 139, 179, Inf)
  breaks_60_plus <- c(0, 50, 89, 149, 179, Inf)
  labels <- c("emergency hypotension", "hypotension", "normal", "hypertension",
              "emergency hypertension")
  dt_sys <- data.table(age, systolic)
  dt_sys[age < 60, sys_cat := cut(x = systolic, breaks = breaks_under_60,
    right = FALSE, labels = labels, ordered_result = TRUE)]
  dt_sys[age >= 60, sys_cat := cut(x = systolic, breaks = breaks_60_plus,
    right = FALSE, labels = labels, ordered_result = TRUE)]
  return(dt_sys[, sys_cat])
}
aux$dia_jama <- function(age, diastolic) {
  breaks <- c(0, 33, 59, 89, 100, Inf)
  labels <- c("emergency hypotension", "hypotension", "normal", "hypertension",
              "emergency hypertension")
  dt_dia <- data.table(age, diastolic)
  dt_dia[, dia_cat := cut(x = diastolic, breaks = breaks,
    right = FALSE, labels = labels, ordered_result = TRUE)]
  return(dt_dia[, dia_cat])
}

# disabled world
aux$bp_dw <-
  data.table(systolic = c("0-50", "51-60", "61-90", "91-110", "111-120",
                        "121-130", "131-140", "141-160", "161-179", "180+"),
           diastolic = c("0-33", "34-40", "41-60", "61-75", "76-80", "81-85",
                         "86-90", "91-100", "101-110", "111-120+"),
           category = c("dangerously low", "too low", "borderline low",
                        "low normal", "normal", "high normal",
                        "hypertension: stage 1", "hypertension: stage 2",
                        "hypertension: stage 3", "hypertension: stage 4"),
           note = c(rep("", 5), "if age 60+, sys = 121-140",
                    "if age 60+, sys = 141-150", "if age 60+, sys = 151-160", "", ""))

aux$sys_dw <- function(age, systolic) {
  breaks_under_60 <- c(0, 50, 60, 90, 110, 120, 130, 140, 160, 180, Inf)
  breaks_60_plus <- c(0, 50, 60, 90, 110, 120, 140, 150, 160, 180, Inf)
  labels <- c("dangerously low bp", "too low bp", "borderline low bp",
             "low normal bp", "normal bp", "high normal bp", "hypertension stage 1",
             "hypertension stage 2", "hypertension stage 3", "hypertension stage 4")
  dt_sys <- data.table(age, systolic)
  dt_sys[age < 60, systolic_cat := cut(x = systolic, breaks = breaks_under_60,
         right = FALSE, labels = labels, ordered_result = TRUE)]
  dt_sys[age >= 60, systolic_cat := cut(x = systolic, breaks = breaks_60_plus,
         right = FALSE, labels = labels, ordered_result = TRUE)]
 return(dt_sys[, systolic_cat])
}

aux$dia_dw <- function(age, diastolic) {
  breaks <- c(0, 33, 40, 60, 75, 80, 85, 90, 100, 110, Inf)
  result <- cut(
    diastolic,
    breaks = breaks,
    right = FALSE,
    labels = c("dangerously low bp", "too low bp", "borderline low bp",
               "low normal bp", "normal bp", "high normal bp",
               "hypertension stage 1", "hypertension stage 2",
               "hypertension stage 3", "hypertension stage 4"),
    ordered_result = TRUE)
  return(result)
}

aux$dw_eval <- function(x1, x2) {
  dw_cats <- c("dangerously low bp", "too low bp", "borderline low bp",
               "low normal bp", "normal bp", "high normal bp",
               "hypertension stage 1", "hypertension stage 2",
               "hypertension stage 3", "hypertension stage 4")
  dw_dt <- data.table(dw_cats = dw_cats,
                      id = seq_along(dw_cats))
  input_dt <- data.table(before = x1, after = x2)
  input_dt[dw_dt, before_id :=  id, on = c(before = "dw_cats")]
  input_dt[dw_dt, after_id :=  id, on = c(after = "dw_cats")]
  input_dt[after_id < before_id & after_id > 3, status := "improved"]
  input_dt[after_id < before_id & after_id <= 3, status := "decreased"]
  input_dt[after_id > before_id & before_id <= 3, status := "improved"]
  input_dt[after_id > before_id & before_id > 3, status := "decreased"]
  input_dt[before_id == after_id, status := "maintained"]
  return(input_dt[, status])
}

aux$jama_eval <- function(x1, x2) {
  jama_cats <- c("emergency hypotension", "hypotension", "normal",
          "hypertension", "emergency hypertension")
  jama_dt <-
    data.table(jama_cats = jama_cats,
               id = seq_along(jama_cats)-median(seq_along(jama_cats)))
  input_dt <- data.table(before = x1, after = x2)
  input_dt[jama_dt, before_id :=  id, on = c(before = "jama_cats")]
  input_dt[jama_dt, after_id :=  id, on = c(after = "jama_cats")]
  # categorization
  input_dt[before_id == after_id, category := "maintained"]
  input_dt[before_id < 0 & before_id < after_id, category := "improved"]
  input_dt[before_id < 0 & before_id > after_id, category := "decreased"]
  input_dt[before_id >= 0 & before_id < after_id, category := "decreased"]
  input_dt[before_id >= 0 & after_id >= 0 & before_id > after_id,
           category := "improved"]
  input_dt[before_id >= 0 & after_id < 0 & before_id > after_id,
           category := "decreased"]
  # perhaps this case is a data error? only one case, 12317, 12/10/15
  input_dt[abs(before_id) == abs(after_id) &
             (before!=after), category := "maintained"]
  return(input_dt[, category])
}



# aux$detail_cat_eval <- function(detail1, detail2) {
#   detail_dt <- data.table(detail1 = as.numeric(detail1),
#                           detail2 = as.numeric(detail2))
#   detail_dt[, detail1_norm := (detail1 - 5)/5]
#   detail_dt[, detail2_norm := (detail2 - 5)/5]
#   detail_dt[detail2_norm < detail1_norm, status := "improved"]
#   detail_dt[detail2_norm == detail1_norm, status := "maintained"]
#   detail_dt[detail2_norm > detail1_norm, status := "decreased"]
#   return(detail_dt[, status])
# }