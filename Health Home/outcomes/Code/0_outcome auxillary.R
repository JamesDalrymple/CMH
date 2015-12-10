### fund only auxillary ###
pkg_loader(packages = c("gdata", "data.table", "zoo", "xlsx", "RODBC",
                        "ReporteRs"))

aux <- new.env(parent = .GlobalEnv)

aux$all_na <- function(x) all(is.na(x))

# blood pressure categorization
# http://www.disabled-world.com/artman/publish/bloodpressurechart.shtml

aux$systolic_cat <- function(age, systolic) {
  breaks_under_60 <- c(0, 50, 60, 90, 110, 120, 130, 140, 160, 180, Inf)
  breaks_60_plus <- c(0, 50, 60, 90, 110, 120, 130, 150, 160, 180, Inf)
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

aux$diastolic_cat <- function(age, diastolic) {
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

aux$avg_sys <- c(120, 120, 121, 122, 123, 125, 127, 129, 131, 134)
aux$avg_dia <- c(85, 79, 80, 81, 82, 83, 84, 85, 86, 87)

# # delete later
# age = c(18, 22, 42, 19)
# systolic <- c(122, 201, 68, 120)
# aux$systolic_distance(age, systolic)

aux$systolic_distance <- function(age, systolic) {
  age_lower <- c(17,  seq(20, 60, by = 5))
  avg_sys_loc <- cut(age, breaks = c(17, seq(20, 60, by = 5), Inf ),
                     labels = FALSE, include.lowest = TRUE, right = FALSE)
  expected_avg <- aux$avg_sys[avg_sys_loc]
  sys_deviation <- round((systolic-expected_avg)/expected_avg, 3)
  return(sys_deviation)
}

aux$diastolic_distance <- function(age, diastolic) {
  age_lower <- c(17,  seq(20, 60, by = 5))
  avg_dia_loc <- cut(age, breaks = c(17, seq(20, 60, by = 5), Inf ),
                     labels = FALSE, include.lowest = TRUE, right = FALSE)
  expected_avg <- aux$avg_dia[avg_dia_loc]
  dia_deviation <- round((diastolic-expected_avg)/expected_avg, 3)
  return(dia_deviation)
}

aux$systolic_distance <- function(age, systolic) {
  age_lower <- c(17,  seq(20, 60, by = 5))
  avg_sys_loc <- cut(age, breaks = c(17, seq(20, 60, by = 5), Inf ),
                     labels = FALSE, include.lowest = TRUE, right = FALSE)
  expected_avg <- aux$avg_sys[avg_sys_loc]
  sys_deviation <- round(abs(expected_avg-systolic)/expected_avg, 3)
  return(sys_deviation)
}

aux$detail_cat_eval <- function(detail1, detail2) {
  detail_dt <- data.table(detail1 = as.numeric(detail1),
                          detail2 = as.numeric(detail2))
  detail_dt[, detail1_norm := (detail1 - 5)/5]
  detail_dt[, detail2_norm := (detail2 - 5)/5]
  detail_dt[detail2_norm < detail1_norm, status := "improved"]
  detail_dt[detail2_norm == detail1_norm, status := "maintained"]
  detail_dt[detail2_norm > detail1_norm, status := "decreased"]
  return(detail_dt[, status])
}

aux$dist_eval <- function(dist1, dist2) {
  dist_dt <- data.table(dist1 = dist1, dist2 = dist2)
  dist_dt[dist2 < dist1, status := "improved"]
  dist_dt[dist2 == dist1, status := "maintained"]
  dist_dt[dist2 > dist1, status := "decreased"]
  return(dist_dt[, status])
}
