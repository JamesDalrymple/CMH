### fund only auxillary ###
pkg_loader(packages = c("gdata", "data.table", "zoo", "xlsx", "RODBC",
                        "ReporteRs", "ggplot2"))

aux <- new.env(parent = .GlobalEnv)

aux$all_na <- function(x) all(is.na(x))

# journal of american medical association (JAMA), plus hybrid for missing data
aux$bp_jama <-
  data.table(
    `systolic 60+` = c("0-50", "51-89", "90-149", "150-179", "180+"),
    `systolic < 60` = c("0-50", "51-89", "90-139", "140-179", "180+"),
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
  input_dt[before_id < 0 & before_id > after_id, category := "regressed"]
  input_dt[before_id >= 0 & before_id < after_id, category := "regressed"]
  input_dt[before_id >= 0 & after_id >= 0 & before_id > after_id,
           category := "improved"]
  input_dt[before_id >= 0 & after_id < 0 & before_id > after_id,
           category := "regressed"]
  # perhaps this case is a data error? only one case, 12317, 12/10/15
  input_dt[abs(before_id) == abs(after_id) &
             (before!=after), category := "maintained"]
  return(input_dt[, category])
}

aux$outcome_colors <- c("lightcyan1", "lightskyblue")
aux$my_theme <- theme(legend.position = "top",
                 panel.background = element_rect(fill = "white", colour = NA),
                 panel.border = element_rect(fill = NA,
                                             colour = "grey50"),
                 panel.grid.major = element_line(colour = "white",size = 0.2),
                 panel.grid.minor = element_line(colour = "white",
                                                 size = .2 ),
                 strip.background = element_rect(fill = "white", colour = "white"),
                 plot.title = element_text(size=10))