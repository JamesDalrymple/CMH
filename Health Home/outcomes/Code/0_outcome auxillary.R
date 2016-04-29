### fund only auxillary ###
options(java.parameters = "- Xmx1024m")
pkg_loader(packages = c("gdata", "data.table", "zoo", "xlsx", "RODBC",
  "ReporteRs", "ggplot2", "plyr", "RColorBrewer"), repos = "https://cran.mtu.edu/")

aux <- new.env(parent = .GlobalEnv)


project_wd$results <- file.path(project_wd$results,
                                format(input$run_date, "%b_%d_%Y"))
# for folder
project_wd$data <- file.path(project_wd$data,
                             gsub(
                               x = input$run_date,
                               pattern = "/",
                               replace = "_"
                             ))
if (!dir.exists(project_wd$results)) {
  dir.create(project_wd$results)
  p_msg("directory created:", project_wd$results)
}


aux$colors <- RColorBrewer::brewer.pal(4, "Blues")

# used to check if all NA's in vector x ---
aux$all_na <- function(x) all(is.na(x))

# combine old and new error; new cannot be NA ---
aux$cat_error <- function(old, new) {
  old[is.na(old)] <- ""
  if (any(is.na(new))) p_stop("new cannot be NA.")
  result <- ifelse(old=="", new, paste(old, new, sep = "|"))
  return(result)
}
# old = c(NA, NA, "bad1")
# new = c("bad2", "bad2", "bad3")
# aux$cat_error(old, new)

# bmi formula
aux$calc_bmi <- function(lb, inches) {
  result <- 703*lb/inches^2
  return(result)
}

# a better way to calculate BMI, but not mainstream so not using, per MH ---
# aux$new_bmi <- function(lb, inches) {
#   result <- 5734*lb/inches^2.5
#   return(result)
# }

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

aux$bp_cat <- function(pre, post) {
  cat_class = c("emergency hypotension", "hypotension", "normal",
                "hypertension", "emergency hypertension")
  cat_diff <- abs(as.int(factor(post, levels = cat_class, labels = -2:2))) -
    abs(as.int(factor(pre, levels = cat_class, labels = -2:2)))
  result <- cut(cat_diff, breaks = c(-Inf, -.01, .01, Inf),
                labels = c("regressed", "maintained", "improved"))
  return(as.character(result))
}

# no longer used ---
# aux$health_num <- function(x) {
#   x[is.na(x)] <- "No Response"
#   as.int(factor(x, levels = Cs("No Response", Poor, Fair, Good, Excellent),
#                 labels = as.chr(c(9, 1:4))))
# }

# no longer used ---
# aux$health_compare <- function(pre, post) {
#   post <- aux$health_num(post)
#   pre <- aux$health_num(pre)
#   post[post==9] <- NA
#   pre[pre==9] <- NA
#   comp_lab <- cut(post-pre, breaks = c(-Inf, -.1, 0.1, Inf),
#                   labels = c("regressed", "maintained", "improved"))
#   return(as.chr(comp_lab))
# }

aux$wn_oh_cat <- function(pre_oh, post_oh){
  pre_oh <- factor(pre_oh, levels = c("Poor", "Fair", "Good", "Excellent"))
  post_oh <- factor(post_oh, levels = c("Poor", "Fair", "Good", "Excellent"))
  pre_oh <- as.integer(pre_oh)
  post_oh <- as.integer(post_oh)
  diff_oh <- post_oh - pre_oh
  result <- cut(diff_oh, breaks = c(-Inf, -.1, 0.1, Inf),
                labels = c("regressed", "maintained", "improved"))
  return(result)
}

aux$pain_num <- function(x) {
  x[is.na(x)] <- "No Response"
  as.int(factor(x,
  levels = Cs("No Response", None, Rarely, Mild, Moderate, Severe, Chronic),
  labels = as.chr(c(9, 6:1))))
}

# x <- c("None", "Moderate", "Chronic", "Mild", "Rarely", "Severe")
# aux$pain_num(x)
# aux$pain_cat(pre = "Chronic", post = "Chronic")
# aux$pain_cat(pre = "Severe", post = "Moderate")

aux$wn_pain_cat <- function(pre, post) {
  post <- aux$pain_num(post)
  pre <- aux$pain_num(pre)
  post[post==9] <- NA
  pre[pre==9] <- NA
  comp_lab <- cut(post-pre, breaks = c(-Inf, -.1, 0.1, Inf),
                  labels = c("regressed", "maintained", "improved"))
  return(as.chr(comp_lab))
}

# aux$pain_compare(pre = c(Cs(Mild, Rarely, Chronic, Moderate, Chronic, Severe)),
#                  post = c(Cs(None, Rarely, Mild, Moderate, Chronic, Severe)))

aux$ovr_health_reduce <- function(x) { # grab worst condition on given day
  x_fac <- factor(x, levels = c("Poor", "Fair", "Good", "Excellent"), ordered = TRUE)
  worst_cond <- sort(x_fac)[1]
  return(as.character(worst_cond))
}
# x <- c("Good", "Fair")
# x <- c("Poor", "Fair", "Good", "Excellent")
# aux$ovr_health_reduce(x)

# BMI categorization based on 2% rule (per Brandie 4/5/2016)
aux$bmi_cat <- function(pre_bmi, post_bmi){
  pp_dt <- data.table(pre_bmi, post_bmi)
  pp_dt[, bmi_class := ifelse(pre_bmi < 18.5, "bmi < 18.5", "bmi >= 18.5")]
  pp_dt[, delta_bmi := (post_bmi-pre_bmi)/pre_bmi]
  pp_dt[abs(delta_bmi) < 0.02, status := "maintained"]
  pp_dt[delta_bmi < -0.02 & pre_bmi >= 18.5, status := "improved"]
  pp_dt[delta_bmi < -0.02 & pre_bmi < 18.5, status := "regressed"]
  pp_dt[delta_bmi > 0.02 & pre_bmi >= 18.5, status := "regressed"]
  pp_dt[delta_bmi > 0.02 & pre_bmi < 18.5, status := "improved"]
  return(pp_dt[, status])
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

aux$health_home_nurse <- c("Achatz, Charles", "Byrd, Kelicia", "Toader, Andreea",
  "Lewis, Destiny", "VanHoeck, Marie", "Fellabaum, Kathleen")
health_home_staff <- c("Hershberger, Merton", "Rama, Linda")

# cholestoral ---
aux$chol_cut <- function(x) {
  closure_cut(x, breaks = c(i=0, ei = 200, ie = 240, e = Inf),
              label_vec = Cs(best, borderline, poor))
}
# aux$chol_cut(0, 200, 240, 300)

# glucose (FBS - fasting blood sugar) ---
aux$gluc_cut <- function(x) {
  closure_cut(x, breaks = c(i=0, ei = 70, ei = 100, ei = 125, e = Inf),
              label_vec = c("risky-", "acceptable", "risky+", "very risky+"))
}
# aux$gluc_cut(c(0, 70, 100, 125, 300))

# triglycerides ---
aux$trig_cut <- function(x) {
  closure_cut(x, breaks = c(i=0, ei = 150, ie = 200, e = Inf),
              label_vec = Cs(best, borderline, poor))
}
# aux$trig_cut(c(0, 150, 200, 220))

# A1-C ---
aux$a1c_cut <- function(x) {
  closure_cut(x, breaks = c(i=0, ei = 6, ei = 6.5, e = Inf),
              label_vec = Cs(normal, risky, "very risky"))
}
# aux$a1c_cut(c(0,6,6.5,7))

# HDL ---
aux$hdl_cut <- function(x) {
  closure_cut(x, breaks = c(i=0, ei = 40, ie = 60, e = Inf),
              label_vec = Cs(poor, borderline, best))
}
# aux$hdl_cut(c(0, 40, 60, 100))

# LDL ---
aux$ldl_cut <- function(x) {
  closure_cut(x, breaks = c(i=0, ei=100, ie=160, e = Inf),
              label_vec = Cs(poor, borderline, best))
}
# aux$ldl_cut(c(0,100,160,999))

aux$status <- function(change){
  ifelse(change > 0, "improved",
         ifelse(change < 0, "regressed", "maintained"))
}

aux$chol_change <- function(pre, post){
  chol_levels <- c("poor", "borderline", "best")
  change <-
    as.integer(factor(post, levels = chol_levels)) -
    as.integer(factor(pre, levels = chol_levels))
  aux$status(change)
}

### glucose needs special coding/rules for 'jumpers' ---
aux$gluc_levels <- c("risky-", "acceptable", "risky+", "very risky+")

aux$trig_change <- function(pre, post){
  trig_levels <- c("poor", "borderline", "best")
  change <-
    as.integer(factor(post, levels = trig_levels)) -
    as.integer(factor(pre, levels = trig_levels))
  aux$status(change)
}

aux$a1c_change <- function(pre, post){
  a1c_levels <- Cs(normal, risky, "very risky")
  change <-
    as.integer(factor(post, levels = a1c_levels)) -
    as.integer(factor(pre, levels = a1c_levels))
  aux$status(change)
}

aux$hdl_change <- function(pre, post){
  hdl_levels <- c("poor", "borderline", "best")
  change <-
    as.integer(factor(post, levels = hdl_levels)) -
    as.integer(factor(pre, levels = hdl_levels))
  aux$status(change)
}

aux$ldl_change <- function(pre, post){
  ldl_levels <- c("poor", "borderline", "best")
  change <-
    as.integer(factor(post, levels = ldl_levels)) -
    as.integer(factor(pre, levels = ldl_levels))
  aux$status(change)
}
