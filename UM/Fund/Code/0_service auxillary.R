### fund only auxillary ###
pkg_loader(packages = c("gdata", "data.table", "zoo", "xlsx", "RODBC"))

## date parameters - for ease of reading ##
input$start_date <- format(date_convert(input$end_date) - 365, "%m/%d/%Y")
input$start_par <- gsub(x = input$start_date, pattern = "/", replace = "_")
input$end_par <- gsub(x = input$end_date, pattern = "/", replace = "_")
input$run_par <- gsub(x = input$run_date, pattern = "/", replace = "_")
project_wd$results <-
  file.path(project_wd$dropbox, project_wd$results, project_wd$sub_folder,
            input$run_par)
aux <- new.env(parent = .GlobalEnv)
svc_agg <- new.env(parent = .GlobalEnv)
# UM code description
um_code_desc <- data.table(read.xlsx2(file.path(
  project_wd$dropbox,
  "Utilization Management/UM_Desc_MDCH_2015.xlsx"
), 1))

file_list <- list(
  csv_files = list.files(
    file.path(project_wd$data),
    pattern = ".csv",
    full.names = TRUE),
  rds_files = list.files(
    file.path(project_wd$data),
    pattern = ".rds",
    full.names = TRUE)
)

# while ("aux" %in% search()) {
#   detach(aux)
# }

# finding ranges using quantiles
aux$my_range <- function(x, q1 = .25, q2 = .75, type = 8) {
  r1 <- as.numeric(quantile(x, q1, type = type, na.rm = TRUE))
  r2 <- as.numeric(quantile(x, q2, type = type, na.rm = TRUE))
  if (identical(r1, r2)) {
    return(as.character(round(r1, 1)))
  } else {
    return(paste(round(r1, 1), round(r2, 1), sep = "-"))
  }
}

# finding ranges and making cost range into human readable format
aux$my_money_range <- function(x, q1 = 0.25, q2 = 0.75, type = 8) {
  r1 <- as.numeric(quantile(x, q1, type = type, na.rm = TRUE))
  r2 <- as.numeric(quantile(x, q2, type = type, na.rm = TRUE))
  if (identical(r1, r2)) {
    return(money_add(r1))
  } else {
    return(paste(money_add(r1), money_add(r2), sep = "-"))
  }
}

# determine if number is in range
aux$in_range <- function(x, range) {
  x <- as.numeric(x)
  range <- as.numeric(unlist(strsplit(range, split = "-")))
  if (identical(length(range), 1L)) {
    range <- c(range, range)
  }
  x %between% range
}

# convert locus scores to numbers
aux$word_to_num <- function(x) {
  result <- switch(x,
         "Level Zero" = 1,
         "Level One" = 1,
         "Level Two" = 2,
         "Level Three" = 3,
         "Level Four" = 4,
         "Level Five" = 5,
         "Level Six" = 3, # per Kelly B. 5/7/2015
         2)
  return(as.numeric(result))
}

# function to convert TCM to levels
aux$levelTCM <- function(x) {
  result <- cut(x, breaks = c(-Inf, 1, 3, 13, Inf),
                labels = c(0, 1, 2, 3), right=FALSE)
  result <- as.numeric(result)
  return(result)
}

# # narrow down team assignment to one per person per day
# aux$teamCMH <- c("WSH - Access/Engagement",
#             "Community Support and Treatment Services - CSTS",
#             "WSH - ACT",
#             "WSH - ATO",
#             "WSH - Children's Services",
#             "WSH - Children's Services - Home Based",
#             "WSH - DD Adult",
#             "WSH - MI - Adult")

# helper function for making file name for export
aux$fund_name <- function(x) {
  x <- paste(tolower(x), collapse="_")
  x <- gsub(x=x, pattern="/", replace="_")
  x <- gsub(x=x, pattern="-", replace="")
  return(x)
}

# x <- copy(non_cmh_crisis)
# x[case_no==1136878 & cost==Inf]
# fb_cpt_mod_cost[`PRI PROCEDURE CODE`=="T2038"]

aux$summary_services <- function(dt) {
  x <- copy(dt)
  # add fb cost
  x[fb_cpt_mod_cost,
    cost := units * i.unit_cost,
    on = c("cpt" = "PRI PROCEDURE CODE", "cpt_modifier" = "MOD")]
  # summarize based on unit_type
  services <- rbindlist(list(x[!(unit_type %in% c("Day", "Encounter")),
                               list(records = length(service_date),
                                    units = sum(units, na.rm = TRUE),
                                    cost = sum(cost, na.rm = TRUE)),
                               by = list(case_no, team, cpt, cpt_desc,
                                         unit_type)],
                             x[unit_type %in% c("Day", "Encounter"),
                               list(records = sum(units, na.rm = TRUE),
                                    units = sum(units, na.rm = TRUE),
                                    cost = sum(cost, na.rm = TRUE)),
                               by = list(case_no, team, cpt, cpt_desc,
                                         unit_type)]))
  # check for duplicates
  if (!identical(nrow(services[, .SD, .SDc = c("case_no", "cpt")]),
                 nrow(unique(services[, .SD, .SDc = c("case_no", "cpt")])))) {
    p_stop("check input", substitute(dt), "as there are duplicates!")
  }
  # find typical range
  services[, c("typical_record_range", "full_record_range",
               "sd_records", "total_records")
           := list(
             aux$my_range(records, 0.25, 0.75),
             aux$my_range(records, 0.0, 1.0),
             round(sd(records), 2),
             sum(records, na.rm = TRUE)
           ),
           by = list(cpt, cpt_desc, unit_type)]
  services[, outlier := !aux$in_range(x = records, range = typical_record_range),
           by = list(cpt, cpt_desc, unit_type, cost)]

  # remove COFRs per Kely B. 11/4/2015
  services <- services[case_no %nin% modify$cofr_cases]
  # remove state hospital consumers per Kelly B. 11/4/2015
  services <- services[case_no %nin% modify$state_hosp[, case_no]]

  # MI Adult Levels -------------------------------------------------------------
  # MI Adult - Level 0 - no contacts
  mi_consumers <- services[team %in% c("ACT", "MI"), unique(case_no)]
  mi_nonzeros <- services[cpt=="T1017" &
                            team %in% c("ACT", "MI"), unique(case_no)]
  services[case_no %in%
             setdiff(mi_consumers, mi_nonzeros), level := "L0_No_TCM"]
  rm(mi_consumers, mi_nonzeros)
  # MI Adult - Level 1 - 1-2 TCM contacts
  modify$mi_TCM <- services[team %in% c("ACT", "MI") & cpt=="T1017",
                            list(TCM = sum(records)),
                            by = list(case_no, team)]
  modify$mi_TCM[, level := cut(TCM, breaks = c(1, 2, 12, Inf),
                               labels = c("L1", "L2", "L3"),
                               include.lowest = TRUE)]
  modify$mi_TCM[, level := as.character(level)]
  services[modify$mi_TCM, level := i.level, on = "case_no"]
  services[locus, level := paste0("L", pmin(replace_level, 3)), on = "case_no"]
  # MI Adult - Level 5 - Residential
  services[team=="ACT", level := "L4_ACT"]
  # MI Adult - Level 5 - Residential
  services[case_load, c("supervisor", "primary_staff") :=
             list(supervisor, primary_staff),
           on = "case_no"]
  services[supervisor == "Hoener, Katie" |
             primary_staff == "Hoener, Katie", level := "L5_Residential"]
  services[team %nin% c("ACT", "MI"), level := NA_character_]
  # create program column
  services[, program := recode_team_prog(x=team)]

  # summary ---------------------------------------------------------------------
  modify$team_summary <-
    services[, list(num_cases = length(unique(case_no))),
             by = list(program, team)]
  if (nrow(services[team %in% c("MI", "ACT")]) == 0) {
    services[, level := NA]
  }
  modify$mi_level_summary <-
    services[team %in% c("MI", "ACT"),
             list(num_cases = length(unique(case_no))),
             keyby = list(level, program, team)]

  svc_summary <-
    services[, list(num_cases = length(unique(case_no)),
                    total_cost = sum(cost, na.rm = TRUE),
                    avg_cost = round(mean(cost, na.rm = TRUE)),
                    sd_cost = round(sd(cost, na.rm = TRUE), 3)),
             by = list(program, team, level, cpt, cpt_desc, unit_type,
                       typical_record_range, full_record_range, sd_records)]

  modify$outlier_cases <-
    services[isTRUE(outlier),
             list(num_outlier_cases = length(unique(case_no)),
                  outlier_cost = sum(cost, na.rm = TRUE),
                  avg_outlier_cost = round(mean(cost, na.rm = TRUE), 2),
                  sd_outlier_cost = sd(cost, na.rm = TRUE),
                  sd_outlier_records = round(sd(records), 2),
                  outlier_cost_range = aux$my_money_range(cost)),
             by = list(program, team, level, cpt, cpt_desc, unit_type,
                       typical_record_range, full_record_range, sd_records)]
  modify$typical_cases <-
    services[isTRUE(outlier),
             list(num_typical_cases = length(unique(case_no)),
                  typical_cost = sum(cost, na.rm = TRUE),
                  avg_typical_cost = round(mean(cost, na.rm = TRUE), 2),
                  sd_typical_cost = sd(cost, na.rm = TRUE),
                  sd_typical_records = round(sd(records), 2),
                  typical_cost_range = aux$my_money_range(cost)),
             by = list(program, team, level, cpt, cpt_desc, unit_type,
                       typical_record_range, full_record_range, sd_records)]
  svc_summary[modify$typical_cases,
              c("cpt", "num_typical_cases", "typical_cost", "avg_typical_cost",
                "sd_typical_cost", "sd_typical_records", "typical_cost_range")
              := list(cpt, num_typical_cases, typical_cost, avg_typical_cost,
                      sd_typical_cost, sd_typical_records, typical_cost_range),
              on = c("program", "team", "level", "cpt", "cpt_desc",
                     "unit_type", "typical_record_range", "full_record_range",
                     "sd_records")]
  svc_summary[modify$outlier_cases,
              c("cpt", "num_outlier_cases", "outlier_cost", "avg_outlier_cost",
                "sd_outlier_cost", "sd_outlier_records", "outlier_cost_range")
              := list(cpt, num_outlier_cases, outlier_cost, avg_outlier_cost,
                      sd_outlier_cost, sd_outlier_records, outlier_cost_range),
              on = c("program", "team", "level", "cpt", "cpt_desc",
                     "unit_type", "typical_record_range", "full_record_range",
                     "sd_records")]

  # column re-ordering for human readability
  setcolorder(svc_summary,
              c("program", "team", "level", "cpt", "cpt_desc", "unit_type",
                "num_cases", "num_typical_cases", "num_outlier_cases",
                "total_cost", "typical_cost", "outlier_cost",
                "avg_cost", "avg_typical_cost", "avg_outlier_cost",
                "sd_cost", "sd_typical_cost", "sd_outlier_cost",
                "full_record_range", "typical_record_range",
                # "outlier_record_range" - not needed unless asked for by Kelly
                "sd_typical_records", "sd_outlier_records", "sd_records",
                "typical_cost_range", "outlier_cost_range"))
  setorder(svc_summary, program, team, cpt)
  return(list(svc_summary = svc_summary[],
              team_summary = modify$team_summary,
              mi_level_summary = modify$mi_level_summary))

}