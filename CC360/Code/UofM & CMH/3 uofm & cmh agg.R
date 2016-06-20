agg <- new.env(parent = .GlobalEnv)

agg$result <- list()
# Q1: number of people in common ----------------------------------------------
# main only
agg$result$q1$main <- modify$cc360_main[uofm == TRUE, length(unique(case_no))]
agg$main_um_cases <- modify$cc360_main[uofm == TRUE, unique(case_no)]
# # med only
# agg$result$q1$med <-
#   modify$cc360_med[uofm == TRUE, length(unique(case_no))]
# # main and med
# agg$main_med_consumers <-
#   union(modify$cc360_main[uofm == TRUE, unique(case_no)],
#         modify$cc360_med[uofm == TRUE, unique(case_no)])
# agg$result$q1$main_med <- length(agg$main_med_consumers)

agg$result$q1$dt <-
  melt(
    data.table(main = agg$result$q1$main),
    id = NULL, variable.name = "dataset",
    measure.vars = Cs(main),
    value.name = "num_cases")

# Q2: top 2 primary diagnoses based on CMH/E2 data ----------------------------
agg$dx_freq <- modify$dx_past[case_no %in% agg$main_um_cases,
  list(num_cases = length(unique(case_no)),
       pct_cases = length(unique(case_no))/agg$result$q1$main*100),
  by = list(icd9_code, icd9_desc)]
setorder(agg$dx_freq, -pct_cases)
agg$result$q2$dt <- agg$dx_freq[order(-pct_cases)][1:10]

agg$result$q2$dt[, icd9_desc := factor(icd9_desc,
  levels = agg$result$q2$dt[order(pct_cases), icd9_desc])]

agg$result$q2$p_dx <-
  ggplot(data = agg$result$q2$dt, aes(x = icd9_desc, y = pct_cases))+
  geom_bar(stat = "identity", color = "black", size = 0.2, fill = "lightblue")+
  coord_flip() +
  theme_light() +
  labs(title = "U of M + CMH consumer Primary Diagnoses", y = "percent of diagnoses", x = NULL)

# Q3: how many visits, in pct, by place of service? ---------------------------
agg$num_uofm_visits <- modify$cc360_main[, length(unique(group))]
agg$result$q3$pct_visits_dt <-
  modify$cc360_main[,
    list(pct_visits = length(unique(group))/agg$num_uofm_visits*100,
         num_visits = length(unique(group))),
    by = Place_of_Service]
setorder(agg$result$q3$pct_visits_dt, -num_visits)
agg$result$q3$pct_visits_dt[, Place_of_Service := factor(Place_of_Service,
  levels = agg$result$q3$pct_visits_dt[order(num_visits), Place_of_Service])]

agg$result$q3$p_pct_visits <- ggplot(data = agg$result$q3$pct_visits_dt[!is.na(Place_of_Service)],
       aes(x = Place_of_Service, y = pct_visits))+
  geom_bar(stat = "identity", color = "black", size = 0.2, fill = "lightblue")+
  coord_flip() +
  theme_light() +
  labs(title = "U of M + CMH consumer Type of Visit", x = "percent of visits", y = NULL)
# Q4: average number of visits per person -------------------------------------
# total
agg$result$q4$total <-
  data.table('average number of visits per person' = modify$cc360_main[uofm == TRUE,
    length(unique(group))/length(unique(case_no))])
# grouped by place of service (facility if place of service was blank)
agg$result$q4$place <-
  modify$cc360_main[uofm == TRUE,
    list(average_num_visits = length(unique(group)) /
           length(unique(case_no))), by = Place_of_Service]
# Q5 number of visits outside of 'UofM + CMH' by 'UofM + CMH' consumers
# total
agg$result$q5$total_dt <- modify$cc360_main[case_no %in% agg$main_um_cases,
  list(total_visits = length(group),
       total_cases = length(unique(case_no)),
       avg_visits = length(unique(group))/length(unique(case_no))),
  by = uofm]
# grouped by place of service
agg$result$q5$place_dt <-
  modify$cc360_main[case_no %in% agg$main_um_cases,
                    list(total_visits = length(group),
                         total_cases = length(unique(case_no)),
                         avg_visits = length(unique(group))/length(unique(case_no))),
                    by = list(uofm, Place_of_Service)]