agg <- new.env(parent = .GlobalEnv)
graph <- new.env(parent = .GlobalEnv)

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

aux$visit_cut <- function(x) {
  cut(x,
      breaks = c(0, 2, 5, 10, 25, 50, 100, 200, 300, 400, 600, Inf),
      labels = c("1-2", "3-5", "6-10", "11-25", "26-50", "51-100", "101-200",
                 "201-300", "301-400", "401-600", "601-Inf"))
}
# total
agg$total_visits_dt <-
  modify$cc360_main[uofm == TRUE, list(total_visits = length(unique(group))),
                    by = list(case_no)]
agg$total_visits_dt[, total_visits_cat :=
                      aux$visit_cut(total_visits),
                    by = list(case_no)]
setf(agg$total_visits_dt, j = "total_visits", as.numeric)
agg$total_visits_binned <-
  agg$total_visits_dt[, list(consumers = length(unique(case_no))),
                      by = total_visits_cat]

# location
agg$loc_visits_dt <-
  modify$cc360_main[uofm == TRUE, list(loc_visits = length(unique(group))),
                    by = list(case_no, Place_of_Service)]
agg$loc_visits_dt[, loc_visits_cat :=
                    aux$visit_cut(loc_visits),
                  by = list(case_no, Place_of_Service)]
setf(agg$loc_visits_dt, j = "loc_visits", as.numeric)
agg$loc_visits_binned <-
  agg$loc_visits_dt[, list(consumers = length(unique(case_no))),
                      by = list(loc_visits_cat, Place_of_Service)]

# total: frequency graph
graph$q4_total_freq <-
  ggplot(data = agg$total_visits_binned, aes(x = total_visits_cat, y = consumers, ymax = 1.1*consumers))+
  geom_bar(stat = "identity", fill = "lightblue", color = "black", size = 0.2) +
  theme_bw() +
  labs(title = "Consumer U of M Visits",
       x = "binned number of visits", y = "number of consumers") +
  geom_text(aes(x = total_visits_cat, y = consumers, label = consumers), vjust = -0.5)

# location: frequency graph
graph$q4_loc_freq <-
  ggplot(data = agg$loc_visits_binned[Place_of_Service %in% c(agg$loc_visits_binned[, .N, by = Place_of_Service][N > 1, Place_of_Service])],
    aes(x = loc_visits_cat, y = consumers,
        ymax = 1.1*consumers))+
  geom_bar(stat = "identity", fill = "lightblue", color = "black", size = 0.2, position = position_dodge(0.5), width = 0.5) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~Place_of_Service, scales = "free")+
  labs(title = "Place: Consumer U of M Visits > 6",
       x = "binned number of visits", y = "number of consumers") +
  geom_text(aes(x = loc_visits_cat, y = consumers,
                ymax = 1.1*consumers, label = consumers), vjust = -0.5)

# total boxplot
graph$q4_total_bp <-
  ggplot(data = agg$total_visits_dt, aes(x = 1, y = total_visits))+
  geom_boxplot(alpha = 0.3) + theme_light() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())+
labs(title = "Consumer U of M Visits", x = NULL, y = "number of visits")

agg$result$q4$total_summary <-
  agg$total_visits_dt[, list(avg_total_visits = mean(total_visits),
                                       median_total_visits = median(total_visits),
                                       min_visit = min(total_visits),
                                       max_visit = max(total_visits))]

agg$result$q4$loc_summary <-
  agg$loc_visits_dt[, list(avg_total_visits = mean(loc_visits),
                         median_total_visits = median(loc_visits),
                         min_visit = min(loc_visits),
                         max_visit = max(loc_visits)), by = Place_of_Service]

setorder(agg$total_visits_binned, -consumers)
agg$result$q4$total_binned <- agg$total_visits_binned
setorder(agg$loc_visits_binned, -consumers)
agg$result$q4$loc_binned <- agg$loc_visits_binned




# data.table('average number of visits per person' = modify$cc360_main[uofm == TRUE,
#                                                                      length(unique(group))/length(unique(case_no))])
# grouped by place of service (facility if place of service was blank)
agg$result$q4$place <-
  agg$loc_visits_dt[, list(average_num_visits = mean(loc_visits),
                           median_num_visits = median(as.numeric(loc_visits))),
                    by = Place_of_Service]
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