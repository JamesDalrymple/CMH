file_filter <- fread(file.path(project_wd$github,
  project_wd$project, "Data/KB_5_2_16.csv"))

# this person has zero funding bucket services
# 10364
# services[case_no == 10364]

project_wd$results <- paste(project_wd$results, "Arrests")

services <- merge(file_filter, services, all.x = TRUE, by = c("case_no"))
services[is.na(fund), fund := "GF"]


yf_services <- merge(file_filter, yf_services, by = c("case_no"))
mi_services <- merge(file_filter[KB_team == "MI Adult" | KB_team == "WCCMHA"],
                     mi_services, all.x = TRUE, by = c("case_no"))
mi_services[is.na(fund), fund := "GF"]




setdiff(
  admit[like(team, "Child")][is.na(team_expdt), length(unique(case_no))],
  services[program == "Y&F" & is.na(cmh_expdt), unique(case_no)]
)

admit[case_no == 259060]
fb_data[case_no == 259060]
services[case_no == 259060]
