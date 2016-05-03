file_filter <- fread(file.path(project_wd$github,
  project_wd$project, "Data/KB_5_2_16.csv"))

services <- merge(file_filter, services, by = c("case_no"))
yf_services <- merge(file_filter, yf_services, by = c("case_no"))
mi_services <- merge(file_filter, mi_services, by = c("case_no"))