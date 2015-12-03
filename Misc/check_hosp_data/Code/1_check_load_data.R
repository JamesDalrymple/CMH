dat <- new.env(parent=.GlobalEnv)

dat$hosp_files <-
  list.files("C:/Users/dalrymplej/Dropbox/PI Projects/Hospitalizations/Data",
           full.names = TRUE, pattern = ".csv")

dat$hosp1 <- fread(dat$hosp_files[1])
dat$hosp2 <- fread(dat$hosp_files[2])