### notes ###
# use these variables: age, gender, zipcode,
# race, CLS_units_last_6_months (determine threshold based on data)

#### initializing working directory and input parameters ####
# clear RAM
rm(list = ls())
# which computer results in correct base working directory
baseWD <- switch(Sys.info()["nodename"],
                 "JAMES-2" = "B:",
                 "JAMES" = "B:", # laptop
                 "JAMES-PC" = "B:", # home PC
                 "CMHJ3DTVZ1-LT" = "C:/Users/dalrymplej",
                 "WSHSQLGP" = "C:/Users/dalrymplej" # county PC
)
#### user input required ####
input <- list(
  current_month = "August",
  current_fy = "2015",
  start_date = '3/1/2014',
  end_date = format(x=Sys.Date(), "%m/%d/%Y")
)
#### sourcing files ####
# read in source file - personal library
source(file.path(baseWD,
                 "Dropbox/WCCMH/R/begin script R code.r"))
# setup for working directories - data and results
dataWD <- "Dropbox/PI Projects/Monthly Management/Data/Fiscal Year"
resultsWD <- "Dropbox/PI Projects/Monthly Management/Results/Fiscal Year"
codeWD <- "Dropbox/PI Projects/Monthly Management/R Code/SSM"
source(file.path(baseWD, codeWD, "L0_ssm_auxillary.r"))
source(file.path(baseWD, codeWD, "L1_ssm_sql.r"))


# rerun sql data and create new rds data files?
rerun <- TRUE
if(rerun) {

} else {
  ssm_details <- readRDS(file=file.path(baseWD, dataWD, current_fy, current_month, "ssm_details.rds"))
}
ssm_scale <- copy(ssm_details)
ssm_scale[, c("d_income", "d_employ", "d_shelter", "d_food", "d_education",
             "d_legal", "d_health_care", "d_life_skills", "d_mental_health",
             "d_substance", "d_family_friends", "d_mobility", "d_community",
             "d_childcare", "d_child_education", "d_healthcare") :=
  list(income1-income2, employment1-employment2, shelter1-shelter2, food1-food2,
       education1-education2, legal1-legal2, health_care1-health_care2, life_skills1-life_skills2,
       mental_health1-mental_health2, substance1-substance2, family_friends1-family_friends2,
       mobility1-mobility2, community1-community2, childcare1-childcare2, child_education1-child_education2,
       healthcare1-healthcare2)]


ssm_scale <- ssm_scale[, .SD, .SDc = c("case_no", "d_income", "d_employ", "d_shelter", "d_food", "d_education",
                         "d_legal", "d_health_care", "d_life_skills", "d_mental_health",
                         "d_substance", "d_family_friends", "d_mobility", "d_community",
                         "d_childcare", "d_child_education", "d_healthcare", "time_diff")]

ssm_cat <- copy(ssm_scale)




