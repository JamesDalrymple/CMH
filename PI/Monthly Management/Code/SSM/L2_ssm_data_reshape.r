#### initializing working directory and input parameters ####

# which computer results in correct base working directory
baseWD <- switch(Sys.info()["nodename"],
                 "JAMES-2" = "B:",
                 "JAMES" = "B:", # laptop
                 "JAMES-PC" = "B:", # home PC
                 "CMHJ3DTVZ1-LT" = "C:/Users/dalrymplej",
                 "WSHSQLGP" = "C:/Users/dalrymplej" # county PC
)
# read in source file - personal library
source(file.path(baseWD,
                 "Dropbox/WCHO work/R source/begin script R code.r"))
# setup for working directories - data and results
dataWD <- "Dropbox/PI Projects/Monthly Management/Data/Fiscal Year"
resultsWD <- "Dropbox/PI Projects/Monthly Management/Results/Fiscal Year"
codeWD <- "Dropbox/PI Projects/Monthly Management/R Code/SSM"
source(file.path(baseWD, codeWD, "L0_ssm_auxillary.r"))
#### user input required ####
# current month
current_month <- "August"
current_fy <- "2015"
start_date <- '3/1/2014'
end_date <- format(x=Sys.Date(), "%m/%d/%Y")

# rerun sql data and create new rds data files?
rerun <- TRUE
if(rerun) {
  source(file.path(baseWD, codeWD, "L1_ssm_sql.r"))
} else {
  ssm_details <- readRDS(file=file.path(baseWD, dataWD, current_fy, current_month, "ssm_details.rds"))
}

# lapply(ssm_details[, .SD, .SDc = c(initial_cols, end_cols)], FUN=function(x) {sum(!is.na(x))} )
# these columns are mostly full of missing values
# childcare, child_education, healthcare

base_cols <- gsub(x=grep(x=colnames(ssm_details), pattern="1", value=TRUE), pattern="1", replace="")
initial_cols <- paste0(base_cols, "1")
end_cols <- paste0(base_cols, "2")
diff_columns <- paste0("d_", base_cols)

# ordinal approach - look for any change
ssm_scale <- copy(ssm_details)
ssm_scale[, (diff_columns) :=
  list(income1-income2, employment1-employment2, shelter1-shelter2, food1-food2,
       education1-education2, legal1-legal2, health_care1-health_care2, life_skills1-life_skills2,
       mental_health1-mental_health2, substance1-substance2, family_friends1-family_friends2,
       mobility1-mobility2, community1-community2, childcare1-childcare2, child_education1-child_education2,
       healthcare1-healthcare2)]
# create total_improvement column: 0 = no change, negative value = decrease, positive value = improvement

# str(diff_columns)
#> chr [1:16] "d_income" "d_employment"
# ssm_scale[, psum("col1", "col2")]
# ssm_scale[, psum(lapply(diff_columns, get))]




#### categorical approach - look for a category change ####
ssm_cat <- copy(ssm_details)
# apply my_safe to all data columns
for(j in c(initial_cols, end_cols)) set(ssm_cat, j=j, value=my_safe(ssm_cat[[j]]))
rm(j)

# add new columns
for (i in seq_along(base_cols)) {
setkeyv(ssm_cat, c(initial_cols[i], end_cols[i]))
new_cols <- c(paste(base_cols[i], "cat_long", sep="_"), paste(base_cols[i], "cat_short", sep="_"))
ssm_cat[J("safe", "safe"), c(new_cols) := list("safe with no improvement", 0)]
ssm_cat[J("safe", "not_safe"), c(new_cols) := list("decrease to not_safe", -1)]
ssm_cat[J("not_safe", "safe"), c(new_cols) := list("improved to safe", 1)]
ssm_cat[J("not_safe", "not_safe"), c(new_cols) := list("not_safe with no improvement", 0)]
}
ssm_cat


