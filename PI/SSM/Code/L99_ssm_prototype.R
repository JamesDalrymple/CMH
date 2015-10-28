# prototype for L2_ssm_data_reshape.r
rm(list=ls())

# required packages
library(data.table)

# required user-defined functions
n=20
my_fake_data <- function() {
  result <- sample(1:5, size=n, replace=TRUE)
  return(result)
}
my_fake_sup <- sample(x=c("John", "Nathan", "Lisa"), replace=TRUE, size=n)
my_fake_staff <- sample(x=c("Joe", "Jake", "John", "Jessi", "Jose", "Jordan", "Jill", "Jaime", "Justina", "Justin", "Judah", "Josephina"), size=20, replace=TRUE)
my_fake_consumers <- sample(x=10001:20099, replace=FALSE, size=n)
my_fake_team <- sample(x=c("MI", "ACT"), replace=TRUE, size=n, prob=c(0.9, 0.1))
my_fake_dates <- sample(x=as.Date(as.Date("3/1/2014", format="%m/%d/%Y"):Sys.Date()), replace=TRUE, size=n)
my_fake_dist <- sample(x=30:400, replace=TRUE, size=n)
# safe/not_safe function
my_safe <- function(x) {
  if(is.na(x)) {result <- NA}
  result <- cut(x, breaks = c(1, 2, 5), labels=c("not_safe", "safe"), include.lowest = TRUE)
  return(result)
}
my_safe <- Vectorize(my_safe)
# example: my_safe(c(1, 2, NA))

# covers up parse eval... convenience function
parse.inst <- function(...) {parse( text = paste(..., sep=""))}
# required input data .. dput(ssm_cat[1:20])
ssm_cat <-
  data.table(case_no = my_fake_consumers,
                 team = my_fake_team,
                 supervisor = my_fake_sup,
                 primary_staff = my_fake_staff,
                 first_matrix_date = my_fake_dates,
                 last_matrix_date = my_fake_dates+my_fake_dist,
                 income1 = my_fake_data(),
                 employment1 = my_fake_data(),
                 shelter1 = my_fake_data(),
                 food1 = my_fake_data(),
                 education1 = my_fake_data(),
                 legal1 = my_fake_data(),
                 health_care1 = my_fake_data(),
                 life_skills1 = my_fake_data(),
                 mental_health1 = my_fake_data(),
                 substance1 = my_fake_data(),
                 family_friends1 = my_fake_data(),
                 mobility1 = my_fake_data(),
                 community1 = my_fake_data(),
                 childcare1 = my_fake_data(),
                 child_education1 = my_fake_data(),
                 healthcare1 = my_fake_data(),
                 income2 = my_fake_data(),
                 employment2 = my_fake_data(),
                 shelter2 = my_fake_data(),
                 food2 = my_fake_data(),
                 education2 = my_fake_data(),
                 legal2 = my_fake_data(),
                 health_care2 = my_fake_data(),
                 life_skills2 = my_fake_data(),
                 mental_health2 = my_fake_data(),
                 substance2 = my_fake_data(),
                 family_friends2 = my_fake_data(),
                 mobility2 = my_fake_data(),
                 community2 = my_fake_data(),
                 childcare2 = my_fake_data(),
                 child_education2 = my_fake_data(),
                 healthcare2 = my_fake_data(),
                 time_diff = my_fake_dist)

# initial prep work ... should not need tweaking
base_cols <- gsub(x=grep(x=colnames(ssm_cat), pattern="1", value=TRUE), pattern="1", replace="")
initial_cols <- paste0(base_cols, "1")
end_cols <- paste0(base_cols, "2")
diff_columns <- paste0("d_", base_cols)

### didnt work
# for (j in c(initial_cols, end_cols)) set(ssm_cat, j=j, value = my_safe( ssm_cat[[j]]))

### works but better ways likely exist
# categorical approach - look for a category change
for (i in seq_along(base_cols)) {
  parse.inst(sprintf("setkeyv(ssm_cat, '%1$s', '%2$s')[J('safe', 'safe'), %3$s := 'safe with no improvement']",
                     initial_cols[i], end_cols[i], paste0(base_cols[i], i) ))
  parse.inst(sprintf("setkeyv(ssm_cat, '%1$s', '%2$s')[J('not_safe', 'safe'), %3$s := 'safe with improvement']",
                     initial_cols[i], end_cols[i], paste0(base_cols[i], i) ))
  parse.inst(sprintf("setkeyv(ssm_cat, '%1$s', '%2$s')[J('safe', 'not_safe'), %3$s := 'decreased to not_safe']",
                     initial_cols[i], end_cols[i], paste0(base_cols[i], i) ))
  parse.inst(sprintf("setkeyv(ssm_cat, '%1$s', '%2$s')[J('not_safe', 'not_safe'), %3$s := 'no improvement from not_safe']",
                     initial_cols[i], end_cols[i], paste0(base_cols[i], i) ))
}
ssm_cat