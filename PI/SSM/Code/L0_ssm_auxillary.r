#### L0: functions particular to only SSM code files ####
# ssm_functions and packages
library(EquaPac)
pkg_loader(packages=c("RODBC", "corrplot"))

# safe/not_safe function
aux <- new.env(parent = .GlobalEnv)

# aux$my_safe <- function(x) {
#   if (is.na(x)) {
#     result <- NA
#   }
#   result <-
#     cut(
#       x,
#       breaks = c(1, 2, 5),
#       labels = c("not_safe", "safe"),
#       include.lowest = TRUE
#     )
#   return(result)
# }


