# ssm_functions and packages
library(EquaPac)
pkg_loader(packages=c("RODBC", "data.table"))

# # safe/not_safe function
# my_safe <- function(x) {
#   if(is.na(x)) {result <- NA}
#   result <- cut(x, breaks = c(1, 2, 5), labels=c("not_safe", "safe"), include.lowest = TRUE)
#   return(result)
# }
# my_safe <- Vectorize(my_safe)
# # example: my_safe(c(1, 2, NA))
#
# # covers up parse eval... convenience function
# parse.inst <- function(...) {parse( text = paste(..., sep=""))}
