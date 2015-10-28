# prototype for L2_ssm_data_reshape.r
rm(list=ls())

# required packages
library(data.table)

# covers up parse eval... convenience function
parse.inst <- function(...) { eval(parse( text = paste(..., sep=""))) }
# required input data .. dput(ssm_cat[1:20])
ssm_cat <- data.table(income1 = sample(x=c("safe", "not_safe"), size=20, replace=TRUE),
                      income2 = sample(c("safe", "not_safe"), size=20, replace=TRUE))
setkeyv(ssm_cat, cols=c("income1", "income2"))
parse.inst(sprintf("ssm_cat[J('safe', 'safe'), %1$s := 'safe with no improvement']", "income") )
