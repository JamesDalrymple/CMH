# auxillary file for community hospital admissions

# # function to fix team names
# my_team <- function(x) {
#   result <- switch(x,
#          "WSH - MI - Adult" = "MI Adult",
#          "WSH - DD Adult" = "DD Adult",
#          "WSH - ACT" = "MI Adult",
#          "WSH - Access/Engagement" = "Non-CMH",
#          "Community Support and Treatment Services - CSTS" = "Non-CMH",
#          "WSH - Children's Services" = "Y&F",
#          "WSH - Children's Services - Home Based" = "Y&F",
#          x)
#   if(result=="") {result <- "Non-CMH"}
#   return(result)
# }
# my_team <- Vectorize(my_team)

# function to fix team names
my_team <- function(x) {
  result <- switch(x,
                   "WSH - MI - Adult" = "MI",
                   "WSH - ATO" = "MI",
                   "WSH - DD Adult" = "DD",
                   "WSH - ACT" = "ACT",
                   "WSH - Access/Engagement" = "Access/Engagement",
                   "Community Support and Treatment Services - CSTS" = "Access/Engagement",
                   "WSH - Children's Services" = "Child",
                   "WSH - Children's Services - Home Based" = "Child HB",
                   "WSH - Sobriety Court" = "Non-CMH",
                   "WSH - ICSS team" = "Non-CMH",
                   "Crisis Residential Services" = "Non-CMH",
                   "WSH - OBRA" = "Non-CMH",
                   "WSH - MH Court" = "Non-CMH",
                   "Home Based Services" = "Child Services HB",
                   x)
  if(result=="") {result <- "Non-CMH"}
  return(result)
}
my_team <- Vectorize(my_team)

# report date is today's date
report_date <- format(as.Date(Sys.time()), "%m_%d_%y")

# CMH teams
cmh_teams <- c("MI", "Child HB", "Child", "DD", "ACT")
