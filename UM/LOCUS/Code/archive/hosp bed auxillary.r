# hosp[, unique(team)]

# shorten team name and fix team names
teamSwitch = function(x, ...) {
  switch(x,
         "WSH - DD Adult" = "DD Adult",
         "WSH - ACT" = "ACT",
         "WSH - MI - Adult" = "MI Adult",
         "WSH - Children's Services - Home Based" = "Children's Services - Home Based",
         "WSH - Children's Services" = "Children's Services",
         "WSH - PATH/PORT" = "PATH/PORT",
         "no team"
         )
}

teamFix = function(x, ...) {
  sapply(x, teamSwitch)
}

# shorten fund and fix fund names
fundSwitch = function(x, ...) {
  switch(x,
         "General Fund - Acute Services" = "GF",
         "Medicaid - Acute Services" = "Medicaid",
         "ABW-Acute Services" = "ABW",
         "MIChild" = "MIChild",
         "HMP-Acute Services" = "HMP",
         "HMP - Acute Services" = "HMP",
         x)
}
fundFix = function(x, ...) {
  sapply(x, fundSwitch)
}

freeStand = c("BCA of Detroit, LLC DBA BCA StoneCrest Center",
              "Forest View Hospital",
              "Havenwyck Hospital",
              "Harbor Oaks",
              "Henry Ford Kingswood Hospital",
              "Pine Rest Christian Mental Health Services",
              "The Behavioral Center of Michigan")