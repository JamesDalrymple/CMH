#### auxillary file for medicine.r ####
require(xlsx)
require(gridExtra)
require(RODBC)
# install.packages("ReporteRs")
require( ReporteRs )

shortVendor <- function(x) {
  x <- as.character(x)
  result <- switch(x,
                   "CHS Group LLC." = "CHS",
                   "JOAK American Homes" = "JOAK",
                   "INI GROUP LLC (Formerly Micholdings)" = "INI",
                   "Synod Community Services" = "Synod",
                   "Partners In Personal Assistance" = "PPA",
                   "Progressive Residential Services, Inc." = "PRS",
                   "Renaissance House, Inc." = "Renaissance",
                   "Catholic Social Services of Washtenaw Co" = "CSS",
                   "QUEST INC" = "Quest",
                   "Adult Learning Systems- Master" = "ALS",
                   "CONSUMER SERVICES, INC" = "CSI",
                   "Real Life Living Services, Inc." = "Real",
                   "His Eye is on the Sparrow" = "HEIOTS",
                   "Macomb Residential Opportunities - Master" = "Macomb",
                   "Community Residence Corp." = "CRC",
                   "Spectrum Community Services" = "Spectrum",
                   "Renaissance Community Homes Inc" = "Renaissance",
                   "TURNING LEAF REHABILITATION SERVICES, INC" = "T.Leaf",
                   "Comprehensive Services for the Developmentally Disabled" =
                     "Comprehensive Svc: DD",
                   x)
return(result)
}
shortVendor <- Vectorize(shortVendor)