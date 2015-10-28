library(wccmh)
scrub()
aux <- new.env(parent=.GlobalEnv)
aux$alter_rates <- function(x) {
  x <- tolower(x)
  x <- gsub(x=x, pattern="washtenaw|rates", replace="")
  x <- gsub(x=x, pattern="\\s+", replace=" ")
  x <- gsub(x=x, pattern="\\s+$", replace="")
  x <- gsub(x=x, pattern="blended", replace="blnd")
  x <- gsub(x=x, pattern="direct", replace="dir")
  x <- gsub(x=x, pattern="contract", replace="ctr")
  return(x)
}