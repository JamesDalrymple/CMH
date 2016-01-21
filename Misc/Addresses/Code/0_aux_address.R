pkg_loader(c("zoo", "xlsx", "RODBC", "ggmap", "mapproj", "RCurl", "RJSONIO",
             "plyr"))

aux <- new.env(parent = .GlobalEnv)

aux$fix_address <- function(x) {
  y <- gsub(x = x, pattern = "\\([^)]*\\)",
            replacement = "", perl = TRUE, ignore.case = TRUE)
  y <- gsub(x = y, pattern = "^Road | Road| Road$",
            replacement = " Rd ", ignore.case = TRUE)
  y <- gsub(x = y, pattern="^Street | Street| Street$",
            replacement=" St ", ignore.case = TRUE)
  y <- gsub(x = y, pattern="^Drive | Drive| Drive$",
            replacement=" Dr ", ignore.case = TRUE)
  y <- gsub(x = y, pattern="^Court | Court| Court$",
            replacement=" Ct ", ignore.case = TRUE)
  y <- gsub(x = y, pattern="^Boulevard | Boulevard| Boulevard$",
            replacement=" Blvd ", ignore.case = TRUE)
  y <- gsub(x = y, pattern = "^ *|(?<= ) | *$",
            replacement = "", perl = TRUE, ignore.case = TRUE)
  y <- gsub(x = y, pattern = "[-.]", replace = "")
  y <- stringi::stri_trim(y)
  return(y)
}
# aux$fix_address("this is a test (blah) ...")
# aux$fix_address("Road Court blueroad Driveway drive")


# GEOCODING ONLY! remove all after [:-.,( Apt Rd St Dr Ct Blvd]
aux$remove_after <- function(x) {
  # remove anything AT and AFTER...
  c1 <- unlist(gregexpr(pattern = '[,#(]', text = x))
  c2 <- unlist(gregexpr(pattern = 'Apt', text = x, ignore.case = TRUE))
  # AT and AFter location
  atAfter = unique(c(c1, c2))
  atAfter = atAfter[atAfter != -1]

  if (length(atAfter) > 0) {
    keepAfter = min(atAfter) - 1
  } else {
    keepAfter = NA
  }

  # remove anything AFTER...
  c3 = unlist(gregexpr(pattern = ' Dr ', text = x, ignore.case = TRUE))
  c4 = unlist(gregexpr(pattern = ' Rd ', text = x, ignore.case = TRUE))
  c5 = unlist(gregexpr(pattern = ' St ', text = x, ignore.case = TRUE))
  c6 = unlist(gregexpr(pattern = ' Blvd ', text = x, ignore.case = TRUE))
  # AFTER location
  after = data.table(start = c(c3, c4, c5, c6),
                     end = c(rep(2, 3), 4))
  after[, end := psum(start, end)]
  after = after[start != -1]
  if (nrow(after) > 0) {
    keepBefore = after[, min(end)]
  } else {
    keepBefore = NA
  }

  if (!is.na(keepAfter) | !is.na(keepBefore)) {
    keepMin = min(keepAfter, keepBefore, na.rm = TRUE)
    result = substr(x, 1, keepMin)
  } else {
    result = x
  }
  result = stringi::stri_trim(result)
  return(result)
}

# get longitude and latitude ---
aux$url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=",
             address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

# google geo code
aux$geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- aux$url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA_character_,NA_character_,NA_character_, NA_character_))
  }
}

aux$fix_city <- function(x) {
  y <- ifelse(substr(x, 1, 3) == "Yps", "Ypsilanti", x)
  y <- ifelse(substr(y, 1, 5) == "Ann A", "Ann Arbor", y)
  return(cap_word(y))
}
# unique( address[, aux$fix_city(city)] )


