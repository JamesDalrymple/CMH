# case load auxillary file

# notes -----------------------------------------------------------------------
# missing non-billable services that are NOT progress notes, such as but not
# limited to med reviews some case managers are on multiple teams - this can be
# confusing (for obvious reasons) check to see if home based consumers get 4+
# hours of service per month, and compare to E2report2128

# NOT AN ERROR - at least not in the code/analysis ---
# F2F_Hours and Non-face-to-face hours sometime overlap, so you cannot
# add F2F and NF2F times to get the total time. Therefore, I (6/11/2014)
# calculate NF2F as follows: total time - F2F time = NF2F time

pkg_loader(c("RODBC", "sqldf", "data.table", "stringr", "xlsx"))
aux <- new.env(parent = .GlobalEnv)

# all CSTS teams (Access included)
aux$cmh_teams <- c("DD", "MI", "ACT", "Child", "Child HB", "Access/Engagement")

# different in time
aux$time_diff <- function(t1, t2, ...) {
    if( is.na(t1) | is.na(t2) ) {elapsed = NA_real_} else {

      # add leading zeros if needed to t1 and t2
      t1Spacer <- paste(rep("0", 4-nchar(t1)), collapse="", sep="")
      t1 <- paste(t1Spacer, t1, sep="", collapse="")
      t2Spacer <- paste(rep("0", 4-nchar(t2)), collapse="", sep="")
      t2 <- paste(t2Spacer, t2, sep="", collapse="")

      # calculate how many minutes are different
        t1Mins <- as.numeric(substr(t1,nchar(t1)-1,nchar(t1)))
        t2Mins <- as.numeric(substr(t2,nchar(t2)-1,nchar(t2)))

      # convert t1 and t2 back to numeric
      t1 <- as.numeric(t1)
      t2 <- as.numeric(t2)

      if(t2 >= 100 | t1 >=100 ) {
        elapsed <- 60*(floor(t2/100)-floor(t1/100))+(t2Mins-t1Mins)
      } else {elapsed <- t2-t1}
    }
    return(elapsed)
  }
aux$time_diff <- Vectorize(aux$time_diff)

# function to convert Y to 1 and N to 0
aux$f2fcontacts <- function(x) {
    result <- sum(replacer(x=x, old=c("Y", "N"), new=c(1, 0)), na.rm=TRUE)
    return(result)
  }

# function to convert Y to 1 and N to 0
aux$nonf2fcontacts <- function(x) {
    result <- sum(replacer(x=x, old=c("Y", "N"), new=c(0, 1)), na.rm=TRUE)
    return(result)
  }

#### deal with overlapping time ####
  ### function to deal with overlapping time ###
aux$overlap_fix <- function(start, end) {
    i<-2 # initialize variable
    repeat {
      # remove NAs
      removeStart <- which(is.na(start) | is.na(end))
      if(length(removeStart) > 0) {
        start <- start[-removeStart]
        end <- end[-removeStart]
      }

      # if vector is of 1 or zero, break
      if(length(start)<2 | length(end)<2) {break}

      if(start[i] %between% c(start[i-1], end[i-1]) |
           end[i] %between% c(start[i-1], end[i-1])) {
        start[i-1] <- min(start[i-1], start[i], na.rm=TRUE)
        end[i-1] <- max(end[i-1], end[i], na.rm=TRUE)
        start <- start[-i]
        end <- end[-i]
      } else {i <- i+1}
      if(i/length(start) > 1) {break}
    }
    return( data.table(start=start, end=end) )
  }

aux$duration <-
  function(start, end) {
    dt_time <- data.table(start = start, end = end)
    setorder(dt_time, start)

    # initialize variables
    fullEnd <- NULL
    fullStart <- NULL

    # results
    fixedTimes <- aux$overlap_fix(start = dt_time$start, end = dt_time$end)
    # save results
    fullStart <- c(fullStart, fixedTimes[, start])
    fullEnd <-  c(fullEnd, fixedTimes[, end])
    # sum elapsed time
    if(length(fullStart) < 1 | length(fullEnd) < 1) {
      totalMin = NA
      } else {
      totalMin <- sum(aux$time_diff(t1=fullStart, t2=fullEnd), na.rm=TRUE)
    }
    return(totalMin)
  }
  # DO NOT VECTORIZE DURATAION!!! ... breaks using this for subset groupings
  aux$elapsed_time <- Vectorize(aux$duration) # to create elpased time

aux$sep_word <- function(x, split = ",") {
  x <- gsub(x = x, pattern = "[()]", replace = "")
  word_parts <- strsplit(x = x, split = ", |,| |-")
  return(word_parts)
}
# x <- "firstname, (middlename), lastname-hyphenlastname"
# aux$sep_word(x)

aux$map_intersect <- function(x, y) {
  return(Map(x, y, f = function(x, y) intersect(x, y))) }
aux$count_intersect <- function(x, y) {
  length(unlist(aux$map_intersect(x, y)))
}


  #   # remove list of 'midnight staff'
  #   midnight_staff <- c("Anderson, Jill", "Burchard, Angela", "Holston, Christine", "Karol, Tiffany",
  #                       "Klevering, Kristin", "Lloyd, Sylvia", "Spalding, Ruth", "Spaulding, Lisa",
  #                       "Tasker, Melisa", "Hyde, Hannah", "LEWIS-GRINWIS, KRISTINA", "Harris, Nicole"
  #                       ) # 407