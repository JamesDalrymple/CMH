# monthly pi - auxillary file
packages = c("xtable", "knitr", "reshape2", "reshape", "grid", "XML", "RODBC",
             "RCurl", "data.table", "ggplot2", "zoo", "xlsx", "gdata")
# sapply(packages, require, character.only=TRUE)
usePackage(packages)
channel <- "WSHSQLGP"

# Function for inserting expressions which changes features
parse.inst <- function(...) {parse( text = paste(..., sep=""))}
# note: use  eval(parse.inst(v1, v2,  ... )) to excute the expersion
# example: eval(parse.inst("testMe = 1"))

# utilization management colors ----
um_colors = c("#94B6D2", "#DD8047", "#EBDDC3", "#775F55", "#B6C999")

# function for converting dates ----
dateConvert = function(x, format = "%m/%d/%Y", origin="1970-01-01") {
  y = as.Date(x, format = format, origin=origin )
  return(y)
}

# remove '$' symbol and ',' from dollar strings, and convert to numeric ----
moneyRm = function(x) {
  result = gsub(x = x, pattern = "[$,)]", replacement = "" )
  result = gsub(x = result, pattern = "[(]", replacement= "-")
  result = as.numeric(result)
  return(result)
}

# add '$' symbol and K, M as appropriate for ggplot graphing ----
moneyAdd = function(x) {
  # if missing values exist, replace result with missing values
  if(is.na(x)==TRUE) { moneyLab = NA_character_ } else
    # millions
    if(x>1e6) {
      moneyLab = paste("$", round(x/1e6,2), "M", sep="")
    } else
      # thousands
      if(x<1e6) {
        moneyLab = paste("$", round(x/1e3,0), "K", sep="")
      } else
        # less than 1000
        if(x<1e3) {
          moneyLab =  paste("$", x, sep="")
        }
  return(moneyLab)
}

# ageConvert ----
ageFn = function(x) { return(floor(as.numeric(Sys.Date() -
                                                dateConvert(x))/365.25)) }

# function to handle errors
tryCatch.W.E <- function(expr) {
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}

# read in data.table ----
read.dtable = function(file, na.strings="NA") {
  # read first line of file, to avoid reading in large datasets twice or copying large datsets
  fileExists = tryCatch.W.E(read.table(file, header = TRUE, nrow = 1, sep=",", strip.white=TRUE))
  if( !is.null(fileExists$warning ) ) {
    return(stop("File does not exist in current working directory."))
  } else{ dtable = data.table(trim(read.csv(file, stringsAsFactors=FALSE, strip.white=TRUE, na.strings=na.strings)))}
  return(dtable)
}

# function for creating a fiscal year from month (date class) ----
my_fy = function(x, format = "%m/%d/%Y" ) {
  if( class(x)!="Date" ) { x = as.Date(x, format = format)}
  quarter = as.yearqtr(x)+.25
  year = as.character(substr(quarter, 1, 4))
  return(year)
}

# function for creating fiscal from quarter (date class) ----
my_qtr = function(x, format = "%m/%d/%Y") {
  if( class(x)!="Date" ) { x = as.Date(x, format = format)}
  quarter = as.yearqtr(x)+.25
  quarter = as.character(quarter)
  return(quarter)
}

# quarter function from Jessica S.
# If date in (10,11,12) then quarter=1;
# Else quarter=round((month(date)/3+0.4))+1;

# my_theme for ggplot----
my_theme=  theme(legend.position = "topleft",
                 panel.background = element_rect(fill = "white", colour = NA),
                 panel.border = element_rect(fill = NA,
                                             colour = "grey50"),
                 panel.grid.major = element_line(colour = "white",size = 0.2),
                 panel.grid.minor = element_line(colour = "white",
                                                 size = .2 ),
                 strip.background = element_rect(fill = "white", colour = "white"),
                 axis.title.x = element_text(colour = "white"),
                 axis.title.y = element_text(colour = "white"),
                 plot.title = element_text(size=10))

# a max function that allows all NA inputs----
my_max = function(x) {    options(warn=-1)
                          step1 = max( x, na.rm = TRUE)
                          if( is.finite(step1) ) {ans = step1} else{
                            ans = NA_real_
                          }
                          return(as.numeric(ans))
}

# specificy the number of decimals
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

# function to place two ggplots on one pdf----
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# function to trim off top 15 % then take the mean----
meanTrim = function(x, trim=.15) {
  x = x[!is.na(x)]
  xOrd = x[order(x)]
  xReduce = xOrd[1:round((1-trim)*length(xOrd)) ]
  result = mean(xReduce)
  return(result)
}

# function to capitalize the first letter of a string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# function to replace multiple values
replacer = function(x, old, new) {
  result = new[ match(x, old)]
  return(result)
}
# example:
# x = c(rep('x',3),rep('y',3), NA, rep('z',3), NA)
# replacer(x, old=c("x","y","z"), new=as.character(1:3))

# max function to deal with integers and NAs and Inf
my_max = function(x) {options(warn=-1)
                      result = max(as.numeric(x), na.rm=TRUE)
                      if(!is.finite(result)) {result=NA_real_}
                      return(result)}
# min function to deal with integers and NAs and Inf
my_min = function(x) {options(warn=-1)
                      result = min(as.numeric(x), na.rm=TRUE)
                      if(!is.finite(result)) {result=NA_real_}
                      return(result)}

# function to take vectors of equal length and sum them row-wise
psum = function(a, b, ...) {
  # if multiple arguments are passed...
  extraVars = list(...)
  # if extraVars exist ...
  if(length(extraVars)>0) {
    my_mat = matrix(ncol=length(extraVars), nrow=length(extraVars[[1]]))
    for( i in seq(extraVars) ) {
      my_mat[,i] = extraVars[[i]]
    }
    # combine extra variables to first 2 variables
    dat = data.frame(a=a, b=b, my_mat)
  } else (dat = data.frame(a=a, b=b))
  # compute the row sums and return result as a vector
  result = rowSums( dat, na.rm = TRUE)
  return(result)
}

#### merge multiple data.tables ####

mergeDT <- function( DT, by, ...)
{
  for(t in seq_along(DT) ) {
    setkeyv(DT[[t]], cols=NULL)
    setkeyv(DT[[t]], cols=by)
  }
  result <- DT[[1]]
  for(t in 2:length(DT) )
  {
    result <- merge(result, DT[[t]], by=by, ...)
  }
  setkeyv(result, by)
  return(result)
}

# function to create boxplot with jittered outliers with optional log and facet features #
bpJitter = function(dataset, xVar, yVar, group=NULL, jitter.x=0.2,
                    title.plot=NULL, title.x.axis=NULL, title.y.axis=NULL, logBase=NULL,
                    xlabAngle=0) {
  # copy data to avoid changing original data.table
  data = copy(dataset)
  # if logBase is not null...
  if(!is.null(logBase)) {
    # transform the data
    data[, eval(yVar) := as.numeric(get(yVar))] # numeric to make sure next line works
    # set 0 to 1 works for discrete data... may need a different method for continuous datasets
    data[get(yVar)==0, eval(yVar) := 1 ]
    data[, eval(yVar) := log( get(yVar)+1e-7, base=logBase) ]

    # change the title.y.axis to include log 2 at the front
    title.y.axis = paste("Log", logBase, "of", title.y.axis)
  }

  # make jitteredFy for boxplot jittering
  data[, jitterTime := psum(as.numeric(factor(get(xVar))), runif(get(xVar), min=-jitter.x, max=jitter.x))]
  ## outliers ##
  if(is.null(group)) {
    # first quartile
    data[, q1 := .SD[, quantile(x=get(yVar), probs=0.25, type=7, na.rm=TRUE)], by=xVar]
    # third quartile
    data[, q3 := .SD[, quantile(x=get(yVar), probs=0.75, type=7, na.rm=TRUE)], by=xVar]
  } else {
    # first quartile
    data[, q1 := .SD[, quantile(x=get(yVar), probs=0.25, type=7, na.rm=TRUE)], by=c(xVar, group)]
    # third quartile
    data[, q3 := .SD[, quantile(x=get(yVar), probs=0.75, type=7, na.rm=TRUE)], by=c(xVar, group)]
  }
  # outlier column
  data[ get(yVar) > q3+1.5*(q3-q1) | get(yVar) < q1-1.5*(q3-q1), outliers := "Y"]
  data[!( get(yVar) > q3+1.5*(q3-q1) | get(yVar) < q1-1.5*(q3-q1)), outliers := "N"]
  # create boxplot
  p.boxplot = ggplot(data=data, aes_string(x=xVar, y=yVar))+
    geom_point(data=data[outliers=="Y"], aes_string(x="jitterTime", y=yVar),
               size=1.5, alpha=0.4, shape=3, color=um_colors[4])+
    geom_boxplot(data=data, aes_string(x=xVar, y=yVar),
                 outlier.size=NA, position=position_dodge(.6), width=.6,
                 fill=um_colors[1], fatten = 1, color=um_colors[4])
  # make a facet if group is not null
  if(!is.null(group)) {
    facets = facet_wrap(formula(paste("~", group)), scales="free")
    p.boxplot = p.boxplot + facets
  }
  # add remainder of plot updates
  p.boxplot = p.boxplot+my_theme+labs(title=title.plot, x=title.x.axis, y=title.y.axis)+
    theme(legend.position = "topleft",
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey80"),
          panel.grid.major = element_line(colour = "grey70", size = 0.2),
          panel.grid.minor = element_line(colour = "grey85", size = .2),
          strip.background = element_rect(fill = "white", colour = "white"),
          axis.title.x = element_text(colour = "grey30", size=8),
          axis.title.y = element_text(colour = "grey30", size=8),
          axis.text.x = element_text(size=7, angle=xlabAngle),
          axis.text.y = element_text(size=7) )+
    scale_x_discrete()
  return(p.boxplot)
}

# function to see if user input is a color
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}

# print function
auto.print = function( ... ) {
  Rinput = c(...)
  result = NULL
  for( i in seq_along(Rinput)) {
    result = paste(result, names(Rinput)[i],
                   ": ", Rinput[i], " | ", collapse="", sep="")
  }
  return( substr(result, 1, nchar(result)-1) )
}

# function to create new subfolder in a given main directory
createWD = function(mainDir, subDir) {
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("subDir exists in mainDir and is a directory")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("subDir exists in mainDir but is a file")
    # you will probably want to handle this separately
  } else {
    cat("subDir does not exist in mainDir - creating")
    dir.create(file.path(mainDir, subDir))
  }
}

# save variables via list[...]
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}
# # it is used like this:
#
# list[QR,,QRaux]  <- qr(c(1,1:3,3:1)) # saves the 1st results to QR, and the 3rd results to QRaux
# QR
# QRaux
#
# list[,Green,Blue]  <- col2rgb("aquamarine") # saves the 2nd results to Green, and the 3rd results to Blue
# Green
# Blue

teamFix <- function(x, ...) {
  switch(x,
         "WSH - DD Adult"  = "DD Adult",
         "WSH - MI - Adult" = "MI Adult",
         "WSH - ACT" = "ACT",
         "WSH - Children's Services" = "Child",
         "WSH - Children's Services - Home Based" = "Child Home Based",
         "WSH - Access/Engagement" = "Access/Engagement",
         "WSH - PATH/PORT" = "PATH/PORT",
         "WSH - PORT" = "PATH/PORT",
         "No Team")
}
teamFix <- Vectorize(teamFix)

# keep these teams for unsigned docs
teamsCMH <- c("ACT", "DD Adult", "MI Adult",
             "Child", "Child Home Based")

stringNoNA =  function(..., collapse="|") {
  x <- c(...)
  x <- x[!is.na(x)]
  result = paste(x, collapse=collapse)
  return(result)
}
stringNoNA(NA, NA, collapse="|")

combCols <- function(a, b, ...) {
  dt1 <- data.table(a, b)[, index := .I]
  result <- dt1[, list(level = stringNoNA(a, b, ...)), by=index][, level]
  return(result)
}

# keep these documents only for unsigned docs
docsKeep <- c("Bio/Psycho/Social",
             "Group Progress Note",
             "Emergency Note",
             "Injection/Dispense Note",
             "IPOS",
             "Medication Renewal",
             "Medication Review Note",
             "Nurse Progress Note",
             "Nutrition Assessment",
             "Personal Health Review",
             "Pre-Screening Assessment",
             "Progress Note",
             "Progress Review",
             "Psychiatric Evalulation",
             "Screening Form",
             "Wellness Note")

# bin function
myBin <- function(x, breaks = c(0, 30, 60, 90, 180)) {
  breaks = breaks
  labs = c(
    paste0("-", breaks[1] ), # lower endpoint
    paste(breaks[-length(breaks)], c(breaks[-1]), sep=" to "),
    paste0(breaks[length(breaks)], "+")) # upper endpoint
  breaks = c(-Inf, breaks, Inf)
  if(is.na(x)) { result = NA_character_} else {
    result = cut(x, breaks = breaks, labels = labs)
  }
  result = as.character(result)
  return(result)
}

myBin = Vectorize(myBin)

# function to make LaTeX columns bold
  bold_cols <- function(x) {
    paste('\\textbf{', x, '}', sep ='')
  }

# is.even
is.even <- function(x) x %% 2 == 0



# modify xtable for shading in groups
shade <- function(group=3, dt_xtable, n_rows, sanitize_text=FALSE, tabular.environment='tabular',
                  table_title, scalebox=0.8, ...) {
  hlines <- c(-1, 0, nrow(dt_xtable))
  n_rows <- seq(nrow(dt_xtable))

  # if only a few rows, dont bother shading anything
  if(max(n_rows) >= group) {
    mult_length <- length(n_rows)/group
    change_rows <- as.numeric(n_rows[is.even(c(unlist(lapply(seq(floor(mult_length)), rep, group)),
                                             rep(x=ceiling(mult_length),
                                             times = round((mult_length-floor(mult_length))*group) )))])-1

    col <- rep("\\rowcolor[gray]{0.90}", length(change_rows))
    # change this to a function probably to use "..." --- sanitize.text.function = identity
    if(sanitize_text==FALSE) {
    result <- print.xtable(dt_xtable, include.rownames = FALSE, floating = FALSE,
               sanitize.colnames.function = bold_cols, print.results = FALSE,
               booktabs = TRUE, hline.after = hlines, tabular.environment = tabular.environment,
               add.to.row = list(pos = as.list(change_rows), command = col), scalebox=scalebox)
    } else {
    result <- print.xtable(dt_xtable, include.rownames = FALSE, floating = FALSE,
               sanitize.colnames.function = bold_cols, print.results = FALSE,
               booktabs = TRUE, hline.after = hlines, tabular.environment = tabular.environment,
               add.to.row = list(pos = as.list(change_rows), command = col),
               sanitize.text.function = identity, scalebox=scalebox)
    }
  } else {
    result <- print.xtable(dt_xtable, include.rownames = FALSE, floating = FALSE,
               sanitize.colnames.function = bold_cols, print.results = FALSE,
               booktabs = TRUE, hline.after = hlines, tabular.environment = tabular.environment,
               sanitize.text.function = identity, scalebox=scalebox)
  }
  return(cat("\\begin{minipage}{\\linewidth} {",
             table_title, "\\newline }", result,
             "\\par \\bigskip", "\\end{minipage}"))
}


# see: http://tex.stackexchange.com/questions/41067/caption-for-longtable-in-sweave
long_table <- function(data_table, digits = 0, display = "s", align,
                       table_caption, table_label, group = 3,
                       hline.after = NULL, ...) {
  # create xtable
  x_long_table <- xtable(data_table, table_label, caption=NULL, align=align)
  # add brackets to column names
  names(x_long_table) <- paste0("\\textbf{", colnames(x_long_table), "}")
  # the first one is for the row.names (+1 to colnames)
  digits(x_long_table) <- rep(digits, length(colnames(x_long_table))+1)
  display(x_long_table) <- rep(display, length(colnames(x_long_table))+1)

  # create header
  longtable_header <-
    paste(paste("\\multicolumn{", as.character(ncol(x_long_table)), "}{l}{{", table_caption, "}}\\",
                sep = "", collapse = ""),
          paste("\\label{", table_label, "}\\\\ ",
                sep = "", collapse = ""),
          "\\toprule ",
          attr(x_long_table, "names")[1],
          paste(" &",
                attr(x_long_table, "names")[2:length(attr(x_long_table, "names"))],
                collapse = ""),
          "\\\\\\midrule ",
          "\\endfirsthead ",
          paste("\\multicolumn{",
                ncol(x_long_table),
                "}{c}{{", table_caption, " -- continued from previous page}}\\\\ ",
                sep = ""),
          "\\toprule ",
          attr(x_long_table, "names")[1],
          paste("&",
                attr(x_long_table, "names")[2:length(attr(x_long_table, "names"))],
                collapse = ""),
          "\\\\\\midrule ",
          "\\endhead ",
          "\\midrule ",
          paste("\\multicolumn{",
                as.character(ncol(x_long_table)),
                "}{r}{{Continued on next page}}\\\\ ",
                sep = "", collapse = ""),
          "\\bottomrule \\endfoot ",
          "\\bottomrule \\endlastfoot ",
          collapse = "")

  n_rows <- nrow(x_long_table)
  n_rows <- seq(n_rows)
  mult_length <- length(n_rows)/group
  change_rows <- as.numeric(n_rows[is.even(c(unlist(lapply(seq(floor(mult_length)), rep, group)),
                                             rep(x=ceiling(mult_length),
                                                 times = round((mult_length-floor(mult_length))*group) )))])-1
  # this avoids a cell that is totally black filled
  if(!is.null(hline.after)) {
    change_rows <- setdiff(change_rows, hline.after)
  }

  col <- rep("\\rowcolor[gray]{0.90}", length(change_rows))

  result <- print.xtable(x_long_table,
        floating = FALSE, # longtable never floats
        hline.after = hline.after,
        add.to.row = list(pos = c(-1,
                                     as.list(change_rows),
                                     nrow(x_long_table)),
                          command = c(longtable_header,
                                      col,
                                      "%")), # % sign removes trailing \hline
        include.rownames = FALSE,include.colnames = FALSE,
        type = "latex", tabular.environment = "longtable",
        sanitize.text.function = identity,
        math.style.negative = FALSE,
        print.results = FALSE)
  result <- gsub(result, pattern="\\begin{longtable}{", replace="\\begin{longtable} { >{\\raggedright}", fixed=TRUE)
  result <- gsub(result, pattern="%\\hline\n", replace="", fixed=TRUE)
  # result2 <- paste("\\scalebox{.8}{", result, "}") # does not work
  return(cat(result))

}

# helper function for my_breaks - function that rounds to the nearest... depending on size
my_round <- function(x) {round(x, -floor(log(round(x), base=10)))}

my_breaks <- function(n) {
  end <- my_round(n)
  breaks <- seq(from=0, to=end, by= end/min(5, n, na.rm=TRUE) )
  return(breaks)
}


# therapyStaff = c("Auiler, Carol", "Frederick, Elizabeth", "Lovelace, Julie",
#                  "McMillion, Tracy")
# sups = c("Edwards, Sheri", "Florence, Tim (inactive)", "Florence, Tim",
#          "Healy III, Daniel", "Negendank, Christine", "Noe, Tori", "Schrader, Lisa",
#          "Stacy, John", "Thacker, Barbara", "Washington, W. Craig")
# dmList = c("Lipke-Nelson, Cari", "Rendon, Amy")
# level5staff = c("Antkowiak")
