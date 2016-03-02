options(repos = c(CRAN="https://cran.mtu.edu/"),
        show.error.messages = TRUE,
        showErrorCalls = TRUE,
        show.error.locations = "top")

aux <- new.env(parent = .GlobalEnv)

aux$packages <- c("xtable", "knitr", "data.table", "ggplot2", "zoo", "xlsx",
  "RODBC")

# lapply(X = aux$packages, FUN = function(x) require(x, character.only = TRUE))
pkg_loader(packages = aux$packages)
aux$channel <- "WSHSQLGP"

# inputs -----------------------------------------------------------------------
input$current_day <- date_convert(input$run_date)
input$fy_start <- as.Date(as.yearmon(paste("Oct",
                                           as.numeric(input$current_fy)-1)))
input$fy_file<- format(input$fy_start, "%m_%d_%Y")
input$fy_file <- gsub(x = input$fy_file,
                      pattern="_01_", replace="_1_", fixed=TRUE)
# calendar year
input$calendar_year <-
  if (input$current_month %in% c("October", "November", "December"))
    as.chr(as.num(input$current_fy)-1) else  input$current_fy
# end date for report which feeds many sql queries
input$report_end <-
  as.Date(as.yearmon(paste(input$current_month,
                           input$calendar_year)), frac = 1)
# modify ----------------------------------------------------------------------
modify <- new.env(parent = .GlobalEnv)
# the number of months that have passed since the start of the fiscal year
modify$num_months <- (as.yearmon(paste(
  input$current_month, input$calendar_year)) -
  as.yearmon(paste("October", as.numeric(input$current_fy) - 1))) * 12 + 1

# my_theme for ggplot----
aux$my_theme <- theme(legend.position = "topleft",
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

# max function to deal with integers and NAs and Inf
aux$my_max <- function(x) {options(warn=-1)
  result = max(as.numeric(x), na.rm=TRUE)
  if(!is.finite(result)) {result=NA_real_}
  return(result)}
# min function to deal with integers and NAs and Inf
aux$my_min <- function(x) {options(warn=-1)
  result = min(as.numeric(x), na.rm=TRUE)
  if(!is.finite(result)) {result=NA_real_}
  return(result)}

# specificy the number of decimals
aux$specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

# function to place two ggplots on one pdf----
aux$multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
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

# replaces simpleCap
# stringi::stri_trans_totitle("this is fun")

# function to trim off top 15 % then take the mean----
aux$mean_trim <- function(x, trim=.15) {
  x = x[!is.na(x)]
  xOrd = x[order(x)]
  xReduce = xOrd[1:round((1-trim)*length(xOrd)) ]
  result = mean(xReduce)
  return(result)
}

# function to replace multiple values
aux$replacer <- function(x, old, new) {
  result = new[ match(x, old)]
  return(result)
}
# example:
# x = c(rep('x',3),rep('y',3), NA, rep('z',3), NA)
# replacer(x, old=c("x","y","z"), new=as.character(1:3))

# function to create boxplot with jittered outliers with optional log and facet features #
aux$bp_jitter = function(dataset, xVar, yVar, group=NULL, jitter.x=0.2,
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
aux$are_colors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}

aux$teamsCMH <- c("ACT", "DD", "MI", "Child HB", "Child")

aux$stringNoNA =  function(..., collapse="|") {
  x <- c(...)
  x <- x[!is.na(x)]
  result = paste(x, collapse=collapse)
  return(result)
}
# aux$stringNoNA(NA, NA, collapse="|")

aux$comb_cols <- function(a, b, ...) {
  dt1 <- data.table(a, b)[, index := .I]
  result <- dt1[, list(level = aux$stringNoNA(a, b, ...)), by=index][, level]
  return(result)
}


# bin function
aux$myBin <- function(x, breaks = c(0, 30, 60, 90, 180)) {
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
aux$myBin = Vectorize(aux$myBin)

# function to make LaTeX columns bold
aux$bold_cols <- function(x) {
  paste('\\textbf{', x, '}', sep ='')
}

# is_even
aux$is_even <- function(x) x %% 2 == 0

# modify xtable for shading in groups
aux$shade <- function(group = 3, dt_xtable, n_rows, sanitize_text = NULL,
                      tabular.environment = NULL, table_title,
                      scalebox = NULL, ...) {
  if (is.null(sanitize_text) | missing(sanitize_text)) {
    sanitize_text <- FALSE
  }
  if (is.null(tabular.environment) | missing(tabular.environment)) {
    tabular.environment <- 'tabular'
  }
  if (is.null(scalebox) | missing(scalebox)) {
    scalebox <- 0.8
  }
  stopifnot(class(sanitize_text) == "logical",
            class(tabular.environment) == "character",
            class(as.num(group)) == "numeric",
            class(scalebox) == "numeric")
  hlines <- c(-1, 0, nrow(dt_xtable))
  n_rows <- seq(nrow(dt_xtable))

  # if only a few rows, dont bother shading anything
  if(max(n_rows) > group) {
    mult_length <- length(n_rows)/group
    change_rows <- as.numeric(
      n_rows[aux$is_even(c(unlist(lapply(seq(floor(mult_length)), rep, group)),
      rep(x = ceiling(mult_length),
          times = round((mult_length-floor(mult_length))*group))))])-1
    col <- rep("\\rowcolor[gray]{0.90}", length(change_rows))
    # change this to a function probably to use "..." --- sanitize.text.function = identity
    if(sanitize_text==FALSE) {
      result <-
        print.xtable(dt_xtable, include.rownames = FALSE, floating = FALSE,
         sanitize.colnames.function = aux$bold_cols, print.results = FALSE,
         booktabs = TRUE, hline.after = hlines,
         tabular.environment = tabular.environment,
         add.to.row = list(pos = as.list(change_rows), command = col),
         scalebox=scalebox)
    } else {
      result <-
        print.xtable(dt_xtable, include.rownames = FALSE, floating = FALSE,
         sanitize.colnames.function = aux$bold_cols, print.results = FALSE,
         booktabs = TRUE, hline.after = hlines,
         tabular.environment = tabular.environment,
         add.to.row = list(pos = as.list(change_rows), command = col),
         sanitize.text.function = identity, scalebox=scalebox)
    }
  } else {
    result <-
      print.xtable(dt_xtable, include.rownames = FALSE, floating = FALSE,
        sanitize.colnames.function = aux$bold_cols, print.results = FALSE,
        booktabs = TRUE, hline.after = hlines, tabular.environment = tabular.environment,
        sanitize.text.function = identity, scalebox=scalebox)
  }
  return(cat("\\begin{minipage}{\\linewidth} {",
             table_title, "\\newline }", result,
             "\\par \\bigskip", "\\end{minipage}"))
}


# see: http://tex.stackexchange.com/questions/41067/caption-for-longtable-in-sweave
aux$long_table <- function(data_table, digits_n = NULL, display_type = NULL, align,
                       table_caption, table_label, group = NULL,
                       hline.after = NULL, ...) {
  if (is.null(digits_n) | missing(digits_n)) {
    digits_n <- 0
  }
  if (is.null(display_type) | missing(display_type)) {
    display_type <- "s"
  }
  if (is.null(group) | missing(group)) {
    group <- 3
  }
  stopifnot(class(as.num(digits_n)) == "numeric",
            class(display_type) == "character",
            class(as.num(group)) == "numeric")
  # create xtable
  x_long_table <- xtable(x = data_table, caption=NULL, label = table_label,
                         align = align, digits = digits_n)
  # add brackets to column names
  names(x_long_table) <- paste0("\\textbf{", names(x_long_table), "}")
  # the first one is for the row.names (+1 to names)
  digits(x_long_table) <- rep(digits_n, length(names(x_long_table))+1)
  display(x_long_table) <- rep(display_type, length(names(x_long_table))+1)

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
  change_rows <- as.numeric(n_rows[aux$is_even(c(unlist(lapply(
    seq(floor(mult_length)), rep, group)),
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
  result <- gsub(result, pattern="\\begin{longtable}{",
                 replace="\\begin{longtable} { >{\\raggedright}", fixed=TRUE)
  result <- gsub(result, pattern="%\\hline\n", replace="", fixed=TRUE)
  # result2 <- paste("\\scalebox{.8}{", result, "}") # does not work
  return(cat(result))
}

# helper function for my_breaks - function that rounds to the nearest... depending on size
aux$my_round <- function(x) {round(x, -floor(log(round(x), base=10)))}

aux$my_breaks <- function(n) {
  end <- aux$my_round(n)
  breaks <- seq(from=0, to=end, by= end/min(5, n, na.rm=TRUE) )
  return(breaks)
}

# not all of these documents are in unsigned_docs, but someday...
# # keep these documents only for unsigned docs
# docsKeep <- c("Bio/Psycho/Social",
#              "Group Progress Note",
#              "Emergency Note",
#              "Injection/Dispense Note",
#              "IPOS",
#              "Medication Renewal",
#              "Medication Review Note",
#              "Nurse Progress Note",
#              "Nutrition Assessment",
#              "Personal Health Review",
#              "Pre-Screening Assessment",
#              "Progress Note",
#              "Progress Review",
#              "Psychiatric Evalulation",
#              "Screening Form",
#              "Wellness Note")