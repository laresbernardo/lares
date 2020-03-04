####################################################################
#' Check if Specific Package is Installed
#' 
#' This function checks library dependencies
#' 
#' @family Tools
#' @param package Character. Name of the library
#' @param stop Boolean. Stop if not installed
#' @export
try_require <- function(package, stop = TRUE) {
  if (length(find.package(package, quiet = TRUE)) > 0) {
    library(package, character.only = TRUE)
    return(invisible())
  }
  if (stop)
    stop(paste0("Package `", package, "` required. Install and try again."), call. = FALSE)
}

####################################################################
#' Convert Date into Year-Month (YYYY-MM)
#' 
#' This function lets the user convert a date into YYYY-MM format
#' 
#' @family Data Wrangling
#' @param date Date. Date we wish to transform 
#' @export
year_month <- function(date) {
  paste(year(date),str_pad(lubridate::month(date), 2, pad = "0"), sep = "-")
}


####################################################################
#' Convert Date into Year-Month (YYYY-MM)
#' 
#' This function returns categorical values for any date(s) using year
#' cuts such as bimonths, quarters, terms, and halves.
#' 
#' @family Data Wrangling
#' @param date Date. Date we wish to transform 
#' @param type Character. Any of the following: B (2 months),
#' Q (3 months), T (4 months), H (6 months)
#' @export
date_cuts <- function(date, type = "Q") {
  df <- data.frame(date = as.Date(as.character(date))) %>%
    mutate(month = month(date), cut = NA)
  if (tolower(type) == "b") aux <- 2
  if (tolower(type) == "q") aux <- 3
  if (tolower(type) == "t") aux <- 4
  if (tolower(type) == "h") aux <- 6
  cuts <- c(seq(1, 12, aux), 12)
  for (i in 1:(12/aux)) {
    df <- df %>% mutate(cut = ifelse(
      month >= cuts[i] & month <= cuts[i+1], 
      paste0(toupper(type), i), cut))
  }
  return(df$cut)
}


####################################################################
#' Convert Date into Year-Week (YYYY-WW)
#' 
#' This function lets the user convert a date into YYYY-WW format
#' 
#' @family Data Wrangling
#' @param date Date. Date we wish to transform
#' @export
year_week <- function(date) {
  paste(year(date), str_pad(lubridate::week(date), 2, pad = "0"),sep = "-")
}


####################################################################
#' Reduce categorical values
#' 
#' This function lets the user reduce categorical values in a vector. 
#' It is tidyverse friendly for use on pipelines
#' 
#' @family Data Wrangling
#' @param df Categorical Vector
#' @param ... Variables. Which variable do you wish to reduce?
#' @param nmin Integer. Number of minimum times a value is repeated
#' @param pmin Numerical. Porcentage of minimum times a value is repeated
#' @param pcummax Numerical. Top cumulative porcentage of most 
#' repeated values
#' @param top Integer. Keep the n most frequently repeated values
#' @param other_label Character. With which text do you wish to replace 
#' the filtered values with?
#' @export
categ_reducer <- function(df, ...,
                          nmin = 0, 
                          pmin = 0, 
                          pcummax = 100, 
                          top = NA, 
                          other_label = "other") {
  
  vars <- quos(...)
  
  dff <- freqs(df, !!!vars)
  
  if (!is.na(top)) {
    tops <- dff %>% slice(1:top)
  } else {
    tops <- dff %>% filter(n >= nmin & p >= pmin & p <= pcummax) 
  }
  
  name <- as.name(names(dff[,1]))
  vector <- df %>% select(as.character(name))
  new_vector <- ifelse(vector[[as.character(name)]] %in% tops[[as.character(name)]], 
                       as.character(vector[[as.character(name)]]), 
                       other_label)
  df[[as.character(name)]] <- new_vector
  
  return(df)
  
}


####################################################################
#' Normalize Vector
#' 
#' This function lets the user normalize numerical values into 
#' the 0 to 1 range
#' 
#' @family Data Wrangling
#' @param x Numeric Vector. Numbers to be transformed into 
#' normalized vector
#' @export
normalize <- function(x) {
  if (is.numeric(x)) {
    x <- (x - min(x)) / (max(x) - min(x))
    return(x) 
  } else {
    stop("Try with a numerical vector!")
  }
}


####################################################################
#' Convert a vector into a comma separated text
#' 
#' Convert a vector into a comma separated text
#' 
#' @family Data Wrangling
#' @param vector Vector. Vector with more than 1 observation
#' @param sep Character. String text wished to insert between values
#' @param quotes Boolean. Bring simple quotes for each 
#' observation (useful for SQL)
#' @export
vector2text <- function(vector, sep = ", ", quotes = TRUE) {
  output <- paste(shQuote(vector), collapse = sep)
  if (!quotes) output <- gsub("'", "", output)
  return(output)
}


####################################################################
#' Find country from a given IP
#' 
#' This function lets the user find a country from a given IP Address
#' 
#' @family Tools
#' @family Scrapper
#' @param ip Vector. Vector with all IP's we wish to search
#' @export
ip_country <- function(ip) {
  
  ip <- ip[!is.na(ip)]
  ip <- ip[grep("^172\\.|^192\\.168\\.|^10\\.", ip, invert = T)]
  
  countries <- data.frame(ip = c(), country = c())
  for (i in 1:length(ip)) {
    message(paste("Searching for", ip[i]))
    url <- paste0("https://db-ip.com/", ip[i])
    scrap <- read_html(url) %>% html_nodes('.card-body tr') %>% html_text()
    country <- gsub("Country", "", trimws(scrap[grepl("Country", scrap)]))
    result <- cbind(ip = ip[i], country = country)
    countries <- rbind(countries, result)
  } 
  return(countries)
}


####################################################################
#' Distance from specific point to line
#' 
#' This function lets the user calculate the mathematical linear distance 
#' Between a specific point and a line (given geometrical 3 points)
#' 
#' @family Calculus
#' @param a Vector. Coordinates of the point from which we want to 
#' measure the distance
#' @param b Vector. Coordinates of 1st point over the line
#' @param c Vector. Coordinates of 2st point over the line
#' @export
dist2d <- function(a, b = c(0, 0), c = c(1, 1)) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1, v2)
  d <- abs(det(m)) / sqrt(sum(v1 * v1))
  return(d)
}


####################################################################
#' Nicely Format Numerical Values
#' 
#' This function lets the user format numerical values nicely
#' 
#' @family Data Wrangling
#' @param x Numerical Vector
#' @param decimals Integer. Amount of decimals to display
#' @param type Integer. 1 for International standards. 2 for 
#' American Standards.  
#' @param scientific Boolean. Scientific notation?
#' @param pre,pos Character. Add string before or after number
#' @param abbr Boolean. Abbreviate using num_abbr()?
#' @export
formatNum <- function(x, decimals = 2, type = 2, 
                      scientific = FALSE,
                      pre = "", pos = "",
                      abbr = FALSE) {
  if (!scientific) {
    on.exit(options(scipen = 999))
  } else x <- formatC(numb, format = "e", digits = 2)
  if (abbr) {
    x <- num_abbr(x) 
  } else {
    if (type == 1) {
      x <- format(round(as.numeric(x), decimals), nsmall = decimals, 
                  big.mark = ".", decimal.mark = ",")
    } else {
      x <- format(round(as.numeric(x), decimals), nsmall = decimals, 
                  big.mark = ",", decimal.mark = ".") 
    }
    x <- trimws(x)
  }
  ret <- paste0(pre, x, pos)
  return(ret)
}


####################################################################
#' One Hot Encoding for a Vector with Comma Separated Values
#' 
#' This function lets the user do one hot encoding on a variable with 
#' comma separated values
#' 
#' @family Data Wrangling
#' @param df Dataframe. May contain one or more columns with comma separated
#' values which will be separated as one hot encoding
#' @param variables Character. Which variables should split into new columns?
#' @param sep Character. Which character separates the elements?
#' @export
one_hot_encoding_commas <- function(df, variables, sep=","){
  # Note that variables must be provided in strings
  for (var in seq_along(variables)) {
    variable <- variables[var]
    df[as.character(df) == "" | is.na(df)] <- "NAs" # Handling missingness
    x <- as.character(df[[variable]])
    x <- gsub(", ", ",", toString(x)) # So it can split on strings like "A1,A2" and "A1, A2"
    vals <- unique(unlist(strsplit(x, sep)))
    x <- paste(variable, vals, sep = "_")
    new_columns <- sort(as.character(x))
    if (length(new_columns) >= 15) {
      message(paste("You are using more than 15 unique values on this variable:", variable))
    }
    for (i in seq_along(new_columns)) {
      df$temp <- NA
      df$temp <- ifelse(grepl(vals[i], df[[variable]]), TRUE, FALSE)
      colnames(df)[colnames(df) == "temp"] <- new_columns[i]
    }
  }
  return(df)
}


####################################################################
#' Balance Binary Data by Resampling: Under-Over Sampling
#' 
#' This function lets the user balance a given data.frame by resampling
#' with a given relation rate and a binary feature.
#' 
#' @family Data Wrangling
#' @param df Vector or Dataframe. Contains different variables in each 
#' column, separated by a specific character
#' @param variable Character. Which binary variable should we use to resample df
#' @param rate Numeric. How many X for every Y we need? Default: 1. If there are
#' more than 2 unique values, rate will represent percentage for number of rows
#' @param seed Numeric. Seed to replicate and obtain same values
#' @export
balance_data <- function(df, variable, rate = 1, seed = 0) {
  
  set.seed(seed)
  
  names(df)[names(df) == variable] <- 'tag'
  tags <- unique(df$tag)
  
  if (length(tags) != 2) {
    # For numerical resampling:
    samp <- round(rate * nrow(df))
    balanced <- sample_n(df, samp)
    if (nrow(df) != samp) {
      message(paste("Resampled from", nrow(df), "to", samp, "rows")) 
    }
  } else {
    # For binary resampling:
    message(paste("Resampled from:", vector2text(formatNum(table(df$tag),0), sep = " x ", quotes = F)))
    ones <- df %>% filter(tag %in% as.character(tags[1]))
    zeros <- df %>% filter(tag %in% as.character(tags[2]))
    
    if (nrow(ones) <= nrow(zeros)) {
      message(paste("Reducing size for:", tags[2]))
      zeros <- sample_n(zeros, round(rate * nrow(ones)))
    } else {
      message(paste("Reducing size for:", tags[1]))
      ones <- sample_n(ones, round(rate * nrow(zeros)))
    }
    balanced <- rbind(ones, zeros)
    message(paste("Into:", vector2text(formatNum(table(balanced$tag),0), sep = " x ", quotes = F)))
  }
  
  balanced <- rename_at(balanced, vars("tag"), funs(paste0(variable)))
  return(balanced)
}


####################################################################
#' List files in a directory
#' 
#' This function lets the user list all files on a given directory.
#' It also lets filter files which contains a string.
#' 
#' @family Tools
#' @param folder Character. Directory which contains files
#' @param recursive Boolean. Should the listing recurse into directories?
#' @param regex Character. String to use for filtering files
#' @param images Boolean. Bring only image files?
#' @param export Boolean. Do you wish to export list as txt file?
#' @param dir Character. In which directory do you wish to save 
#' the results? Working directory as default.
#' @export
listfiles <- function(folder = getwd(), 
                      recursive = TRUE, regex = NA, images = FALSE, 
                      export = FALSE, dir = getwd()) {
  
  # require(dplyr)
  # require(lubridate)
  # require(exifr)
  
  if (!file.exists(folder)) {
    stop("That directory doesn't exist; please try again!")
  }
  
  files <- list.files(folder, recursive = recursive)
  address <- paste0(folder, "/", files)
  info <- file.info(address)
  
  df <- data.frame(filename = files, address, info)
  df$size <- as.integer(df$size/1024)
  #imgs <- "jpg|JPG|jpeg|JPEG|png|PNG|gif|GIF"
  
  if (!is.na(regex)) df <- df[grep(regex, df$filename),] 
  
  if (images) {
    try_require("exifr")
    if (nrow(df) > 250) {
      message(paste("This might take a while... Analizing around", 
                    formatNum(nrow(df), decimals = 0), "files!"))
    }
    
    tags <- c("FileName", "SourceFile",
              "CreateDate", "DateTimeOriginal", "FileModifyDate",
              "FileTypeExtension", "Megapixels",
              "ImageSize", "ImageWidth", "ImageHeight", 
              "GPSLongitude", "GPSLatitude",
              "Rotation", "Flash", "Duration")
    
    df <- read_exif(folder, recursive = TRUE) %>% 
      select(one_of(tags)) %>%
      mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal),
             CreateDate = ymd_hms(CreateDate),
             FileModifyDate = ymd_hms(FileModifyDate))
    
    df <- df[,colSums(is.na(df)) < nrow(df)]
    
  }
  
  if (export) 
    write.table(df$filename, file = file.path(dir, "files.txt"), 
                quote = FALSE, row.names = FALSE)
  
  row.names(df) <- NULL
  df$address <- NULL
  
  return(df)
  
}

####################################################################
#' Replace Values With
#' 
#' This function lets the user replace all specific values in a 
#' vector or data.frame into another value. If replacing more than
#' one value, order matters so they will be replaced in the same
#' order that you pass them to the function.
#' 
#' @family Data Wrangling
#' @family Text Mining
#' @param df Data.frame or Vector
#' @param original String or Vector. Original text you wish to replace
#' @param change String or Vector. Values you wish to replace the originals with
#' @param which Character vector. Name of columns to use. Leave "all" for everything
#' @param quiet Boolean. Keep quiet? (or print replacements)
#' @export
replaceall <- function(df, original, change, which = "all", quiet = TRUE) {
  if (is.vector(df)) {
    vector <- TRUE
    df <- data.frame(x = df)
    dic <- data.frame(original, change)
    original <- dic[,1]
    change <- dic[,2]
  } else vector <- FALSE 
  if (length(original) != length(change))
    stop("Vectors original and change should have the same length!")
  if (length(unique(original)) != length(original)) {
    aux <- freqs(dic, original) %>% filter(n > 1) %>% .$original
    stop("You have repeated original values to replace: ", vector2text(aux))
  }
  if (which[1] != "all") {
    aux <- df
    df <- select(df, one_of(which))
  }
  if (sum(is.na(original)) > 0) {
    df <- df %>% replace(is.na(.), change[is.na(original)])
    change <- change[!is.na(original)]
    original <- original[!is.na(original)]
  }
  if (sum(is.null(original)) > 0) {
    df <- df %>% replace(is.null(.), change[is.null(original)])
    change <- change[!is.null(original)]
    original <- original[!is.null(original)]
  }
  if (length(original) > 0) {
    for (i in 1:length(original)) {
      if (!quiet)
        message(paste("Transforming all", original[i], "into", change[i])) 
      df[] <- lapply(df, function(x) gsub(original[i], change[i], x))
    } 
  }
  if (which[1] != "all")
    df <- select(aux, -one_of(which)) %>% cbind(df) %>% 
      select(one_of(colnames(aux)))
  if (vector) df <- df[,1]
  return(df)
}


####################################################################
#' Remove/Drop Columns in which ALL or SOME values are NAs
#' 
#' This function lets the user remove all columns that have some or
#' all values as NAs
#' 
#' @family Data Wrangling
#' @param df Data.frame
#' @param all Boolean. Remove columns which contains ONLY NA values.
#' If set to FALSE, columns which contains at least one NA will be removed
#' @export
removenacols <- function(df, all = TRUE) {
  if (all == TRUE) {
    return(df[,colSums(is.na(df)) != nrow(df)]) 
  } else {
    return(df[,complete.cases(t(df))] )
  }
}

####################################################################
#' Remove/Drop Rows in which ALL or SOME values are NAs
#' 
#' This function lets the user remove all rows that have some or
#' all values as NAs
#' 
#' @family Data Wrangling
#' @param df Data.frame
#' @param all Boolean. Remove rows which contains ONLY NA values.
#' If set to FALSE, rows which contains at least one NA will be removed
#' @export
removenarows <- function(df, all = TRUE) {
  if (all == TRUE) {
    return(df[rowSums(is.na(df)) != ncol(df), ]) 
  } else {
    return(df[complete.cases(df), ])
  }
}


####################################################################
#' Filter only Numerical Values and
#' 
#' This function lets the user remove all rows that have some or
#' all values as NAs
#' 
#' @family Data Wrangling
#' @param df Data.frame
#' @param dropnacols Boolean. Drop columns with only NA values?
#' @param logs Boolean. Calculate log(x)+1 for numerical columns?
#' @param natransform String. "mean" or 0 to impute NA values. If
#' set to NA no calculation will run.
#' @export
numericalonly <- function(df, dropnacols = TRUE, logs = FALSE, natransform = NA) {
  
  # Drop ALL NAs columns
  if (dropnacols) df <- removenacols(df, all = TRUE) 
  
  # Which character columns may be used as numeric?
  transformable <- apply(df, 2, function(x) length(unique(x)))
  which <- names(transformable[transformable == 2])
  dfn <- data.frame(df[,colnames(df) %in% which])
  colnames(dfn) <- which
  non_numeric <- mutate_all(dfn, function(x) as.integer(as.factor(x)) - 1)
  # Which are already numeric?
  numeric <- select_if(df, is.numeric)
  
  # Calculate logs
  if (logs) {
    # Non binary numeric features
    whichlog <- colnames(numeric)[!colnames(numeric) %in% which]
    numeric <- numeric %>% mutate_at(vars(whichlog), funs(log = log(. + 1)))
    is.na(numeric) <- do.call(cbind, lapply(numeric, is.infinite))
  }
  
  # Join everything
  d <- cbind(numeric, non_numeric[!colnames(non_numeric) %in% colnames(numeric)])
  
  if (!is.na(natransform)) {
    if (natransform == 0) {
      d[is.na(d)] <- 0 
    }
    if (natransform == "mean") {
      for (i in 1:ncol(d)) {
        if (median(d[,i], na.rm = TRUE) != 0) {
          d[is.na(d[,i]), i] <- mean(d[,i], na.rm = TRUE) 
        } else {
          d[is.na(d[,i]), i] <- 0
        }
      }
    }
  }
  return(d)
}


####################################################################
#' Transform any date input into Date
#' 
#' This function lets the user transform any date input format into
#' a conventional R date format. The following formats are some of 
#' the permitted: 10-05-2019, 2019-10-05 5/22/2015, 9:45:03 AM, 
#' 42348.44, 9/2/18 23:16, 10-05-19
#' 
#' @family Data Wrangling
#' @param dates Vector. Dates in any of the permitted formats
#' @param metric Boolean. Metric or Imperial inputs. The main 
#' difference is that Metric follows the DD/MM/YYYY pattern, and 
#' Imperial follows the MM/DD/YYYY pattern.
#' @param origin Date. When importing from Excel, integers usually
#' have 1900-01-01 as origin. In R, origin is 1970-01-01.
#' @export
dateformat <- function(dates, metric = TRUE, origin = '1900-01-01') {
  
  # Check if all values are NA
  if (length(dates) == sum(is.na(dates))) {
    message("No dates where transformed becase all values are NA")
    return(dates)
  }
  
  # Delete hours
  dates <- gsub(" .*", "", as.character(dates))
  dates <- gsub("\\..*", "", as.character(dates))
  
  # Is it in integer format?
  x <- dates[!is.na(dates)][1]
  int <- as.integer(as.character(x))
  
  # When date is not an integer:
  if (is.na(int)) {
    
    year <- ifelse(nchar(x) == 10, "Y", "y")
    sym <- ifelse(grepl("/", x),"/","-")
    pattern <- str_locate_all(x, "/")[[1]][,1]
    
    if (sum(pattern == c(3,6)) == 2) {
      return(as.Date(dates, format = paste0("%m",sym,"%d",sym,"%",year)))
    } else {
      return(as.Date(dates, format = paste0("%",year,sym,"%m",sym,"%d")))
    }
  }
  
  # When date is an integer:
  if (int %in% 30000:60000) {
    dates <- as.Date(as.integer(as.character(dates)), origin = origin)
  } else {
    comps <- unlist(strsplit(x, "/|-|\\."))
    lasts <- as.integer(comps[3])
    firsts <- as.integer(comps[1])
    firstlast <- nchar(paste0(firsts, lasts))
    if (metric == FALSE) {
      # Does dates end in year?
      if (nchar(lasts) == 4 | firstlast <= 4) {
        dates <- lubridate::mdy(dates)
      }
      # Does dates start in year?
      if (nchar(firsts) == 4) {
        dates <- lubridate::ydm(dates)
      }
    } else {
      # Does dates end in year?
      if (nchar(lasts) == 4 | firstlast <= 4) {
        dates <- lubridate::dmy(dates)
      }
      # Does dates start in year?
      if (nchar(firsts) == 4) {
        dates <- lubridate::ymd(dates)
      }
    }
  }
  return(dates)
}


####################################################################
#' Pass Through a dplyr's Pipeline
#' 
#' This function lets the user print, save or do something inside a
#' pipeline without affecting the output or pipeline.
#' 
#' @family Tools
#' @param df Dataframe
#' @param fun Function. What function do you wish to run? For example:
#' pass(. \%>\% ncol \%>\% print)
#' @export
pass <- function(df, fun) { 
  fun(df)
  return(df) 
}


####################################################################
#' What's my IP
#' 
#' This function lets the user find his IP quickly
#' 
#' @family Tools
#' @export
myip <- function(){
  # require(rvest)
  # require(xml2)
  ipify <- "https://api.ipify.org/"
  ip <- xml2::read_html(ipify) %>% rvest::html_text(.)
  return(ip)
}


####################################################################
#' Plot Result with Nothing to Plot
#' 
#' This function lets the user print a plot without plot, with a 
#' customizable message. It is quite useful for Shiny renderPlot when
#' using filters and no data is returned.
#' 
#' @family Visualization
#' @param message Character. What message do you wish to show?
#' @export
noPlot <- function(message = "Nothing to show here!") {
  ggplot(data.frame(), aes(x = 0, y = 0, label = message)) + 
    geom_label() + theme_minimal() +
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank())
}

####################################################################
#' Install latest version of H2O
#' 
#' This function lets the user un-install the current version of
#' H2O installed and update to latest stable version.
#' 
#' @family Tools
#' @param p ggplot2 or gridExtra object. Plot to export
#' @param name Character. File's name or sufix if vars is not null
#' @param vars Vector. Variables in plot
#' @param sep Character. Separator for variables
#' @param width,height,res Numeric. Plot's width, height, and res (for grids)
#' @param dir Character. In which directory do you wish to save 
#' the results? Working directory as default.
#' @param subdir Character. Into which subdirectory do you wish to save the plot to?
#' @param quiet Boolean. Display succesful message with filename when saved?
#' @export
export_plot <- function(p, 
                        name = "plot", vars = NA, sep = ".vs.", 
                        width = 8, height = 6, res = 300,
                        dir = getwd(), subdir = NA,
                        quiet = FALSE) {
  
  # File name
  if (!is.na(vars)) {
    names <- vector2text(
      cleanText(as.character(vars), spaces = FALSE), sep = sep, quotes = FALSE)
    file_name <- paste0(name, "_", names, ".png")  
  } else {
    file_name <- paste0(name, ".png")  
  }
  
  # Create directory if needed
  if (!is.na(subdir)) {
    dir <- file.path(dir, subdir)
    if (!dir.exists(dir))
      dir.create(dir)
    file_name <- paste(subdir, file_name, sep = "/")
  }
  
  # Export plot to file
  png(file_name, height = height * res, width = width * res, res = res)
  plot(p)
  dev.off() 
  
  if (!quiet) message(paste("Plot saved as", file_name)) 
  
}


####################################################################
#' Calculate cuts by quantiles
#' 
#' This function lets the user quickly calculate cuts for quantiles
#' and discretize numerical values into cateogorical values.
#' 
#' @family Calculus
#' @param values Vector. Values to calculate quantile cuts
#' @param splits Integer. How many cuts should split the values?
#' @param return Character. Return "summary" or "labels"
#' @export
quants <- function(values, splits = 10, return = "summary") {
  
  if (splits > length(unique(values[!is.na(values)])) - 1) 
    stop("There are not enough observations to split the data in ", splits)
  
  cuts <- quantile(values, probs = seq(0, 1, length = splits + 1), na.rm = TRUE)
  decimals <- min(nchar(values), na.rm = TRUE) + 1
  decimals <- ifelse(decimals >= 4, 4, decimals)
  labels <- cut(values, unique(cuts), dig.lab = decimals, include.lowest = TRUE)
  
  if (return == "labels") return(labels)
  if (return == "summary") {
    output <- data.frame(percentile = names(cuts)[-1], cut = cuts[-1]) %>%
      mutate(label = paste0("(", signif(lag(cut),4), "-", signif(cut,4),"]"),
             label = gsub("\\(NA", paste0("[", signif(min(cut), 4)), label),
             label = factor(label, levels = unique(label), ordered = T))
    return(output) 
  }
}


####################################################################
#' Download Historical Currency Exchange Rate
#' 
#' This function lets the user download historical currency exchange
#' rate between two currencies
#' 
#' @family Tools
#' @param currency_pair Character. Which currency exchange do you
#' wish to get the history from? i.e, USD/COP, EUR/USD...
#' @param from Date. From date
#' @param to Date. To date
#' @param fill Boolean. Fill weekends and non-quoted dates with 
#' previous values?
#' @export
get_currency <- function(currency_pair, from = Sys.Date() - 99, to = Sys.Date(), fill = FALSE) {
  
  options("getSymbols.warning4.0" = FALSE)
  options("getSymbols.yahoo.warning" = FALSE)
  string <- paste0(toupper(cleanText(currency_pair)), "=X")
  
  if (from == to) to <- from + 1
  if (to > Sys.Date()) to <- Sys.Date()
  
  if (Sys.Date() == from) {
    x <- getQuote(string, auto.assign = FALSE)
    rownames(x) <- Sys.Date()
    x[,1] <- NULL
  } else {
    x <- data.frame(getSymbols(
      string, 
      env = NULL,
      from = from, to = to,
      src = "yahoo"))
    if (substr(rownames(x),1,1)[1] == "X") {
      x <- x[1,]
      rownames(x) <- Sys.Date()
    }
  }
  
  rate <- data.frame(date = as.Date(rownames(x)), rate = x[,1])
  
  if (fill) {
    rate <- data.frame(date = as.character(
      as.Date(as.Date(from):Sys.Date(), origin = "1970-01-01"))) %>%
      left_join(rate %>% mutate(date = as.character(date)), "date") %>%
      tidyr::fill(rate, .direction = "down") %>%
      tidyr::fill(rate, .direction = "up") %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= as.Date(from))
  }
  
  return(rate)
  
}


####################################################################
#' Convert JSON string to vector (data.frame with 1 row)
#' 
#' This function lets the user transform a JSON string into vector 
#' (data.frame with 1 row). You can also pass a Python's dictionary.
#' 
#' @family Tools
#' @param json Character. JSON string. Example of a string: '{"feat1": 
#' null, "feat2": "M"}'
#' @export
json2vector <- function(json) {
  string <- paste0("[", gsub('"',"\"", json), "]")
  string <- gsub("'",'"', string)
  string <- gsub("None","null", string)
  string <- gsub("True","true", string)
  string <- gsub("False","false", string)
  vector <- fromJSON(string)
  df <- data.frame(t(unlist(vector)))
  return(df)
}


####################################################################
#' Progressive Status Bar (Loading)
#' 
#' This function lets the user view a progressbar for a 'for' loop. 
#' 
#' @family Tools
#' @param run Iterator. for loop or an integer with the current loop number.
#' Start with 1 preferibly
#' @param max.run Number. Maximum number of loops
#' @param label String. With additionaly information to be printed 
#' at the end of the line. The default is \code{run}.
#' @param msg Character. Finish message
#' @param type Character. Loading type style: equal, domino
#' @examples
#' for (i in 1:100) {
#'   statusbar(i, 100)
#'   Sys.sleep(0.05)
#' }
#' @export
statusbar <- function(run = 1, max.run = 100, label = run, 
                      msg = "DONE", type = "equal"){
  
  if (length(run) > 1 & !is.numeric(run)) 
    stop("run must be a numerical value!")
  if (length(max.run) == 0 & !is.numeric(run)) 
    stop("max.run needs to be greater than 0!")
  
  percent.max <- getOption("width") * 0.5
  
  smart.time.format <- function(x) {
    if (x < 60) {
      suffix <- "s"
      value <- x
    } else if (x < 60 * 60) {
      suffix <- "m"
      value <- x / 60
    } else {
      suffix <- "h"
      value <- x / (60 * 60)
    }
    return(paste0(sprintf('%.1f', value), suffix))
  }
  
  if (run == 1) options("startclock" = Sys.time())
  
  if (length(max.run) > 1) {
    percent <- which(run == max.run) / length(max.run)
  } else percent <- run / max.run
  
  if (type == "domino") {
    first <- "|"
    middle <- "/"
    last <- "_"
  }
  if (type == "equal") {
    first <- " "
    middle <- "="
    last <- "="
  }
  
  percent.step <- trunc(percent * percent.max, 5)
  space <- paste(rep(" ", 20), collapse = " ")
  progress <- paste0(
    "[", paste0(rep(last, percent.step), collapse = ""), 
    ifelse(percent.step != percent.max, middle, last),
    paste0(rep(first, percent.max - percent.step), collapse = ""),"] ", 
    round(percent * 100, 0), "% | ", 
    paste(ifelse(run != max.run, paste(label, space), paste(
      msg,paste(rep(" ", 18), collapse = ""),"\n"))))
  
  now <- difftime(Sys.time(), getOption("startclock"), units = "secs")
  now <- smart.time.format(now)
  flush.console()
  cat("\r", paste(now, progress))
  if (run == max.run) options("startclock" = NULL)
}


####################################################################
#' Right: Last n characters
#' 
#' This function lets the user extract the last n characters of a
#' string or vector of strings.
#' 
#' @family Data Wrangling
#' @param string String or Vector
#' @param n Integer. How many characters from right to left?
#' @export
right <- function(string, n = 1){
  string <- as.character(string)
  r <- substr(string, nchar(string) - n + 1, nchar(string))
  return(r)
}


####################################################################
#' Left: First n characters
#' 
#' This function lets the user extract the first n characters of a
#' string or vector of strings.
#' 
#' @family Data Wrangling
#' @param string String or Vector
#' @param n Integer. How many characters from left to right?
#' @export
left <- function(string, n = 1){
  string <- as.character(string)
  l <- substr(string, 1, n)
  return(l)
}


####################################################################
#' Import Excel File with All Its Tabs
#' 
#' This function lets the user import an Excel file's tabs into a list
#' 
#' @family Tools
#' @param file String. Excel's name
#' @export
importxlsx <- function(file) {
  sheets <- getSheetNames(file)
  if (length(sheets) > 1) {
    mylist <- list()
    for (i in 1:length(sheets)) {
      sheet <- read.xlsx(file, sheet = i, skipEmptyRows = TRUE, detectDates = TRUE)  
      mylist[[i]] <- sheet
    } 
  } else {
    mylist <- read.xlsx(file, sheet = sheets, skipEmptyRows = TRUE, detectDates = TRUE)  
  }
  return(mylist)
}


####################################################################
#' Quiet prints and verbose noice
#' 
#' This function silences (verbose) output prints. Thanks to Hadley Wickham
#' for bringing the idea.
#' 
#' @family Tools
#' @param fx Function to quiet
#' @export
quiet <- function(fx) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(fx)) 
} 


####################################################################
#' Internet Connection Check
#' 
#' This function checks if your R session currently have Wifi or 
#' Internet connection.
#' 
#' @family Tools
#' @param thresh Numeric. How many seconds to consider a slow connection?  
#' @param url Character. URL to test the readLines 1 command
#' @export
haveInternet <- function(thresh = 3, url = "http://www.google.com") {
  start <- Sys.time()
  if (!capabilities(what = "http/ftp")) return(FALSE)
  test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
  if (as.numeric(Sys.time() - start) > thresh) {
    message("Slow internet connection but available...")
  }
  return(!inherits(test, "try-error"))
}


####################################################################
#' Zero Variance Columns
#' 
#' This function quickly detectes which columns have the same value
#' for each observation
#' 
#' @family Tools
#' @param df Dataframe
#' @export
zerovar <- function(df) {
  out <- lapply(df, function(x) length(unique(x)))
  which <- which(out <= 1)
  names(unlist(which))
}

####################################################################
#' Read Files Quickly (Auto-detected)
#' 
#' This function lets the user import csv, xlsx, xls, sav files.
#' 
#' @family Tools
#' @param filename Character
#' @param current_wd Boolean. Use current working directory before
#' the file's name? Use this param to NOT get absolute root directory.
#' @param sheet Character. Name or index of the sheet to read data 
#' from if file is xlsx or xls
#' @param quiet Boolean. Quiet summary message?
#' @export
read.file <- function(filename, current_wd = TRUE, sheet = 1, quiet = FALSE) {
  
  if (current_wd) filename <- paste0(getwd(), "/", filename) 
  
  if (!file.exists(filename)) {
    stop("That file doesn't exist.. try with another!")
  } else {
    
    filetype <- gsub("\\.","", right(filename, 4))
    
    if (filetype == "csv") {
      try_require("data.table")
      results <- data.frame(fread(filename))
    }
    if (filetype == "xlsx") {
      try_require("openxlsx")
      results <- read.xlsx(filename, sheet)
    }
    if (filetype == "xls") {
      try_require("gdata")
      results <- read.xls(filename, read.xls)
    }
    if (filetype == "sav") {
      try_require("foreign")
      results <- quiet(read.spss(filename, to.data.frame = T))
    }
    if (filetype == "dta") {
      # Stata version 5-12 .dta file
      #results <- foreign::read.dta(filename)
      # Stata version 13 .dta file
      try_require("readstata13")
      results <- read.dta13(filename)
    }
    if (filetype == "dat") {
      results <- read.table(filename, header = TRUE)
    } 
    
    if (!quiet)
      message(paste(
        "Imported", filetype, "file with", 
        formatNum(nrow(results), 0), "rows x", 
        formatNum(ncol(results), 0), "columns, succesfully!"))
    
  }
  if (nrow(results) == 0) warning("There is no data in that file...")
  return(results)
}


####################################################################
#' Bind Files into Dataframe
#' 
#' This function imports and binds multiple files into a single 
#' data.frame. Files must be inserted with absolute roots filenames. 
#' 
#' @family Tools
#' @param files Dataframe
#' @export
bindfiles <- function(files) {
  alldat <- data.frame()
  for (i in 1:length(files)) {
    file <- files[i]
    dfi <- read.file(file, current_wd = FALSE) 
    alldat <- rbind_full(alldat, dfi)
    statusbar(i, length(files))
  }
  return(data.frame(alldat))
}


####################################################################
#' New Line Feed for Long Character Strings
#' 
#' Add a break or new line without breaking words. Automatically,
#' the function can detect your plot's width and will dynamically
#' set an auto width. You can adjust the relation (rel) parameter
#' for different fonts and sizes until perfect harmony found.
#' 
#' @family Tools
#' @param text Character.
#' @param top Integer. How many characters aprox should be on each line
#' @param rel Numeric. Relation of pixels and characters per line
#' @export
autoline <- function(text, top = "auto", rel = 9.5) {
  
  # Auto-maximum
  if (top == "auto") top <- round(dev.size("px")[1]/rel)
  # Auto-minimum
  if (top < 15) top <- 15 
  
  # Add new lines for long texts
  iters <- ceiling(nchar(text)/top)
  if (iters > 1) {
    for (i in 1:iters) {
      if (i == 1) texti <- text
      if (i == 1) n <- 0
      texti <- gsub(".*\\n", "", text)
      pos <- as.vector(gregexpr(' ', texti)[[1]])
      sp <- pos[pos > top][1]
      if (is.na(sp) & i > 1) break
      n <- n + sp + ifelse(i > 1, 1, 0)
      text <- gsub(paste0('^(.{', n, '})(.*)$'), '\\1\n\\2', text)
    } 
  }
  return(text)
}


####################################################################
#' Auto Detect Time-Date Format
#' 
#' This function tries to detect which format is your date value and returns
#' a valid time class or same values if non-found.
#' 
#' @family Data Wrangling
#' @param vector Vector containing dates or timestamps
#' @export
formatTime <- function(vector) {
  if (class(vector)[1] == "POSIXlt" | class(vector)[1] == "POSIXct" ) {
    return(vector)
  }
  if (str_detect(vector[1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} \\+\\d{4}$")) {
    vector <- as.POSIXct(strptime(vector, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  else if (str_detect(vector[1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")) {
    vector <- as.POSIXct(strptime(vector, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  else if (str_detect(vector[1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$")) {
    vector <- as.POSIXct(strptime(vector, format = "%Y-%m-%d %H:%M", tz = "UTC"))
  }
  else if (str_detect(vector[1], "^\\d{4}-\\d{2}-\\d{2} \\d{1}$")) {
    vector <- as.POSIXct(strptime(vector, format = "%Y-%m-%d %H", tz = "UTC"))
  }
  else if (str_detect(vector[1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}$")) {
    vector <- as.POSIXct(strptime(vector, format = "%Y-%m-%d %H", tz = "UTC"))
  }
  else if (str_detect(vector[1], "^\\d{4}\\d{2}\\d{2} \\d{2}$")) {
    vector <- as.POSIXct(strptime(vector, format = "%Y%m%d %H", tz = "UTC"))
  }
  else if (str_detect(vector[1], "^\\d{4}-\\d{2}-\\d{2}$")) {
    vector <- as.POSIXct(strptime(vector, format = "%Y-%m-%d", tz = "UTC"))
  }
  else if (str_detect(vector[1], "^\\d{2}/\\d{2}/\\d{2}$")) {
    vector <- as.POSIXct(strptime(vector, format = "%m/%d/%y", tz = "UTC"))
  }
  else if (str_detect(vector[1], "^\\d{2}/\\d{2}/\\d{4}$")) {
    vector <- as.POSIXct(strptime(vector, format = "%m/%d/%Y", tz = "UTC"))
  }
  else if (str_detect(vector[1], "^\\d{4}\\d{2}\\d{2}$")) {
    vector <- as.POSIXct(strptime(vector, format = "%Y%m%d", tz = "UTC"))
  }
  else if (str_detect(vector[1], "^\\d{4}/\\d{2}/\\d{2}/\\d{2}$")) {
    vector <- as.POSIXct(strptime(vector, format = "%Y/%m/%d/%H", tz = "UTC"))
  }
  else if (str_detect(vector[1],"^\\d{4}-\\d{2}$")) {
    vector <- as.POSIXct(strptime(paste0(vector, "-01"), format = "%Y-%m-%d", tz = "UTC"))
  }
  else if (str_detect(vector[1],"^\\d{4}/\\d{2}$")) {
    vector <- as.POSIXct(strptime(paste0(vector, "/01"), format = "%Y/%m/%d", tz = "UTC"))
  }
  return(vector)
}


####################################################################
#' Check if Font is Installed
#' 
#' This function check if specific font is installed
#' 
#' @param font Character. Which font to check
#' @export
font_exists <- function(font = "Arial Narrow") {
  # Thanks to extrafont for this code
  ttf_find_default_path <- function() {
    if (grepl("^darwin", R.version$os)) {
      paths <-
        c("/Library/Fonts/",                      # System fonts
          "/System/Library/Fonts",                # More system fonts
          "/System/Library/Fonts/Supplemental",   # More system fonts
          "~/Library/Fonts/")                     # User fonts
      return(paths[file.exists(paths)])
      
    } else if (grepl("^linux-gnu", R.version$os)) {
      # Possible font paths, depending on the system
      paths <-
        c("/usr/share/fonts/",                    # Ubuntu/Debian/Arch/Gentoo
          "/usr/X11R6/lib/X11/fonts/TrueType/",   # RH 6
          "~/.fonts/")                            # User fonts
      return(paths[file.exists(paths)])
      
    } else if (grepl("^freebsd", R.version$os)) {
      # Possible font paths, depending on installed ports
      paths <-
        c("/usr/local/share/fonts/truetype/",
          "/usr/local/lib/X11/fonts/",
          "~/.fonts/")                            # User fonts
      return(paths[file.exists(paths)])
      
    } else if (grepl("^mingw", R.version$os)) {
      return(paste(Sys.getenv("SystemRoot"), "\\Fonts", sep = ""))
    } else {
      stop("Unknown platform. Don't know where to look for truetype fonts. Sorry!")
    }
  }
  check <- function(font) {
    pattern <- "\\.ttf$"
    fonts_path <- ttf_find_default_path()
    ttfiles <- list.files(fonts_path, pattern = pattern,
                          full.names = TRUE, ignore.case = TRUE)
    ret <- font %in% gsub(pattern, "", basename(ttfiles))
    return(ret)
  }
  check(font)
}


####################################################################
#' Attribute checker
#' 
#' This function checks if an object has a specific attribute and
#' stops if not
#' 
#' @param object Object of any kind
#' @param attr Character. Attribute to check
#' @param check Character. Attribute value
#' @param stop Boolean. Stop if doesn't check?
#' @export
check_attr <- function(object, attr = "type", check = "h2o_automl", stop = TRUE) {
  aux <- attr(object, attr)
  if (is.null(aux)) aux <- "Noclass"
  if (aux != check) {
    msg <- paste("Your object must be", attr, check)
    if (stop) stop(msg) else message(msg)
  }
}


####################################################################
#' Abbreviate numbers
#' 
#' This function converts a numeric vector's values into their
#' abbreviated character equivalent, i.e. 100,000,000 into 100M.
#'
#' @param x Numeric vector
#' @param n Integer. Single numeric value, specifying number of 
#' significant figures to show. Range 1 to 6.
#' @return A vector of character values that contain converted values
#' @author William Chon, \email{wchon@fb.com}
#' @examples
#' num_abbr(rnorm(100) * 1e6)
#' num_abbr(rnorm(100) * 1e6, n = 1)
#' @export
num_abbr <- function(x, n = 3) {
  
  if (!is.numeric(x)) stop('Input vector x needs to be numeric.')
  if (!is.numeric(n)) stop('n needs to be numeric.')
  if (length(n) > 1) stop('Please make sure that n takes on a single value.')
  if (!n %in% 1:6) stop('Please make sure that n takes on an interger value between 1 to 6.')
  if (any(x > 1e15))
    message('Note: You have some really large numbers, note that Qa = Quadrillion; Qi = Quintillion')
  if (any(x >= 1e21))
    stop('You have some really large numbers, output for numbers greater 
          than 1e21 are not supported by this function')
  
  # To handle scientific notation inputs correctly
  original_scipen <- getOption('scipen')
  on.exit(options(scipen = original_scipen), add = TRUE)
  options(scipen = 999)
  
  # Clean up x
  negative_positions <- ifelse(x < 0, '-', '')
  x <- abs(x)
  
  div <- findInterval(x, c(0, 1e3, 1e6, 1e9, 1e12, 1e15, 1e18))
  
  # Round x with some cleaning
  x <- round(x, -nchar(round(x, 0)) + n) / 10 ^ (3 * (div -  1))
  
  # Fix numbers rounded up to another digit 
  # i.e. 999k -> 1000k should actually be 1M
  div <- ifelse(nchar(as.integer(x)) > 3, div + 1, div)
  x <- ifelse(nchar(as.integer(x)) > 3, x / 1e3, x)
  
  # Cap decimal places to 3
  x <- round(x, 3)
  
  # Qa = Quadrillion; Qi = Quintillion
  x <- paste0(x, c('', 'K', 'M', 'B', 'T', 'Qa', 'Qi')[div])
  return(paste0(negative_positions, x))
  
}


####################################################################
#' Validate options within vector
#' 
#' This function validates if inputs match all/any of your options
#' and return error/message with possible options to use.
#'
#' @param inputs Vector character
#' @param options Vector character
#' @param type Character. Options: all, any
#' @param not Character. Options: stop, message, print, return
#' @param quiet Boolean. Keep quiet? If not, returns TRUE when checked
#' @export
check_opts <- function(inputs, options, 
                       type = "all", not = "stop", 
                       quiet = TRUE) {
  aux <- base::get(type)
  not <- base::get(not)
  if (!aux(inputs %in% options))
    not(paste("Input(s) not valid.", toupper(type), 
              "input(s) should match your options:", 
              vector2text(options)))
  if (!quiet) return(TRUE)
}
