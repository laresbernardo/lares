####################################################################
#' Convert year month format YYYY-MM
#' 
#' This function lets the user convert a date into YYYY-MM format
#' 
#' @param date Date. Date we wish to transform 
#' @export
year_month <- function(date) {
  
  return(paste(
    lubridate::year(date),
    stringr::str_pad(lubridate::month(date), 2, pad = "0"),
    sep="-"))
}


####################################################################
#' Convert year week format YYYY-WW
#' 
#' This function lets the user convert a date into YYYY-WW format
#' 
#' @param date Date. Date we wish to transform
#' @export
year_week <- function(date) {
  
  return(paste(
    lubridate::year(date),
    stringr::str_pad(lubridate::week(date), 2, pad = "0"),
    sep="-"))
}


####################################################################
#' Count Categories on a Dataframe
#' 
#' This function lets the user count unique values in a categorical 
#' dataframe
#'
#' @param df Categorical Vector
#' @export
categoryCounter <- function (df) {
  
  cats <- df %>% select_if(is.character)
  result <- c()
  
  for (i in 1:ncol(cats)) {
    x <- freqs(cats, cats[,i])
    y <- colnames(cats)[i]
    x <- cbind(variable = y, x)
    result <- rbind(result, x)
  }
  result <- rename(result, category = `cats[, i]`)
  return(result)
}


####################################################################
#' Reduce categorical values
#' 
#' This function lets the user reduce categorical values in a vector. 
#' It is tidyverse friendly for use on pipelines
#' 
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
  
  dff <- df %>%
    group_by(!!!vars) %>%
    tally() %>% arrange(desc(n)) %>%
    mutate(p = round(100*n/sum(n),2), pcum = cumsum(p))
  
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
#' Normalize values
#' 
#' This function lets the user normalize numerical values into 
#' the 0 to 1 range
#' 
#' @param x Numeric Vector. Numbers to be transformed into 
#' normalized vector
#' @export
normalize <- function(x) {
  if (is.numeric(x)) {
    x <- (x-min(x)) / (max(x)-min(x))
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
#' @param vector Vector. Vector with more than 1 observation
#' @param sep Character. String text wished to insert between values
#' @param quotes Boolean. Bring simple quotes for each 
#' observation (useful for SQL)
#' @export
vector2text <- function(vector, sep=", ", quotes = TRUE) {
  output <- paste(shQuote(vector), collapse=sep)
  if (quotes == FALSE) {
    output <- gsub("'", "", output)
  }
  return(output)
}


####################################################################
#' Clean text
#' 
#' This function lets the user clean text into getting only alphanumeric 
#' characters and no accents/symbols on letters.
#' 
#' @param text Character Vector
#' @param spaces Boolean. Keep spaces?
#' @export
cleanText <- function(text, spaces = TRUE) {
  text <- as.character(text)
  output <- tolower(gsub("[^[:alnum:] ]", "", 
                         iconv(text, from="UTF-8", to="ASCII//TRANSLIT")))
  if (spaces == FALSE) {
    output <- gsub(" ", "", output)
  }
  return(output)
}


####################################################################
#' Find country from a given IP
#' 
#' This function lets the user find a country from a given IP Address
#' 
#' @param ip Vector. Vector with all IP's we wish to search
#' @export
ip_country <- function(ip) {
  
  # require(rvest)
  # require(dplyr)
  
  ip <- ip[!is.na(ip)]
  ip <- ip[grep("^172\\.|^192\\.168\\.|^10\\.", ip, invert = T)]
  
  countries <- data.frame(ip = c(), country = c())
  for(i in 1:length(ip)) {
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
}


####################################################################
#' Nicely Format Numerical Values
#' 
#' This function lets the user format numerical values nicely
#' 
#' @param x Numerical Vector
#' @param decimals Integer. Amount of decimals to display
#' @param type Integer. 1 for International standards. 2 for 
#' American Standards.  
#' @param scientific Boolean. Scientific notation
#' @export
formatNum <- function(x, decimals = 2, type = 1, scientific = FALSE) {
  if (scientific == FALSE) {
    options(scipen=999)
  } else {
    x <- formatC(numb, format = "e", digits = 2)
  }
  if (type == 1) {
    x <- format(round(as.numeric(x), decimals), nsmall=decimals, 
                big.mark=".", decimal.mark = ",")
  } else {
    x <- format(round(as.numeric(x), decimals), nsmall=decimals, 
                big.mark=",", decimal.mark = ".") 
  }
  return(trimws(x))
}


####################################################################
#' One Hot Encoding for a Vector with Comma Separated Values
#' 
#' This function lets the user do one hot encoding on a variable with 
#' comma separated values
#' 
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
    x <- paste(variable, vals, sep="_")
    new_columns <- sort(as.character(x))
    if (length(new_columns) >= 15) {
      message(paste("You are using more than 15 unique values on this variable:", variable))
    }
    for (i in seq_along(new_columns)){
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
#' @param df Vector or Dataframe. Contains different variables in each 
#' column, separated by a specific character
#' @param variable Character. Which binary variable should we use to resample df
#' @param rate Numeric. How many X for every Y we need? Default: 1. If there are
#' more than 2 unique values, rate will represent percentage for number of rows
#' @param seed Numeric. Seed to replicate and obtain same values
#' @export
balance_data <- function(df, variable, rate = 1, seed = 0) {
  
  # require(dplyr)
  
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
#' @param folder Character. Directory which contains files
#' @param recursive Boolean. Should the listing recurse into directories?
#' @param regex Character. String to use for filtering files
#' @param images Boolean. Bring only image files?
#' @param export Boolean. Do you wish to export list as txt file?
#' @export
listfiles <- function(folder = "~", recursive = TRUE, regex = NA, images = FALSE, export = FALSE) {
  
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
  imgs <- "jpg|JPG|jpeg|JPEG|png|PNG|gif|GIF"
  
  if (!is.na(regex)) {
    df <- df[grep(regex, df$filename),] 
  }
  
  if (images == TRUE) {
    if (nrow(df) > 250) {
      message(paste("This might take a while... Analizing around", 
                    lares::formatNum(nrow(df), decimals = 0), "files!"))
    }
    
    tags <- c("FileName", "SourceFile",
              "CreateDate", "DateTimeOriginal", "FileModifyDate",
              "FileTypeExtension", "Megapixels",
              "ImageSize", "ImageWidth", "ImageHeight", 
              "GPSLongitude", "GPSLatitude",
              "Rotation", "Flash", "Duration")
    
    df <- read_exif(folder, recursive = TRUE, tags = tolower(tags)) %>% 
      select(one_of(tags)) %>%
      mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal),
             CreateDate = ymd_hms(CreateDate),
             FileModifyDate = ymd_hms(FileModifyDate))
    
    df <- df[,colSums(is.na(df)) < nrow(df)]
    
  }
  
  if (export == TRUE) {
    write.table(df$filename, file = "files.txt", quote = FALSE, row.names = FALSE) 
  }
  
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
#' @param df Data.frame or Vector
#' @param original String or Vector. Original text you wish to replace
#' @param change String or Vector. Values you wish to replace the originals with
#' @param quiet Boolean. Keep quiet? (or print replacements)
#' @export
replaceall <- function(df, original, change, quiet = TRUE) {
  if (is.vector(df)) {
    vector <- TRUE
    df <- data.frame(x = df)
    dic <- data.frame(original, change)
    original <- dic[,1]
    change <- dic[,2]
  } else { 
    vector <- FALSE 
  }
  if (length(original) != length(change)) {
    stop("Vectors original and change should have the same length!")
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
      if (!quiet) {
        message(paste("Transforming all", original[i], "into", change[i])) 
      }
      df[] <- lapply(df, function(x) gsub(original[i], change[i], x))
    } 
  }
  if (vector) {
    df <- df[,1]
  }
  return(df)
}

####################################################################
#' Remove/Drop Columns in which ALL or SOME values are NAs
#' 
#' This function lets the user remove all columns that have some or
#' all values as NAs
#' 
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
#' @param df Data.frame
#' @param dropnacols Boolean. Drop columns with only NA values?
#' @param logs Boolean. Calculate log(x)+1 for numerical columns?
#' @param natransform String. "mean" or 0 to impute NA values. If
#' set to NA no calculation will run.
#' @export
numericalonly <- function(df, dropnacols = TRUE, logs = FALSE, natransform = NA) {
  
  # Drop ALL NAs columns
  if (dropnacols == TRUE) {
    df <- removenacols(df, all = TRUE) 
  }
  
  # Which character columns may be used as numeric?
  transformable <- apply(df, 2, function(x) length(unique(x)))
  which <- names(transformable[transformable==2])
  dfn <- data.frame(df[,colnames(df) %in% which])
  colnames(dfn) <- which
  non_numeric <- mutate_all(dfn, function(x) as.integer(as.factor(x))-1)
  # Which are already numeric?
  numeric <- select_if(df, is.numeric)
  
  # Calculate logs
  if (logs == TRUE) {
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
      for(i in 1:ncol(d)){
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
#' 
#' @param dates Vector. Dates in any of the permitted formats
#' @param metric Boolean. Metric or Imperial inputs. The main 
#' difference is that Metric follows the DD/MM/YYYY pattern, and 
#' Imperial follows the MM/DD/YYYY pattern.
#' @param origin Date. When importing from Excel, integers usually
#' have 1900-01-01 as origin. In R, origin is 1970-01-01.
#' @export
dateformat <- function(dates, metric = TRUE, origin = '1900-01-01') {
  
  #require(dplyr)
  #require(stringr)
  #require(lubridate)
  options(warn=-1)
  
  # Check if all values are NA
  if(length(dates) == sum(is.na(dates))){
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
    
    if(sum(pattern == c(3,6)) == 2) {
      return(as.Date(dates, format = paste0("%m",sym,"%d",sym,"%",year)))
    } else {
      return(as.Date(dates, format = paste0("%",year,sym,"%m",sym,"%d")))
    }
  }
  
  # When date is an integer:
  if (int %in% 30000:60000) {
    dates <- as.Date(as.integer(as.character(dates)), origin=origin)
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
#' @export
myip <- function(){
  # require(rvest)
  # require(xml2)
  ipify <- "https://api.ipify.org/"
  ip <- xml2::read_html(ipify) %>% rvest::html_text()
  return(ip)
}


####################################################################
#' Plot Result with Nothing to Plot
#' 
#' This function lets the user print a plot without plot, with a 
#' customizable message. It is quite useful for Shiny renderPlot when
#' using filters and no data is returned.
#' 
#' @param message Character. What message do you wish to show?
#' @export
noPlot <- function(message = "Nothing to show here!") {
  
  # require(ggplot2)
  
  p <- ggplot(data.frame(), aes(x = 0, y = 0, label = message)) + 
    geom_label() + theme_minimal() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  return(p)
}


####################################################################
#' Install latest version of H2O
#' 
#' This function lets the user un-install the current version of
#' H2O installed and update to latest stable version.
#' 
#' @param run Boolean. Do you want to run and start an H2O cluster?
#' @export
h2o_update <- function(run = TRUE){
  
  # require(rvest)
  
  url <- "http://h2o-release.s3.amazonaws.com/h2o/latest_stable.html"
  end <- xml2::read_html(url) %>% rvest::html_node("head") %>% 
    as.character() %>% gsub(".*url=","",.) %>% gsub("/index.html.*","",.)
  newurl <- paste0(gsub("/h2o/.*","",url), end, "/R")
  # The following commands remove any previously installed H2O version
  if ("package:h2o" %in% search()) { detach("package:h2o", unload = TRUE) }
  if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
  # Now we download, install and initialize the H2O package for R.
  message(paste("Installing h2o from", newurl))
  install.packages("h2o", type="source", repos=newurl)
  if(run == TRUE){
    # require(h2o)
    h2o.init()
  }
}

####################################################################
#' Install latest version of H2O
#' 
#' This function lets the user un-install the current version of
#' H2O installed and update to latest stable version.
#' 
#' @param p ggplot2 object. Plot to export
#' @param name Character. File's name or sufix if vars is not null
#' @param vars Vector. Variables in plot
#' @param sep Character. Separator for variables
#' @param width Numeric. Plot's width on file
#' @param height Numeric. Plot's height on file
#' @param subdir Character. Into which subdirectory do you wish to save the plot to?
#' @param quiet Boolean. Display succesful message with filename when saved?
#' @export
export_plot <- function(p, 
                        name = "plot", 
                        vars = NA, 
                        sep = ".vs.", 
                        width = 8, 
                        height = 6, 
                        subdir = NA,
                        quiet = FALSE) {
  
  # File name
  if (!is.na(vars)) {
    names <- vector2text(
      cleanText(as.character(vars), spaces = FALSE), sep=sep, quotes = FALSE)
    file_name <- paste0(name, "_", names, ".png")  
  } else {
    file_name <- paste0(name, ".png")  
  }
  
  # Create directory if needed
  if (!is.na(subdir)) {
    options(warn=-1)
    dir.create(file.path(getwd(), subdir), recursive = T)
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  # Export plot to file
  p <- p + ggsave(file_name, width = width, height = height)
  
  if (quiet == FALSE) {
    message(paste("Plot saved as", file_name)) 
  }
  
}


####################################################################
#' Calculate cuts by quantiles
#' 
#' This function lets the user quickly calculate cuts for quantiles
#' 
#' @param values Vector. Values to calculate quantile cuts
#' @param splits Integer. How many cuts should split the values?
#' @param return Character. Return "summary" or "labels"
#' @export
quants <- function(values, splits = 10, return = "summary") {
  if (splits > length(unique(values[!is.na(values)]))-1) {
    stop("There are not enough observations to split the data in ", splits)
  }
  value <- as.numeric(as.character(values))
  cuts <- quantile(values, 
                   probs = seq(0, 1, length = splits+1), 
                   na.rm = TRUE)
  decimals <- min(nchar(values), na.rm = TRUE) + 1
  decimals <- ifelse(decimals >= 5, 5, decimals)
  labels <- cut(values, unique(cuts), 
                dig.lab = decimals, 
                include.lowest = TRUE)
  if (return == "labels") {
    return(labels) 
  }
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
  
  if (from == to) {
    to <- from + 1
  }
  
  if (to > Sys.Date()) {
    to <- Sys.Date()
  }
  
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
  
  rate <- data.frame(date = as.Date(rownames(x)), rate=x[,1])
  
  if (fill) {
    options(warn=-1)
    rate <- data.frame(date = as.character(
      as.Date(as.Date(from):Sys.Date(), origin="1970-01-01"))) %>%
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
#' Progressbar for for loops
#' 
#' This function lets the user view a progressbar for a 'for' loop. 
#' Taken from https://github.com/STATWORX/helfRlein/blob/master/R/statusbar.R
#' 
#' @param run Iterator. for loop or an integer with the current loop number
#' @param max.run Number. Maximum number of loops
#' @param percent.max Integer. Indicates how wide the progress bar is printed
#' @param info String. With additionaly information to be printed 
#' at the end of the line. The default is \code{run}.
#' @export
statusbar <- function (run, max.run, percent.max = 40L, info = run){
 
  if (length(run) > 1) {
    stop("run needs to be of length one!")
  }
  
  if (length(max.run) == 0) {
    stop("max.run has length 0")
  }
  
  if (length(max.run) > 1) {
    percent <- which(run == max.run) / length(max.run)
  } else {
    percent <- run / max.run
  }
  
  percent.step <- round(percent * percent.max, 0)
  progress <- paste0("[",
                     paste0(rep("=", percent.step), collapse = ""),
                     paste0(rep(" ", percent.max - percent.step), collapse = ""),
                     "] ",
                     sprintf("%7.1f", percent * 100, 2),
                     "% - ",
                     info)
  cat("\r", progress)
  flush.console()
}


####################################################################
#' Right: Last n characters
#' 
#' This function lets the user extract the last n characters of a
#' string or vector of strings.
#' 
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
#' @param file String. Excel's name
#' @export
importxlsx <- function(file) {
  sheets <- getSheetNames(file)
  if (length(sheets) > 1) {
    mylist <- list()
    for (i in 1:length(sheets)) {
      sheet <- read.xlsx(file, sheet = i, skipEmptyRows=TRUE, detectDates=TRUE)  
      mylist[[i]] <- sheet
    } 
  } else {
    mylist <- read.xlsx(file, sheet = sheets, skipEmptyRows=TRUE, detectDates=TRUE)  
  }
  return(mylist)
}


####################################################################
#' Import Excel File with All Its Tabs
#' 
#' This function silences (verbose) output prints. Thanks to Hadley Wickham!
#' 
#' @param fx Function
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
#' @param df Dataframe
#' @export
zerovar <- function(df) {
  out <- lapply(df, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}


####################################################################
#' Read Files (Auto-detected)
#' 
#' This function lets the user import csv, xlsx, xls, sav files.
#' 
#' @param filename Character
#' @param current_wd Boolean. Use current working directory before
#' the file's name? Use this param to NOT get absolute root directory.
#' @export
read.file <- function(filename, current_wd = TRUE) {
  if (current_wd) {
    wd <- getwd()
    filename <- paste0(wd, "/", filename) 
  }
  if (!file.exists(filename)) {
    stop("That file doesn't exist.. try with another!")
  } else {
    filetype <- gsub("\\.","", right(filename, 4))
    message("File type: ", filetype)
    if (filetype == "csv") {
      results <- data.frame(data.table::fread(filename))
    }
    if (filetype == "xlsx") {
      results <- openxlsx::read.xlsx(filename)
    }
    if (filetype == "xls") {
      results <- gdata::read.xls(filename)
    }
    if (filetype == "sav") {
      results <- quiet(foreign::read.spss(filename, to.data.frame = T))
    }
    message(paste("Imported", filetype, "file with", 
                  formatNum(nrow(results),0), "rows x", 
                  formatNum(ncol(results),0), "columns, succesfully!"))
  }
  if (nrow(results) == 0) {
    warning("There is no data in that file...")
  }
  return(results)
}


####################################################################
#' Bind Files into Dataframe
#' 
#' This function imports and binds multiple files into a single 
#' data.frame. Files must be inserted with absolute roots filenames. 
#' 
#' @param files Dataframe
#' @export
bindfiles <- function(files) {
  alldat <- data.frame()
  for (i in 1:length(files)) {
    file <- files[i]
    dfi <- read.file(file, current_wd = FALSE) 
    alldat <- gtools::smartbind(alldat, dfi)
    statusbar(i, length(files))
  }
  return(data.frame(alldat))
}
