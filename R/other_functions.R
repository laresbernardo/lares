####################################################################
#' Check if Specific Package is Installed
#' 
#' This function checks library dependencies
#' 
#' @family Tools
#' @param package Character. Name of the library
#' @param stop Boolean. Stop if not installed
#' @examples 
#' # Check if library dummylibrary is installed. If not, do not break as error.
#' try_require("dummylibrary", stop = FALSE)
#' # Check if library base is installed. If not, stop and show error
#' try_require("base", stop = TRUE)
#' @export
try_require <- function(package, stop = TRUE) {
  if (length(find.package(package, quiet = TRUE)) > 0) {
    suppressMessages(library(package, character.only = TRUE))
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
#' @examples 
#' year_month(Sys.Date())
#' @export
year_month <- function(date) {
  paste(year(date), str_pad(lubridate::month(date), 2, pad = "0"), sep = "-")
}


####################################################################
#' Convert Date into Year + Cut
#' 
#' This function returns categorical values for any date(s) using year
#' cuts such as bimonths, quarters, terms, and halves.
#' 
#' @family Data Wrangling
#' @param date Date. Date we wish to transform 
#' @param type Character. Any of the following: B (2 months),
#' Q (3 months), T (4 months), H (6 months)
#' @examples 
#' date_cuts(Sys.Date(), type = "Q")
#' date_cuts(Sys.Date(), type = "H")
#' @export
date_cuts <- function(date = Sys.Date(), type = "Q") {
  df <- data.frame(date = as.Date(as.character(date))) %>%
    mutate(month = month(.data$date), cut = NA)
  if (tolower(type) == "b") aux <- 2
  if (tolower(type) == "q") aux <- 3
  if (tolower(type) == "t") aux <- 4
  if (tolower(type) == "h") aux <- 6
  cuts <- c(seq(1, 12, aux), 12)
  for (i in 1:(12/aux)) {
    df <- df %>% mutate(cut = ifelse(
      .data$month >= cuts[i] & .data$month <= cuts[i+1], 
      paste0(toupper(type), i), .data$cut))
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
#' @examples 
#' year_week(Sys.Date())
#' @export
year_week <- function(date) {
  paste(year(date), str_pad(lubridate::week(date), 2, pad = "0"), sep = "-")
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
#' @examples 
#' data(dft) # Titanic dataset
#' categ_reducer(dft, Embarked, top = 2) %>% freqs(Embarked)
#' categ_reducer(dft, Ticket, nmin = 7, other_label = "Other Ticket") %>% freqs(Ticket)
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
    tops <- dff %>% filter(.data$n >= nmin & .data$p >= pmin & .data$p <= pcummax) 
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
#' @examples 
#' x <- c(0, 1, 4, 7.5, 10)
#' normalize(x)
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
#' @param vector Vector. Vector with more than 1 observation.
#' @param sep Character. String text wished to insert between values.
#' @param quotes Boolean. Bring simple quotes for each observation.
#' @param and Character. Add 'and' or something before last observation. 
#' Not boolean variable so it can be used on other languages. Note that
#' the last comma will be suppressed if \code{options("lares.formatNum")} 
#' is set to \code{1} and you have less than 3 values.
#' @examples
#' vector2text(LETTERS[1:5])
#' vector2text(c(1:5), quotes = FALSE)
#' vector2text(c(1:5), quotes = FALSE, sep = "-")
#' vector2text(c(1:5), and = "and also")
#' # Shorter function with same purpose
#' v2t(LETTERS[1:5])
#' @export
vector2text <- function(vector, sep = ", ", quotes = TRUE, and = "") {
  
  # Add "and" or something before the last value
  n <- length(vector)
  if (and != "") {
    # Makes no sense to keep quotes but leave the option
    quotes <- !quotes 
    if (n > 1) {
      vector <- c(vector[1:(n - 1)], paste(and, vector[n]))
    } 
  }
  
  # Paste everythign together
  output <- paste(shQuote(vector), collapse = sep)
  
  # Get rid of quotes
  if (!quotes) 
    output <- gsub("'", "", output)
  
  # Get rid of the last comma when using and?
  if (and != "" & (getOption("lares.formatNum") == 1 | n == 2)) {
    last_comma <- tail(c(gregexpr(",", output)[[1]]), 1)
    output <- paste0(substr(output, 1, last_comma - 1),
                     substr(output, last_comma + 1, nchar(output)))
  }
  
  return(output)
}
#' @rdname vector2text
#' @export
v2t <- vector2text

####################################################################
#' Find country from a given IP
#' 
#' This function lets the user find a country from a given IP Address
#' 
#' @family Tools
#' @family Scrapper
#' @param ip Vector. Vector with all IP's we wish to search
#' @examples 
#' \dontrun{
#' ip_country(ip = myip())
#' ip_country(ip = "163.114.132.0")
#' }
#' @export
ip_country <- function(ip = myip()) {
  
  ip <- ip[!is.na(ip)]
  ip <- ip[grep("^172\\.|^192\\.168\\.|^10\\.", ip, invert = TRUE)]
  
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
#' @param x Vector. Coordinates of the point from which we want to 
#' measure the distance
#' @param a Vector. Coordinates of 1st point over the line
#' @param b Vector. Coordinates of 2st point over the line
#' @examples 
#' dist2d(x = c(5, 2))
#' dist2d(x = c(5, 2), a = c(0, 0), b = c(0, 1))
#' dist2d(x = c(5, 2), a = c(0, 0), b = c(1, 0))
#' @export
dist2d <- function(x, a = c(0, 0), b = c(1, 1)) {
  if (all(c(length(x), length(a), length(b)) == 2)) {
    v1 <- a - b
    v2 <- x - a
    m <- cbind(v1, v2)
    d <- abs(det(m)) / sqrt(sum(v1 * v1))
    return(d)
  } else {
    stop("Every point most consist of 2 values (X & Y), thus all input lengths must be 2.")
  } 
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
#' American Standards. Use options("lares.formatNum" = 2) to
#' set this parameter globally.
#' @param scientific Boolean. Scientific notation?
#' @param pre,pos Character. Add string before or after number
#' @param abbr Boolean. Abbreviate using num_abbr()? You can use
#' the `decimals` parameter to set abbr's `n`(-1) parameter.
#' @examples 
#' formatNum(1.23456, decimals = 3)
#' formatNum(1.23456, type = 1)
#' formatNum(1.23456, pre = "$", pos = "/p")
#' formatNum(123456, abbr = TRUE)
#' formatNum(1234567890, abbr = TRUE)
#' formatNum(1234567890, decimals = 0, abbr = TRUE)
#' @export
formatNum <- function(x, decimals = 2, 
                      type = getOption("lares.formatNum"), 
                      scientific = FALSE,
                      pre = "", pos = "",
                      abbr = FALSE) {
  if (!scientific) {
    on.exit(options(scipen = 999))
  } else x <- formatC(x, format = "e", digits = 2)
  if (abbr) {
    x <- num_abbr(x, n = decimals + 1) 
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
#' @param sep Character. Which regular expression separates the elements?
#' @examples 
#' df <- data.frame(id = c(1:5),
#'                  x = c("AA, D", "AA,B", "B,  D", "A,D,B", NA),
#'                  z = c("AA+BB+AA", "AA", "BB,  AA", NA, "BB+AA"))
#' ohe_commas(df, "x")
#' ohe_commas(df, c("x", "z"), sep = "\\+|,")
#' @export
ohe_commas <- function(df, variables, sep = ",") {
  for (var in variables) {
    df$temp <- as.character(df[,var])
    # Handling missingness
    df$temp[as.character(df$temp) == "" | is.na(df$temp)] <- "NoVal"
    vals <- v2t(as.character(df$temp), quotes = FALSE)
    vals <- unique(trimws(unlist(strsplit(vals, sep))))
    # aux <- sprintf("--%s--", vals)
    l <- strsplit(df$temp, sep)
    mat <- c()
    for (i in 1:length(vals)) {
      which <- unlist(lapply(l, function(x) any(trimws(x) %in% vals[i])))
      mat <- cbind(mat, which)
    }
    colnames(mat) <- paste(var, vals, sep = "_")
    df$temp <- NULL
    df <- cbind(df, mat)
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
#' @examples 
#' data(dft) # Titanic dataset
#' df <- balance_data(dft, "Survived", rate = 1, seed = 123)
#' freqs(df, Survived)
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
    ones <- df %>% filter(.data$tag %in% as.character(tags[1]))
    zeros <- df %>% filter(.data$tag %in% as.character(tags[2]))
    
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
#' Get Meta Data from Image Files
#' 
#' This function lets the user get meta data from image files or directory.
#' 
#' @family Tools
#' @param files Character vector. Files or directory which contains files.
#' @export
image_metadata <- function(files) {
  
  try_require("exifr")
  
  files <- as.character(files)
  if (length(files) == 1)
    if (dir.exists(files))
      files <- listfiles(files, recursive = TRUE)
  
  df <- data.frame(file = files)
  tags <- c("FileName", "SourceFile",
            "CreateDate", "DateTimeOriginal", "FileModifyDate",
            "FileTypeExtension", "Megapixels",
            "ImageSize", "ImageWidth", "ImageHeight", 
            "GPSLongitude", "GPSLatitude", 
            "GPSLatitudeRef", "GPSLongitudeRef",
            "Rotation", "Flash", "Duration", 
            "Make", "Model")
  
  aux <- ceiling(nrow(df)/500)
  for (i in 1:aux) {
    if (i == 1) {
      ret <- c()
      if (aux > 2)
        message(paste("This might take a while... Analizing", 
                      formatNum(nrow(df), decimals = 0), "files!"))
    }
    from <- (i - 1) * 500 + 1
    to <- i*500
    x <- slice(df, from:to)
    temp <- read_exif(as.character(x$file), tags = tags)
    if (nrow(temp) > 0)
      if ("DateTimeOriginal" %in% colnames(temp))
        ret <- bind_rows(ret, select(temp, one_of(tags)))
    statusbar(i, aux, label = paste(formatNum(from, 0), "-", formatNum(to, 0)))
  }
  
  if (length(ret) > 0) {
    if ("DateTimeOriginal" %in% colnames(ret)) {
      df <- ret %>% 
        mutate(DateTimeOriginal = ymd_hms(.data$DateTimeOriginal),
               CreateDate = ymd_hms(.data$CreateDate),
               FileModifyDate = ymd_hms(.data$FileModifyDate))
      df <- df[,colSums(is.na(df)) < nrow(df)] 
      if (aux > 10)
        tryCatch({
          try_require("beepr", stop = FALSE)
          beep() 
        }) 
      return(df)
    }
  } else message("No images found to process...")
  
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
#' @examples 
#' # All files in current directory (without recursive files)
#' df <- listfiles(recursive = TRUE)
#' head(df, 3)
#' 
#' # All files in current directory (with recursive files)
#' df <- listfiles(recursive = TRUE)
#' tail(df, 3)
#' 
#' # Check R files using regex
#' df <- listfiles(regex = "\\.R$")
#' @export
listfiles <- function(folder = getwd(), 
                      recursive = TRUE, 
                      regex = NA, 
                      images = FALSE) {
  
  if (!file.exists(folder))
    stop("That directory doesn't exist; please try again!")
  
  files <- list.files(folder, recursive = recursive)
  address <- paste0(folder, "/", files)
  info <- file.info(address)
  
  df <- data.frame(filename = files, address, info)
  df$size <- as.integer(df$size/1024)
  #imgs <- "jpg|JPG|jpeg|JPEG|png|PNG|gif|GIF"
  
  if (!is.na(regex)) df <- df[grep(regex, df$filename),] 
  
  if (images)
    df <- image_metadata(df$address)
  
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
#' order that you pass them to the function. Factors will be refactored.
#' 
#' @family Data Wrangling
#' @family Text Mining
#' @param df Data.frame or Vector
#' @param original String or Vector. Original text you wish to replace
#' @param change String or Vector. Values you wish to replace the originals with
#' @param which Character vector. Name of columns to use. Leave "all" for everything
#' @param fixclass Boolean. Try to detect logical classes after transformations (or 
#' leave as default classes as character)?
#' @param quiet Boolean. Keep quiet? (or print replacements)
#' @examples 
#' df <- data.frame(one = c(1:4, NA), 
#'                  two = LETTERS[1:5], 
#'                  three = rep("A", 5), 
#'                  four = c(NA, "Aaa", 123, "B", "C"))
#' print(df)
#' 
#' replaceall(df, "A", NA)
#' 
#' replaceall(df, "A", "a")
#' 
#' replaceall(df, 1, "*")
#' 
#' replaceall(df, NA, "NotNA")
#' 
#' replaceall(df, NA, 0)
#' 
#' replaceall(df, c("A", "B"), c("'A'", "'B'"))
#' 
#' replaceall(df, "a", "*", which = "four")
#' @export
replaceall <- function(df, original, change, which = "all", 
                       fixclass = TRUE, quiet = TRUE) {
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
    aux <- freqs(dic, original) %>% filter(.data$n > 1) %>% .$original
    stop("You have repeated original values to replace: ", vector2text(aux))
  }
  if (which[1] != "all") {
    aux <- df
    df <- select(df, one_of(which))
  }
  df <- df %>% mutate_all(as.character)
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
    df <- select(aux, -one_of(which)) %>% 
    cbind(df) %>% 
    select(one_of(colnames(aux)))
  if (vector) 
    df <- df[,1]
  if (fixclass)
    df <- suppressMessages(type.convert(df, numerals = "no.loss", as.is = TRUE))
  if (vector)
    return(as.vector(df))
  else 
    return(as_tibble(df))
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
#' all values as NAs. Note that as logical columns may be treated
#' as numerical (1s and 0s), those will be kept.
#' 
#' @family Data Wrangling
#' @param df Data.frame
#' @param dropnacols Boolean. Drop columns with only NA values?
#' @param logs Boolean. Calculate log(x)+1 for numerical columns?
#' @param natransform String. "mean" or 0 to impute NA values. If
#' set to NA no calculation will run.
#' @examples 
#' data(dft) # Titanic dataset
#' str(dft)
#' numericalonly(dft) %>% head()
#' numericalonly(dft, natransform = "mean") %>% head()
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
  ipify <- "https://api.ipify.org/"
  ip <- xml2::read_html(ipify) %>% html_text()
  return(ip)
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
#' @examples 
#' data(dft) # Titanic dataset
#' quants(dft$Age, splits = 5)
#' quants(dft$Age, splits = 5, return = "labels")[1:10]
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
      mutate(label = paste0("(", signif(lag(.data$cut),4), "-", signif(.data$cut,4),"]"),
             label = gsub("\\(NA", paste0("[", signif(min(.data$cut), 4)), .data$label),
             label = factor(.data$label, levels = unique(.data$label), ordered = TRUE))
    return(output) 
  }
}


####################################################################
#' Convert Python JSON string to R vector (data.frame with 1 row)
#' 
#' This function lets the user transform a JSON string into vector 
#' (data.frame with 1 row). You can also pass a Python's dictionary.
#' For any other JSON transformation, `jsonlite` is recommended.
#' 
#' @family Tools
#' @param json Character. JSON string
#' @examples 
#' json2vector('{"id": 1, "nodata": null, "gender": "M"}')
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
#' Right: Last n characters
#' 
#' This function lets the user extract the last n characters of a
#' string or vector of strings.
#' 
#' @family Data Wrangling
#' @param string String or Vector
#' @param n Integer. How many characters from right to left?
#' @examples
#' right("Bernardo", 3)
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
#' @examples
#' left("Bernardo", 3)
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
  mylist <- list()
  for (i in 1:length(sheets)) {
    sheet <- read.xlsx(file, 
                       sheet = i, 
                       skipEmptyRows = TRUE, 
                       skipEmptyCols = TRUE,
                       startRow = 1,
                       detectDates = TRUE)  
    mylist[[sheets[i]]] <- sheet
  } 
  if (length(sheets) == 1)
    mylist <- mylist[[1]][[1]]
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
#' @examples 
#' df <- data.frame(a = c(1, NA, 3), b = rep(NA, 3), c = rep(5, 3))
#' print(df)
#' zerovar(df)
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
#' @param text Character vector.
#' @param top Integer. How many characters aprox should be on each line
#' @param rel Numeric. Relation of pixels and characters per line
#' @examples 
#' \dontrun{
#' autoline("This is a long text that may not fit into a single line")
#' autoline("This is a long text that may not fit into a single line", rel = 25)
#' }
#' @export
autoline <- function(text, top = "auto", rel = 9.5) {
  
  # Auto-maximum
  if (top == "auto") top <- round(dev.size("px")[1]/rel)
  # Auto-minimum
  if (top < 5) top <- 5
  
  ret <- lapply(text, function(x) {
    # Add new lines for long texts
    iters <- ceiling(nchar(x)/top)
    if (iters > 1) {
      for (i in 1:iters) {
        if (i == 1) texti <- x
        if (i == 1) n <- 0
        texti <- gsub(".*\\n", "", x)
        pos <- as.vector(gregexpr(' ', texti)[[1]])
        sp <- pos[pos > top][1]
        if (is.na(sp)) break
        n <- n + sp + ifelse(i > 1, 1, 0)
        x <- gsub(paste0('^(.{', n, '})(.*)$'), '\\1\n\\2', x)
      } 
    } else x
    return(x)
  })
  return(unlist(ret))
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
#' This function checks if a font is installed in your machine.
#' 
#' @param font Character. Which font to check
#' @examples
#' font_exists(font = "Arial Narrow")
#' font_exists(font = "Weird Font")
#' @export
font_exists <- function(font = "Arial Narrow") {
  
  tryCatch({
    
    # Thanks to extrafont for the idea for this code
    ttf_find_default_path <- function() {
      if (grepl("^darwin", R.version$os)) {
        paths <- c("/Library/Fonts/",
                   "/System/Library/Fonts",
                   "/System/Library/Fonts/Supplemental",
                   "~/Library/Fonts/")
        return(paths[file.exists(paths)])
        
      } else if (grepl("^linux-gnu", R.version$os)) {
        # Possible font paths, depending on the system
        paths <- c("/usr/share/fonts/",
                   "/usr/X11R6/lib/X11/fonts/TrueType/",
                   "~/.fonts/")
        return(paths[file.exists(paths)])
        
      } else if (grepl("^freebsd", R.version$os)) {
        # Possible font paths, depending on installed ports
        paths <- c("/usr/local/share/fonts/truetype/",
                   "/usr/local/lib/X11/fonts/",
                   "~/.fonts/")
        return(paths[file.exists(paths)])
        
      } else if (grepl("^mingw", R.version$os)) {
        return(paste(Sys.getenv("SystemRoot"), "\\Fonts", sep = ""))
      } else {
        stop("Unknown platform. Don't know where to look for truetype fonts. Sorry!")
      }
    }
    check <- function(font) {
      pattern <- "\\.ttf$|\\.otf"
      fonts_path <- ttf_find_default_path()
      ttfiles <- list.files(fonts_path, pattern = pattern,
                            full.names = TRUE, ignore.case = TRUE)
      ret <- font %in% gsub(pattern, "", basename(ttfiles))
      return(ret)
    }
    check(font)
  }, error = function(err) {
    message(paste("Font issue detected:", err))
    options("lares.font" = NA)
    return(FALSE)
  })
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
#' @examples
#' num_abbr(rnorm(10) * 1e6)
#' num_abbr(rnorm(10) * 1e6, n = 1)
#' @export
num_abbr <- function(x, n = 3) {
  
  if (!is.numeric(x)) stop('Input vector x needs to be numeric.')
  if (!is.numeric(n)) stop('n needs to be numeric.')
  if (length(n) > 1) stop('Please make sure that n takes on a single value.')
  if (!n %in% 1:6) stop('Please make sure that n takes on an interger value between 1 to 6.')
  
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
  
  output <- paste0(negative_positions, x)
  output[grepl("NA", output)] <- NA
  
  return(output)
  
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
#' @param quiet Boolean. Keep quiet? If not, returns TRUE or FALSE
#' @examples 
#' options <- c("A", "B", "C")
#' # Let's check the "all" logic
#' check_opts(inputs = c("A", "B"), options, quiet = FALSE)
#' check_opts(inputs = c("X"), options, not = "message", quiet = FALSE)
#' check_opts(inputs = c("A","X"), options, not = "warning")
#' # Now let's check the "any" logic
#' check_opts(inputs = c("A","X"), options, type = "any")
#' check_opts(inputs = c("X"), options, type = "any", not = "message")
#' check_opts(inputs = c("A", NA), options, type = "any")
#' # Final trick: just ignore results
#' check_opts(inputs = "X", options, not = "invisible")
#' @export
check_opts <- function(inputs, options, 
                       type = "all", not = "stop", 
                       quiet = TRUE) {
  aux <- base::get(type)
  not <- base::get(not)
  isit <- aux(inputs %in% options)
  if (!isit) {
    if (type == "all")
      inputs <- inputs[which(!inputs %in% options)]
    not(paste("Your input", vector2text(inputs), 
              "is not valid;", toupper(type),
              "of the inputs should match these options:", 
              vector2text(options))) 
  }
  if (!quiet) 
    return(isit)
}

####################################################################
#' Flatten lists into data.frame
#' 
#' Flatten a list with possible multiple lists within into a single
#' data.frame with valid column names.
#'
#' @param x List
#' @param quiet Boolean. Keep quiet? (or show statusbar)
#' @export
flatten_list <- function(x, quiet = FALSE) {
  n <- length(x)
  for (i in 1:n) {
    if (i == 1) ret <- c()
    values <- unlist(x[[i]])
    aux <- data.frame(t(values))
    ret <- suppressWarnings(bind_rows(ret, aux))
    if (n > 500 & !quiet) statusbar(i, n, i)
    if (i == n) ret <- as_tibble(ret)
  }  
  return(ret)
}


####################################################################
#' List categorical values for data.frame
#' 
#' Make a list with all categorical values and
#'
#' @param df data.frame
#' @param ... Variables to segment counters
#' @param abc Boolean. Sort alphabetically?
#' @examples 
#' data(dft) # Titanic dataset
#' df <- dft[,1:5]
#' head(df)
#' list_cats(df)
#' @export
list_cats <- function(df, ..., abc = TRUE) {
  is.categorical <- function(x) is.character(x) | is.factor(x)
  category <- which(sapply(df, is.categorical))
  ret <- list()
  for (i in 1:length(category)) {
    which <- as.character(names(category)[i])
    df[,which] <- as.character(df[,which])
    aux <- freqs(df, cats = base::get(which), ...) %>%
      select(-.data$pcum, -.data$order)
    if (abc) aux <- arrange(aux, .data$cats)
    colnames(aux)[1] <- which
    ret[[which]] <- aux
  }
  return(ret)
}

####################################################################
#' Replace Factor Values
#' 
#' This function lets the user replace levels on a factor vector.
#' 
#' @param x Factor (or Character) Vector
#' @param original String or Vector. Original text you wish to replace
#' @param change String or Vector. Values you wish to replace the originals with
#' @examples
#' \dontrun{
#'  data(dft)
#'  # Replace a single value
#'  dft <- mutate(dft, Pclass = replacefactor(Pclass, original = "1", change = "First"))
#'  # Replace multiple values
#'  dft <- mutate(dft, Pclass = replacefactor(Pclass, c("2","3"), c("Second", "Third")))
#' }
#' @export
replacefactor <- function(x, original, change) {
  if (length(original) != length(change))
    stop("You must provide same length vectors for original and change!")
  for (i in 1:length(change)) {
    if (!is.factor(x)) {
      warning("Automatically turned non-factor variable into factor")
      x <- factor(x, levels = unique(as.character(x)))
    }
    levels(x)[levels(x) == original[i]] <- change[i]
  }
  return(x)
}

####################################################################
#' List all functions used in an R script file by package
#'
#' Parses all functions called by an R script and then lists
#' them by package. Wrapper for 'getParseData'. May be of great
#' use for those developing a package to help see what 
#' namespace 'importsFrom' calls will be required.
#' @param filename Character. Path to an R file containing R code.
#' @param alphabetic Boolean. List functions alphabetically.
#' If FALSE, will list in order of appearance.
#' @return Returns a list. Parses all functions called by an R script 
#' and then lists them by package. Those from the script itself are listed
#' under '.GlobalEnv' and any functions that may originate
#' from multiple packages have all possibilities listed. Those listed under
#' 'character(0)' are those for which a package could not be found- may be
#' functions within functions, or from packages that aren't loaded.
#' @examples
#' \dontrun{
#' # Choose an R script file with functions
#' rfile <- file.choose()
#' list_fxs_file(rfile)
#' }
#' @export 
list_fxs_file <- function(filename, alphabetic = TRUE) {
  if (!file.exists(filename)) 
    stop("Couldn't find file ", filename)
  if (!right(toupper(filename), 1) == "R")
    warning("Expecting *.R file, will try to proceed")
  tmp <- getParseData(parse(filename, keep.source = TRUE))
  nms <- tmp$text[which(tmp$token == "SYMBOL_FUNCTION_CALL")]
  funs <- unique(if (alphabetic) {sort(nms)} else {nms})
  src <- paste(as.vector(sapply(funs, find)))
  outlist <- tapply(funs, factor(src), c)
  return(outlist)
}


####################################################################
#' Move files from A to B
#'
#' Move one or more files from a directory to another using R.
#'
#' @param from Character. Filanames and directories. All files 
#' will be moved recursively. 
#' @param to Character. Filenames for each \code{from} file or 
#' directory. If directory does not exist, it will be created.
#' @export 
move_files <- function(from, to) {
  
  froms <- dirs <- c()
  for (i in 1:length(from)) {
    fromi <- from[i]
    if (isTRUE(file.info(fromi)$isdir)) {
      fromi <- list.files(fromi, recursive = TRUE)
      fromi <- paste(from[i], fromi, sep = "/")
      dirs <- c(dirs, from[i])
    }
    froms <- c(froms, fromi)   
  }
  froms <- froms[grepl("\\.", basename(froms))]
  froms <- gsub(paste0(getwd(),"/"), "", froms)
  
  tos <- to
  # If it is a directory
  if (length(tos) == 1) {
    if (!isTRUE(file.info(tos)$isdir)) {
      dir.create(tos, recursive = FALSE)
      message(sprintf("Directory '%s' did not exist and was created", tos))
    }
    tos <- paste(to, sub(".*?/", "", froms), sep = "/")
  }
  
  # Final check for all files
  if (length(froms) == 0) {
    warning(sprintf("No files to move from %s...", from))
    return(invisible(NULL)) 
  }
  if (length(tos) != length(froms))
    stop("Every 'from' must have a respective 'to' filename")
  
  # Now move/rename all files
  for (i in 1:length(froms)) {
    todir <- dirname(tos[i])
    if (!isTRUE(file.info(todir)$isdir)) 
      dir.create(todir, recursive = FALSE)
    file.rename(from = froms[i],  to = tos[i])
  }
  
}


####################################################################
#' Pattern Matching and Replacement Anywhere with Blanks
#'
#' Match and replace pattern with blanks within each element of a 
#' character vector, allowing counted characters between patterns.
#'
#' @param vector Character vector
#' @param pattern Character. Character string containing a 
#' semi-regular expression which uses the following logic:
#' "a_b" means any character that contains "a" followed by 
#' something followed by "b", anywhere in the string.
#' @param blank Character. String to use between regular expressions.
#' @export 
grepl_anywhere <- function(vector, pattern, blank = "_") {
  if (!grepl(blank, pattern))
    return(grepl(pattern, vector))
  # if (nchar(blank != 1))
  #   stop(paste("Your 'blank' parameter", v2t(blank), "must be length 1"))
  forced <- tolower(unlist(strsplit(pattern, "")))
  forced_which <- which(forced != blank)
  combs <- res <- c()
  for(i in 0:(max(nchar(vector))-max(forced_which)))
    combs <- rbind(combs, (forced_which + i))
  for (i in 1:length(vector)) {
    temp <- c()
    for (k in 1:nrow(combs)) {
      aux <- c()
      for (j in 1:ncol(combs)) {
        aux <- c(aux, substr(vector[i], combs[k, j], combs[k, j]))
      }
      aux <- paste0(aux, collapse = "") == gsub(blank, "", pattern)
      temp <- c(temp, aux)
    }
    res <- c(res, any(temp))
  }
  return(res)
}
