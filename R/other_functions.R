####################################################################
#' Convert year month format YYYY-MM
#' 
#' This function lets the user convert a date into YYYY-MM format
#' 
#' @param date Date. Date we wish to transform 
#' @export
year_month <- function(date) {
  
  require(lubridate)
  require(stringr)
  
  return(paste(
    lubridate::year(date),
    str_pad(lubridate::month(date), 2, pad = "0"),
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
  
  require(lubridate)
  require(stringr)
  
  return(paste(
    lubridate::year(date),
    str_pad(lubridate::week(date), 2, pad = "0"),
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
  
  require(dplyr)
  
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
#' This function lets the user reduce categorical values in a vector
#' 
#' @param vector Categorical Vector
#' @param nmin Integer. Number of minimum times a value is repeated
#' @param pmin Numerical. Porcentage of minimum times a value is repeated
#' @param pcummax Numerical. Top cumulative porcentage of most 
#' repeated values
#' @param top Integer. Keep the n most frequently repeated values
#' @param other_label Character. Which value do you wish to replace 
#' the filtered values with?
#' @export
categ_reducer <- function(vector, 
                          nmin = 0, 
                          pmin = 0, 
                          pcummax = 100, 
                          top = NA, 
                          other_label = "other") {
  require(dplyr)
  df <- data.frame(name = vector) %>% lares::freqs(., name)
  if (!is.na(top)) {
    top <- df %>% slice(1:top)
  } else {
    top <- df %>% filter(n >= nmin & p >= pmin & p <= pcummax) 
  }
  vector <- ifelse(vector %in% top$name, 
                   as.character(vector), other_label)
  return(vector)
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
  require(rvest)
  require(dplyr)
  
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
#' @param df Vector or Dataframe. Contains different variables in each column, separated by a specific character
#' @param variable Character. Which binary variable should we use to resample df
#' @param rate Numeric. How many X for every Y we need? Default: 1
#' @param seed Numeric. Seed to replicate and obtain same values
#' @export
balance_data <- function(df, variable, rate = 1, seed = 0) {
  
  require(dplyr)
  
  set.seed(seed)
  
  df <- rename(df, tag = variable)
  tags <- unique(df$tag)
  
  if (length(unique(df$tag)) != 2) {
    stop("You should use a variable with only 2 unique values!")
  } else {
    message(paste("Resampled from:",lares::vector2text(table(df$tag), sep = " x ", quotes = F)))
    ones <- df %>% filter(tag == tags[1])
    zeros <- df %>% filter(tag == tags[2]) %>% 
      sample_n(rate * nrow(ones))
    balanced <- rbind(ones, zeros)
    message(paste("Into:",lares::vector2text(table(balanced$tag), sep = " x ", quotes = F)))
    balanced <- rename_at(balanced, vars("tag"), funs(paste0(variable)))
    return(balanced)
  }
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
listfiles <- function(folder, recursive = TRUE, regex = NA, images = FALSE, export = FALSE) {
  
  if (!file.exists(folder)) {
    stop("That directory doesn't exist; please try again!")
  }
  
  files <- list.files(folder, recursive = recursive)
  address <- paste0(folder, "/", files)
  info <- file.info(address)
  files <- gsub("_", " ", files)
  
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
    
    require(exifr)
    require(dplyr)
    require(lubridate)
    
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
#' vector or data.frame into another value.
#' 
#' @param df Data.frame
#' @param original String. Original text you wish to replace
#' @param change String. Values you wish to replace the originals with
#' @export
replaceall <- function(df, original, change) {
  data.frame(
    lapply(df, function(x) {
      gsub(original, change, x) 
    }
    )
  )
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
  
  require(dplyr)
  
  # Drop ALL NAs columns
  if (dropnacols == TRUE) {
    df <- lares::removenacols(df, all = TRUE) 
  }
  
  # Which character columns may be used as numeric?
  transformable <- apply(df, 2, function(x) length(unique(x)))
  which <- names(transformable[transformable==2])
  dfn <- df[,colnames(df) %in% which]
  non_numeric <- mutate_all(dfn, function(x) as.integer(as.factor(x))-1)
  numeric <- select_if(df, is.numeric)
  if (logs == TRUE) {
    numeric <- data.frame(mutate_if(numeric, is.numeric, funs(log = log(.))))
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
#' @param date Vector. Dates in any of the permitted formats
#' @param metric Boolean. Metric or Imperial inputs. The main 
#' difference is that Metric follows the DD/MM/YYYY pattern, and 
#' Imperial follows the MM/DD/YYYY pattern.
#' @param origin Date. When importing from Excel, integers usually
#' have 1900-01-01 as origin. In R, origin is 1970-01-01.
#' @export
dateformat <- function(dates, metric = FALSE, origin = '1900-01-01') {
  
  suppressMessages(require(dplyr))
  suppressMessages(require(stringr))
  suppressMessages(require(lubridate))
  options(warn=-1)
  
  dates <- gsub(" .*", "", dates)
  x <- dates[!is.na(dates)][1]
  
  # Is it in integer format?
  int <- as.integer(as.character(x))
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
#' pipeline without affecting the output (or pipeline).
#' 
#' 
#' @param df Dataframe
#' @param fun Function. What function do you wish to run? For example:
#' pass(. %>% ncol %>% print)
#' @export
pass <- function(df, fun) { 
  fun(df)
  return(df) 
}
