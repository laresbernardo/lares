####################################################################
#' Check if Specific Package is Installed
#'
#' This function checks library dependencies
#'
#' @family Tools
#' @param package Character. Name of the library
#' @param stop Boolean. Stop if not installed. If \code{FALSE} and
#' library is not available, warning will be shown.
#' @return No return value, called for side effects.
#' @examples
#' # Check if library base is installed. If not, stop and show error
#' try_require("base", stop = TRUE)
#' # Check if library xxx is installed. If not, show warning
#' try_require("xxx", stop = FALSE)
#' @export
try_require <- function(package, stop = TRUE) {
  present <- length(find.package(package, quiet = TRUE)) > 0
  if (present) {
    suppressMessages(library(package, character.only = TRUE))
  } else {
    if (stop) {
      stop(paste0("Package '", package, "' required. Install and try again."), call. = FALSE)
    } else {
      warning(paste0("Package '", package, "' recommended. Install for better results."), call. = FALSE)
    }
  }
}

####################################################################
#' Convert Date into Year-Month, Year-Quarter or Year-Week Format
#'
#' This function lets the user convert a date into YYYY-MM, YYYY-QX,
#' or YYYY-WW format easily.
#'
#' @family Data Wrangling
#' @param date Date vector. Date to transform format.
#'
#' @return Vector with dates reformatted
#' @examples
#' year_month(Sys.Date())
#' year_quarter(Sys.Date())
#' year_week(Sys.Date())
#' @export
year_month <- function(date) {
  paste(year(date), str_pad(lubridate::month(date), 2, pad = "0"), sep = "-")
}
#' @rdname year_month
#' @export
year_quarter <- function(date) {
  paste(year(date), date_cuts(date, type = "Q"), sep = "-")
}
#' @rdname year_month
#' @export
year_week <- function(date) {
  paste(year(date), str_pad(lubridate::week(date), 2, pad = "0"), sep = "-")
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
#' @return Vector with date cut for each \code{date}
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
  for (i in 1:(12 / aux)) {
    df <- df %>% mutate(cut = ifelse(
      .data$month >= cuts[i] & .data$month <= cuts[i + 1],
      paste0(toupper(type), i), .data$cut
    ))
  }
  return(df$cut)
}


####################################################################
#' Reduce categorical values
#'
#' This function lets the user reduce categorical values in a vector.
#' It is tidyverse friendly for use on pipelines
#'
#' @family Data Wrangling
#' @inheritParams ohse
#' @param df Categorical Vector
#' @param var Variable. Which variable do you wish to reduce?
#' @param nmin Integer. Number of minimum times a value is repeated
#' @param pmin Numerical. Percentage of minimum times a value is repeated
#' @param pcummax Numerical. Top cumulative percentage of most
#' repeated values
#' @param top Integer. Keep the n most frequently repeated values
#' @param pvalue_max Numeric (0-1]. Max pvalue categories
#' @param cor_var Character. If pvalue_max < 1, you must define which
#' column name will be compared with (numerical or binary).
#' @param other_label Character. With which text do you wish to replace
#' the filtered values with?
#' @param ... Additional parameters
#' @return data.frame \code{df} on which \code{var} has been transformed
#' @examples
#' data(dft) # Titanic dataset
#' categ_reducer(dft, Embarked, top = 2) %>% freqs(Embarked)
#' categ_reducer(dft, Ticket, nmin = 7, other_label = "Other Ticket") %>% freqs(Ticket)
#' categ_reducer(dft, Ticket, pvalue_max = 0.05, cor_var = "Survived") %>% freqs(Ticket)
#' @export
categ_reducer <- function(df, var,
                          nmin = 0,
                          pmin = 0,
                          pcummax = 100,
                          top = NA,
                          pvalue_max = 1,
                          cor_var = "tag",
                          limit = 20,
                          other_label = "other",
                          ...) {
  var <- enquo(var)

  dff <- freqs(df, !!var)

  if (!is.na(top)) {
    tops <- dff %>% slice(1:top)
  } else {
    tops <- dff %>% filter(.data$n >= nmin & .data$p >= pmin & .data$p <= pcummax)
  }

  name <- as.name(names(dff[, 1]))
  vector <- df %>% select(as.character(name))
  new_vector <- ifelse(vector[[as.character(name)]] %in% tops[[as.character(name)]],
    as.character(vector[[as.character(name)]]),
    other_label
  )

  if (pvalue_max < 1) {
    best_vars <- df %>%
      mutate(cor_var_temp = unlist(df[, cor_var])) %>%
      select(ncol(.), !!var) %>%
      corr_var(.data$cor_var_temp,
        plot = FALSE,
        limit = limit, quiet = TRUE, ...
      ) %>%
      filter(.data$pvalue < pvalue_max) %>%
      rowwise() %>%
      mutate(variables = gsub(paste0(name, "_"), "", .data$variables)) %>%
      pull(.data$variables)
    new_vector[!new_vector %in% best_vars] <- other_label
  }

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
#' @return Vector with normalized \code{x} values
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
#' @param force_single Boolean. Force single quotes by replacing \code{\"}.
#' @param and Character. Add 'and' or something before last observation.
#' Not boolean variable so it can be used on other languages. Note that
#' the last comma will be suppressed if \code{Sys.getenv("LARES_NUMFORMAT")}
#' is set to \code{1} and you have less than 3 values.
#' @return Vector pasting \code{vector} values into a single string
#' @examples
#' vector2text(LETTERS[1:5])
#' vector2text(c(1:5), quotes = FALSE)
#' vector2text(c(1:5), quotes = FALSE, sep = "-")
#' vector2text(c(1:5), and = "and also")
#' vector2text(c("Text", "R's"), force_single = TRUE)
#' # Shorter function with same purpose
#' v2t(LETTERS[1:5])
#' @export
vector2text <- function(vector, sep = ", ", quotes = TRUE, force_single = FALSE, and = "") {

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
  if (!quotes) {
    output <- gsub("'", "", output)
  }

  # Get rid of the last comma when using and?
  if (and != "" & (Sys.getenv("LARES_NUMFORMAT") == 1 | n == 2)) {
    last_comma <- tail(c(gregexpr(",", output)[[1]]), 1)
    output <- paste0(
      substr(output, 1, last_comma - 1),
      substr(output, last_comma + 1, nchar(output))
    )
  }

  if (force_single) {
    output <- gsub('"', "'", output)
    output <- gsub('\"', "'", output)
  }

  return(output)
}
#' @rdname vector2text
#' @export
v2t <- vector2text

####################################################################
#' Scrap data based on IP address
#'
#' This function lets the user scrap https://db-ip.com/ given
#' IP address(es) to get their associated address type, ASN, ISP,
#' organization, country, state or region, county, city, ZIP postal code,
#' weather station, coordinates, Timezone, local time, languages, and currency.
#'
#' @family Tools
#' @family Scrapper
#' @param ip Vector. Vector with all IP's we wish to search.
#' @param quiet Boolean. Do not show the loading \code{statusbar}?
#' @return data.frame. Each row is an unique \code{ip} address,
#' and columns will bee created for all the additional information found.
#' @examples
#' \donttest{
#' ip_data("163.114.132.0")
#' ip_data(ip = c(myip(), "201.244.197.199"), quiet = TRUE)
#' }
#' @export
ip_data <- function(ip = myip(), quiet = FALSE) {
  ip <- ip[!is.na(ip)]
  ip <- ip[is_ip(ip)]
  ip <- unique(ip)
  output <- data.frame()
  for (i in seq_along(ip)) {
    url <- paste0("https://db-ip.com/", ip[i])
    scrap <- content(GET(url)) %>% html_table()
    clean <- bind_rows(scrap[[1]], scrap[[3]])
    row <- data.frame(t(clean[, 2]))
    colnames(row) <- clean$X1
    row <- data.frame(id = ip[i], row)
    output <- bind_rows(output, row)
    if (length(ip) > 1 & !quiet) statusbar(i, length(ip), ip[i])
  }
  output <- cleanNames(output)
  row.names(output) <- NULL
  return(output)
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
#' @return Numeric value result
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
#' Nicely Format Texts and Numericals
#'
#' This function lets the user format numerical values nicely
#'
#' @family Data Wrangling
#' @param x Numerical Vector
#' @param decimals Integer. Amount of decimals to display. If set to
#' \code{NULL}, then \code{getOption("digits")} will be used.
#' @param signif Integer. Rounds the values in its first argument to
#' the specified number of significant digits.
#' @param type Integer. \code{1} for International standards. \code{2}
#' for American Standards. Use \code{Sys.setenv("LARES_NUMFORMAT" = 2)}
#' to set this parameter globally.
#' @param pre,pos Character. Add string before or after number.
#' @param sign Boolean. Add \code{+} sign to positive values.
#' @param abbr Boolean. Abbreviate using num_abbr()? You can use
#' the `decimals` parameter to set abbr's \code{n}(-1) parameter.
#' @param ... Additional lazy eval parameters.
#' @return Character. String vector with reformatted continuous numbers
#' @examples
#' formatNum(1.23456, decimals = 3)
#' formatNum(1.23456, type = 1)
#' formatNum(1.23456, pre = "$", pos = "/person")
#' formatNum(123456, abbr = TRUE)
#' formatNum(1234567890, abbr = TRUE, signif = 2)
#' formatNum(1234567890, decimals = 0, abbr = TRUE)
#' formatNum(c(-3:3), sign = TRUE)
#' @export
#' @rdname format_string
formatNum <- function(x, decimals = 2, signif = NULL,
                      type = Sys.getenv("LARES_NUMFORMAT"),
                      pre = "", pos = "", sign = FALSE,
                      abbr = FALSE,
                      ...) {

  # Auxiliary function to save signs
  if (sign) signs <- ifelse(x > 0, "+", "")

  # Decimals: Round numbers
  if (is.null(decimals)) decimals <- getOption("digits")
  x <- base::round(x, digits = decimals)

  # Significant digits
  if (!is.null(signif)) x <- base::signif(x, signif)

  if (abbr) {
    x <- num_abbr(x, n = decimals + 1)
  } else {
    if (is.null(decimals)) decimals <- 0L
    if (type == 1) {
      x <- format(as.numeric(x), big.mark = ".", decimal.mark = ",")
    } else {
      x <- format(as.numeric(x), big.mark = ",", decimal.mark = ".")
    }
    x <- trimws(x)
  }

  if (pre == "$") x <- gsub("\\$-", "-$", x)
  if (sign) x <- paste0(signs, x)
  ret <- paste0(pre, x, pos)
  return(ret)
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
#' @param variable Variable. Which variable should we used to re-sample dataset?
#' @param rate Numeric. How many X for every Y we need? Default: 1. If there are
#' more than 2 unique values, rate will represent percentage for number of rows
#' @param target Character. If binary, which value should be reduced? If kept in
#' \code{"auto"}, then the most frequent value will be reduced.
#' @param seed Numeric. Seed to replicate and obtain same values
#' @param quiet Boolean. Keep quiet? If not, messages will be printed
#' @return data.frame. Reduced sampled data.frame following the \code{rate} of
#' appearance of a specific variable.
#' @examples
#' data(dft) # Titanic dataset
#' df <- balance_data(dft, Survived, rate = 0.5)
#' df <- balance_data(dft, .data$Survived, rate = 0.1, target = "TRUE")
#' @export
balance_data <- function(df, variable, rate = 1, target = "auto", seed = 0, quiet = FALSE) {
  on.exit(set.seed(seed))
  var <- enquo(variable)
  variable <- rlang::as_label(var)
  stopifnot(variable %in% names(df))
  names(df)[names(df) == variable] <- "tag"
  tags <- group_by(df, .data$tag) %>%
    summarize(n = n()) %>%
    arrange(desc(.data$n)) %>%
    pull(.data$tag) %>%
    as.character()

  if (length(tags) != 2) {
    # For numerical re-sampling:
    samp <- round(rate * nrow(df))
    balanced <- sample_n(df, samp)
    if (nrow(df) != samp) {
      if (!quiet) message(paste("Resampled from", nrow(df), "to", samp, "rows"))
    }
  } else {
    # For binary re-sampling:
    if (!quiet) {
      message(paste("Resampled from:", vector2text(
        formatNum(table(df$tag), 0),
        sep = " v ", quotes = FALSE
      )))
    }
    if (as.character(target) == "auto") {
      zero <- tags[1]
      one <- tags[2]
    } else {
      check_opts(target, tags)
      zero <- target
      one <- tags[tags != target]
    }
    if (!quiet) message(paste("Reducing size for label:", zero))
    ones <- filter(df, .data$tag == one)
    zeros <- filter(df, .data$tag == zero)
    zeros <- sample_n(zeros, round(rate * nrow(ones)))
    balanced <- rbind(ones, zeros)
    if (!quiet) {
      message(paste("New label distribution:", vector2text(
        formatNum(table(balanced$tag), 0),
        sep = " v ", quotes = FALSE
      )))
    }
  }
  balanced <- rename_at(balanced, vars("tag"), list(~ paste0(variable)))
  return(as_tibble(balanced))
}


####################################################################
#' Get Meta Data from Image Files
#'
#' This function lets the user get meta data from image files or directory.
#'
#' @family Tools
#' @param files Character vector. Files or directory which contains files.
#' @return data.frame with meta-data for each image file.
#' @export
image_metadata <- function(files) {
  try_require("exifr")

  files <- as.character(files)
  if (length(files) == 1) {
    if (dir.exists(files)) {
      files <- listfiles(files, recursive = TRUE)
    }
  }

  df <- data.frame(file = files)
  tags <- c(
    "FileName", "SourceFile",
    "CreateDate", "DateTimeOriginal", "FileModifyDate",
    "FileTypeExtension", "Megapixels",
    "ImageSize", "ImageWidth", "ImageHeight",
    "GPSLongitude", "GPSLatitude",
    "GPSLatitudeRef", "GPSLongitudeRef",
    "Rotation", "Flash", "Duration",
    "Make", "Model"
  )

  aux <- ceiling(nrow(df) / 500)
  for (i in 1:aux) {
    if (i == 1) {
      ret <- NULL
      if (aux > 2) {
        message(paste(
          "This might take a while... Analizing",
          formatNum(nrow(df), decimals = 0), "files!"
        ))
      }
    }
    from <- (i - 1) * 500 + 1
    to <- i * 500
    x <- slice(df, from:to)
    temp <- read_exif(as.character(x$file), tags = tags)
    if (nrow(temp) > 0) {
      if ("DateTimeOriginal" %in% colnames(temp)) {
        ret <- bind_rows(ret, select(temp, one_of(tags)))
      }
    }
    statusbar(i, aux, label = paste(formatNum(from, 0), "-", formatNum(to, 0)))
  }

  if (length(ret) > 0) {
    if ("DateTimeOriginal" %in% colnames(ret)) {
      df <- ret %>%
        mutate(
          DateTimeOriginal = ymd_hms(.data$DateTimeOriginal),
          CreateDate = ymd_hms(.data$CreateDate),
          FileModifyDate = ymd_hms(.data$FileModifyDate)
        )
      df <- df[, colSums(is.na(df)) < nrow(df)]
      if (aux > 10) {
        tryCatch({
          try_require("beepr", stop = FALSE)
          beep()
        })
      }
      return(df)
    }
  } else {
    message("No images found to process...")
  }
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
#' @return data.frame with relevant data for each file on
#' \code{folder} directory.
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
  if (!file.exists(folder)) {
    stop("That directory doesn't exist; please try again!")
  }

  files <- list.files(folder, recursive = recursive)
  address <- paste0(folder, "/", files)
  info <- file.info(address)

  df <- data.frame(filename = files, address, info)
  df$size <- as.integer(df$size / 1024)
  # imgs <- "jpg|JPG|jpeg|JPEG|png|PNG|gif|GIF"

  if (!is.na(regex)) df <- df[grep(regex, df$filename), ]

  if (images) {
    df <- image_metadata(df$address)
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
#' @return data.frame with replaced values based on inputs.
#' @examples
#' df <- data.frame(
#'   one = c(1:4, NA),
#'   two = LETTERS[1:5],
#'   three = rep("A", 5),
#'   four = c(NA, "Aaa", 123, "B", "C")
#' )
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
    original <- dic[, 1]
    change <- dic[, 2]
  } else {
    vector <- FALSE
  }
  if (length(original) != length(change)) {
    stop("Vectors original and change should have the same length!")
  }
  if (length(unique(original)) != length(original)) {
    aux <- freqs(dic, original) %>%
      filter(.data$n > 1) %>%
      .$original
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
    for (i in seq_along(original)) {
      if (!quiet) {
        message(paste("Transforming all", original[i], "into", change[i]))
      }
      df[] <- lapply(df, function(x) gsub(original[i], change[i], x))
    }
  }
  if (which[1] != "all") {
    df <- select(aux, -one_of(which)) %>%
      cbind(df) %>%
      select(one_of(colnames(aux)))
  }
  if (vector) {
    df <- df[, 1]
  }
  if (fixclass) {
    df <- suppressMessages(type.convert(df, numerals = "no.loss", as.is = TRUE))
  }
  if (vector) {
    return(as.vector(df))
  } else {
    return(as_tibble(df))
  }
}


####################################################################
#' Remove/Drop Columns in which ALL or SOME values are NAs
#'
#' This function lets the user remove all columns that have some or
#' all values as NAs
#'
#' @family Data Wrangling
#' @param df Data.frame
#' @param all Boolean. Remove columns containing ONLY \code{NA} values.
#' If set to \code{FALSE}, remove columns containing at least one \code{NA}.
#' @param ignore Character vector. Column names to ignore validation.
#' @return data.frame with removed columns.
#' @export
#' @rdname filterdata
removenacols <- function(df, all = TRUE, ignore = NULL) {
  if (is.null(df)) {
    return(NULL)
  }
  if (!is.data.frame(df)) stop("df must be a valid data.frame")
  if (all) {
    not_all_nas <- colSums(is.na(df)) != nrow(df)
    keep <- colnames(df) %in% ignore | not_all_nas
    df[, keep]
  } else {
    df[, complete.cases(t(df[!colnames(df) %in% ignore]))]
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
#' @return data.frame with removed rows.
#' @export
#' @rdname filterdata
removenarows <- function(df, all = TRUE) {
  if (is.null(df)) {
    return(NULL)
  }
  if (!is.data.frame(df)) stop("df must be a valid data.frame")
  if (all) {
    return(df[rowSums(is.na(df)) != ncol(df), ])
  } else {
    return(df[complete.cases(df), ])
  }
}


####################################################################
#' Select only numerical columns in a dataframe
#'
#' @family Data Wrangling
#' @param df Data.frame
#' @param dropnacols Boolean. Drop columns with only NA values?
#' @param logs Boolean. Calculate log(x)+1 for numerical columns?
#' @param natransform String. "mean" or 0 to impute NA values. If
#' set to NA no calculation will run.
#' @return data.frame with all numerical columns selected.
#' @examples
#' data(dft) # Titanic dataset
#' str(dft)
#' numericalonly(dft) %>% head()
#' numericalonly(dft, natransform = "mean") %>% head()
#' @export
#' @rdname filterdata
numericalonly <- function(df, dropnacols = TRUE, logs = FALSE, natransform = NA) {

  # Drop ALL NAs columns
  if (dropnacols) df <- removenacols(df, all = TRUE)

  # Which character columns may be used as numeric?
  transformable <- apply(df, 2, function(x) length(unique(x)))
  which <- names(transformable[transformable == 2])
  dfn <- data.frame(df[, colnames(df) %in% which])
  colnames(dfn) <- which
  non_numeric <- mutate_all(dfn, list(~ as.integer(as.factor(.)) - 1))
  # Which are already numeric?
  numeric <- select_if(df, is.numeric)

  # Calculate logs
  if (logs) {
    # Non binary numeric features
    whichlog <- colnames(numeric)[!colnames(numeric) %in% which]
    numeric <- numeric %>% mutate_at(vars(whichlog), list(log = ~ log(. + 1)))
    is.na(numeric) <- do.call(cbind, lapply(numeric, is.infinite))
  }

  # Join everything
  d <- cbind(numeric, non_numeric[!colnames(non_numeric) %in% colnames(numeric)])

  if (!is.na(natransform)) {
    if (natransform == 0) {
      d[is.na(d)] <- 0
    }
    if (natransform == "mean") {
      for (i in seq_along(d)) {
        if (median(d[, i], na.rm = TRUE) != 0) {
          d[is.na(d[, i]), i] <- mean(d[, i], na.rm = TRUE)
        } else {
          d[is.na(d[, i]), i] <- 0
        }
      }
    }
  }
  return(d)
}


####################################################################
#' What's my IP?
#'
#' Reveal your current IP address.
#'
#' @family Tools
#' @return Character. Result of your IP address based on ipify.org
#' @examples
#' \donttest{
#' myip()
#' }
#' @export
myip <- function() {
  ipify <- "https://api.ipify.org/"
  ip <- content(GET(ipify), encoding = "UTF-8")
  return(ip)
}


####################################################################
#' Calculate cuts by quantiles
#'
#' This function lets the user quickly calculate cuts for quantiles
#' and discretize numerical values into categorical values.
#'
#' @family Calculus
#' @param values Vector. Values to calculate quantile cuts
#' @param splits Integer. How many cuts should split the values?
#' @param return Character. Return "summary" or "labels"
#' @param n Integer. Determines the number of digits used in
#' formatting the break numbers.
#' @return Factor vector or data.frame. Depending on \code{return} input:
#' \itemize{
#'   \item \code{labels} a factor ordered vector with each observation's quantile
#'   \item \code{summary} a data.frame with information on each quantile cut
#' }
#' @examples
#' data(dft) # Titanic dataset
#' quants(dft$Age, splits = 5, "summary")
#' quants(dft$Age, splits = 5, "labels")[1:10]
#' @export
quants <- function(values, splits = 10, return = "labels", n = 2) {
  if (splits > length(unique(values[!is.na(values)])) - 1) {
    stop("There are not enough observations to split the data in ", splits)
  }

  cuts <- quantile(values, probs = seq(0, 1, length = splits + 1), na.rm = TRUE)
  labels <- cut(values, unique(cuts), dig.lab = n, include.lowest = TRUE, ordered_result = TRUE)

  if (return == "labels") {
    return(labels)
  }
  if (return == "summary") {
    output <- data.frame(percentile = names(cuts)[-1], cut = cuts[-1]) %>%
      mutate(
        label = paste0("(", signif(lag(.data$cut), n), "-", signif(.data$cut, n), "]"),
        label = gsub("\\(NA", paste0("[", signif(min(.data$cut), n)), .data$label),
        label = factor(.data$label, levels = unique(.data$label), ordered = TRUE)
      )
    return(output)
  }
}


####################################################################
#' Convert Python JSON string to R vector (data.frame with 1 row)
#'
#' This function lets the user transform a JSON string into vector
#' (data.frame with 1 row). You can also pass a Python's dictionary.
#' For any other JSON transformation, \code{jsonlite} is recommended.
#'
#' @family Tools
#' @param json Character. JSON string.
#' @return List, data.frame, or vector. Depends on the \code{json} string.
#' @examples
#' json2vector('{"id": 1, "nodata": null, "gender": "M"}')
#' @export
json2vector <- function(json) {
  string <- paste0("[", gsub('"', "\"", json), "]")
  string <- gsub("'", '"', string)
  string <- gsub("None", "null", string)
  string <- gsub("True", "true", string)
  string <- gsub("False", "false", string)
  vector <- fromJSON(string)
  df <- data.frame(t(unlist(vector)))
  return(df)
}


####################################################################
#' Left or Right N characters of a string
#'
#' This functions lets the user extract the first or last n characters
#' of a string or vector of strings.
#'
#' @family Data Wrangling
#' @param string String or Vector.
#' @param n Integer. How many characters starting on right/left?
#' @return Character. Trimmed strings.
#' @examples
#' left("Bernardo", 3)
#' right(c("Bernardo", "Lares", "V"), 3)
#' @export
#' @rdname left_right
left <- function(string, n = 1) {
  string <- as.character(string)
  l <- substr(string, 1, n)
  return(l)
}

#' @rdname left_right
#' @export
right <- function(string, n = 1) {
  string <- as.character(string)
  r <- substr(string, nchar(string) - n + 1, nchar(string))
  return(r)
}


####################################################################
#' Import Excel File with All Its Tabs
#'
#' This function lets the user import an Excel file's tabs into a list
#'
#' @family Tools
#' @param file String. Local Excel file name
#' @return List or data.frame. If single tab is found, a data.frame; if
#' multiple tabs are found on file, a list of data.frames.
#' @export
importxlsx <- function(file) {
  sheets <- getSheetNames(file)
  mylist <- list()
  for (i in seq_along(sheets)) {
    sheet <- read.xlsx(file,
      sheet = i,
      skipEmptyRows = TRUE,
      skipEmptyCols = TRUE,
      startRow = 1,
      detectDates = TRUE
    )
    mylist[[sheets[i]]] <- sheet
  }
  if (length(sheets) == 1) {
    mylist <- mylist[[1]][[1]]
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
#' @param quiet Quiet outputs? If not, skip quietness.
#' @return Same as \code{fx} but with no messages or prints.
#' @export
quiet <- function(fx, quiet = TRUE) {
  if (!quiet) return(fx)
  invisible(capture.output(fx))
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
#' @return Boolean. Result of checking if device has internet connection.
#' @export
haveInternet <- function(thresh = 3, url = "http://www.google.com") {
  start <- Sys.time()
  if (!capabilities(what = "http/ftp")) {
    return(FALSE)
  }
  test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
  if (as.numeric(Sys.time() - start) > thresh) {
    message("Slow internet connection but available...")
  }
  return(!inherits(test, "try-error"))
}


####################################################################
#' Zero Variance Columns
#'
#' This function detects which columns have the same value (whichever)
#' for each column.
#'
#' @family Tools
#' @param df Dataframe
#' @return Character vector with column names on which its values have no variance.
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
#' @param filename Character. File name to import.
#' @param current_wd Boolean. Use current working directory before
#' the file's name? Use this param to NOT get absolute root directory.
#' @param sheet Character. Name or index of the sheet to read data
#' from if file is xlsx or xls.
#' @param quiet Boolean. Quiet summary message?
#' @return List or data.frame, depending on \code{filename}'s data.
#' @export
read.file <- function(filename, current_wd = TRUE, sheet = 1, quiet = FALSE) {
  if (current_wd) filename <- paste0(getwd(), "/", filename)

  if (!file.exists(filename)) {
    stop("That file doesn't exist.. try with another!")
  } else {
    filetype <- gsub("\\.", "", right(filename, 4))

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
    # if (filetype == "sav") {
    #   try_require("foreign")
    #   results <- quiet(read.spss(filename, to.data.frame = T))
    # }
    # if (filetype == "dta") {
    #   # Stata version 5-12 .dta file
    #   # results <- foreign::read.dta(filename)
    #   # Stata version 13 .dta file
    #   try_require("readstata13")
    #   results <- read.dta13(filename)
    # }
    if (filetype == "dat") {
      results <- read.table(filename, header = TRUE)
    }

    if (!quiet) {
      message(paste(
        "Imported", filetype, "file with",
        formatNum(nrow(results), 0), "rows x",
        formatNum(ncol(results), 0), "columns, succesfully!"
      ))
    }
  }
  if (nrow(results) == 0) warning("There is no data in that file...")
  return(results)
}


####################################################################
#' Bind Files into Dataframe
#'
#' This function imports and binds multiple files into a single
#' data.frame. Files must be inserted with absolute roots files names.
#'
#' @family Tools
#' @param files Character vector. Files names.
#' @return data.frame with data joined from all \code{files} passed.
#' @export
bind_files <- function(files) {
  alldat <- data.frame()
  for (i in seq_along(files)) {
    file <- files[i]
    dfi <- read.file(file, current_wd = FALSE)
    alldat <- bind_rows(alldat, dfi)
    statusbar(i, length(files))
  }
  return(data.frame(alldat))
}


####################################################################
#' New Line Feed for Long Strings (Wrapper)
#'
#' Add a break or new line without breaking words. Automatically,
#' the function can detect your plot's width and will dynamically
#' set an auto width. You can adjust the relation (rel) parameter
#' for different fonts and sizes until perfect harmony found.
#' Quite similar to \code{stringr::str_wrap} but, if the text vector
#' is a factor, the levels will be kept in order and transformed.
#'
#' @family Tools
#' @param text Character or factor vector.
#' @param top Integer. How many characters aprox. should be on each line?
#' @param rel Numeric. Relation of pixels and characters per line
#' @return Character. String (vector) including some \code{\\n} within.
#' @examples
#' cat(autoline("This is a long text that may not fit into a single line", 8))
#'
#' text <- factor(c("First value", "Second value", "First value"),
#'   levels = c("First value", "Second value")
#' )
#' autoline(text, 1)
#'
#' path <- file.path(R.home("doc"), "THANKS")
#' text <- paste(readLines(path), collapse = " ")
#' cat(autoline(text))
#' @export
autoline <- function(text, top = "auto", rel = 9) {

  # Auto-maximum
  if (top == "auto") top <- round(dev.size("px")[1] / rel)

  # Keep factors
  is_factor <- is.factor(text)
  if (is_factor) {
    levs <- levels(text)
    text <- as.character(text)
  }

  ret <- stringr::str_wrap(text, top)

  if (is_factor) {
    aux <- data.frame(ret, text) %>%
      mutate(text = factor(text, levels = levs)) %>%
      arrange(text) %>%
      unique()
    ret <- factor(ret, levels = aux$ret)
  }
  return(ret)
}


####################################################################
#' Check if Font is Installed
#'
#' This function checks if a font is installed in your machine.
#'
#' @param font Character. Which font to check
#' @param quiet Boolean. Keep quiet? If not, show message
#' @return Boolean result of the existing fonts check.
#' @examples
#' font_exists(font = "Arial")
#' font_exists(font = "XOXO")
#' font_exists(font = "")
#' @export
font_exists <- function(font = "Arial Narrow", quiet = FALSE) {
  if (isTRUE(font == "")) {
    return(FALSE)
  }
  if (isTRUE(is.na(font))) {
    return(FALSE)
  }
  if (isTRUE(is.null(font))) {
    return(FALSE)
  }

  tryCatch(
    {

      # Thanks to extrafont for the idea for this code
      ttf_find_default_path <- function() {
        if (grepl("^darwin", R.version$os)) {
          paths <- c(
            "/Library/Fonts/",
            "/System/Library/Fonts",
            "/System/Library/Fonts/Supplemental",
            "~/Library/Fonts/"
          )
          return(paths[file.exists(paths)])
        } else if (grepl("^linux-gnu", R.version$os)) {
          # Possible font paths, depending on the system
          paths <- c(
            "/usr/share/fonts/",
            "/usr/X11R6/lib/X11/fonts/TrueType/",
            "~/.fonts/"
          )
          return(paths[file.exists(paths)])
        } else if (grepl("^freebsd", R.version$os)) {
          # Possible font paths, depending on installed ports
          paths <- c(
            "/usr/local/share/fonts/truetype/",
            "/usr/local/lib/X11/fonts/",
            "~/.fonts/"
          )
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
        ttfiles <- list.files(fonts_path,
          pattern = pattern,
          full.names = TRUE, ignore.case = TRUE
        )
        ret <- font %in% gsub(pattern, "", basename(ttfiles))
        return(ret)
      }
      check(font)
    },
    error = function(err) {
      if (!quiet) message(paste("Font issue:", err))
      return(FALSE)
    }
  )
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
#' @return Vector of character values that contain converted values
#' @examples
#' num_abbr(rnorm(10) * 1e6)
#' num_abbr(rnorm(10) * 1e6, n = 1)
#' @export
num_abbr <- function(x, n = 3) {
  if (!is.numeric(x)) stop("Input vector x needs to be numeric.")
  if (!is.numeric(n)) stop("n needs to be numeric.")
  if (length(n) > 1) stop("Please make sure that n takes on a single value.")
  if (!n %in% 1:6) stop("Please make sure that n takes on an interger value between 1 to 6.")

  # # To handle scientific notation inputs correctly
  # on.exit(options("scipen" = getOption('scipen')))
  # options("scipen" = 999)

  # Clean up x
  negative_positions <- ifelse(x < 0, "-", "")
  x <- abs(x)

  div <- findInterval(x, c(0, 1e3, 1e6, 1e9, 1e12, 1e15, 1e18))

  # Round x with some cleaning
  x <- round(x, -nchar(round(x, 0)) + n) / 10^(3 * (div - 1))

  # Fix numbers rounded up to another digit
  # i.e. 999k -> 1000k should actually be 1M
  div <- ifelse(nchar(as.integer(x)) > 3, div + 1, div)
  x <- ifelse(nchar(as.integer(x)) > 3, x / 1e3, x)

  # Cap decimal places to 3
  x <- round(x, 3)

  # Qa = Quadrillion; Qi = Quintillion
  x <- paste0(x, c("", "K", "M", "B", "T", "Qa", "Qi")[div])

  output <- paste0(negative_positions, x)
  output[grepl("NA", output)] <- NA

  return(output)
}


####################################################################
#' List categorical values for data.frame
#'
#' Make a list with all categorical values and
#'
#' @param df data.frame
#' @param ... Variables to segment counters
#' @param abc Boolean. Sort alphabetically?
#' @return List. Length same as number of categorical columns, each with a
#' frequency data.frame using \code{freqs()}.
#' @examples
#' data(dft) # Titanic dataset
#' df <- dft[, 1:5]
#' head(df)
#' list_cats(df)
#' @export
list_cats <- function(df, ..., abc = TRUE) {
  is.categorical <- function(x) is.character(x) | is.factor(x)
  category <- which(unlist(lapply(df, is.categorical)))
  ret <- list()
  for (i in seq_along(category)) {
    which <- as.character(names(category)[i])
    df[, which] <- as.character(df[, which])
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
#' @return Factor vector with transformed levels.
#' @examples
#' library(dplyr)
#' data(dft)
#' # Replace a single value
#' dft <- mutate(dft, Pclass = replacefactor(Pclass, original = "1", change = "First"))
#' levels(dft$Pclass)
#' # Replace multiple values
#' dft <- mutate(dft, Pclass = replacefactor(Pclass, c("2", "3"), c("Second", "Third")))
#' levels(dft$Pclass)
#' @export
replacefactor <- function(x, original, change) {
  if (length(original) != length(change)) {
    stop("You must provide same length vectors for original and change!")
  }
  for (i in seq_along(change)) {
    if (!is.factor(x)) {
      warning("Automatically turned non-factor variable into factor")
      x <- factor(x, levels = unique(as.character(x)))
    }
    levels(x)[levels(x) == original[i]] <- change[i]
  }
  return(x)
}

####################################################################
#' List all functions used in R script files by package
#'
#' Parses all functions called by an R script and then lists
#' them by package. Wrapper for 'getParseData'. May be of great
#' use for those developing a package to help see what
#' namespace 'importsFrom' calls will be required.
#'
#' @param filename Character. Path to an R file (or directory)
#' containing R code files.
#' @param abc Boolean. List functions alphabetically.
#' If FALSE, will list in order of frequency.
#' @param quiet Boolean. Keep quiet? If not, print messages and
#' \code{statusbar}.
#' @return data.frame. Each row is a function and columns stating
#' number of appearances, percentage, packages, and files searched.
#' @examples
#' \dontrun{
#' # Choose an R script file with functions
#' rfile <- file.choose()
#' files_functions(rfile)
#' }
#' @export
files_functions <- function(filename, abc = TRUE, quiet = FALSE) {
  if (dir.exists(filename)) {
    if (!quiet) message("Importing R files from directory")
    filename <- sprintf("%s/%s", filename, listfiles(filename, regex = "\\.R")$filename)
  }
  results <- data.frame()
  for (i in seq_along(filename)) {
    if (!file.exists(filename[i])) {
      stop("Couldn't find file ", filename[i])
    }
    tmp <- getParseData(parse(filename[i], keep.source = TRUE))
    funs <- tmp$text[which(tmp$token == "SYMBOL_FUNCTION_CALL")] %>%
      freqs(abc = abc) %>%
      rename(fun = 1)
    pkgs <- lapply(funs$fun, find)
    pkgs[unlist(lapply(pkgs, function(x) length(x) == 0))] <- NA
    pkgs <- lapply(pkgs, function(x) v2t(x, quotes = FALSE))
    funs$package <- gsub("package:", "", unlist(pkgs))
    funs$file <- filename[i]
    results <- rbind(results, funs)
    if (length(filename) > 1 & !quiet) {
      statusbar(i, length(filename), filename[i])
    }
  }
  return(results)
}


####################################################################
#' Move files from A to B
#'
#' Move one or more files from a directory to another using R.
#'
#' @param from Character. File names and directories. All files
#' will be moved recursively.
#' @param to Character. File names for each \code{from} file or
#' directory. If directory does not exist, it will be created.
#' @return No return value, called for side effects.
#' @export
move_files <- function(from, to) {
  froms <- dirs <- NULL
  for (i in seq_along(from)) {
    fromi <- from[i]
    if (isTRUE(file.info(fromi)$isdir)) {
      fromi <- list.files(fromi, recursive = TRUE)
      fromi <- paste(from[i], fromi, sep = "/")
      dirs <- c(dirs, from[i])
    }
    froms <- c(froms, fromi)
  }
  froms <- froms[grepl("\\.", basename(froms))]
  froms <- gsub(paste0(getwd(), "/"), "", froms)

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
  if (length(tos) != length(froms)) {
    stop("Every 'from' must have a respective 'to' filename")
  }

  # Now move/rename all files
  for (i in seq_along(froms)) {
    todir <- dirname(tos[i])
    if (!isTRUE(file.info(todir)$isdir)) {
      dir.create(todir, recursive = FALSE)
    }
    file.rename(from = froms[i], to = basename(tos[i]))
  }
}


####################################################################
#' Extract file raw name and type from file names
#'
#' @param filepath Character vector. File path(s) to get file raw names
#' without extension nor path OR extension without path nor raw name.
#' @examples
#' file_name("file.aux")
#' file_name("temp/file.R")
#' file_name("/temp/temp3/music.mp3")
#' @export
file_name <- function(filepath) {
  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filepath))
}


####################################################################
#' Get file extensions without file names
#'
#' @rdname file_name
#' @examples
#' file_type("file.aux")
#' file_type("temp/file.R")
#' file_type("/temp/temp3/music.mp3")
#' @export
file_type <- function(filepath) tolower(gsub(".*\\.", "", filepath))


####################################################################
#' Spread list column into new columns
#'
#' Spread an existing list column into new columns on a data.frame. Note
#' that every element on every observation must have a name for the function
#' to do its work. Original column will be automatically suppressed but you
#' can set the \code{replace} argument to avoid it.
#'
#' @param df Dataframe
#' @param col Variable name.
#' @param str Character. Start column names with. If set to \code{NULL},
#' original name of column will be used.
#' @param replace Boolean. Replace original values (delete column)
#' @return data.frame. Result of un-nesting named or un-named list columns.
#' @examples
#' df <- dplyr::starwars
#' # Un-named list columns
#' spread_list(df, films, replace = FALSE) %>%
#'   dplyr::select(name, dplyr::starts_with("films")) %>%
#'   head(8)
#' # Named (and un-nammed) list columns
#' df <- dplyr::tibble(id = 1:3, platform = list(
#'   list("fb" = 1, "ig" = 2),
#'   list("fb" = 3),
#'   list()
#' ))
#' spread_list(df, platform, str = "ptf_")
#' @export
spread_list <- function(df, col, str = NULL, replace = TRUE) {
  var <- enquo(col)
  col <- as_label(var)
  cols <- colnames(df)

  if (!"list" %in% unlist(lapply(df[, cols == col], class))) {
    warning("The variable provided is not a list variable")
    return(df)
  }

  # If all values are NA, no need to proceed but to change into vector with NAs
  if (all(unlist(lapply(df[, cols == col], is.na)))) {
    df[, cols == col] <- rep(NA, nrow(df))
    return(df)
  }

  # Automatic naming based on original
  if (is.null(str)) str <- sprintf("%s_", col)

  # Add character NAs name to those observations with no data, thus no names
  nonames <- rowwise(df) %>%
    mutate(len = length(names(!!var))) %>%
    pull(.data$len) == 0
  # Non-named list columns
  if (sum(nonames) == nrow(df)) {
    binded <- select(df, !!var) %>%
      mutate(temp_cross_id = row_number()) %>%
      tidyr::unnest_longer(!!var) %>%
      mutate(key = TRUE) %>%
      tidyr::spread(key = !!var, value = .data$key) %>%
      replace(is.na(.), FALSE)
  } else {
    # Named list columns
    binded <- lapply(df[!nonames, cols == col], bind_rows) %>%
      bind_rows() %>%
      mutate(temp_cross_id = which(!nonames))
    if (is.numeric(binded[[1]])) binded <- binded %>% replace(is.na(.), 0)
  }

  if (is.na(str)) str <- paste0(col, "_")
  ids <- which(colnames(binded) == "temp_cross_id")
  colnames(binded)[-ids] <- paste0(str, colnames(binded))[-ids]
  done <- df %>%
    mutate(temp_cross_id = row_number()) %>%
    left_join(binded, "temp_cross_id") %>%
    select(-.data$temp_cross_id)

  original <- which(cols == col)
  done <- done %>% select(1:original, starts_with(str), (original + 1):ncol(done))
  if (replace) done <- done[, -original]
  return(as_tibble(done))
}


####################################################################
#' Format a string text as markdown/HTML
#'
#' Format any character string to HTML or markdown format. We
#' recommend using this format with the \code{ggtext::geom_richtext}
#' function to format text in \code{ggplot2} objects.
#'
#' @param text Character. Strings to format.
#' @param color Character. Hex colour code.
#' @param size Numeric. Text size.
#' @param bold Boolean. Should the text be bold?
#' @return String with format characters included.
#' @examples
#' formatHTML("Text test", color = "#000000")
#' formatHTML(c(123, 456), color = "orange", size = 120, bold = TRUE)
#'
#' # If you want to use it with \code{ggtext}:
#' \dontrun{
#' col1 <- "grey"
#' col2 <- "orange"
#' pt <- data.frame(
#'   label = paste0(
#'     formatHTML(123, color = col2, size = 120, bold = TRUE), "<br/>",
#'     formatHTML("of children had a", col1), "<br/>",
#'     formatHTML("traditional stay-at-home mom", color = col2, bold = TRUE), "<br/>",
#'     formatHTML(paste0("in 2012, compared to ", 321, " in 1970"), color = col1)
#'   )
#' )
#' ggplot(pt, aes(x = 0, y = 0)) +
#'   ggtext::geom_richtext(
#'     aes(label = label),
#'     hjust = 0,
#'     label.color = NA,
#'     lineheight = 1.5
#'   ) +
#'   xlim(0, 0.01) +
#'   theme_void()
#' }
#' @export
#' @rdname format_string
formatHTML <- function(text, color = "black", size = 20, bold = FALSE) {
  opening_span <- paste0("<span style='font-size:", size, "px; color:", color, "'>")
  if (bold) text <- paste0("**", text, "**")
  closing_span <- "</span>"
  text <- paste(text, collapse = "<br/>")
  ret <- paste0(opening_span, text, closing_span)
  return(ret)
}


####################################################################
#' Interpolate a string [glue wrapper]
#'
#' Format and interpolate a string using a \code{glue} wrapper. Allows
#' simple operations, \code{NULL} values as input, and interactions with
#' internal (created within \code{glued}) and external (environment) objects.
#'
#' @inheritParams stringr::str_glue
#' @param empty_lines Character. Set to \code{"keep"} to keep or
#' \code{"drop"} to drop empty lines.
#' @return Same as input but transformed (glued).
#' @examples
#' name <- "Bernardo"
#' age <- 29
#' anniversary <- as.Date("2016-04-30")
#' glued("
#'   My name is {name},
#'   my age next year will be {age + 1},
#'   and I got married on {format(anniversary, '%A, %B %d, %Y')}.")
#'
#' # Single braces can be inserted by doubling them
#' glued("My name is {name}, not {{name}}.")
#'
#' # You can also used named arguments
#' glued(
#'   "Her name is {name}, ",
#'   "and her age next year will be {age + 1}.",
#'   name = "Maru",
#'   age = 6
#' )
#'
#' # And run operations with memories (beware!)
#' glued("My name, {name}, has {n <- nchar(name); n} characters.
#'        If we multiply by ten, we'll have {10 * n} characters!")
#'
#' # If you pass a vector, the operation will be repeated for each element
#' glued("Here's the value #{1:3}")
#' @export
glued <- function(..., .sep = "", empty_lines = "keep", .envir = parent.frame()) {
  null_transformer <- function(text, envir) {
    out <- eval(parse(text = text, keep.source = FALSE), envir)
    if (is.null(out)) out <- ""
    return(out)
  }
  output <- stringr::str_glue(
    ...,
    .sep = .sep,
    .transformer = null_transformer,
    .envir = .envir
  )
  if (empty_lines == "drop") {
    lines <- stringr::str_split(output, "\n")[[1]]
    output <- glued(paste(lines[trimws(lines) != ""], collapse = "\n"))
  }
  return(output)
}


####################################################################
#' Pattern Matching for Any or All Multiple Matches
#'
#' This function returns a boolean vector of the same length as `x`,
#' each element of which is the result of applying the `type` of matches
#' to the corresponding element of `x`, using regular expressions.
#'
#' @inheritParams base::grep
#' @param type Character. Type of match. Choose one of:
#' \code{any}, \code{all}
#' @param ... Additional arguments to pass to \code{grepl}
#' @return Boolean of same length as \code{x}
#' @examples
#' x <- c(123, 876, 18761)
#' patterns <- c(1, 2)
#' grepm(patterns, x, type = "any")
#' grepm(patterns, x, type = "all")
#' @export
grepm <- function(pattern, x, type = "all", ...) {
  lapply(x, function(a) lapply(pattern, function(i) grepl(i, a, ...))) %>%
    lapply(get(type)) %>%
    unlist() %>%
    suppressWarnings()
}

####################################################################
#' Print Coloured Messages
#'
#' @param txt Character. Text to print or transform.
#' @param colour Character. Any of: grey, red, green, yellow, blue, or purple.
#' @param cat Boolean. Print with cat? If not, raw string
#' @return Depends on \code{cat}: NULL if TRUE or character string if FALSE.
#' @examples
#' opts <- c("GREY", "RED", "GREEN", "YELLOW", "BLUE", "PURPLE")
#' for (colour in opts) formatColoured(paste("Colour:", colour, "\n"), colour)
#' @export
formatColoured <- function(txt, colour = c("blue", "yellow", "grey"), cat = TRUE) {
  colour <- toupper(colour)[1]
  opts <- c("GREY", "RED", "GREEN", "YELLOW", "BLUE", "PURPLE")
  check_opts(colour, opts)
  if (colour == opts[1]) code <- 30
  if (colour == opts[2]) code <- 31
  if (colour == opts[3]) code <- 32
  if (colour == opts[4]) code <- 33
  if (colour == opts[5]) code <- 34
  if (colour == opts[6]) code <- 35
  out <- paste0("\033[0;", code, "m", txt, "\033[0m")
  if (cat) cat(out) else return(out)
}
