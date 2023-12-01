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
#' Zero Variance Columns
#'
#' This function detects which columns have the same value (whichever)
#' for each column.
#'
#' @family Data Wrangling
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
#' Normalize Vector
#'
#' This function lets the user normalize numerical values into
#' the 0 to 1 range
#'
#' @family Data Wrangling
#' @inheritParams corr_var
#' @param x Numeric Vector. Numbers to be transformed into
#' normalized vector
#' @return Vector with normalized \code{x} values
#' @examples
#' x <- c(0, 1, 4, 7.5, 10)
#' normalize(x)
#' @export
normalize <- function(x, ...) {
  if (is.numeric(x)) {
    if (length(unique(x)) == 1) {
      return(rep(1, length(x)))
    }
    x <- (x - min(x, ...)) / (max(x, ...) - min(x, ...))
    return(x)
  } else {
    stop("Try with a numerical vector!")
  }
}


####################################################################
#' Abbreviate numbers
#'
#' This function converts a numeric vector's values into their
#' abbreviated character equivalent, i.e. 100,000,000 into 100M.
#'
#' @family Data Wrangling
#' @param x Numeric vector
#' @param n Integer. Single numeric value, specifying number of
#' significant figures to show. Range 1 to 6.
#' @return Vector of character values that contain converted values
#' @examples
#' num_abbr(rnorm(10) * 1e6)
#' num_abbr(rnorm(10) * 1e6, n = 1)
#' @export
num_abbr <- function(x, n = 3) {
  if (is.null(x)) return(x)
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
    tops <- filter(dff, .data$n >= nmin & .data$p >= pmin & .data$p <= pcummax)
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
  if (and != "" && (Sys.getenv("LARES_NUMFORMAT") == 1 || n == 2)) {
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
  if (is.null(x)) return(x)
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
      x <- format(as.numeric(x), big.mark = ".", decimal.mark = ",", ...)
    } else {
      x <- format(as.numeric(x), big.mark = ",", decimal.mark = ".", ...)
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
#' Calculate cuts by quantiles
#'
#' This function lets the user quickly calculate cuts for quantiles
#' and discretize numerical values into categorical values.
#'
#' @family Data Wrangling
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
  if (!is.data.frame(df)) stop("Input 'df' must be a valid data.frame")
  if (all) {
    not_all_nas <- colSums(is.na(df)) != nrow(df)
    keep <- (colnames(df) %in% ignore) | as.vector(not_all_nas)
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
#' Replace Factor Values
#'
#' This function lets the user replace levels on a factor vector.
#'
#' @family Data Wrangling
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
#' Extract file raw name and type from file names
#'
#' @family Data Wrangling
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
#' @family Data Wrangling
#' @rdname file_name
#' @examples
#' file_type("file.aux")
#' file_type("temp/file.R")
#' file_type("/temp/temp3/music.mp3")
#' @export
file_type <- function(filepath) tolower(gsub(".*\\.", "", filepath))
