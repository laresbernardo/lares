####################################################################
#' One Hot Smart Encoding (Dummy Variables)
#'
#' This function lets the user automatically transform a dataframe with
#' categorical columns into numerical by one hot encoding technic.
#'
#' @family Data Wrangling
#' @family Feature Engineering
#' @family One Hot Encoding
#' @param df Dataframe
#' @param redundant Boolean. Should we keep redundant columns? i.e. If the
#' column only has two different values, should we keep both new columns?
#' Is set to \code{NULL}, only binary variables will dump redundant columns.
#' @param drop Boolean. Drop automatically some useless features?
#' @param ignore Vector or character. Which column should be ignored?
#' @param dates Boolean. Do you want the function to create more features
#' out of the date/time columns?
#' @param holidays Boolean. Include holidays as new columns?
#' @param country Character or vector. For which countries should the holidays
#' be included?
#' @param currency_pair Character. Which currency exchange do you
#' wish to get the history from? i.e, USD/COP, EUR/USD...
#' @param trim Integer. Trim names until the nth character
#' @param limit Integer. Limit one hot encoding to the n most frequent
#' values of each column. Set to \code{NA} to ignore argument.
#' @param variance Numeric. Drop columns with more than n variance.
#' Range: 0-1. For example: if a variable contains 91 unique different
#' values out of 100 observations, this column will be suppressed if
#' value is set to 0.9
#' @param other_label Character. With which text do you wish to replace
#' the filtered values with?
#' @param sep Character. Separator's string
#' @param quiet Boolean. Quiet all messages and summaries?
#' @param ... Additional parameters
#' @return data.frame on which all features are numerical by nature or
#' transformed with one hot encoding.
#' @examples
#' data(dft)
#' dft <- dft[, c(2, 3, 5, 9, 11)]
#'
#' ohse(dft, limit = 3) %>% head(3)
#' ohse(dft, limit = 3, redundant = NULL) %>% head(3)
#'
#' # Getting rid of columns with no (or too much) variance
#' dft$no_variance1 <- 0
#' dft$no_variance2 <- c("A", rep("B", nrow(dft) - 1))
#' dft$no_variance3 <- as.character(rnorm(nrow(dft)))
#' dft$no_variance4 <- c(rep("A", 20), round(rnorm(nrow(dft) - 20), 4))
#' ohse(dft, limit = 3) %>% head(3)
#' ohse(dft, limit = 3, var = 1) %>% head(3)
#' @export
ohse <- function(df,
                 redundant = FALSE,
                 drop = TRUE,
                 ignore = NA,
                 dates = FALSE,
                 holidays = FALSE, country = "Colombia",
                 currency_pair = NA,
                 trim = 0,
                 limit = 10,
                 variance = 0.9,
                 other_label = "OTHER",
                 sep = "_",
                 quiet = FALSE,
                 ...) {
  if (is.vector(df)) {
    df <- data.frame(var = df)
  } else {
    df <- data.frame(df)
  }

  order <- colnames(df)

  # Dummy variables that will be filled
  no_need_to_convert <- converted <- converted_binary <- NULL

  # No variance columns
  no_variance <- zerovar(df)
  if (drop) {
    df <- df[, !colnames(df) %in% no_variance]
  }

  # Create features out of date/time variables
  if (dates == TRUE | holidays == TRUE | !is.na(currency_pair)) {
    times <- df_str(df, return = "names", quiet = TRUE)$time
    if (length(times) <= 1) {
      df_dates <- date_feats(df,
        drop = FALSE,
        features = dates,
        holidays = holidays,
        country = country,
        currency_pair = currency_pair,
        quiet = quiet
      )

      if (ncol(df_dates) != ncol(df)) {
        df <- left_join(df, df_dates, by = as.character(times[1])) %>% distinct()
      }
    }
  }

  # Leave some columns out of the logic
  if (!is.na(ignore)[1]) {
    if (!quiet) message(">>> Omitting transformations for ", vector2text(ignore))
    ignored <- df %>% select(one_of(ignore))
    df <- df %>% select(-one_of(ignore))
  }

  # Name and type of variables
  types <- data.frame(
    name = colnames(df),
    type = unlist(lapply(lapply(df, class), `[[`, 1))
  )

  # Iterate all columns
  for (i in seq_along(df)) {
    vector_type <- types[i, "type"]
    vector_name <- as.character(types$name[i])
    vector_levels <- length(unique(df[, c(vector_name)]))
    vector_values <- df[toString(types[i, "name"])]

    # Non numeric or date/time variables
    if (!vector_type %in% c("integer", "numeric", "POSIXct", "POSIXt", "Date")) {

      # Char columns with too much variance (unique values vs total observations)
      if (vector_levels >= variance * nrow(df)) {
        no_variance <- c(no_variance, vector_name)
      }

      vector_values <- vector_values %>%
        mutate_all(as.character) %>%
        replace(., is.na(.), "NAs")
      vector_values[, 1] <- paste0(sep, vector_values[, 1])

      # Columns with 2 possible values
      if (vector_levels == 2 & !isTRUE(redundant)) {
        which <- as.character(levels(as.factor(df[, c(vector_name)]))[2])
        df[, c(vector_name)] <- as.integer(as.factor(df[, c(vector_name)])) - 1
        converted_binary <- rbind(converted_binary, vector_name)
        df <- rename_at(df, vars(vector_name), list(~ paste0(vector_name, "_", which)))
      }

      # ONE HOT ENCODING
      if (!colnames(vector_values) %in% c(converted_binary, no_variance)) {
        if (vector_levels >= 2 & !vector_name %in% converted_binary) {
          options("na.action" = "na.pass")
          reduced <- categ_reducer(
            vector_values, !!as.name(vector_name),
            top = limit,
            other_label = paste0(sep, other_label)
          )
          dummy_matx <- data.frame(model.matrix(~ . - 1, reduced))
          colnames(dummy_matx) <- paste0(vector_name, sort(unique(reduced[, 1])))
          if (isFALSE(redundant)) dummy_matx <- dummy_matx[, 1:(ncol(dummy_matx) - 1)]
          df <- cbind(df, dummy_matx)
          converted <- rbind(converted, vector_name)
        }
      }
    }
    no_need_to_convert <- rbind(no_need_to_convert, vector_name)
  }

  # Shorten up the long names of some variables
  if (trim > 0) colnames(df) <- substr(colnames(df), 1, trim)

  # Summary of transformations
  if (!quiet) {
    total_converted <- rbind(converted, converted_binary)
    if (length(total_converted) > 1) {
      message(paste(
        ">>> One Hot Encoding applied to", length(total_converted),
        "variables:", vector2text(total_converted)
      ))
    }
    if (length(no_variance) > 0 & drop) {
      message(paste0(
        ">>> Automatically dropped ", length(no_variance),
        " columns with 0% or >=", round(variance * 100),
        "% variance: ", vector2text(no_variance)
      ))
    }
  }

  # Return only useful columns
  if (drop) {
    df <- df[, c(!colnames(df) %in% c(converted, no_variance))]
  }

  # Bind ignored untouched columns
  order <- order[order %in% colnames(df)]
  if (!is.na(ignore)[1]) {
    df <- df %>% select(one_of(order), everything())
  }

  return(as_tibble(df))
}


####################################################################
#' One Hot Encoding for a Vector with Comma Separated Values
#'
#' This function lets the user do one hot encoding on a variable with
#' comma separated values
#'
#' @family Data Wrangling
#' @family One Hot Encoding
#' @param df Dataframe. May contain one or more columns with comma separated
#' values which will be separated as one hot encoding
#' @param ... Variables. Which variables to split into new columns?
#' @param sep Character. Which regular expression separates the elements?
#' @param noval Character. No value text
#' @param remove Boolean. Remove original variables?
#' @return data.frame on which all features are numerical by nature or
#' transformed with one hot encoding.
#' @examples
#' df <- data.frame(
#'   id = c(1:5),
#'   x = c("AA, D", "AA,B", "B,  D", "A,D,B", NA),
#'   z = c("AA+BB+AA", "AA", "BB,  AA", NA, "BB+AA")
#' )
#' ohe_commas(df, x, remove = TRUE)
#' ohe_commas(df, z, sep = "\\+")
#' ohe_commas(df, x, z)
#' @export
ohe_commas <- function(df, ..., sep = ",", noval = "NoVal", remove = FALSE) {
  vars <- quos(...)
  var <- gsub("~", "", as.character(vars))
  
  df <- as.data.frame(df)
  
  for (i in var) {
    df$temp <- as.character(df[, i])
    # Handling missingness
    df$temp[as.character(df$temp) == "" | is.na(df$temp)] <- noval
    vals <- v2t(as.character(df$temp), quotes = FALSE)
    vals <- unique(trimws(unlist(strsplit(vals, sep))))
    # aux <- sprintf("--%s--", vals)
    l <- strsplit(df$temp, sep)
    mat <- NULL
    for (i in seq_along(vals)) {
      which <- unlist(lapply(l, function(x) any(trimws(x) %in% vals[i])))
      mat <- cbind(mat, which)
    }
    colnames(mat) <- gsub('"', "", paste(var, vals, sep = "_"))
    df$temp <- NULL
    df <- cbind(df, mat)
  }
  if (remove) df <- df[, !colnames(df) %in% var]
  return(as_tibble(df, .name_repair = "minimal"))
}


####################################################################
#' One Hot Encoding for Date/Time Variables (Dummy Variables)
#'
#' This function lets the user automatically create new columns out
#' of a dataframe or vector with date/time variables.
#'
#' @family Data Wrangling
#' @family Feature Engineering
#' @family One Hot Encoding
#' @param dates Vector or dataframe. Non-date/time columns will be
#' automatically ignored/extracted.
#' @param drop Boolean. Should the original date/time columns be
#' kept in the results? Only valid when input is a dataframe.
#' @param only Character or vector. Which columns do you wish to process? If
#' non are explicitly defined, all will be processed
#' @param features Create features out of date/time columns?
#' @param holidays Boolean. Include holidays as new columns?
#' @param country Character or vector. For which countries should the holidays
#' be included?
#' @param currency_pair Character. Which currency exchange do you
#' wish to get the history from? i.e, USD/COP, EUR/USD...
#' @param quiet Boolean. Quiet all messages?
#' @return data.frame with additional features calculated out of time or date vectors.
#' @examples
#' df <- data.frame(
#'   dates = sample(seq(Sys.Date() - 365, Sys.Date(), by = 1), 50),
#'   times = sample(seq(Sys.time() - 1e7, Sys.time(), by = 1), 50)
#' )
#'
#' # Input as a vector or dataframe
#' date_feats(df, drop = TRUE) %>% head(10)
#'
#' # Holidays given a date range and country
#' \donttest{
#' hol <- date_feats(
#'   seq(Sys.Date() - 365, Sys.Date(), by = 1),
#'   holidays = TRUE,
#'   country = "Colombia"
#' )
#' head(hol[!is.na(hol$holidayname), ])
#' }
#' @export
date_feats <- function(dates,
                       drop = FALSE,
                       only = NA,
                       features = TRUE,
                       holidays = FALSE,
                       country = "Venezuela",
                       currency_pair = NA,
                       quiet = FALSE) {
  results <- NULL
  date_cols <- df_str(dates, return = "names", quiet = TRUE)$time

  if (length(date_cols) == 0) {
    return(dates)
  }

  if (!is.na(only)) {
    date_cols <- date_cols[date_cols %in% only]
  }

  iters <- ifelse(date_cols == "df", 1, length(date_cols))[1]
  if (!is.na(iters)) {
    if (!quiet) {
      message(paste(">>> Processing", iters, "date/time columns:", vector2text(date_cols)))
    }
  } else {
    return(dates)
  }

  if (!"data.frame" %in% class(dates) & iters == 1) {
    dates <- data.frame(values_date = dates)
    date_cols <- "values_date"
  }

  if (holidays | !is.na(currency_pair)) {
    search_dates <- dates[, c(colnames(dates) %in% date_cols)]
    search_dates[] <- unlist(lapply(search_dates, function(x) gsub(" .*", "", as.character(x))))
    alldates <- as.Date(unlist(search_dates, use.names = FALSE))
    alldates <- alldates[!is.na(alldates)]
  }

  if (holidays) {
    years <- sort(unique(year(alldates)))
    holidays_dates <- holidays(countries = country, years)
    colnames(holidays_dates)[1] <- "values_date"
    holidays_dates$values_date <- as.character(as.Date(holidays_dates$values_date))
    cols <- paste0("values_date_holiday_", colnames(holidays_dates)[4:ncol(holidays_dates)])
    colnames(holidays_dates)[-c(1:3)] <- cols
  }

  # Features creator
  if (features == TRUE | !is.na(currency_pair) | holidays == TRUE) {
    for (col in 1:iters) {
      col_name <- date_cols[col]
      result <- dates %>% select(!!as.name(col_name))
      values <- as.POSIXlt(result[, 1], origin = "1970-01-01")
      result$values_date <- as.character(as.Date(values))

      # Features creator
      if (features == TRUE) {
        result$values_date_year <- year(values)
        result$values_date_month <- month(values)
        result$values_date_day <- day(values)
        result$values_date_week <- week(values)
        result$values_date_weekday <- weekdays(values, abbreviate = TRUE)
        result$values_date_weekend <- format(values, "%u") %in% c(6, 7)
        result$values_date_year_day <- as.integer(difftime(
          values, floor_date(values, unit = "year"),
          units = "day"
        ))

        if ("POSIXlt" %in% class(values)) {
          result$values_date_hour <- hour(values)
          result$values_date_minute <- minute(values)
          result$values_date_minutes <- as.integer(difftime(
            values, floor_date(values, unit = "day"),
            units = "mins"
          ))
          result$values_date_second <- second(values)
        }
      }

      # Holidays data
      if (holidays) {
        result <- result %>%
          left_join(holidays_dates, by = "values_date") %>%
          mutate_at(vars(cols), list(~ replace(., which(is.na(.)), FALSE)))
      }

      # Currencies data
      if (!is.na(currency_pair)) {
        currency <- get_currency(currency_pair, from = min(alldates), to = max(alldates))
        colnames(currency) <- c("values_date", paste0("values_date_", tolower(cleanText(currency_pair))))
        currency[, 1] <- as.character(currency[, 1])
        result <- result %>% left_join(currency, by = "values_date")
      }

      col_name <- ifelse(col_name == "values_date", "", paste0(col_name, "_"))
      colnames(result)[-1] <- gsub("values_date_", col_name, colnames(result)[-1])
      results <- results %>%
        bind_cols(result) %>%
        select(-contains("values_date"))
    }
  }
  if (drop) {
    results <- results[, !colnames(results) %in% date_cols]
  }
  return(as_tibble(results))
}


####################################################################
#' Holidays in your Country
#'
#' This function lets the user automatically scrap holiday dates from
#' any country and year within +- 5 years. Thanks to timeanddate.com!
#'
#' @family Data Wrangling
#' @family Feature Engineering
#' @family Scrapper
#' @family One Hot Encoding
#' @param years Character or vector. For which year(s) do you wish to import
#' holiday dates?
#' @param countries Character or vector. For which country(ies) should the
#' holidays be imported?
#' @return data.frame with holidays data for given \code{countries} and \code{years}.
#' @examples
#' \donttest{
#' holidays(countries = "Argentina")
#' holidays(countries = c("Argentina", "Venezuela"), years = c(2019, 2020))
#' }
#' @export
holidays <- function(countries = "Colombia", years = year(Sys.Date())) {

  # Further improvement: let the user bring more than +-5 years

  results <- NULL
  year <- year(Sys.Date())
  years <- years[years %in% ((year - 5):(year + 5))]
  combs <- expand.grid(years, countries) %>% dplyr::rename(year = "Var1", country = "Var2")
  for (i in seq_len(nrow(combs))) {
    message(paste0(">>> Extracting ", combs$country[i], "'s holidays for ", combs$year[i]))
    url <- paste0("https://www.timeanddate.com/holidays/", tolower(combs$country[i]), "/", combs$year[i])
    holidays <- content(GET(url))
    holidays <- holidays %>%
      html_nodes(".table") %>%
      html_table(fill = TRUE) %>%
      data.frame(.) %>%
      filter(!is.na(.data$Date))
    holidays <- holidays[, -2]
    colnames(holidays) <- c("Date", "Holiday", "Holiday.Type")
    holidays$Date <- paste(holidays$Date, combs$year[i])
    if (sum(grepl("de", holidays$Date)) > 0) {
      holidays$Date <- gsub("de ", "", holidays$Date)
    }
    first <- as.numeric(as.character(substr(holidays$Date, 1, 1)))
    if (!is.na(first[1])) {
      holidays$Date <- as.Date(holidays$Date, format = "%d %b %Y")
    } else {
      holidays$Date <- as.Date(holidays$Date, format = "%b %d %Y")
    }
    result <- data.frame(
      holiday = holidays$Date,
      holiday_name = holidays$Holiday,
      holiday_type = holidays$Holiday.Type
    ) %>%
      mutate(
        national = grepl("National|Federal", holidays$Holiday.Type),
        observance = grepl("Observance", holidays$Holiday.Type),
        bank = grepl("Bank", holidays$Holiday.Type),
        nonwork = grepl("Non-working", holidays$Holiday.Type),
        season = grepl("Season", holidays$Holiday.Type),
        hother = !grepl("National|Federal|Observance|Season", holidays$Holiday.Type)
      ) %>%
      {
        if (length(unique(countries)) > 1) {
          mutate(., country = combs$country[i])
        } else .
      }
    results <- bind_rows(results, result)
  }
  results <- results %>%
    filter(!is.na(.data$holiday)) %>%
    cleanNames() %>%
    as_tibble()
  return(results)
}
