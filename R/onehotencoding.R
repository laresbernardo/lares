####################################################################
#' One Hot Smart Encoding (Dummy Variables)
#'
#' This function lets the user automatically transform a dataframe with
#' categorical columns into numerical by one hot encoding technic.
#'
#' @family Data Wrangling
#' @family Feature Engineering
#' @family One Hot Encoding
#' @inheritParams cache_write
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
#' @export
ohse <- function(df,
                 redundant = FALSE,
                 drop = TRUE,
                 ignore = NULL,
                 dates = FALSE,
                 holidays = FALSE,
                 country = "Venezuela",
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

  # Leave some columns out of the logic
  ignore <- unique(ignore)
  if (!is.null(ignore)) {
    if (!quiet) message(">>> Omitting transformations for ", vector2text(ignore))
    ignored <- select(df, any_of(ignore))
    df <- select(df, -any_of(ignore))
  } else {
    ignored <- NULL
  }

  # No variance columns
  no_variance <- zerovar(df)
  if (drop) df <- select(df, !any_of(no_variance))

  # Create features out of date/time variables
  if (dates == TRUE || holidays == TRUE || !is.na(currency_pair)) {
    times <- df_str(df, return = "names", quiet = TRUE)$time
    if (length(times) <= 1) {
      df_dates <- date_feats(df,
        drop = FALSE,
        append = FALSE,
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
      if (vector_levels == 2 && !isTRUE(redundant)) {
        which <- as.character(levels(as.factor(df[, c(vector_name)]))[2])
        df[, c(vector_name)] <- as.integer(as.factor(df[, c(vector_name)])) - 1
        converted_binary <- rbind(converted_binary, vector_name)
        df <- rename_at(df, vars(vector_name), list(~ paste0(vector_name, "_", which)))
      }

      # ONE HOT ENCODING
      if (!colnames(vector_values) %in% c(converted_binary, no_variance)) {
        if (vector_levels >= 2 && !vector_name %in% converted_binary) {
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
    if (length(no_variance) > 0 && drop) {
      no_variance <- no_variance[no_variance %in% ignore]
      if (length(no_variance) > 0) {
        message(paste0(
          ">>> Automatically dropped ", length(no_variance),
          " columns with 0% or >=", round(variance * 100),
          "% variance: ", vector2text(no_variance)
        ))
      }
    }
  }

  # Return only useful columns
  if (drop && length(c(converted, no_variance)) > 0) {
    df <- df[, c(!colnames(df) %in% c(converted, no_variance))]
  }

  # Bind ignored untouched columns and order
  order <- order[order %in% colnames(df)]
  df <- bind_cols(df, ignored) %>% select(any_of(order), everything())
  as_tibble(df)
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
  as_tibble(df, .name_repair = "minimal")
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
#' @inheritParams mp3_get
#' @param dates Vector or dataframe. Non-date/time columns will be
#' automatically ignored/extracted.
#' @param drop Boolean. Should the original date/time columns be
#' kept in the results? Only valid when input is a dataframe.
#' @param only Character or vector. Which columns do you wish to process? If
#' non are explicitly defined, all will be processed
#' @param append Boolean. Append results to existing data.frame? If FALSE,
#' only calculated values will be returned.
#' @param holidays Boolean. Include holidays as new columns?
#' @param country Character or vector. For which countries should the holidays
#' be included?
#' @param currency_pair Character. Which currency exchange do you
#' wish to get the history from? i.e, USD/COP, EUR/USD...
#' @return data.frame with additional features calculated out of time or date vectors.
#' @examples
#' df <- data.frame(
#'   dates = sample(seq(Sys.Date() - 365, Sys.Date(), by = 1), 50),
#'   times = sample(seq(Sys.time() - 1e7, Sys.time(), by = 1), 50)
#' )
#'
#' # Input as a vector or dataframe
#' date_feats(df, drop = TRUE, quiet = TRUE) %>% head(10)
#'
#' # Holidays given a date range and country
#' \dontrun{
#' hol <- date_feats(
#'   seq(Sys.Date() - 365, Sys.Date(), by = 1),
#'   holidays = TRUE,
#'   country = "Venezuela"
#' )
#' head(hol[!is.na(hol$holiday_name), ])
#' }
#' @export
date_feats <- function(dates,
                       drop = FALSE,
                       only = NA,
                       append = FALSE,
                       holidays = FALSE,
                       country = "Venezuela",
                       currency_pair = NA,
                       quiet = FALSE) {
  results <- NULL
  original <- dates
  date_cols <- df_str(dates, return = "names", quiet = TRUE)$time
  vector <- is.null(dim(dates))

  if (length(date_cols) == 0) {
    dates
  } else {
    if (!is.na(only)) {
      date_cols <- date_cols[date_cols %in% only]
    }

    iters <- ifelse(date_cols == "df", 1, length(date_cols))[1]

    if (is.na(iters)) {
      dates
    } else {
      if (!quiet) message(paste(">>> Processing", iters, "date/time columns:", vector2text(date_cols)))

      if (!"data.frame" %in% class(dates) && iters == 1) {
        dates <- data.frame(values_date = dates)
        date_cols <- "values_date"
      }

      alldates <- NULL
      if (holidays || !is.na(currency_pair)) {
        search_dates <- dates[, date_cols, drop = FALSE]
        search_dates[] <- lapply(search_dates, function(x) gsub(" .*", "", as.character(x)))
        alldates <- as.Date(unlist(search_dates, use.names = FALSE))
        alldates <- alldates[!is.na(alldates)]
      }

      holidays_dates <- NULL
      if (holidays && length(alldates) > 0) {
        years <- sort(unique(year(alldates)))
        holidays_dates <- holidays(countries = country, years)
        colnames(holidays_dates)[1] <- "values_date"
        holidays_dates$values_date <- as.character(as.Date(holidays_dates$values_date))
        cols <- paste0("values_date_holiday_", colnames(holidays_dates)[4:ncol(holidays_dates)])
        colnames(holidays_dates)[-(1:3)] <- cols
      }

      for (col in date_cols) {
        result <- dates %>% select(!!sym(col))
        values <- result[[1]]
        result$values_date <- as.character(as.Date(values))

        result$values_date_year <- year(values)
        result$values_date_month <- month(values)
        result$values_date_day <- day(values)
        result$values_date_week <- week(values)
        result$values_date_weekday <- weekdays(values, abbreviate = TRUE)
        result$values_date_weekend <- format(values, "%u") %in% c("6", "7")
        result$values_date_year_day <- as.integer(difftime(
          values, floor_date(values, unit = "year"),
          units = "day"
        ))

        if (any(grepl("POSIX", class(values)))) {
          result$values_date_hour <- hour(values)
          result$values_date_minute <- minute(values)
          result$values_date_minutes <- as.integer(difftime(
            values, floor_date(values, unit = "day"),
            units = "mins"
          ))
          result$values_date_second <- second(values)
        }

        if (!is.null(holidays_dates)) {
          result <- result %>%
            left_join(holidays_dates, by = "values_date", relationship = "many-to-many") %>%
            mutate(values_date_holiday_county = as.character(.data$values_date_holiday_county)) %>%
            mutate(across(starts_with("values_date_holiday_"), ~ replace(., is.na(.), FALSE)))
        }

        if (!is.na(currency_pair) && length(alldates) > 0) {
          currency <- get_currency(currency_pair, from = min(alldates), to = max(alldates))
          colnames(currency) <- c("values_date", paste0("values_date_", tolower(cleanText(currency_pair))))
          currency[[1]] <- as.character(currency[[1]])
          result <- result %>% left_join(currency, by = "values_date")
        }

        prefix <- if (col == "values_date") "" else paste0(col, "_")
        colnames(result)[-1] <- gsub("^values_date_", prefix, colnames(result)[-1])

        results <- results %>%
          bind_cols(result) %>%
          select(-contains("values_date"))

        if (vector) colnames(results)[1] <- "values"
      }

      if (append) {
        results <- bind_cols(original, select(results, -any_of(colnames(original))))
      }

      if (drop) {
        results <- results[, !colnames(results) %in% date_cols, drop = FALSE]
      }

      as_tibble(results)
    }
  }
}


####################################################################
#' Holidays in your Country
#'
#' This function lets the user automatically retrieve public holiday dates
#' for any country supported by the Nager.Date API. Accepts country names
#' (e.g., "Portugal") or ISO 3166-1 alpha-2 codes (e.g., "PT").
#' Thanks to \href{https://date.nager.at}{Nager.Date}!
#'
#' @family Data Wrangling
#' @family Feature Engineering
#' @family Scrapper
#' @family One Hot Encoding
#' @inheritParams mp3_get
#' @param years Character or vector. For which year(s) do you wish to import
#' holiday dates?
#' @param countries Character or vector. For which country(ies) should the
#' holidays be imported? Accepts country names or ISO 3166-1 alpha-2 codes.
#' @param include_regions Boolean. Default FALSE. If TRUE, for countries with
#' internal subdivisions, it will provide details on which sub-state the found
#' holidays apply.
#' @return \code{data.frame} with holidays data for given \code{countries} and \code{years}.
#' @examples
#' \donttest{
#' holidays(countries = "Argentina")
#' year <- as.integer(format(Sys.Date(), format = "%Y"))
#' holidays(countries = c("Spain", "Venezuela"), years = year)
#' holidays(countries = "Germany", include_regions = TRUE)
#' holidays(countries = "PT") # Also accepts ISO country codes
#' }
#' @export
holidays <- function(countries = "Venezuela",
                     years = year(Sys.Date()),
                     quiet = FALSE,
                     include_regions = FALSE) {
  if (!haveInternet()) {
    message("No internet connection...")
    invisible(NULL)
  } else {
    results <- NULL

    # Resolve country names to ISO codes via Nager.Date API
    available <- tryCatch(
      jsonlite::fromJSON("https://date.nager.at/api/v3/AvailableCountries"),
      error = function(e) {
        stop("Failed to fetch available countries from Nager.Date API: ", e$message)
      }
    )

    country_codes <- vapply(countries, function(cntry) {
      # If already a valid 2-letter ISO code, use directly
      if (nchar(cntry) == 2 && toupper(cntry) %in% available$countryCode) {
        return(toupper(cntry))
      }
      # Match by name (case-insensitive)
      match_idx <- which(tolower(available$name) == tolower(cntry))
      if (length(match_idx) == 1) return(available$countryCode[match_idx])
      # Partial match fallback
      match_idx <- grep(cntry, available$name, ignore.case = TRUE)
      if (length(match_idx) >= 1) return(available$countryCode[match_idx[1]])
      warning(
        "Country '", cntry, "' not found in Nager.Date API. ",
        "Use a supported country name or ISO code."
      )
      NA_character_
    }, character(1))

    combs <- expand.grid(year = years, country = countries, stringsAsFactors = FALSE)
    combs$code <- country_codes[combs$country]

    for (i in seq_len(nrow(combs))) {
      if (is.na(combs$code[i])) next
      if (!quiet) {
        message(
          ">>> Extracting ", combs$country[i],
          "'s holidays for ", combs$year[i]
        )
      }
      url <- sprintf(
        "https://date.nager.at/api/v3/PublicHolidays/%s/%s",
        combs$year[i], combs$code[i]
      )
      hol_data <- tryCatch(
        jsonlite::fromJSON(url),
        error = function(e) {
          warning(
            "Failed to fetch holidays for ", combs$country[i],
            " (", combs$year[i], "): ", e$message
          )
          NULL
        }
      )
      if (is.null(hol_data) || nrow(hol_data) == 0) next

      # Flatten the types list-column into a single string per row
      hol_types <- vapply(hol_data$types, function(x) {
        paste(x, collapse = ", ")
      }, character(1))

      result <- data.frame(
        holiday = as.Date(hol_data$date),
        holiday_name = hol_data$name,
        holiday_type = hol_types,
        stringsAsFactors = FALSE
      )

      if (include_regions) {
        result$holiday_details <- vapply(hol_data$counties, function(x) {
          if (is.null(x)) NA_character_ else paste(x, collapse = ", ")
        }, character(1))
      }

      result <- result %>%
        mutate(
          national = grepl("Public", .data$holiday_type) & hol_data$global,
          observance = grepl("Observance", .data$holiday_type),
          bank = grepl("Bank", .data$holiday_type),
          nonwork = grepl("Optional", .data$holiday_type),
          season = grepl("School|Authorities", .data$holiday_type),
          hother = !grepl("Public|Observance|Bank|Optional", .data$holiday_type)
        )

      if (length(unique(countries)) > 1L) {
        result$country <- combs$country[i]
      }
      result$county <- combs$country[i]
      results <- bind_rows(results, result)
    }

    if (is.null(results) || nrow(results) == 0) {
      return(as_tibble(data.frame(
        holiday = as.Date(character()),
        holiday_name = character(),
        holiday_type = character(),
        stringsAsFactors = FALSE
      )))
    }

    results %>%
      filter(!is.na(.data$holiday)) %>%
      cleanNames() %>%
      as_tibble()
  }
}
