####################################################################
#' One Hot Smart Encoding (Dummy Variables)
#'
#' This function lets the user automatically transform a dataframe with
#' categorical columns into numerical by one hot encoding technic.
#'
#' @family Data Wrangling
#' @family Feature Engineering
#' @param df Dataframe
#' @param redundant Boolean. Should we keep redundat columns? i.e. If the
#' column only has two different values, should we keep both new columns?
#' @param drops Boolean. Drop automatically some useless features?
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
#' values of each column
#' @param variance Numeric. Drop columns with more than n variance. 
#' Range: 0-1. For example: if a variable contains 91 unique different
#' values out of 100 observations, this column will be suppressed if 
#' value is set to 0.9
#' @param other_label Character. With which text do you wish to replace 
#' the filtered values with?
#' @param sep Character. Separator's string
#' @param summary Boolean. Print a summary of the operations?
#' @examples 
#' # Example with Dataset 1
#' data(dft)
#' ohse(dft, limit = 3) %>% head(3)
#' ohse(dft, limit = 3, redundant = TRUE) %>% head(3)
#' # Getting rid of columns with no (or too much) variance
#' dft$no_variance1 <- 0
#' dft$no_variance2 <- c("A", rep("B", nrow(dft) - 1))
#' dft$no_variance3 <- as.character(rnorm(nrow(dft)))
#' dft$no_variance4 <- c(rep("A", 20), rnorm(nrow(dft) - 20))
#' ohse(dft, limit = 3) %>% head(3)
#' ohse(dft, limit = 3, var = 1) %>% head(3)
#' 
#' # Examples with Dataset 2
#' data(dfl)
#' dfl$novar <- "novar"
#' ohse(dfl, dates = TRUE) %>% head(3)
#' ohse(dfl, dates = TRUE, ignore = "issued") %>% head(3)
#' \dontrun{
#' ohse(dfl, dates = FALSE, holidays = TRUE, country = "Venezuela") %>% head()
#' }
#' @export
ohse <- function(df, 
                 redundant = FALSE, 
                 drops = TRUE,
                 ignore = NA,
                 dates = FALSE, 
                 holidays = FALSE, country = "Colombia",
                 currency_pair = NA, 
                 trim = 0, 
                 limit = 10, 
                 variance = 0.9, 
                 other_label = "OTHER", 
                 sep = "_", 
                 summary = TRUE) {
  
  df <- data.frame(df)
  order <- colnames(df)
  
  # Dummy variables that will be filled
  no_need_to_convert <- converted <- converted_binary <- c()
  
  # No variance columns
  no_variance <- zerovar(df)
  if (drops)
    df <- df[,!colnames(df) %in% no_variance]

  # Create features out of date/time variables
  if (dates == TRUE | holidays == TRUE | !is.na(currency_pair)) {
    times <- df_str(df, return = "names", quiet = TRUE)$time
    if (length(times) <= 1) {
      df_dates <- date_feats(df, 
                             keep_originals = TRUE,
                             features = dates,
                             holidays = holidays, 
                             country = country, 
                             currency_pair = currency_pair, 
                             summary = summary)
      
      if (ncol(df_dates) != ncol(df)) {
        df <- df %>% left_join(df_dates, by = as.character(times[1])) %>% distinct()
      } 
    } else {
      message("Can't join more than one date feature yet!")
    }
  }
  
  # Leave some columns out of the logic
  if (!is.na(ignore)[1]) {
    message(">>> Omitting transformations for ", vector2text(ignore))
    ignored <- df %>% select(one_of(ignore))
    df <- df %>% select(-one_of(ignore))
  }
  
  # Name and type of variables
  types <- data.frame(name = colnames(df), 
                      type = unlist(lapply(lapply(df, class), `[[`, 1)))
  
  # Iterate all columns
  for (i in 1:ncol(df)) {
    vector_type <- types[i, "type"]
    vector_name <- as.character(types$name[i])
    vector_levels <- length(unique(df[,c(vector_name)]))
    vector_values <- df[toString(types[i, "name"])]
    
    # Non numeric or date/time variables
    if (!vector_type %in% c("integer","numeric","POSIXct","POSIXt","Date")) {
      
      # Char columns with too much variance (unique values vs total observations)
      if (vector_levels >= variance * nrow(df)) {
        no_variance <- c(no_variance, vector_name)
      }
      
      vector_values <- vector_values %>% 
        mutate_all(as.character) %>%
        replace(., is.na(.), 'NAs')
      vector_values[,1] <- paste0(sep, vector_values[,1])
      
      # Columns with 2 possible values
      if (vector_levels == 2 & redundant == FALSE) {
        which <- as.character(levels(as.factor(df[,c(vector_name)]))[2])
        df[,c(vector_name)] <- as.integer(as.factor(df[,c(vector_name)])) - 1
        converted_binary <- rbind(converted_binary, vector_name)
        df <- rename_at(df, vars(vector_name), funs(paste0(vector_name, "_", which)))
      }
      
      # ONE HOT ENCODING
      if (!colnames(vector_values) %in% c(converted_binary, no_variance)) {
        if (vector_levels >= 2 & !vector_name %in% converted_binary) {
          options(na.action = 'na.pass')
          vector_values <- categ_reducer(vector_values, !!as.name(vector_name), top = limit,
                                         other_label = paste0(sep, other_label))
          dummy_matx <- data.frame(model.matrix( ~ . -1, data = vector_values))
          if (!redundant) dummy_matx <- dummy_matx[, 1:(ncol(dummy_matx) - 1)]
          df <- cbind(df, dummy_matx)
          converted <- rbind(converted, vector_name)
        }
      }
    }
    no_need_to_convert <- rbind(no_need_to_convert, vector_name)
  }
  
  # Shorten up the long names of some variables
  if (trim > 0) colnames(df) <- substr(colnames(df), 1, trim)
  
  # Summary
  if (summary) {
    total_converted <- rbind(converted, converted_binary)
    if (length(total_converted) > 1) {
      message(paste(">>> One Hot Encoding applied to", length(total_converted), 
                    "variables:", vector2text(total_converted))) 
    }
    if (length(no_variance) > 0 & drops) {
      message(paste0(">>> Automatically dropped ", length(no_variance), 
                     " columns with 0% or >=", round(variance*100),
                     "% variance: ", vector2text(no_variance))) 
    }
  }
  
  # Return only useful columns
  if (drops) 
    df <- df[, c(!colnames(df) %in% c(converted, no_variance))] 
  
  # Bind ignored untouched columns
  order <- order[order %in% colnames(df)]
  if (!is.na(ignore)[1]) 
    df <- df %>% select(one_of(order), everything())
  
  return(df)
  
}


####################################################################
#' One Hot Encoding for Date/Time Variables (Dummy Variables)
#'
#' This function lets the user automatically create new columns out
#' of a dataframe or vector with date/time variables.
#'
#' @family Data Wrangling
#' @family Feature Engineering
#' @param dates Vector or dataframe. Non-date/time columns will be 
#' automatically ignored.
#' @param keep_originals Boolean. Should the original date/time columns be
#' kept in the results?
#' @param only Character or vector. Which columns do you wish to process? If
#' non are explicitly defined, all will be processed
#' @param features Create features out of date/time columns?
#' @param holidays Boolean. Include holidays as new columns?
#' @param country Character or vector. For which countries should the holidays
#' be included?
#' @param currency_pair Character. Which currency exchange do you
#' wish to get the history from? i.e, USD/COP, EUR/USD...
#' @param summary Boolean. Print a summary of the operations?
#' @export
date_feats <- function(dates, 
                       keep_originals = FALSE, only = NA,
                       features = TRUE,
                       holidays = FALSE, country = "Venezuela",
                       currency_pair = NA,
                       summary = TRUE) {
  results <- c()
  date_cols <- df_str(dates, return = "names", quiet = TRUE)$time
  
  if (length(date_cols) == 0)
    return(dates)
  
  if (!is.na(only))
    date_cols <- date_cols[date_cols %in% only]
  
  iters <- ifelse(date_cols == "df", 1, length(date_cols))[1]
  if (!is.na(iters)) {
    if (summary)
      message(paste(">>> Processing", iters, "date/time columns:", vector2text(date_cols))) 
  } else return(dates)
  
  if (!"data.frame" %in% class(dates) & iters == 1) {
    dates <- data.frame(values_date = dates)
    date_cols <- "values_date"
  }
  
  if (holidays | !is.na(currency_pair)) {
    invisible(Sys.setlocale("LC_TIME", "C"))
    search_dates <- dates[, c(colnames(dates) %in% date_cols)]
    search_dates[] <- sapply(search_dates, function(x) gsub(" .*", "", as.character(x)))
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
      values <- as.POSIXlt(result[,1], origin = "1970-01-01")
      result$values_date <- as.character(as.Date(values))
      
      # Features creator
      if (features == TRUE) {
        invisible(Sys.setlocale("LC_TIME", "C"))
        result$values_date_year <- year(values)
        result$values_date_month <- month(values)
        result$values_date_day <- day(values)
        result$values_date_week <- week(values)
        result$values_date_weekday <- weekdays(values, abbreviate = TRUE)
        result$values_date_weekend <-  grepl("S(at|un)", result$values_date_weekday)
        result$values_date_year_day <- as.integer(difftime(
          values, floor_date(values, unit = "year"), units = "day"))
        
        if (!is.na(ymd_hms(values[1]))) {
          values <- ymd_hms(values)
          result$values_date_hour <- hour(values)  
          result$values_date_minute <- minute(values)  
          result$values_date_minutes <- as.integer(difftime(
            values, floor_date(values, unit = "day"), units = "mins"))
          result$values_date_second <- second(values)
          # result$values_date_seconds <- as.integer(difftime(
          #   values, floor_date(values, unit="day"), units="secs"))
        }
      }

      # Holidays data
      if (holidays) {
        result <- result %>% 
          left_join(holidays_dates, by = "values_date") %>% 
          mutate_at(vars(cols), funs(replace(., which(is.na(.)), FALSE)))
      }
      
      # Currencies data
      if (!is.na(currency_pair)) {
        currency <- get_currency(currency_pair, from = min(alldates), to = max(alldates))
        colnames(currency) <- c("values_date", paste0("values_date_", tolower(cleanText(currency_pair))))
        currency[,1] <- as.character(currency[,1])
        result <- result %>% left_join(currency, by = "values_date")
      }
      
      col_name <- ifelse(date_cols == "values_date", "", paste0(col_name,"_"))
      colnames(result)[-1] <- gsub("values_date_", col_name, colnames(result)[-1])
      results <- results %>% 
        bind_cols(result) %>%
        select(-contains("values_date"))
    }
  }
  
  if (!keep_originals) results <- results[, c(!colnames(results) %in% date_cols)]
  
  return(results)
  
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
#' @param years Character or vector. For which year(s) do you wish to import
#' holiday dates?
#' @param countries Character or vector. For which country(ies) should the 
#' holidays be imported?
#' @examples 
#' \dontrun{
#' holidays(countries = "Argentina") 
#' holidays(countries = c("Argentina", "Venezuela"), years = c(2019, 2020))
#' }
#' @export
holidays <- function(countries = "Colombia", years = year(Sys.Date())) {
  
  # Further improvement: let the user bring more than +-5 years
  
  invisible(Sys.setlocale("LC_TIME", "C"))
  results <- c()
  year <- year(Sys.Date())
  years <- years[years %in% ((year-5):(year+5))]
  combs <- expand.grid(years, countries) %>% dplyr::rename(year = "Var1", country = "Var2")
  for (i in 1:nrow(combs)) {
    message(paste0(">>> Extracting ", combs$country[i], "'s holidays for ", combs$year[i]))
    url <- paste0("https://www.timeanddate.com/holidays/", tolower(combs$country[i]), "/", combs$year[i])
    holidays <- xml2::read_html(url)
    holidays <- holidays %>% html_nodes(".table") %>% html_table(fill = TRUE) %>% data.frame(.) %>% filter(!is.na(Date))
    holidays <- holidays[,-2]
    colnames(holidays) <- c("Date", "Holiday", "Holiday.Type")
    holidays$Date <- paste(holidays$Date, combs$year[i])
    if (sum(grepl("de",holidays$Date)) > 0) {
      invisible(Sys.setlocale("LC_TIME", "es_ES"))
      holidays$Date <- gsub("de ","", holidays$Date)
    }
    first <- as.numeric(as.character(substr(holidays$Date,1,1)))
    if (!is.na(first[1])) {
      holidays$Date <- as.Date(holidays$Date, format = c("%d %b %Y"))
    } else {
      holidays$Date <- as.Date(holidays$Date, format = c("%b %d %Y"))
    }
    result <- data.frame(holiday = holidays$Date,
                         holiday_name = holidays$Holiday, 
                         holiday_type = holidays$Holiday.Type) %>%
      mutate(national = grepl("National|Federal", holidays$Holiday.Type),
             observance = grepl("Observance", holidays$Holiday.Type),
             bank = grepl("Bank", holidays$Holiday.Type),
             nonwork = grepl("Non-working", holidays$Holiday.Type),
             season = grepl("Season", holidays$Holiday.Type),
             hother = !grepl("National|Federal|Observance|Season", holidays$Holiday.Type)) %>%
      if (length(unique(countries)) > 1) { mutate(., country = combs$country[i]) } else .
    results <- rbind(results, result)
  } 
  results <- results %>% filter(!is.na(holiday)) %>% as_tibble()
  return(results)
}
