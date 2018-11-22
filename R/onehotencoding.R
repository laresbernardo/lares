####################################################################
#' One Hot Encoding (Dummy Variables)
#'
#' This function lets the user automatically transform a dataframe with
#' categorical columns into numerical by one hot encoding technic.
#'
#' @param df Dataframe
#' @param redundant Boolean. Should we keep redundat columns? i.e. If the
#' column only has two different values, should we keep both new columns?
#' @param dates Boolean. Do you want the function to create more features
#' out of the date/time columns?
#' @param holidays Boolean. Include holidays as new columns?
#' @param country Character or vector. For which countries should the holidays
#' be included?
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
#' @export
ohe <- function(df, redundant = FALSE, 
                dates = FALSE, holidays = FALSE, country = "Colombia",
                trim = 0, limit = 10, variance = 0.9, 
                other_label = "OTHER", sep = "_", summary = TRUE) {
  
  # Create features out of date/time variables
  if (dates == TRUE) {
    df_dates <- date_feats(df, holidays = holidays, country = country, summary = summary)
    df <- cbind(df, df_dates)
  }
  
  # Dummy variables that will be filled
  no_need_to_convert <- converted <- converted_binary <- not_converted <- no_variance <- c()
  
  # Name and type of variables
  cols <- lares::df_str(df, "names", plot=F)
  types <- data.frame(name = colnames(df), 
                      type = unlist(lapply(lapply(df, class), `[[`, 1)))
  
  # Iterate all columns
  for (i in 1:ncol(df)) {
    vector_type <- types[i, "type"]
    vector_name <- as.character(types$name[i])
    vector_levels <- length(unique(df[,c(vector_name)]))
    vector_values <- df[toString(types[i, "name"])]
    
    # Columns with no variance or too much variance (unique values vs observations)
    if (vector_levels <= 1 | 
        vector_levels >= variance * length(vector_values[!is.na(vector_values),1])) {
      no_variance <- rbind(no_variance, vector_name)
    }
    
    # Non numeric or date/time variables
    if (!vector_type %in% c("integer","numeric","POSIXct","POSIXt","Date")) {
      vector_values <- vector_values %>% 
        mutate_all(as.character) %>%
        replace(., is.na(.), 'NAs')
      vector_values[,1] <- paste0(sep, vector_values[,1])
      
      # Columns with 2 possible values
      if (vector_levels == 2) {
        df[,c(vector_name)] <- as.integer(as.factor(df[,c(vector_name)]))-1
        converted_binary <- rbind(converted_binary, vector_name)
      }
      # Columns with more than 2 possible values
      if (!colnames(vector_values) %in% c(converted_binary, no_variance)) {
        if (vector_levels >= 3) {
          options(na.action = 'na.pass')
          vector_values <- categ_reducer(vector_values, !!as.name(vector_name), top = limit,
                                         other_label = paste0(sep, other_label))
          dummy_matx <- data.frame(model.matrix( ~ . -1, data = vector_values))
          if (redundant == FALSE) {
            dummy_matx <- dummy_matx[, 1:(ncol(dummy_matx)-1)]
          }
          df <- cbind(df, dummy_matx)
          converted <- rbind(converted, vector_name)
        }
      }
    }
    no_need_to_convert <- rbind(no_need_to_convert, vector_name)
  }
  
  # Shorten up the long names of some variables
  if (trim > 0) {
    colnames(df) <- substr(colnames(df), 1, trim)
  }
  
  # Summary
  if (summary == TRUE) {
    total_converted <- rbind(converted, converted_binary)
    message(paste("One Hot Encoding applied to", length(total_converted), 
                  "variables:", vector2text(total_converted)))
    message(paste0("Automatically dropped ", length(no_variance), 
                   " columns with 0% or +", round(variance*100),
                   "% variance: ", vector2text(no_variance)))
  }
  
  # Return only useful columns
  df <- df[, c(!colnames(df) %in% c(no_variance, converted))]
  return(df)
  
}


####################################################################
#' One Hot Encoding for Date/Time Variables (Dummy Variables)
#'
#' This function lets the user automatically create new columns out
#' of a dataframe or vector with date/time variables.
#'
#' @param dates Vector or dataframe. Non-date/time columns will be 
#' automatically ignored.
#' @param keep_originals Boolean. Should the original date/time columns be
#' kept in the results?
#' @param only Character or vector. Which columns do you wish to process? If
#' non are explicitly defined, all will be processed
#' @param holidays Boolean. Include holidays as new columns?
#' @param country Character or vector. For which countries should the holidays
#' be included?
#' @param summary Boolean. Print a summary of the operations?
#' @export
date_feats <- function(dates, keep_originals = FALSE, only = NA,
                       holidays = FALSE, country = "Colombia",
                       summary = TRUE) {
  
  results <- c()
  date_cols <- df_str(dates, return="names", plot=F)$time
  
  if (!is.na(only)) {
    date_cols <- date_cols[date_cols %in% only]
  }
  
  iters <- ifelse(date_cols == "df", 1, length(date_cols))[1]
  if (iters > 1) { 
    if (summary == TRUE) {
      message(paste("Processing", iters, "date/time columns:", vector2text(date_cols)))
    }
  } else {
    if (!class(dates) == "data.frame") {
      dates <- data.frame(df = dates)
    }
  }
  
  if (holidays == TRUE) {
    search_dates <- dates[, c(colnames(dates) %in% date_cols)]
    search_dates[] <- sapply(search_dates, function(x) gsub(" .*", "", as.character(x)))
    alldates <- as.Date(unlist(search_dates, use.names = FALSE))
    years <- sort(unique(year(alldates)))
    holidays_dates <- holidays(countries = country, years)
    colnames(holidays_dates)[1] <- "values_date"
  }
  
  for (col in 1:iters) {
    col_name <- date_cols[col]
    result <- dates %>% select(!!as.name(col_name)) 
    values <- as.POSIXlt(result[,1], origin = "1970-01-01")
    
    result$date_year <- year(values)
    result$date_month <- month(values)
    result$date_day <- day(values)
    result$date_week <- week(values)
    
    if (!is.na(ymd_hms(values[1]))) {
      values <- ymd_hms(values)
      result$date_hour <- hour(values)  
      result$date_minute <- minute(values)  
      result$date_minutes <- as.integer(difftime(
        values, floor_date(values, unit="day"), units="mins"))
      result$date_second <- second(values)  
      result$date_seconds <- as.integer(difftime(
        values, floor_date(values, unit="day"), units="secs"))
    }
    
    if (holidays == TRUE) {
      cols <- paste0("date_",c("national","observance","season","other",tolower(country)))
      if (ncol(holidays_dates) == 6) {
        holidays_dates <- holidays_dates %>% 
          mutate(dummy=TRUE) %>% tidyr::spread(country, dummy)
      } else {
        cols <- cols[1:4]
      }
      colnames(holidays_dates)[-1] <- cols
      result$values_date <- as.Date(values)
      result <- result %>% 
        left_join(holidays_dates, by="values_date") %>% 
        select(-contains("values_date")) %>%
        mutate_at(vars(cols), funs(replace(., which(is.na(.)), FALSE)))
    }
    
    colnames(result)[-1] <- gsub("date_", paste0(col_name,"_"), colnames(result)[-1])
    colnames(result) <- gsub("df_", "", colnames(result))
    results <- results %>% bind_cols(result)
    
  }
  
  if (keep_originals == FALSE) {
    results <- results[, c(!colnames(results) %in% date_cols)]
  }
  
  return(results)
  
}


####################################################################
#' Holidays in your Country
#'
#' This function lets the user automatically scrap holiday dates from
#' any country and year within +- 5 years. Thanks to timeanddate.com!
#'
#' @param years Character or vector. For which year(s) do you wish to import
#' holiday dates?
#' @param countries Character or vector. For which country(ies) should the 
#' holidays be imported?
#' @export
holidays <- function(countries = "Colombia", years = year(Sys.Date())) {
  
  # Further improvement: let the user use moran than +- 5 years
  
  results <- c()
  year <- year(Sys.Date())
  years <- years[years %in% ((year-5):(year+5))]
  combs <- expand.grid(years, countries) %>% dplyr::rename(year = "Var1", country = "Var2")
  for (i in 1:nrow(combs)) {
    message(paste0("Extracting ", combs$country[i], "'s holidays for ", combs$year[i]))
    url <- paste0("https://www.timeanddate.com/holidays/", tolower(combs$country[i]), "/", combs$year[i])
    holidays <- xml2::read_html(url)
    holidays <- holidays %>% html_nodes(".tb-hover") %>% html_table() %>% data.frame(.) %>% .[-1,1:4]
    holidays$Date <- paste(holidays$Date, combs$year[i])
    holidays$Date <- as.Date(holidays$Date, tryFormats = c("%d %b %Y", "%b %d %Y"))
    result <- data.frame(holiday = holidays$Date) %>%
      mutate(national = grepl("National|Federal", holidays$Holiday.Type),
             observance = grepl("Observance", holidays$Holiday.Type),
             season = grepl("Season", holidays$Holiday.Type),
             other = !grepl("National|Federal|Observance|Season", holidays$Holiday.Type)) %>%
      if (length(unique(countries)) > 1) { mutate(., country = combs$country[i]) } else .
    results <- rbind(results, result)
  } 
  return(results)
}
