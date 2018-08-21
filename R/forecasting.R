####################################################################
#' Simple Forecast
#' 
#' This function lets the user create a forecast setting a time series
#' and a numerical value.
#' 
#' @param time POSIX. Vector with dates or time values
#' @param values Numeric. Vector with numerical values
#' @param n_future Integer. How many steps do you wish to forecast?
#' @param plot Boolean. If you wish to plot your results
#' @param project Character. Name of your forecast project for plot title
#' @export
time_forecast <- function(time, values, n_future = 15, plot = TRUE, 
                          project = "Simple Forecast using Machine Learning") {
  library(timetk)
  library(tidyquant)
  
  if (length(time) != length(values)) {
    stop("The parameters 'time' and 'values' should have the same length")
  }
  
  df <- data.frame(date = time, amount = values)
  
  # STEP 1: AUGMENT TIME SERIES SIGNATURE
  augmented <- df %>% tk_augment_timeseries_signature()
  
  # STEP 2: MODEL - Linear regression model used, but can use any model
  fit_lm <- lm(amount ~ ., data = select(augmented, -c(date, diff)))
  #summary(fit_lm)
  
  # STEP 3: BUILD FUTURE (NEW) DATA
  idx <- augmented %>% tk_index()
  future_idx <- idx %>% tk_make_future_timeseries(n_future = n_future)
  new_data_tbl <- future_idx %>% tk_get_timeseries_signature()
  
  # STEP 4: PREDICT THE NEW DATA
  # Make predictions
  pred <- predict(fit_lm, newdata = select(new_data_tbl, -c(index, diff)))
  predictions_tbl <- tibble(date  = future_idx, amount = pred)
  
  # STEP 5: COMPARE ACTUAL VS PREDICTIONS
  rects <- data.frame(start = min(future_idx), end = max(future_idx))
  message("Predicted range: ", rects$start, " to ", rects$end)
  forecast <- df %>%
    ggplot(aes(x = date, y = amount)) + 
    labs(title = project, y = "Amount",
         subtitle = "Using basic multivariate linear regression") +
    # Training data
    geom_line(color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]]) +
    geom_smooth(method = 'loess', formula = 'y ~ x', alpha = 0.5) +
    # Predictions
    geom_line(aes(y = amount), color = palette_light()[[2]], data = predictions_tbl) +
    geom_point(aes(y = amount), color = palette_light()[[2]], data = predictions_tbl) +
    # Actuals
    geom_line(color = palette_light()[[1]], data = df) +
    geom_point(color = palette_light()[[1]], data = df) +
    # Aesthetics
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    theme_tq() + 
    geom_rect(data = rects, inherit.aes = FALSE, 
              aes(
                xmin = start, xmax = end, ymin = 0,
                ymax = max(df$amount) * 1.02), 
              color = "transparent", fill = "orange", alpha = 0.3)
  
  if (plot == TRUE) {
    print(forecast)
  }
  
  output <- rbind(df, predictions_tbl)
  return(output)
  
}
