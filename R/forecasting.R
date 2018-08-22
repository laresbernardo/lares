####################################################################
#' Simple Forecast
#' 
#' This function lets the user create a forecast setting a time series
#' and a numerical value.
#' 
#' @param time POSIX. Vector with dates or time values
#' @param values Numeric. Vector with numerical values
#' @param n_future Integer. How many steps do you wish to forecast?
#' @param use_last Boolena. Use last observation?
#' @param plot Boolean. If you wish to plot your results
#' @param automl Boolean. Use lares::h2o_automl()
#' @param project Character. Name of your forecast project for plot title
#' @export
time_forecast <- function(time, values, n_future = 15, use_last = TRUE, plot = TRUE, automl = FALSE, 
                          project = "Simple Forecast using Machine Learning") {
  require(timetk)
  require(tidyquant)
  
  if (length(time) != length(values)) {
    stop("The parameters 'time' and 'values' should have the same length")
  }
  
  df <- data.frame(time = time, amount = values)
  if (use_last == FALSE) {
    df <- arrange(df, desc(time)) %>% slice(-1)
    n_future <- n_future + 1
  }
  
  # STEP 1: AUGMENT TIME SERIES SIGNATURE
  augmented <- df %>% tk_augment_timeseries_signature()
  augmented <- mutate(augmented, 
                      month.lbl = as.character(month.lbl),
                      wday.lbl = as.character(wday.lbl))
  
  # STEP 2: BUILD FUTURE (NEW) DATA
  idx <- augmented %>% tk_index()
  future_idx <- idx %>% tk_make_future_timeseries(n_future = n_future)
  new_data_tbl <- future_idx %>% tk_get_timeseries_signature() %>%
    mutate(month.lbl = as.character(month.lbl),
           wday.lbl = as.character(wday.lbl))
  
  # STEP 3: MODEL
  if (automl == FALSE) {
    fit_lm <- lm(amount ~ ., data = select(augmented, -c(time)))
    pred <- predict(fit_lm, newdata = select(new_data_tbl, -c(index)))
    predictions_tbl <- tibble(time = future_idx, amount = pred) 
  } else {
    augmented_h2o <- augmented %>% dplyr::rename(tag = amount)
    fit_auto <- lares::h2o_automl(df = augmented_h2o, alarm = FALSE, project = project)
    pred <- h2o.predict(fit_auto$model, as.h2o(new_data_tbl))
    predictions_tbl <- tibble(time = future_idx, amount = as.vector(pred))
  }
  
  # STEP 5: COMPARE ACTUAL VS PREDICTIONS
  rects <- data.frame(start = min(future_idx), end = max(future_idx))
  message("Predicted range: ", rects$start, " to ", rects$end)
  forecast <- df %>%
    ggplot(aes(x = time, y = amount)) + 
    labs(title = project, y = "Amount", x = "",
         subtitle = "Using simple multivariate regressions on time series with Machine Learning") +
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
  
  if (automl == TRUE) {
    results <- fit_auto
    score <- fit_auto$model$scores$score
  } else {
    results <- fit_lm
    score <- fit_lmt$model$fitted.values
  }
  
  df_final <- rbind(df, predictions_tbl)
  tag <- df$amount
  errors <- lares::errors(tag, score)
  
  output <- list(data = df_final, model = results, errors = errors)
  
  return(output)
  
}
