####################################################################
#' ARIMA Forecast
#'
#' This function automates the ARIMA iterations and modeling for
#' time forecasting. For the moment, units can only be days.
#'
#' The ARIMA method is appropriate only for a time series that is
#' stationary (i.e., its mean, variance, and autocorrelation should
#' be approximately constant through time) and it is recommended
#' that there are at least 50 observations in the input data.
#'
#'  The model consists of two parts, an autoregressive (AR) part
#'  and a moving average (MA) part. The AR part involves regressing
#'  the variable on its own lagged (i.e., past) values. The MA part
#'  involves modeling the error term as a linear combination of error
#'  terms occurring contemporaneously and at various times in the past.
#'
#' One thing to keep in mind when we think about ARIMA models is
#' given by the great power to capture very complex patters of
#' temporal correlation (Cochrane, 1997: 25)
#'
#' @family Forecast
#' @param time POSIX. Vector with date values
#' @param values Numeric. Vector with numerical values
#' @param n_future Integer. How many steps do you wish to forecast?
#' @param ARMA Integer. How many days should the model look back for ARMA?
#' Between 5 and 10 days recommmended. If set to 0 then it will forecast
#' until the end of max date's month; if set to -1, until the end of
#' max date's following month
#' @param ARMA_min Integer. How many days should the model look back for ARMA?
#' Between 5 and 10 days recommmended. If set to 0 then it will forecast
#' until the end of max date's month; if set to -1, until the end of
#' max date's following month
#' @param AR Integer. Force AR value if known
#' @param MA Integer. Force MA value if known
#' @param wd_excluded Character vector. Which weekdays are excluded in
#' your training set. If there are, please define know which ones. Example:
#' c('Sunday','Thursday'). If set to 'auto' then it will detect automatically
#' which weekdays have no data and forcast without these days.
#' @param plot Boolean. If you wish to plot your results
#' @param plot_days Integer. How many days back you wish to plot?
#' @param project Character. Name of your forecast project
#' @return List. Containing the trained model, forecast accuracy results,
#' data.frame for forecast (test) and train, and if \code{plot=TRUE}, a plot.
#' @export
forecast_arima <- function(time, values, n_future = 30,
                           ARMA = 8, ARMA_min = 5,
                           AR = NA, MA = NA,
                           wd_excluded = NA,
                           plot = TRUE, plot_days = 90, project = NA) {
  try_require("forecast")

  # ARIMA doesn't use zeroes!
  time <- time[!values == 0]
  values <- values[!values == 0]

  if (length(time) < 50) {
    message("It is recommended that there are at least 50 observations in the input data")
  }

  if (Sys.Date() %in% time) {
    message("It is recommended that you do NOT use today's data for training your data")
  }

  if (n_future == -1) {
    n_future <- ceiling_date(Sys.Date(), "month") + months(1) - Sys.Date()
  }
  if (n_future == 0) {
    n_future <- ceiling_date(Sys.Date(), "month") - Sys.Date()
  }

  # Which AR and MA values minimize our AIC
  if (is.na(AR) && is.na(MA)) {
    arma <- c(ARMA_min:ARMA)
    aic <- expand.grid(AR = arma, MA = arma, cals = 0)
    message("Iterating for best AR / MA combinations; there are ", nrow(aic), "!")
    # if (length(time) > 1000) { method <- "ML" } else { method <- "CSS" }
    for (i in seq_len(nrow(aic))) {
      Tmodel <- Arima(values, order = c(aic$AR[i], 1, aic$MA[i]), method = "ML")
      aic$cals[i] <- Tmodel$aic
    }
    AR <- aic$AR[which.min(aic$cals)]
    MA <- aic$MA[which.min(aic$cals)]
    message("Best combination:", AR, "and", MA)
    aic_ARIMA <- min(aic$cals)
  }

  model <- Arima(values, order = c(AR, 1, MA), method = "ML")
  train <- data.frame(time, values,
    pred = model$fitted,
    resid = model$residuals
  )

  # Forecast
  future_dates <- seq.Date(max(time) + 1, max(time) %m+% days(n_future), by = 1)
  if (!is.na(wd_excluded)) {
    if (wd_excluded == "auto") {
      weekdays <- data.frame(table(weekdays(time)))
      weekdays_real <- c(weekdays(seq.Date(Sys.Date(), Sys.Date() + 6, by = 1)))
      wd_excluded <- weekdays_real[!weekdays_real %in% weekdays$Var1]
      message("Automatically excluding ", vector2text(wd_excluded))
    }
    exclude <- vector2text(wd_excluded, quotes = FALSE)
    future_dates <- future_dates[!weekdays(future_dates) %in% wd_excluded]
    n_future <- length(future_dates)
  }
  f <- forecast(model, h = n_future)
  test <- data.frame(time = future_dates, pred = f$mean, data.frame(f)[, -1])

  # Outut list with all results
  output <- list(
    model = model,
    metrics = accuracy(model),
    forecast = test,
    train = train
  )

  # Plot results
  if (plot == TRUE) {
    if (nrow(train) > plot_days) {
      train <- train[(nrow(train) - plot_days):nrow(train), ]
    }

    plotdata <- data.frame(
      rbind(
        data.frame(date = train$time, values = train$values, type = "Real"),
        data.frame(date = train$time, values = train$pred, type = "Model"),
        data.frame(date = test$time, values = test$pred, type = "Forecast")
      )
    )
    rects <- data.frame(start = min(future_dates), end = max(future_dates))

    output$plot <- ggplot(plotdata, aes(.data$date)) +
      geom_smooth(aes(y = .data$values), method = "loess", alpha = 0.5) +
      geom_line(aes(y = .data$values, colour = .data$type)) +
      labs(x = "Date", y = "Counter", colour = "") +
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 60, hjust = 1)
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
      ggtitle("Real & Fitted Model vs Forecast (ARIMA)",
        subtitle = paste(
          "AIC", signif(output$model$aic, 4), "|",
          "MAE", signif(output$metrics[3], 3), "|",
          "RMSE", signif(output$metrics[2], 3), "|",
          "ARIMA:", AR, "- 1 -", MA
        )
      ) +
      scale_color_manual(values = c("orange", "navy", "purple")) +
      geom_rect(
        data = rects, inherit.aes = FALSE,
        aes(
          xmin = .data$start, xmax = .data$end,
          ymin = min(plotdata$values),
          ymax = max(plotdata$values)
        ),
        color = "transparent", fill = "grey", alpha = 0.25
      )
    if (!is.na(project)) {
      output$plot <- output$plot + labs(caption = project)
    }
    plot(output$plot)
  }
  output
}


####################################################################
#' Facebook's Prophet Forecast
#'
#' Prophet is Facebook's procedure for forecasting time series data
#' based on an additive model where non-linear trends are fit with
#' yearly, weekly, and daily seasonality, plus holiday effects. It
#' works best with time series that have strong seasonal effects and
#' several seasons of historical data. Prophet is robust to missing
#' data and shifts in the trend, and typically handles outliers well.
#'
#' Official documentation: \url{https://github.com/facebook/prophet}
#'
#' @family Forecast
#' @param df Data frame. Must contain date/time column and values column,
#' in that order.
#' @param n_future Integer. How many steps do you wish to forecast?
#' @param country Character. Country code for holidays.
#' @param trend.param Numeric. Flexibility of trend component. Default is 0.05,
#' and as this value becomes larger, the trend component will be more flexible.
#' @param logged Boolean. Convert values into logs?
#' @param pout Numeric. Get rid of pout \% of outliers.
#' @param project Character. Name of your forecast project for plot title
#' @return List. Containing the forecast results, the prophet model, and a plot.
#' @export
prophesize <- function(df, n_future = 60, country = NULL,
                       trend.param = 0.05, logged = FALSE, pout = 0.03,
                       project = "Prophet Forecast") {
  try_require("prophet")

  df <- data.frame(df[, c(1, 2)])
  metric <- colnames(df)[2]
  colnames(df) <- c("ds", "y")
  df <- arrange(df, .data$ds)
  if (logged) df$y <- log(df$y)

  # Outliers
  df <- df[!rank(-df$y) %in% 1:round(nrow(df) * pout), ]

  # Run prophet functions
  m <- prophet(
    yearly.seasonality = TRUE, daily.seasonality = FALSE,
    changepoint.prior.scale = trend.param
  )
  if (!is.null(country)) {
    m <- add_country_holidays(m, country_name = country)
  }
  m <- fit.prophet(m, df)
  future <- make_future_dataframe(m, periods = n_future)

  forecast <- predict(m, future)
  forecast$y <- forecast$trend + forecast$additive_terms

  p <- plot(m, forecast) + theme_lares() +
    labs(
      y = metric, x = "Dates",
      title = project,
      subtitle = paste("Forecast results for the next", n_future, "days")
    ) +
    scale_y_comma()

  plots2 <- prophet_plot_components(m, forecast, render_plot = FALSE, weekly_start = 1)
  plots2 <- lapply(plots2, function(x) x + theme_lares())
  plot2 <- wrap_plots(plots2, ncol = 1) +
    plot_annotation(title = "Forecast components", theme = theme_lares())

  list(result = forecast, model = m, plot = p, components = plot2)
}
