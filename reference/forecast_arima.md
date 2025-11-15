# ARIMA Forecast

This function automates the ARIMA iterations and modeling for time
forecasting. For the moment, units can only be days.

## Usage

``` r
forecast_arima(
  time,
  values,
  n_future = 30,
  ARMA = 8,
  ARMA_min = 5,
  AR = NA,
  MA = NA,
  wd_excluded = NA,
  plot = TRUE,
  plot_days = 90,
  project = NA
)
```

## Arguments

- time:

  POSIX. Vector with date values

- values:

  Numeric. Vector with numerical values

- n_future:

  Integer. How many steps do you wish to forecast?

- ARMA:

  Integer. How many days should the model look back for ARMA? Between 5
  and 10 days recommmended. If set to 0 then it will forecast until the
  end of max date's month; if set to -1, until the end of max date's
  following month

- ARMA_min:

  Integer. How many days should the model look back for ARMA? Between 5
  and 10 days recommmended. If set to 0 then it will forecast until the
  end of max date's month; if set to -1, until the end of max date's
  following month

- AR:

  Integer. Force AR value if known

- MA:

  Integer. Force MA value if known

- wd_excluded:

  Character vector. Which weekdays are excluded in your training set. If
  there are, please define know which ones. Example:
  c('Sunday','Thursday'). If set to 'auto' then it will detect
  automatically which weekdays have no data and forcast without these
  days.

- plot:

  Boolean. If you wish to plot your results

- plot_days:

  Integer. How many days back you wish to plot?

- project:

  Character. Name of your forecast project

## Value

List. Containing the trained model, forecast accuracy results,
data.frame for forecast (test) and train, and if `plot=TRUE`, a plot.

## Details

The ARIMA method is appropriate only for a time series that is
stationary (i.e., its mean, variance, and autocorrelation should be
approximately constant through time) and it is recommended that there
are at least 50 observations in the input data.

The model consists of two parts, an autoregressive (AR) part and a
moving average (MA) part. The AR part involves regressing the variable
on its own lagged (i.e., past) values. The MA part involves modeling the
error term as a linear combination of error terms occurring
contemporaneously and at various times in the past.

One thing to keep in mind when we think about ARIMA models is given by
the great power to capture very complex patters of temporal correlation
(Cochrane, 1997: 25)

## See also

Other Forecast:
[`prophesize()`](https://laresbernardo.github.io/lares/reference/prophesize.md)
