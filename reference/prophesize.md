# Facebook's Prophet Forecast

Prophet is Facebook's procedure for forecasting time series data based
on an additive model where non-linear trends are fit with yearly,
weekly, and daily seasonality, plus holiday effects. It works best with
time series that have strong seasonal effects and several seasons of
historical data. Prophet is robust to missing data and shifts in the
trend, and typically handles outliers well.

## Usage

``` r
prophesize(
  df,
  n_future = 60,
  country = NULL,
  trend.param = 0.05,
  logged = FALSE,
  pout = 0.03,
  project = "Prophet Forecast"
)
```

## Arguments

- df:

  Data frame. Must contain date/time column and values column, in that
  order.

- n_future:

  Integer. How many steps do you wish to forecast?

- country:

  Character. Country code for holidays.

- trend.param:

  Numeric. Flexibility of trend component. Default is 0.05, and as this
  value becomes larger, the trend component will be more flexible.

- logged:

  Boolean. Convert values into logs?

- pout:

  Numeric. Get rid of pout % of outliers.

- project:

  Character. Name of your forecast project for plot title

## Value

List. Containing the forecast results, the prophet model, and a plot.

## Details

Official documentation: <https://github.com/facebook/prophet>

## See also

Other Forecast:
[`forecast_arima()`](https://laresbernardo.github.io/lares/reference/forecast_arima.md)
