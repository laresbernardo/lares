# Investing Portfolio Reporting Plots

`splot_summary()` plots a summary for the whole portfolio, showing how
much have you invested, how much has each ticker changed, etc.

`splot_change()` plots each stock's change through history, since
inception, with weighted attributions or absolute values.

`splot_growth()` plots your portfolio's growth, in cash and investment,
since inception.

`stocks_plots()` plots a portfolio's historical dividends incomes
grouped by quarter an year.

`splot_roi()` plots a portfolio's historical ROI since inception or
since last n days, with 2 moving average lines.

`splot_types()` lets the user plot types or categories of tickers.

`splot_etf()` lets the user plot his portfolio's distribution,
specifically ETF's sectors.

## Usage

``` r
splot_summary(p, s, save = FALSE)

splot_change(
  p,
  s,
  rel = TRUE,
  group = FALSE,
  n_days = 365,
  keep_old = FALSE,
  save = FALSE
)

splot_growth(p, save = FALSE)

splot_divs(p, type = 1)

splot_roi(p, n_days = 365, historical = TRUE, ma = c(12, 50), save = FALSE)

splot_types(s, save = FALSE)

splot_etf(s, keep_all = FALSE, cache = TRUE, save = FALSE)
```

## Arguments

- p:

  Dataframe. Result from
  [`daily_portfolio()`](https://laresbernardo.github.io/lares/reference/stocks_report.md)

- s:

  Dataframe. Result from
  [`daily_stocks()`](https://laresbernardo.github.io/lares/reference/stocks_report.md)

- save:

  Boolean. Save plot into a local file?

- rel:

  Boolean. Relative delta values (weighted with portfolio)? If not,
  absolute monetary delta values.

- group:

  Boolean. Group stocks by stocks type?

- n_days:

  Integer. How many days back you want to see?

- keep_old:

  Boolean. Include sold tickers even though not currently in portfolio?

- type:

  Integer. Typo of plot. 1 for incomes.

- historical:

  Boolean. Historical ROI metric? If not, ROI will be calculated locally
  for n_days parameter

- ma:

  Numeric Vector. Select 2 values for moving averages. Set to NA to turn
  this metric off

- keep_all:

  Boolean. Keep "Not Known / Not ETF"?

- cache:

  Boolean. Use daily cache if available?

## Value

ggplot object

## See also

Other Investment:
[`etf_sector()`](https://laresbernardo.github.io/lares/reference/etf_sector.md),
[`stocks_file()`](https://laresbernardo.github.io/lares/reference/stocks_report.md),
[`stocks_quote()`](https://laresbernardo.github.io/lares/reference/stocks_hist.md)

Other Scrapper:
[`filesGD()`](https://laresbernardo.github.io/lares/reference/filesGD.md),
[`gtrends_related()`](https://laresbernardo.github.io/lares/reference/google_trends.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`ip_data()`](https://laresbernardo.github.io/lares/reference/ip_data.md),
[`mp3_get()`](https://laresbernardo.github.io/lares/reference/mp3_get.md),
[`readGS()`](https://laresbernardo.github.io/lares/reference/google_sheets.md),
[`stocks_quote()`](https://laresbernardo.github.io/lares/reference/stocks_hist.md)
