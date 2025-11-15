# Download Stocks Historical and Current Values

`stocks_quote()` lets the user download stocks live data.

`stocks_hist()` lets the user download stocks historical data.

## Usage

``` r
stocks_quote(symbols, ...)

stocks_hist(
  symbols = c("VTI", "META"),
  from = Sys.Date() - 365,
  to = Sys.Date(),
  today = TRUE,
  tax = 15,
  parg = FALSE,
  cache = TRUE,
  quiet = FALSE,
  ...
)

# S3 method for class 'stocks_hist'
plot(x, type = 1, ...)
```

## Arguments

- symbols:

  Character Vector. List of symbols to download historical data.

- ...:

  Additional parameters.

- from, to:

  Date. Dates for range. If not set, 1 year will be downloaded. Do use
  more than 4 days or will be over-written.

- today:

  Boolean. Do you wish to add today's live quote? This will happen only
  if to value is the same as today's date

- tax:

  Numeric. How much \[0-99\] of your dividends are gone with taxes?

- parg:

  Boolean. Personal argument. Used to personalize stuff, in this case,
  taxes changed from A to B in given date (hard-coded)

- cache:

  Boolean. Use daily cache if available?

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- x:

  stocks_hist object

- type:

  Integer. Select type of plot.

## Value

data.frame with Symbol, Type of stock, Quote time, current value, Daily
Change, Market, and Symbol Name.

## See also

Other Investment:
[`etf_sector()`](https://laresbernardo.github.io/lares/reference/etf_sector.md),
[`splot_summary()`](https://laresbernardo.github.io/lares/reference/stocks_plots.md),
[`stocks_file()`](https://laresbernardo.github.io/lares/reference/stocks_report.md)

Other Scrapper:
[`filesGD()`](https://laresbernardo.github.io/lares/reference/filesGD.md),
[`gtrends_related()`](https://laresbernardo.github.io/lares/reference/google_trends.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`ip_data()`](https://laresbernardo.github.io/lares/reference/ip_data.md),
[`mp3_get()`](https://laresbernardo.github.io/lares/reference/mp3_get.md),
[`readGS()`](https://laresbernardo.github.io/lares/reference/google_sheets.md),
[`splot_summary()`](https://laresbernardo.github.io/lares/reference/stocks_plots.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Multiple quotes at the same time
stocks_quote(c("VTI", "VOO", "TSLA"))
} # }
if (FALSE) { # \dontrun{
df <- stocks_hist(symbols = c("VTI", "META", "FIW"), from = Sys.Date() - 180)
print(head(df))
plot(df)
} # }
```
