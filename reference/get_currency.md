# Download Historical Currency Exchange Rate

This function lets the user download historical currency exchange rate
between two currencies.

## Usage

``` r
get_currency(
  currency_pair,
  from = Sys.Date() - 99,
  to = Sys.Date(),
  fill = FALSE,
  ...
)
```

## Arguments

- currency_pair:

  Character. Which currency exchange do you wish to get the history
  from? i.e, USD/COP, EUR/USD...

- from:

  Date. From date

- to:

  Date. To date

- fill:

  Boolean. Fill weekends and non-quoted dates with previous values?

- ...:

  Additional parameters.

## Value

data.frame. Result of fetching online data for `currency_pair` grouped
by date.

## Examples

``` r
# \donttest{
# For today (or any one single date)
get_currency("USD/COP", from = Sys.Date())
#>         date    rate
#> 1 2025-11-26 3811.94
# For multiple dates
get_currency("EUR/USD", from = Sys.Date() - 7, fill = TRUE)
#>         date     rate
#> 1 2025-11-19 1.158104
#> 2 2025-11-20 1.154068
#> 3 2025-11-21 1.153536
#> 4 2025-11-22 1.153536
#> 5 2025-11-23 1.153536
#> 6 2025-11-24 1.150788
#> 7 2025-11-25 1.152074
#> 8 2025-11-26 1.157274
# }
```
