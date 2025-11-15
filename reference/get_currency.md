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
#>         date   rate
#> 1 2025-11-15 3755.5
# For multiple dates
get_currency("EUR/USD", from = Sys.Date() - 7, fill = TRUE)
#>         date     rate
#> 1 2025-11-08 1.155455
#> 2 2025-11-09 1.155455
#> 3 2025-11-10 1.155455
#> 4 2025-11-11 1.155735
#> 5 2025-11-12 1.158883
#> 6 2025-11-13 1.159245
#> 7 2025-11-14 1.163075
#> 8 2025-11-15 1.163075
# }
```
