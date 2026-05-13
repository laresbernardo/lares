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
#> 1 2026-05-13 3767.51
# For multiple dates
get_currency("EUR/USD", from = Sys.Date() - 7, fill = TRUE)
#>         date     rate
#> 1 2026-05-06 1.174812
#> 2 2026-05-07 1.173089
#> 3 2026-05-08 1.173089
#> 4 2026-05-09 1.173089
#> 5 2026-05-10 1.176886
#> 6 2026-05-11 1.177565
#> 7 2026-05-12 1.174398
#> 8 2026-05-13 1.174398
# }
```
