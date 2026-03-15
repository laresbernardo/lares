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
#> 1 2026-03-14 3697.43
# For multiple dates
get_currency("EUR/USD", from = Sys.Date() - 7, fill = TRUE)
#>         date     rate
#> 1 2026-03-08 1.152565
#> 2 2026-03-09 1.152565
#> 3 2026-03-10 1.161669
#> 4 2026-03-11 1.161319
#> 5 2026-03-12 1.154175
#> 6 2026-03-13 1.152220
#> 7 2026-03-14 1.152220
#> 8 2026-03-15 1.152220
# }
```
