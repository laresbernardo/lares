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
#> Error in getSymbols.yahoo(Symbols = "USDCOP=X", env = NULL, verbose = FALSE,  : 
#>   Unable to import “USDCOP=X”.
#> attempt to set an attribute on NULL
#> Warning: Error in getSymbols.yahoo(Symbols = "USDCOP=X", env = NULL, verbose = FALSE,  : 
#>   Unable to import “USDCOP=X”.
#> attempt to set an attribute on NULL
#> Error in x[, 1]: incorrect number of dimensions
# For multiple dates
get_currency("EUR/USD", from = Sys.Date() - 7, fill = TRUE)
#>         date     rate
#> 1 2026-09-20 1.147934
#> 2 2026-09-21 1.146434
#> 3 2026-09-22 1.144715
#> 4 2026-09-23 1.138291
#> 5 2026-09-24 1.137359
#> 6 2026-09-25 1.137359
#> 7 2026-09-26 1.139991
#> 8 2026-09-27 1.139991
# }
```
