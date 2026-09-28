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
by date. A single-day request returns the last available quote within
the previous 14 days when that day is not quoted. If the provider cannot
be reached or has no quotes, returns an empty data.frame with `date` and
`rate` columns and an informative message. No cached quotes are used
when the provider is unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires an external currency provider; do not run in automated checks.
# For today (or any one single date)
get_currency("USD/COP", from = Sys.Date())
# For multiple dates
get_currency("EUR/USD", from = Sys.Date() - 7, fill = TRUE)
} # }
```
