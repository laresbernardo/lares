# One Hot Encoding for Date/Time Variables (Dummy Variables)

This function lets the user automatically create new columns out of a
dataframe or vector with date/time variables.

## Usage

``` r
date_feats(
  dates,
  drop = FALSE,
  only = NA,
  append = FALSE,
  holidays = FALSE,
  country = "Venezuela",
  currency_pair = NA,
  quiet = FALSE
)
```

## Arguments

- dates:

  Vector or dataframe. Non-date/time columns will be automatically
  ignored/extracted.

- drop:

  Boolean. Should the original date/time columns be kept in the results?
  Only valid when input is a dataframe.

- only:

  Character or vector. Which columns do you wish to process? If non are
  explicitly defined, all will be processed

- append:

  Boolean. Append results to existing data.frame? If FALSE, only
  calculated values will be returned.

- holidays:

  Boolean. Include holidays as new columns?

- country:

  Character or vector. For which countries should the holidays be
  included?

- currency_pair:

  Character. Which currency exchange do you wish to get the history
  from? i.e, USD/COP, EUR/USD...

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## Value

data.frame with additional features calculated out of time or date
vectors.

## See also

Other Data Wrangling:
[`balance_data()`](https://laresbernardo.github.io/lares/reference/balance_data.md),
[`categ_reducer()`](https://laresbernardo.github.io/lares/reference/categ_reducer.md),
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`date_cuts()`](https://laresbernardo.github.io/lares/reference/date_cuts.md),
[`file_name()`](https://laresbernardo.github.io/lares/reference/file_name.md),
[`formatHTML()`](https://laresbernardo.github.io/lares/reference/format_string.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`impute()`](https://laresbernardo.github.io/lares/reference/impute.md),
[`left()`](https://laresbernardo.github.io/lares/reference/left_right.md),
[`normalize()`](https://laresbernardo.github.io/lares/reference/normalize.md),
[`num_abbr()`](https://laresbernardo.github.io/lares/reference/num_abbr.md),
[`ohe_commas()`](https://laresbernardo.github.io/lares/reference/ohe_commas.md),
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md),
[`quants()`](https://laresbernardo.github.io/lares/reference/quants.md),
[`removenacols()`](https://laresbernardo.github.io/lares/reference/filterdata.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`replacefactor()`](https://laresbernardo.github.io/lares/reference/replacefactor.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`vector2text()`](https://laresbernardo.github.io/lares/reference/vector2text.md),
[`year_month()`](https://laresbernardo.github.io/lares/reference/year_month.md),
[`zerovar()`](https://laresbernardo.github.io/lares/reference/zerovar.md)

Other Feature Engineering:
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md)

Other One Hot Encoding:
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`ohe_commas()`](https://laresbernardo.github.io/lares/reference/ohe_commas.md),
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md)

## Examples

``` r
df <- data.frame(
  dates = sample(seq(Sys.Date() - 365, Sys.Date(), by = 1), 50),
  times = sample(seq(Sys.time() - 1e7, Sys.time(), by = 1), 50)
)

# Input as a vector or dataframe
date_feats(df, drop = TRUE, quiet = TRUE) %>% head(10)
#> # A tibble: 10 × 18
#>    dates_year dates_month dates_day dates_week dates_weekday dates_weekend
#>         <dbl>       <dbl>     <int>      <dbl> <chr>         <lgl>        
#>  1       2025           9         9         36 Tue           FALSE        
#>  2       2025           3        28         13 Fri           FALSE        
#>  3       2025           9        25         39 Thu           FALSE        
#>  4       2026           1        14          2 Wed           FALSE        
#>  5       2025           7        10         28 Thu           FALSE        
#>  6       2026           1         7          1 Wed           FALSE        
#>  7       2025          10        29         44 Wed           FALSE        
#>  8       2025          11        13         46 Thu           FALSE        
#>  9       2026           3        14         11 Sat           TRUE         
#> 10       2025           8        14         33 Thu           FALSE        
#> # ℹ 12 more variables: dates_year_day <int>, times_year <dbl>,
#> #   times_month <dbl>, times_day <int>, times_week <dbl>, times_weekday <chr>,
#> #   times_weekend <lgl>, times_year_day <int>, times_hour <int>,
#> #   times_minute <int>, times_minutes <int>, times_second <dbl>

# Holidays given a date range and country
if (FALSE) { # \dontrun{
hol <- date_feats(
  seq(Sys.Date() - 365, Sys.Date(), by = 1),
  holidays = TRUE,
  country = "Venezuela"
)
head(hol[!is.na(hol$holiday_name), ])
} # }
```
