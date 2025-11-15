# Normalize Vector

This function normalizes numerical values into a specified range,
defaulting to the 0 to 1 range.

## Usage

``` r
normalize(x, range = c(0, 1), ...)
```

## Arguments

- x:

  Numeric vector. The numbers to be normalized.

- range:

  A numeric vector of length 2 specifying the desired range for
  normalization. Default is c(0, 1).

- ...:

  Additional parameters.

## Value

A numeric vector with normalized `x` values.

## See also

Other Data Wrangling:
[`balance_data()`](https://laresbernardo.github.io/lares/reference/balance_data.md),
[`categ_reducer()`](https://laresbernardo.github.io/lares/reference/categ_reducer.md),
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`date_cuts()`](https://laresbernardo.github.io/lares/reference/date_cuts.md),
[`date_feats()`](https://laresbernardo.github.io/lares/reference/date_feats.md),
[`file_name()`](https://laresbernardo.github.io/lares/reference/file_name.md),
[`formatHTML()`](https://laresbernardo.github.io/lares/reference/format_string.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`impute()`](https://laresbernardo.github.io/lares/reference/impute.md),
[`left()`](https://laresbernardo.github.io/lares/reference/left_right.md),
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

## Examples

``` r
x <- c(0, 1, 4, 7.5, 10)
normalize(x)
#> [1] 0.00 0.10 0.40 0.75 1.00
normalize(x, range = c(-1, 1))
#> [1] -1.0 -0.8 -0.2  0.5  1.0
```
