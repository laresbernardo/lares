# Abbreviate or dis-abbreviate numerical values

This function converts a numeric vector's values into their abbreviated
character equivalent, i.e. 100E6 into 100M and viceversa.

## Usage

``` r
num_abbr(x, n = 3, numeric = FALSE, ...)
```

## Arguments

- x:

  Numeric vector

- n:

  Integer. Single numeric value, specifying number of significant
  figures to show. Range 1 to 6.

- numeric:

  Boolean. Transform abbreviated number into numeric?

- ...:

  Additional parameters.

## Value

Vector of character or numeric values that contain converted values

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
[`normalize()`](https://laresbernardo.github.io/lares/reference/normalize.md),
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
num_abbr(rnorm(10) * 1e6)
#>  [1] "1.72M"  "461K"   "-1.27M" "-687K"  "-446K"  "1.22M"  "360K"   "401K"  
#>  [9] "111K"   "-556K" 
num_abbr(rnorm(10) * 1e6, n = 1)
#>  [1] "2M"    "500K"  "-2M"   "700K"  "-500K" "-1M"   "-200K" "-1M"   "-700K"
#> [10] "-600K"
num_abbr(c("3K", "-58.3M", NA, 1), numeric = TRUE)
#> [1]      3000 -58300000        NA         1
```
