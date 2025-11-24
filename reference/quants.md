# Calculate cuts by quantiles

This function lets the user quickly calculate cuts for quantiles and
discretize numerical values into categorical values.

## Usage

``` r
quants(values, splits = 10, return = "labels", n = 2)
```

## Arguments

- values:

  Vector. Values to calculate quantile cuts

- splits:

  Integer. How many cuts should split the values?

- return:

  Character. Return "summary" or "labels"

- n:

  Integer. Determines the number of digits used in formatting the break
  numbers.

## Value

Factor vector or data.frame. Depending on `return` input:

- `labels` a factor ordered vector with each observation's quantile

- `summary` a data.frame with information on each quantile cut

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
[`num_abbr()`](https://laresbernardo.github.io/lares/reference/num_abbr.md),
[`ohe_commas()`](https://laresbernardo.github.io/lares/reference/ohe_commas.md),
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md),
[`removenacols()`](https://laresbernardo.github.io/lares/reference/filterdata.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`replacefactor()`](https://laresbernardo.github.io/lares/reference/replacefactor.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`vector2text()`](https://laresbernardo.github.io/lares/reference/vector2text.md),
[`year_month()`](https://laresbernardo.github.io/lares/reference/year_month.md),
[`zerovar()`](https://laresbernardo.github.io/lares/reference/zerovar.md)

Other Calculus:
[`corr()`](https://laresbernardo.github.io/lares/reference/corr.md),
[`corr_cross()`](https://laresbernardo.github.io/lares/reference/corr_cross.md),
[`dist2d()`](https://laresbernardo.github.io/lares/reference/dist2d.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md)

## Examples

``` r
data(dft) # Titanic dataset
quants(dft$Age, splits = 5, "summary")
#>      percentile  cut   label
#> 20%         20% 19.0 [19-19]
#> 40%         40% 25.0 (19-25]
#> 60%         60% 31.8 (25-32]
#> 80%         80% 41.0 (32-41]
#> 100%       100% 80.0 (41-80]
quants(dft$Age, splits = 5, "labels")[1:10]
#>  [1] (19,25]   (32,41]   (25,32]   (32,41]   (32,41]   <NA>      (41,80]  
#>  [8] [0.42,19] (25,32]   [0.42,19]
#> Levels: [0.42,19] < (19,25] < (25,32] < (32,41] < (41,80]
```
