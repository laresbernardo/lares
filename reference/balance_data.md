# Balance Binary Data by Resampling: Under-Over Sampling

This function lets the user balance a given data.frame by resampling
with a given relation rate and a binary feature.

## Usage

``` r
balance_data(df, var, rate = 1, target = "auto", seed = 0, quiet = FALSE)
```

## Arguments

- df:

  Vector or Dataframe. Contains different variables in each column,
  separated by a specific character

- var:

  Variable. Which variable should we used to re-sample dataset?

- rate:

  Numeric. How many X for every Y we need? Default: 1. If there are more
  than 2 unique values, rate will represent percentage for number of
  rows

- target:

  Character. If binary, which value should be reduced? If kept in
  `"auto"`, then the most frequent value will be reduced.

- seed:

  Numeric. Seed to replicate and obtain same values

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## Value

data.frame. Reduced sampled data.frame following the `rate` of
appearance of a specific variable.

## See also

Other Data Wrangling:
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
data(dft) # Titanic dataset
df <- balance_data(dft, Survived, rate = 1)
#> Resampled from: 549 v 342
#> Reducing size for label: FALSE
#> New label distribution: 342 v 342
df <- balance_data(dft, .data$Survived, rate = 0.5, target = "TRUE")
#> Resampled from: 549 v 342
#> Reducing size for label: TRUE
#> New label distribution: 549 v 274
```
