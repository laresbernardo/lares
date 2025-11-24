# Correlation table

This function correlates a whole dataframe, running one hot smart
encoding (`ohse`) to transform non-numerical features. Note that it will
automatically suppress columns with less than 3 non missing values and
warn the user.

## Usage

``` r
corr(
  df,
  method = "pearson",
  use = "pairwise.complete.obs",
  pvalue = FALSE,
  padjust = NULL,
  half = FALSE,
  dec = 6,
  ignore = NULL,
  dummy = TRUE,
  redundant = NULL,
  logs = FALSE,
  limit = 10,
  top = NA,
  ...
)
```

## Arguments

- df:

  Dataframe. It doesn't matter if it's got non-numerical columns: they
  will be filtered.

- method:

  Character. Any of: c("pearson", "kendall", "spearman").

- use:

  Character. Method for computing covariances in the presence of missing
  values. Check [`stats::cor`](https://rdrr.io/r/stats/cor.html) for
  options.

- pvalue:

  Boolean. Returns a list, with correlations and statistical
  significance (p-value) for each value.

- padjust:

  Character. NULL to skip or any of `p.adjust.methods` to calculate
  adjust p-values for multiple comparisons using
  [`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

- half:

  Boolean. Return only half of the matrix? The redundant symmetrical
  correlations will be `NA`.

- dec:

  Integer. Number of decimals to round correlations and p-values.

- ignore:

  Vector or character. Which column should be ignored?

- dummy:

  Boolean. Should One Hot (Smart) Encoding
  ([`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md))
  be applied to categorical columns?

- redundant:

  Boolean. Should we keep redundant columns? i.e. If the column only has
  two different values, should we keep both new columns? Is set to
  `NULL`, only binary variables will dump redundant columns.

- logs:

  Boolean. Calculate log(x)+1 for numerical columns?

- limit:

  Integer. Limit one hot encoding to the n most frequent values of each
  column. Set to `NA` to ignore argument.

- top:

  Integer. Select top N most relevant variables? Filtered and sorted by
  mean of each variable's correlations.

- ...:

  Additional parameters passed to `ohse`, `corr`, and/or `cor.test`.

## Value

data.frame. Squared dimensions (N x N) to match every correlation
between every `df` data.frame column/variable. Notice that when using
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md) you
may get more dimensions.

## See also

Other Calculus:
[`corr_cross()`](https://laresbernardo.github.io/lares/reference/corr_cross.md),
[`dist2d()`](https://laresbernardo.github.io/lares/reference/dist2d.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md),
[`quants()`](https://laresbernardo.github.io/lares/reference/quants.md)

Other Correlations:
[`corr_cross()`](https://laresbernardo.github.io/lares/reference/corr_cross.md),
[`corr_var()`](https://laresbernardo.github.io/lares/reference/corr_var.md)

## Examples

``` r
data(dft) # Titanic dataset
df <- dft[, 2:5]

# Correlation matrix (without redundancy)
corr(df, half = TRUE)
#>                     Age Survived_TRUE  Sex_male  Pclass_1 Pclass_2 Pclass_3
#> Age                  NA            NA        NA        NA       NA       NA
#> Survived_TRUE -0.077221            NA        NA        NA       NA       NA
#> Sex_male       0.093254     -0.543351        NA        NA       NA       NA
#> Pclass_1       0.348941      0.285904 -0.098013        NA       NA       NA
#> Pclass_2       0.006954      0.093349 -0.064746 -0.288585       NA       NA
#> Pclass_3      -0.312271     -0.322308  0.137143 -0.626738 -0.56521       NA

# Ignore specific column
corr(df, ignore = "Pclass")
#>                     Age Survived_TRUE  Sex_male
#> Age            1.000000     -0.077221  0.093254
#> Survived_TRUE -0.077221      1.000000 -0.543351
#> Sex_male       0.093254     -0.543351  1.000000

# Calculate p-values as well
corr(df, pvalue = TRUE, limit = 1)
#> $cor
#>                     Age Survived_TRUE  Sex_male  Pclass_3 Pclass_OTHER
#> Age            1.000000     -0.077221  0.093254 -0.312271     0.312271
#> Survived_TRUE -0.077221      1.000000 -0.543351 -0.322308     0.322308
#> Sex_male       0.093254     -0.543351  1.000000  0.137143    -0.137143
#> Pclass_3      -0.312271     -0.322308  0.137143  1.000000    -1.000000
#> Pclass_OTHER   0.312271      0.322308 -0.137143 -1.000000     1.000000
#> 
#> $pvalue
#>                        Age Survived_TRUE     Sex_male     Pclass_3 Pclass_OTHER
#> Age           0.000000e+00  3.912465e-02 1.267130e-02 1.295594e-17 1.295594e-17
#> Survived_TRUE 3.912465e-02  0.000000e+00 1.406066e-69 5.510281e-23 5.510281e-23
#> Sex_male      1.267130e-02  1.406066e-69 0.000000e+00 4.002500e-05 4.002500e-05
#> Pclass_3      1.295594e-17  5.510281e-23 4.002500e-05 0.000000e+00 0.000000e+00
#> Pclass_OTHER  1.295594e-17  5.510281e-23 4.002500e-05 0.000000e+00 0.000000e+00
#> 

# Test when no more than 2 non-missing values
df$trash <- c(1, rep(NA, nrow(df) - 1))
# and another method...
corr(df, method = "spearman")
#> Warning: Dropped columns with less than 3 non-missing values: 'trash'
#>                     Age Survived_TRUE  Sex_male  Pclass_1  Pclass_2  Pclass_3
#> Age            1.000000     -0.052565  0.083330  0.333881  0.031291 -0.319907
#> Survived_TRUE -0.052565      1.000000 -0.543351  0.285904  0.093349 -0.322308
#> Sex_male       0.083330     -0.543351  1.000000 -0.098013 -0.064746  0.137143
#> Pclass_1       0.333881      0.285904 -0.098013  1.000000 -0.288585 -0.626738
#> Pclass_2       0.031291      0.093349 -0.064746 -0.288585  1.000000 -0.565210
#> Pclass_3      -0.319907     -0.322308  0.137143 -0.626738 -0.565210  1.000000
```
