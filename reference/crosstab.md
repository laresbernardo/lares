# Weighted Cross Tabulation

A cross-tabulation function with output similar to STATA, tidy friendly,
with weighting possibility.

## Usage

``` r
crosstab(
  df,
  ...,
  wt = NULL,
  prow = FALSE,
  pcol = FALSE,
  pall = FALSE,
  decimals = 2,
  rm.na = FALSE,
  total = TRUE,
  order = TRUE
)
```

## Arguments

- df:

  Data.frame.

- ...:

  Variables. Dependent and independent variables.

- wt:

  Variable, numeric. Weights.

- prow, pcol, pall:

  Boolean. Calculate percent values for rows, columns, or the whole
  table, respectively.

- decimals:

  Integer. How many decimals should be returned?

- rm.na:

  Boolean. Remove NA values?

- total:

  Boolean. Return total values column?

- order:

  Boolean. Sort columns and rows by frequencies? Else, will be sorted
  alphabetically

## Value

data.frame. Result of crossing the variables provided in `...` and
counting how many observations (rows) fall into each criteria.

## See also

Other Exploratory:
[`corr_var()`](https://laresbernardo.github.io/lares/reference/corr_var.md),
[`df_str()`](https://laresbernardo.github.io/lares/reference/df_str.md),
[`distr()`](https://laresbernardo.github.io/lares/reference/distr.md),
[`freqs()`](https://laresbernardo.github.io/lares/reference/freqs.md),
[`freqs_df()`](https://laresbernardo.github.io/lares/reference/freqs_df.md),
[`freqs_list()`](https://laresbernardo.github.io/lares/reference/freqs_list.md),
[`freqs_plot()`](https://laresbernardo.github.io/lares/reference/freqs_plot.md),
[`lasso_vars()`](https://laresbernardo.github.io/lares/reference/lasso_vars.md),
[`missingness()`](https://laresbernardo.github.io/lares/reference/missingness.md),
[`plot_cats()`](https://laresbernardo.github.io/lares/reference/plot_cats.md),
[`plot_df()`](https://laresbernardo.github.io/lares/reference/plot_df.md),
[`plot_nums()`](https://laresbernardo.github.io/lares/reference/plot_nums.md),
[`tree_var()`](https://laresbernardo.github.io/lares/reference/tree_var.md)

## Examples

``` r
data(dft) # Titanic dataset
crosstab(dft, Survived, Pclass, total = FALSE)
#> # A tibble: 2 × 4
#>   `Survived x Pclass`   `3`   `1`   `2`
#>   <fct>               <int> <int> <int>
#> 1 FALSE                 372    80    97
#> 2 TRUE                  119   136    87
# Show values in percentages
crosstab(dft, Pclass, Survived, prow = TRUE)
#> # A tibble: 3 × 4
#>   `Pclass x Survived` `FALSE` `TRUE` total
#>   <fct>                 <dbl>  <dbl> <dbl>
#> 1 3                      67.8   34.8  55.1
#> 2 1                      14.6   39.8  24.2
#> 3 2                      17.7   25.4  20.6
crosstab(dft, Pclass, Survived, pall = TRUE)
#> # A tibble: 3 × 4
#>   `Pclass x Survived` `FALSE` `TRUE` total
#>   <fct>                 <dbl>  <dbl> <dbl>
#> 1 3                     41.8   13.4   55.1
#> 2 1                      8.98  15.3   24.2
#> 3 2                     10.9    9.76  20.6
# Weighted by another variable
crosstab(dft, Survived, Pclass, wt = Fare, prow = TRUE)
#> # A tibble: 2 × 5
#>   `Survived x Pclass`   `1`   `3`   `2` total
#>   <fct>               <dbl> <dbl> <dbl> <dbl>
#> 1 TRUE                 71.5  24.3  50.5  57.7
#> 2 FALSE                28.5  75.7  49.5  42.3
```
