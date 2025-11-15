# Confidence Intervals on Dataframe

Calculate confidence intervals for a continuous numerical column on a
dataframe, given a confidence level. You may also group results using
another variable. Tidyverse friendly.

## Usage

``` r
ci_var(df, var, group_var = NULL, conf = 0.95)
```

## Arguments

- df:

  Dataframe

- var:

  Variable name. Must be a numerical column.

- group_var:

  Variable name. Group results by another variable.

- conf:

  Numeric. Confidence level (0-1).

## Value

data.frame mean, standard deviation, counter, upper and lower CIs.

## See also

Other Confidence:
[`ci_lower()`](https://laresbernardo.github.io/lares/reference/ci_lower.md)

## Examples

``` r
data(dft) # Titanic dataset
ci_var(dft, Fare)
#> Warning: There were 2 warnings in `summarise()`.
#> The first warning was:
#> ℹ In argument: `smean = mean("Fare", na.rm = TRUE)`.
#> Caused by warning in `mean.default()`:
#> ! argument is not numeric or logical: returning NA
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
#> # A tibble: 1 × 5
#>   Fare_mean Fare_sd     n lower_ci upper_ci
#>       <dbl>   <dbl> <int> <lgl>    <lgl>   
#> 1        NA      NA   891 NA       NA      
ci_var(dft, Fare, Pclass)
#> Warning: There were 6 warnings in `summarise()`.
#> The first warning was:
#> ℹ In argument: `smean = mean("Fare", na.rm = TRUE)`.
#> ℹ In group 1: `Pclass = 1`.
#> Caused by warning in `mean.default()`:
#> ! argument is not numeric or logical: returning NA
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 5 remaining warnings.
#> # A tibble: 3 × 6
#>   Pclass Fare_mean Fare_sd     n lower_ci upper_ci
#>   <fct>      <dbl>   <dbl> <int> <lgl>    <lgl>   
#> 1 1             NA      NA   216 NA       NA      
#> 2 2             NA      NA   184 NA       NA      
#> 3 3             NA      NA   491 NA       NA      
ci_var(dft, Fare, Pclass, conf = 0.99)
#> Warning: There were 6 warnings in `summarise()`.
#> The first warning was:
#> ℹ In argument: `smean = mean("Fare", na.rm = TRUE)`.
#> ℹ In group 1: `Pclass = 1`.
#> Caused by warning in `mean.default()`:
#> ! argument is not numeric or logical: returning NA
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 5 remaining warnings.
#> # A tibble: 3 × 6
#>   Pclass Fare_mean Fare_sd     n lower_ci upper_ci
#>   <fct>      <dbl>   <dbl> <int> <lgl>    <lgl>   
#> 1 1             NA      NA   216 NA       NA      
#> 2 2             NA      NA   184 NA       NA      
#> 3 3             NA      NA   491 NA       NA      
```
