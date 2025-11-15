# Lower/Upper Confidence Intervals

Calculate lower and upper confidence intervals given a mean, standard
deviation, sample size, and confidence level. You may want to use
[`ci_var()`](https://laresbernardo.github.io/lares/reference/ci_var.md)
to calculate all values quickly.

## Usage

``` r
ci_lower(mean, ssd, n, conf = 0.95)

ci_upper(mean, ssd, n, conf = 0.95)
```

## Arguments

- mean:

  Numeric. Mean: `mean(var, na.rm = TRUE)`

- ssd:

  Numeric. Standard deviation: `sd(var, na.rm = TRUE)`

- n:

  Integer. Amount of observations: `n()`

- conf:

  Numeric (0-1). Confidence level.

## Value

Vector with confidence limit value.

## See also

Other Confidence:
[`ci_var()`](https://laresbernardo.github.io/lares/reference/ci_var.md)

## Examples

``` r
ci_lower(100, 5, 10)
#> [1] 96.42322
ci_upper(100, 5, 10)
#> [1] 103.5768
```
