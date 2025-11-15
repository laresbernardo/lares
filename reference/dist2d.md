# Distance from specific point to line

This function lets the user calculate the mathematical linear distance
Between a specific point and a line (given geometrical 3 points)

## Usage

``` r
dist2d(x, a = c(0, 0), b = c(1, 1))
```

## Arguments

- x:

  Vector. Coordinates of the point from which we want to measure the
  distance

- a:

  Vector. Coordinates of 1st point over the line

- b:

  Vector. Coordinates of 2st point over the line

## Value

Numeric value result

## See also

Other Calculus:
[`corr()`](https://laresbernardo.github.io/lares/reference/corr.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md),
[`quants()`](https://laresbernardo.github.io/lares/reference/quants.md)

## Examples

``` r
dist2d(x = c(5, 2))
#> [1] 2.12132
dist2d(x = c(5, 2), a = c(0, 0), b = c(0, 1))
#> [1] 5
dist2d(x = c(5, 2), a = c(0, 0), b = c(1, 0))
#> [1] 2
```
