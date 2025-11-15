# Outliers: Z-score method

Z-score, also called a standard score, of an observation is a distance
from the population center measured in number of normalization units.
The default choice for center is sample mean and for normalization unit
is standard deviation. Values are considered outliers based on z-score
if its absolute value of default z-score is higher then the threshold
(popular choice is 3).

## Usage

``` r
outlier_zscore(x, thresh = 3, mad = FALSE)
```

## Arguments

- x:

  Numeric. Distribution

- thresh:

  Numeric. Z-Score threshold for n standard deviations.

- mad:

  Boolean. Use median absolute deviation instead?

## Value

data.frame. Each row is an `x` observation with its respective std/mean
or mad/med calculations depending on `mad` input.

## See also

Other Outliers:
[`outlier_tukey()`](https://laresbernardo.github.io/lares/reference/outlier_tukey.md),
[`outlier_zscore_plot()`](https://laresbernardo.github.io/lares/reference/outlier_zscore_plot.md),
[`winsorize()`](https://laresbernardo.github.io/lares/reference/winsorize.md)
