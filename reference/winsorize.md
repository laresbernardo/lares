# Outliers: Winsorize

Winsorizing a vector means that a predefined quantum of the smallest
and/or the largest values are replaced by less extreme values. Thereby
the substitute values are the most extreme retained values.

## Usage

``` r
winsorize(x, thresh = c(0.05, 0.95), na.rm = FALSE)
```

## Arguments

- x:

  Numeric vector. Distribution to be winsorized.

- thresh:

  Numeric vector. Lower and upper quantiles thresholds. Set values
  within \[0,1\].

- na.rm:

  Boolean. Should `NA` be omitted to calculate the quantiles? Note that
  `NA` in `x` are preserved and left unchanged anyway.

## Value

Numeric vector transformed.

## See also

Other Outliers:
[`outlier_tukey()`](https://laresbernardo.github.io/lares/reference/outlier_tukey.md),
[`outlier_zscore()`](https://laresbernardo.github.io/lares/reference/outlier_zscore.md),
[`outlier_zscore_plot()`](https://laresbernardo.github.io/lares/reference/outlier_zscore_plot.md)
