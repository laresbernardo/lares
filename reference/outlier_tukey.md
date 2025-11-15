# Outliers: Tukey’s fences

Tukey’s fences is a technique used in box plots. The non-outlier range
is defined with `[Q1-k(Q3-Q1), Q3+k(Q3-Q1)]`, where Q1 and Q3 are the
lower and upper quartiles respectively, k - some non-negative constant
(popular choice is 1.5). A value is an outlier based on Tukey’s fences
when its value does not lie in non-outlier range.

## Usage

``` r
outlier_tukey(x, k = 1.5)

outlier_turkey(x, k = 1.5)
```

## Arguments

- x:

  Numeric. Distribution

- k:

  Positive Numeric. K-multiplier.

## Value

Boolean vector detecting outliers.

## See also

Other Outliers:
[`outlier_zscore()`](https://laresbernardo.github.io/lares/reference/outlier_zscore.md),
[`outlier_zscore_plot()`](https://laresbernardo.github.io/lares/reference/outlier_zscore_plot.md),
[`winsorize()`](https://laresbernardo.github.io/lares/reference/winsorize.md)
