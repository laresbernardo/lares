# Top Hit Ratios for Multi-Classification Models

Calculate and plot a multi-class model's predictions accuracy based on
top N predictions and distribution of probabilities.

## Usage

``` r
mplot_topcats(tag, score, multis, model_name = NA)
```

## Arguments

- tag:

  Vector. Real known label.

- score:

  Vector. Predicted value or model's result.

- multis:

  Data.frame. Containing columns with each category probability or score
  (only used when more than 2 categories coexist).

- model_name:

  Character. Model's name

## Value

Plot with performance results over most frequent categories.

## See also

Other ML Visualization:
[`mplot_conf()`](https://laresbernardo.github.io/lares/reference/mplot_conf.md),
[`mplot_cuts()`](https://laresbernardo.github.io/lares/reference/mplot_cuts.md),
[`mplot_cuts_error()`](https://laresbernardo.github.io/lares/reference/mplot_cuts_error.md),
[`mplot_density()`](https://laresbernardo.github.io/lares/reference/mplot_density.md),
[`mplot_full()`](https://laresbernardo.github.io/lares/reference/mplot_full.md),
[`mplot_gain()`](https://laresbernardo.github.io/lares/reference/mplot_gain.md),
[`mplot_importance()`](https://laresbernardo.github.io/lares/reference/mplot_importance.md),
[`mplot_lineal()`](https://laresbernardo.github.io/lares/reference/mplot_lineal.md),
[`mplot_metrics()`](https://laresbernardo.github.io/lares/reference/mplot_metrics.md),
[`mplot_response()`](https://laresbernardo.github.io/lares/reference/mplot_response.md),
[`mplot_roc()`](https://laresbernardo.github.io/lares/reference/mplot_roc.md),
[`mplot_splits()`](https://laresbernardo.github.io/lares/reference/mplot_splits.md)

## Examples

``` r
Sys.unsetenv("LARES_FONT") # Temporal
data(dfr) # Results for AutoML Predictions
mplot_topcats(dfr$class3$tag, dfr$class3$score,
  multis = subset(dfr$class3, select = -c(tag, score)),
  model_name = "Titanic Class Model"
)
```
