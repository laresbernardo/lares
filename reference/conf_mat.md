# Confussion Matrix

This function calculates a Confussion Matrix using crosstab for 2 or
more categories. You can either set the score and threshold or the
labels you wish to cross with.

## Usage

``` r
conf_mat(tag, score, thresh = 0.5, sense = ">=", diagonal = TRUE, plot = FALSE)
```

## Arguments

- tag:

  Vector. Real known label

- score:

  Vector. Predicted value or model's result

- thresh:

  Integer. Threshold for selecting binary or regression models: this
  number is the threshold of unique values we should have in `'tag'`
  (more than: regression; less than: classification)

- sense:

  Character. Inequation sense for threshold: \<, \<=, \>=, \>

- diagonal:

  Boolean. `FALSE` to convert diagonal numbers to zeroes. Ideal to
  detect must confusing categories.

- plot:

  Boolean. Plot result? Uses
  [`mplot_conf()`](https://laresbernardo.github.io/lares/reference/mplot_conf.md)

## Value

data.frame. Result of counting `tag` and `score`'s tag given a
`thresh`old, similar to
[`base::table()`](https://rdrr.io/r/base/table.html).

## Details

You may use
[`mplot_conf()`](https://laresbernardo.github.io/lares/reference/mplot_conf.md)
or set `plot=TRUE`.

## See also

Other Machine Learning:
[`ROC()`](https://laresbernardo.github.io/lares/reference/ROC.md),
[`export_results()`](https://laresbernardo.github.io/lares/reference/export_results.md),
[`gain_lift()`](https://laresbernardo.github.io/lares/reference/gain_lift.md),
[`h2o_automl()`](https://laresbernardo.github.io/lares/reference/h2o_automl.md),
[`h2o_predict_MOJO()`](https://laresbernardo.github.io/lares/reference/h2o_predict.md),
[`h2o_selectmodel()`](https://laresbernardo.github.io/lares/reference/h2o_selectmodel.md),
[`impute()`](https://laresbernardo.github.io/lares/reference/impute.md),
[`iter_seeds()`](https://laresbernardo.github.io/lares/reference/iter_seeds.md),
[`lasso_vars()`](https://laresbernardo.github.io/lares/reference/lasso_vars.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md),
[`model_preprocess()`](https://laresbernardo.github.io/lares/reference/model_preprocess.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md)

Other Model metrics:
[`ROC()`](https://laresbernardo.github.io/lares/reference/ROC.md),
[`errors()`](https://laresbernardo.github.io/lares/reference/errors.md),
[`gain_lift()`](https://laresbernardo.github.io/lares/reference/gain_lift.md),
[`loglossBinary()`](https://laresbernardo.github.io/lares/reference/loglossBinary.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md)

## Examples

``` r
data(dfr) # Results for AutoML Predictions
lapply(dfr[c(1, 2)], head)
#> $class2
#>     tag    scores
#> 1  TRUE 0.3155498
#> 2  TRUE 0.8747599
#> 3  TRUE 0.8952823
#> 4 FALSE 0.0436517
#> 5  TRUE 0.2196593
#> 6 FALSE 0.2816101
#> 
#> $class3
#>   tag score        n_1        n_2        n_3
#> 1 n_3   n_2 0.20343865 0.60825062 0.18831071
#> 2 n_2   n_3 0.17856154 0.07657769 0.74486071
#> 3 n_1   n_1 0.50516951 0.40168718 0.09314334
#> 4 n_3   n_2 0.30880713 0.39062151 0.30057135
#> 5 n_2   n_3 0.01956827 0.07069011 0.90974158
#> 6 n_2   n_3 0.07830017 0.15408720 0.76761264
#> 

# Results for Binomial Model
conf_mat(dfr$class2$tag, dfr$class2$scores)
#> # A tibble: 2 × 3
#>   `Real x Pred` `FALSE` `TRUE`
#>   <fct>           <int>  <int>
#> 1 FALSE               9    156
#> 2 TRUE               68     35
conf_mat(dfr$class2$tag, dfr$class2$scores, thresh = 0.3)
#> # A tibble: 2 × 3
#>   `Real x Pred` `FALSE` `TRUE`
#>   <fct>           <int>  <int>
#> 1 FALSE              27    138
#> 2 TRUE               84     19
conf_mat(dfr$class2$tag, dfr$class2$scores, sense = "<=")
#> # A tibble: 2 × 3
#>   `Real x Pred` `FALSE` `TRUE`
#>   <fct>           <int>  <int>
#> 1 FALSE             156      9
#> 2 TRUE               35     68

# Results for Multi-Categorical Model
conf_mat(dfr$class3$tag, dfr$class3$score)
#> # A tibble: 3 × 4
#>   `Real x Pred`   n_3   n_1   n_2
#>   <fct>         <int> <int> <int>
#> 1 n_3             120    11    18
#> 2 n_1              12    43     8
#> 3 n_2              26    15    15
```
