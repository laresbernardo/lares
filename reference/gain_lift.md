# Cumulative Gain, Lift and Response

This function calculates cumulative gain, lift, and response values for
a predictive score of a specific target. You can use the
[`mplot_gain()`](https://laresbernardo.github.io/lares/reference/mplot_gain.md)
function to create a plot.

## Usage

``` r
gain_lift(
  tag,
  score,
  target = "auto",
  splits = 10,
  plot = FALSE,
  quiet = FALSE
)
```

## Arguments

- tag:

  Vector. Real known label

- score:

  Vector. Predicted value or model's result

- target:

  Value. Which is your target positive value? If set to 'auto', the
  target with largest mean(score) will be selected. Change the value to
  overwrite. Only used when binary categorical model.

- splits:

  Integer. Number of percentiles to split the data

- plot:

  Boolean. Plot results? Uses
  [`mplot_gain()`](https://laresbernardo.github.io/lares/reference/mplot_gain.md)

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## Value

data.frame when `plot=FALSE` or plot when `plot=TRUE`.

## See also

Other Machine Learning:
[`ROC()`](https://laresbernardo.github.io/lares/reference/ROC.md),
[`conf_mat()`](https://laresbernardo.github.io/lares/reference/conf_mat.md),
[`export_results()`](https://laresbernardo.github.io/lares/reference/export_results.md),
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
[`conf_mat()`](https://laresbernardo.github.io/lares/reference/conf_mat.md),
[`errors()`](https://laresbernardo.github.io/lares/reference/errors.md),
[`loglossBinary()`](https://laresbernardo.github.io/lares/reference/loglossBinary.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md)

## Examples

``` r
data(dfr) # Results for AutoML Predictions
head(dfr$class2)
#>     tag    scores
#> 1  TRUE 0.3155498
#> 2  TRUE 0.8747599
#> 3  TRUE 0.8952823
#> 4 FALSE 0.0436517
#> 5  TRUE 0.2196593
#> 6 FALSE 0.2816101

# Results for Binomial Model
gain_lift(dfr$class2$tag, dfr$class2$scores, target = "FALSE")
#> Target value: FALSE
#> # A tibble: 10 × 10
#>    percentile value random target total  gain optimal  lift response score
#>    <fct>      <chr>  <dbl>  <int> <int> <dbl>   <dbl> <dbl>    <dbl> <dbl>
#>  1 1          FALSE   10.1     27    27  16.4    16.4 62.4     16.4  93.9 
#>  2 2          FALSE   20.1     23    27  30.3    32.7 50.4     13.9  92.5 
#>  3 3          FALSE   30.2     26    27  46.1    49.1 52.4     15.8  90.7 
#>  4 4          FALSE   39.9     23    26  60      64.8 50.3     13.9  86.8 
#>  5 5          FALSE   50       22    27  73.3    81.2 46.7     13.3  80.4 
#>  6 6          FALSE   60.1     18    27  84.2    97.6 40.2     10.9  68.8 
#>  7 7          FALSE   69.8     14    26  92.7   100   32.9      8.48 54.0 
#>  8 8          FALSE   80.2      8    28  97.6   100   21.6      4.85 19.8 
#>  9 9          FALSE   90.3      2    27  98.8   100    9.40     1.21  6.66
#> 10 10         FALSE  100        2    26 100     100    0        1.21  1.76
gain_lift(dfr$class2$tag, dfr$class2$scores, target = "TRUE", splits = 5)
#> Target value: TRUE
#> # A tibble: 5 × 10
#>   percentile value random target total  gain optimal  lift response score
#>   <fct>      <chr>  <dbl>  <int> <int> <dbl>   <dbl> <dbl>    <dbl> <dbl>
#> 1 1          TRUE    20.5     50    55  48.5    53.4 137.     48.5  79.7 
#> 2 2          TRUE    39.9     31    52  78.6   100    97.0    30.1  31.5 
#> 3 3          TRUE    60.1     14    54  92.2   100    53.5    13.6  13.3 
#> 4 4          TRUE    79.9      5    53  97.1   100    21.6     4.85  7.49
#> 5 5          TRUE   100        3    54 100     100     0       2.91  3.19
```
