# Iterate Seeds on AutoML

This functions lets the user iterate and search for best seed. Note that
if the results change a lot, you are having a high variance in your
data.

## Usage

``` r
iter_seeds(df, y, tries = 10, ...)
```

## Arguments

- df:

  Dataframe. Dataframe containing all your data, including the dependent
  variable labeled as `'tag'`. If you want to define which variable
  should be used instead, use the `y` parameter.

- y:

  Variable or Character. Name of the dependent variable or response.

- tries:

  Integer. Number of iterations

- ...:

  Additional arguments passed to `h2o_automl`

## Value

data.frame with performance results by seed tried on every row.

## See also

Other Machine Learning:
[`ROC()`](https://laresbernardo.github.io/lares/reference/ROC.md),
[`conf_mat()`](https://laresbernardo.github.io/lares/reference/conf_mat.md),
[`export_results()`](https://laresbernardo.github.io/lares/reference/export_results.md),
[`gain_lift()`](https://laresbernardo.github.io/lares/reference/gain_lift.md),
[`h2o_automl()`](https://laresbernardo.github.io/lares/reference/h2o_automl.md),
[`h2o_predict_MOJO()`](https://laresbernardo.github.io/lares/reference/h2o_predict.md),
[`h2o_selectmodel()`](https://laresbernardo.github.io/lares/reference/h2o_selectmodel.md),
[`impute()`](https://laresbernardo.github.io/lares/reference/impute.md),
[`lasso_vars()`](https://laresbernardo.github.io/lares/reference/lasso_vars.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md),
[`model_preprocess()`](https://laresbernardo.github.io/lares/reference/model_preprocess.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md)
