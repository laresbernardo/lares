# Calculate Continuous Values Errors

This function lets the user calculate all errors and R squared
simultaneously.

This function lets the user calculate Root Mean Squared Error

This function lets the user calculate Mean Absolute Error

This function lets the user calculate Mean Squared Error

This function lets the user calculate Mean Squared Error

This function lets the user calculate R Squared

This function lets the user calculate Adjusted R Squared

## Usage

``` r
errors(tag, score)

rmse(tag, score)

mae(tag, score)

mse(tag, score)

mape(tag, score)

rsq(tag, score)

rsqa(tag, score)
```

## Arguments

- tag:

  Vector. Real known label

- score:

  Vector. Predicted value or model's result

## Value

data.frame or numeric values results for multiple error metrics on
continuous numerical vectors inputs.

## See also

Other Model metrics:
[`ROC()`](https://laresbernardo.github.io/lares/reference/ROC.md),
[`conf_mat()`](https://laresbernardo.github.io/lares/reference/conf_mat.md),
[`gain_lift()`](https://laresbernardo.github.io/lares/reference/gain_lift.md),
[`loglossBinary()`](https://laresbernardo.github.io/lares/reference/loglossBinary.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md)

## Examples

``` r
data(dfr) # Results for AutoML Predictions
head(dfr$regr)
#>       tag    score
#> 1 11.1333 25.93200
#> 2 30.0708 39.91900
#> 3 26.5500 50.72246
#> 4 31.2750 47.81292
#> 5 13.0000 30.12853
#> 6 26.0000 13.24153
df <- errors(dfr$regr$tag, dfr$regr$score)
head(df)
#>       rmse      mae       mape      mse    rsq   rsqa
#> 1 20.30881 14.24359 0.07303959 412.4477 0.3169 0.3143
```
