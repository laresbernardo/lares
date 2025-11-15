# Logarithmic Loss Function for Binary Models

This function calculates log loss/cross-entropy loss for binary models.
NOTE: when result is 0.69315, the classification is neutral; it assigns
equal probability to both classes.

## Usage

``` r
loglossBinary(tag, score, eps = 0.001)
```

## Arguments

- tag:

  Vector. Real known label

- score:

  Vector. Predicted value or model's result

- eps:

  Numeric. Epsilon value

## See also

Other Model metrics:
[`ROC()`](https://laresbernardo.github.io/lares/reference/ROC.md),
[`conf_mat()`](https://laresbernardo.github.io/lares/reference/conf_mat.md),
[`errors()`](https://laresbernardo.github.io/lares/reference/errors.md),
[`gain_lift()`](https://laresbernardo.github.io/lares/reference/gain_lift.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md)
