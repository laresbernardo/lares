# Set Target Value in Target Variable

This function detects or forces the target value when predicting a
categorical binary model. This is an auxiliary function.

## Usage

``` r
target_set(tag, score, target = "auto", quiet = FALSE)
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

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## Value

List. Contains original data.frame `df` and `which` with the target
variable.
