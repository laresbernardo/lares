# SHAP values for H2O Models

SHAP (SHapley Additive exPlanations) by Lundberg and Lee (2016) is a
method to explain individual predictions. SHAP is based on the game
theoretically optimal Shapley Values. Calculate SHAP values for h2o
models in which each row is an observation and each column a feature.
Use `plot` method to visualize features importance and distributions.

## Usage

``` r
h2o_shap(model, test = "auto", scores = "auto", y = "y", ...)

# S3 method for class 'h2o_shap'
plot(x, relevant = TRUE, top = 15, quiet = FALSE, ...)
```

## Arguments

- model:

  `h2o_automl` object or `h2o` model.

- test:

  String or Dataframe. Leave "auto" to use `h2o_automl`'s test dataset
  or pass a valid dataframe.

- scores:

  Numeric vector. If test != "auto", you must provide predicted values

- y:

  Character. If test != "auto", you must provide y variable's name

- ...:

  Additional argument for `predict_contributions.H2OModel`

- x:

  h2o_shap object

- relevant:

  Boolean. Keep only relevant non-trivial (\>0) features

- top:

  Integer. Plot only top n values (as in importance)

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## Value

H2OFrame with shap values for every observation and feature.

## See also

Other SHAP:
[`shap_var()`](https://laresbernardo.github.io/lares/reference/shap_var.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Train a h2o_automl model
model <- h2o_automl(dft, Survived,
  max_models = 1, target = TRUE,
  ignore = c("Ticket", "Cabin", "PassengerId"),
  quiet = TRUE
)

# Calculate SHAP values
SHAP_values <- h2o_shap(model)
# Equivalent to:
# SHAP_values <- h2o_shap(
#  model = model$model,
#  test = model$datasets$test,
#  scores = model$scores_test$scores)

# Check SHAP results
head(SHAP_values)

# You must have "ggbeeswarm" library to use this auxiliary function:
# Plot SHAP values (feature importance)
plot(SHAP_values)

# Plot some of the variables (categorical)
shap_var(SHAP_values, Pclass)

# Plot some of the variables (numerical)
shap_var(SHAP_values, Fare)
} # }
```
