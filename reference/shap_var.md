# SHAP-based dependence plots for categorical/numerical features (PDP)

Having a `h2o_shap` object, plot a dependence plot for any categorical
or numerical feature.

## Usage

``` r
shap_var(x, var, keep_outliers = FALSE)
```

## Arguments

- x:

  `h2o_shap` object

- var:

  Variable name

- keep_outliers:

  Boolean. Outliers detected with z-score and 3sd may be suppress or
  kept in your plot. Keep them?

## Value

ggplot2 objct with shap values plotted

## See also

Other SHAP:
[`h2o_shap()`](https://laresbernardo.github.io/lares/reference/h2o_shap.md)

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
