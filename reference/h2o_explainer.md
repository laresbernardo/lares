# DALEX Explainer for H2O

DALEX helper function to create an `explainer` object using a `h2o`
trained model.

## Usage

``` r
h2o_explainer(df, model, y = "tag", ignore = NULL, ...)
```

## Arguments

- df:

  Dataframe. Must contain all columns and predictions

- model:

  Model object (H2O)

- y:

  Character or Variable name. Variable's column name.

- ignore:

  Character vector. Which columns should be ignored?

- ...:

  Additional parameters to pass to `h2o_predict_model` or
  `h2o_predict_MOJO`.

## Value

List; explainer. Containing the model, data, y, predict_function, y_hat,
residuals, class, label, model_info, residual_function, and weights.

## See also

Other Interpretability:
[`dalex_local()`](https://laresbernardo.github.io/lares/reference/dalex_local.md),
[`dalex_residuals()`](https://laresbernardo.github.io/lares/reference/dalex_residuals.md),
[`dalex_variable()`](https://laresbernardo.github.io/lares/reference/dalex_variable.md)

## Examples

``` r
# You must have "DALEX" library to use this auxiliary function:
if (FALSE) { # \dontrun{
data(dft) # Titanic dataset

# TRAIN A SIMPLE MODEL
dfm <- h2o_automl(dft,
  y = "Survived",
  ignore = c("Ticket", "PassengerId", "Cabin"),
  max_models = 1
)

# EXPLAINER
explainer <- h2o_explainer(df = dfm$datasets$test, model = dfm$model, y = "Survived")
explainer$data <- na.omit(explainer$data)

# CATEGORICAL EXAMPLE
class <- dalex_variable(explainer, vars = c("Pclass", "Sex"))
class$plot

# NUMERICAL EXAMPLE
num <- dalex_variable(explainer, vars = c("Fare", "Age"))
num$plot

# LOCAL EXAMPLE
local <- dalex_local(explainer, row = 1)
# OR YOU COULD MANUALLY INPUT THE OBSERVATION
local <- dalex_local(explainer, observation = explainer$data[1, ])
local$plot

# xai2shiny's UI (needs to be installed from ModelOriented/xai2shiny)
xai2shiny(explainer, run = TRUE)
} # }
```
