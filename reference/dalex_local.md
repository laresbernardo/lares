# DALEX Local

DALEX function for local interpretations

## Usage

``` r
dalex_local(explainer, observation = NA, row = 1, type = "break_down")
```

## Arguments

- explainer:

  Object. Result from h2o_explainer function

- observation:

  Data.frame. If you want to use an observation that was not in the
  original explainer function, add here. Else, use row

- row:

  Dataframe. Row number from the data.frame used in explainer.

- type:

  Character. The type of variable attributions. Either shap,
  oscillations, break_down or break_down_interactions.

## Value

List. Containing observation, breakdown results, and breakdown plot.

## See also

Other Interpretability:
[`dalex_residuals()`](https://laresbernardo.github.io/lares/reference/dalex_residuals.md),
[`dalex_variable()`](https://laresbernardo.github.io/lares/reference/dalex_variable.md),
[`h2o_explainer()`](https://laresbernardo.github.io/lares/reference/h2o_explainer.md)
