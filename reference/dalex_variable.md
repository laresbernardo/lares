# DALEX Partial Dependency Plots (PDP)

DALEX auxiliary function for creating Partial Dependency Plots and study
variable's responses vs independent vector.

## Usage

``` r
dalex_variable(explainer, vars, force_class = NA, seed = 123, ...)
```

## Arguments

- explainer:

  Object. Result from `h2o_explainer` function.

- vars:

  Character vector. Which features do you wish to study?

- force_class:

  Character. If you wish to force a class on your vars, which one do you
  need?

- seed:

  Numeric. Seed for reproducibility

- ...:

  Additional parameters passed to `model_profile`.

## Value

List. Containing PDP results, plot and `vars` input.

## See also

Other Interpretability:
[`dalex_local()`](https://laresbernardo.github.io/lares/reference/dalex_local.md),
[`dalex_residuals()`](https://laresbernardo.github.io/lares/reference/dalex_residuals.md),
[`h2o_explainer()`](https://laresbernardo.github.io/lares/reference/h2o_explainer.md)

## Examples

``` r
# You must have "DALEX" library to use this auxiliary function:
if (FALSE) { # \dontrun{
# Having an "explainer" object created with \code{h2o_explainer}:
# For numerical variables
dalex_variable(explainer, vars = c("Age", "Fare"))
# For categorical variables
dalex_variable(explainer, vars = c("Pclass", "Sex"))
} # }
```
