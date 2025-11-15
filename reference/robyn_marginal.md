# Robyn: Marginal Performance (mROAS & mCPA) \[Experimental\]

Calculate and plot marginal performance of any spend or organic
variable.

## Usage

``` r
robyn_marginal(..., marginal_unit = 1)
```

## Arguments

- ...:

  Additional parameters.

- marginal_unit:

  Additional units to calculate the marginal performance.

## Value

list with base and marginal response results, marginal performance
metric and value, and plot.

## See also

Other Robyn:
[`robyn_hypsbuilder()`](https://laresbernardo.github.io/lares/reference/robyn_hypsbuilder.md),
[`robyn_modelselector()`](https://laresbernardo.github.io/lares/reference/robyn_modelselector.md),
[`robyn_performance()`](https://laresbernardo.github.io/lares/reference/robyn_performance.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# You may load an exported model to recreate Robyn objects
mod <- Robyn::robyn_recreate(json_file = "your_model.json")
robyn_marginal(
  InputCollect = mod$InputCollect,
  OutputCollect = mod$OutputCollect,
  metric_name = "emails_O",
  metric_value = 100000,
  date_range = "all",
  marginal_unit = 10000000
)
} # }
```
