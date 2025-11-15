# Robyn: Generate default hyperparameters

Generate a list with hyperparameter default values, ready to be passed
to `Robyn::robyn_inputs()`.

## Usage

``` r
robyn_hypsbuilder(
  channels,
  media_type = "default",
  adstock = "geometric",
  date_type = "weekly",
  lagged = FALSE
)
```

## Arguments

- channels:

  Character vector. Paid media and organic variables names.

- media_type:

  Character vector. Must be length 1 or same as `channels`. Pick, for
  every `channels` value, what type of media it is: "online" or
  "offline".

- adstock:

  Character. Pick one of: "geometric" or "weibull".

- date_type:

  Character. Pick one of: "daily", "weekly", or "monthly". Only valid to
  transform thetas when using geometric adstock. Set to "skip" in case
  you wish to leave default weekly values.

- lagged:

  Boolean vector. Must be length 1 or same as `channels`. Pick, for
  every `channels` value, if you wish to have a lagged effect. Only
  valid for Weibull adstock.

## Value

list with default hyperparameters ranges.

## See also

Other Robyn:
[`robyn_marginal()`](https://laresbernardo.github.io/lares/reference/robyn_marginal.md),
[`robyn_modelselector()`](https://laresbernardo.github.io/lares/reference/robyn_modelselector.md),
[`robyn_performance()`](https://laresbernardo.github.io/lares/reference/robyn_performance.md)

## Examples

``` r
robyn_hypsbuilder(
  channels = c(
    "branded_search_spend",
    "nonbranded_search_spend",
    "print_spend",
    "ooh_spend",
    "tv_spend",
    "radio_spend"
  ),
  media_type = c(
    "online", "online", "offline",
    "offline", "offline", "offline"
  ),
  adstock = "geometric",
  date_type = "weekly"
)
#> $branded_search_spend_alphas
#> [1] 0.01 3.00
#> 
#> $branded_search_spend_gammas
#> [1] 0.3 1.0
#> 
#> $branded_search_spend_thetas
#> [1] 0.1 0.5
#> 
#> $nonbranded_search_spend_alphas
#> [1] 0.01 3.00
#> 
#> $nonbranded_search_spend_gammas
#> [1] 0.3 1.0
#> 
#> $nonbranded_search_spend_thetas
#> [1] 0.1 0.5
#> 
#> $print_spend_alphas
#> [1] 0.01 1.00
#> 
#> $print_spend_gammas
#> [1] 0.3 1.0
#> 
#> $print_spend_thetas
#> [1] 0.1 0.5
#> 
#> $ooh_spend_alphas
#> [1] 0.01 1.00
#> 
#> $ooh_spend_gammas
#> [1] 0.3 1.0
#> 
#> $ooh_spend_thetas
#> [1] 0.1 0.5
#> 
#> $tv_spend_alphas
#> [1] 0.01 1.00
#> 
#> $tv_spend_gammas
#> [1] 0.3 1.0
#> 
#> $tv_spend_thetas
#> [1] 0.1 0.5
#> 
#> $radio_spend_alphas
#> [1] 0.01 1.00
#> 
#> $radio_spend_gammas
#> [1] 0.3 1.0
#> 
#> $radio_spend_thetas
#> [1] 0.1 0.5
#> 
#> $train_size
#> [1] 0.5 0.8
#> 
```
