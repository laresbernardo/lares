# Robyn: Dynamic Performance and Contribution Report

Given a date range, calculate specific and total performance and
contribution for each of your marketing and non-marketing channels.

## Usage

``` r
robyn_performance(
  InputCollect,
  OutputCollect,
  start_date = NULL,
  end_date = NULL,
  solID = NULL,
  totals = TRUE,
  non_promo = FALSE,
  marginals = FALSE,
  carryovers = FALSE,
  new_version = FALSE,
  quiet = FALSE,
  ...
)
```

## Arguments

- InputCollect, OutputCollect:

  Robyn output objects.

- start_date, end_date:

  Date. Start and end date to filter the data to be reported.

- solID:

  Character. Single ID of the model to report. If there's only one
  available in OutputCollect, no need to define.

- totals:

  Boolean. Add total rows. This includes summary rows (promotional which
  is paid and organic channels, baseline, grand total).

- non_promo:

  Boolean. Add non-promotional responses as well?

- marginals:

  Boolean. Include mROAS or mCPA marginal performance metric as an
  additional column called "marginal". Calculations are based on mean
  spend and mean response with mean carryover results, between
  `start_date` and `end_date`.

- carryovers:

  Boolean. Add mean percentage of carryover response for date range
  between `start_date` and `end_date` on paid channels. Keep in mind
  organic variables also have carryover but currently not showing.

- new_version:

  Boolean. Use dev version's new function for marginal calculations (if
  available)?

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- ...:

  Additional parameters.

## Value

data.frame with results on ROAS/CPA, spend, response, contribution per
channel, with or without total rows.

## See also

Other Robyn:
[`robyn_hypsbuilder()`](https://laresbernardo.github.io/lares/reference/robyn_hypsbuilder.md),
[`robyn_marginal()`](https://laresbernardo.github.io/lares/reference/robyn_marginal.md),
[`robyn_modelselector()`](https://laresbernardo.github.io/lares/reference/robyn_modelselector.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# You may load an exported model to recreate Robyn objects
mod <- Robyn::robyn_recreate(json_file = "your_model.json")
robyn_performance(mod$InputCollect, mod$OutputCollect)
} # }
```
