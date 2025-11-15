# Cross-MMM Budget Optimization across Models

Given a list of recreated Robyn models, this function optimizes budget
allocation across MMM with respective constraints by maximizing
incremental revenue/conversions. This method assumes each model is
independent, that can be compared given its spends were cleanly and
properly split, they modeled the same metric (revenue or conversion) and
units (currency or type of conversion), and date granularity. For best
results, ensure channels have similar granularity across markets to
simplify interpretation and application of the outputs.

Given a list of recreated Robyn models, this function optimizes budget
allocation across MMM with respective constraints by maximizing response
across all channels. This method assumes each model is independent, that
can be compared given its spends were cleanly and properly split, they
modeled the same metric (revenue or conversion) and units (currency or
type of conversion), and date granularity. Recommended to have same
channels granularity across markets to simplify results readings and
application.

## Usage

``` r
robyn_xmodels(
  models,
  initial_budgets = NULL,
  start_dates = NULL,
  end_dates = NULL,
  budget_constr_low = 0.5,
  budget_constr_up = 1.5,
  channel_constr_low = budget_constr_low,
  channel_constr_up = budget_constr_up,
  cores = NULL,
  quiet = FALSE,
  ...
)

# S3 method for class 'robyn_crossmmm'
print(x, ...)

robyn_xchannels(
  models,
  initial_budgets = NULL,
  start_dates = NULL,
  end_dates = NULL,
  channel_constr_low = 0.5,
  channel_constr_up = 2,
  quiet = FALSE,
  ...
)
```

## Arguments

- models:

  Lists. Recreated Robyn models with `robyn_recreate()`.

- initial_budgets:

  Numeric vector. Default will use the total spends per model for the
  specified or total date range. Must be length 1 or same as `models`.

- start_dates, end_dates:

  Character vector. Start and end dates for each specific model. You can
  specify a single date and will be used in all models. Default empty
  value will assume you want all available data and date range. Must be
  length 1 or same as `models`.

- budget_constr_low, budget_constr_up:

  Numeric vector. Relative minimum and maximum budgets to consider based
  on `initial_budgets`. By default it'll consider 50 Must be length 1 or
  same as `models`.

- channel_constr_low, channel_constr_up:

  Numeric vector. Relative lower and upper constraints per channel
  compared with mean spend during the time period defined. If mean was
  zero for date range, historical mean spend value will be used. Must
  have length 1 to replicate for all channels or same length (and order
  )as `paid_media_spends`.

- cores:

  Integer. How many cores to use for parallel computations? Set to 1 to
  not use this option. Default will the minimum between 10 cores and all
  available cores - 1.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- ...:

  Additional parameters to be passed to internal functions.

- x:

  Object to print: robyn_crossmmm() output.

## Value

Invisible vector with results by letter.

List. Contains optimized allocation results and plots.

## Details

This approach is faster and cleaner compared with previous proposal
using `robyn_xmodels()`.

## Examples

``` r
# You must have Robyn installed and some models stored as JSON files
if (FALSE) { # \dontrun{
# Import and recreate the models
files <- c("BrandA.json", "BrandB.json", "BrandC.json", "BrandN.json")
models <- lapply(files, function(x) Robyn::robyn_recreate(x))
names(models) <- gsub("\\.json", "", files)

# Calculate cross-brand optimal allocation
res <- robyn_xmodels(
  models,
  cores = 10,
  start_dates = "2023-01-01",
  end_dates = "2023-12-01"
)
print(res)
res$summary
} # }
if (FALSE) { # \dontrun{
# Calculate cross-brand optimal allocation
res <- robyn_xchannels(
  models,
  start_dates = "2023-01-01",
  end_dates = "2023-12-01"
)
} # }
```
