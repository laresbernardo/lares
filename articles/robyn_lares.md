# Robyn Enhancers

## Introduction

The `lares` package provides a suite of auxiliary functions to enhance
your experience with
[Robyn](https://github.com/facebookexperimental/Robyn), a Marketing Mix
Modeling (MMM) package developed by Meta’s Marketing Science Team. These
functions help with hyperparameter building, model selection,
performance analysis, and cross-model budget allocation.

## Hyperparameter Builder

The function
[`robyn_hypsbuilder()`](https://laresbernardo.github.io/lares/reference/robyn_hypsbuilder.md)
automates the creation of hyperparameter lists for Robyn. It’s
particularly useful when you have many media channels and want to set
reasonable default ranges.

``` r
# Define your input variables
paid_media_spends <- c("tv_spend", "digital_spend")
organic_vars <- c("newsletter")

# Generate hyperparameters
hyperparameters <- robyn_hypsbuilder(
    channels = c(paid_media_spends, organic_vars),
    media_type = "default",
    adstock = "weibull_pdf", # or "weibull_cdf", "weibull_pdf"
    date_type = "weekly",
    lagged = FALSE
)

# The output is a list ready to be passed to robyn_inputs()
print(hyperparameters)
#> $tv_spend_alphas
#> [1] 0.01 3.00
#> 
#> $tv_spend_gammas
#> [1] 0.3 1.0
#> 
#> $tv_spend_shapes
#> [1]  0 10
#> 
#> $tv_spend_scales
#> [1] 0.0 0.2
#> 
#> $digital_spend_alphas
#> [1] 0.01 3.00
#> 
#> $digital_spend_gammas
#> [1] 0.3 1.0
#> 
#> $digital_spend_shapes
#> [1]  0 10
#> 
#> $digital_spend_scales
#> [1] 0.0 0.2
#> 
#> $newsletter_alphas
#> [1] 0.01 3.00
#> 
#> $newsletter_gammas
#> [1] 0.3 1.0
#> 
#> $newsletter_shapes
#> [1]  0 10
#> 
#> $newsletter_scales
#> [1] 0.0 0.2
#> 
#> $train_size
#> [1] 0.5 0.8
```

## Model Selection

The function
[`robyn_modelselector()`](https://laresbernardo.github.io/lares/reference/robyn_modelselector.md)
provides a visual and metric-based approach to select the best models
from a Robyn run. It ranks models based on their performance metrics and
helps you identify the best candidates to evaluate. For a deeper dive
into this topic, check out this [Medium
post](https://medium.com/@laresbernardo/select-the-right-mmm-candidate-based-on-your-specific-criteria-and-business-knowledge-1f583c3cb97a).

``` r
# Assuming you have a Robyn OutputCollect object
# OutputCollect <- robyn_run(...)

# Select best models
best_models <- robyn_modelselector(
    InputCollect = InputCollect,
    OutputCollect = OutputCollect,
    metrics = c(
        "rsq_train", "performance", "potential_improvement", "non_zeroes",
        "incluster_models", "cluster_sd", "certainty", "baseline_dist"
    ),
    wt = c(2, 0.1, 0, 1, 0.1, 0, 1.5, 0)
)

# View the results
print(best_models)

# Plot the clusters
plot(best_models)
```

## Model Performance

The function
[`robyn_performance()`](https://laresbernardo.github.io/lares/reference/robyn_performance.md)
calculates detailed performance metrics for a specific model or a set of
models. It breaks down contribution, ROAS or CPA for each channel, etc.

``` r
# Get performance for a specific model and date range
performance <- robyn_performance(
    InputCollect = InputCollect,
    OutputCollect = OutputCollect,
    select_model = "1_123_4",
    # Calculate for specific date range
    start_date = NULL, end_date = NULL,
    non_promo = TRUE, # Add non-promo decomposition as well
    marginals = TRUE, # Add mROAS or mCPA for spend levels
    carryovers = FALSE # Add mean percentage of carryover
)

# View performance table
print(performance)

# Plot performance
plot(performance)
```

## Marginal Performance (Experimental)

The function
[`robyn_marginal()`](https://laresbernardo.github.io/lares/reference/robyn_marginal.md)
allows you to explore the marginal returns of your media channels (mROAS
or mCPA).

``` r
# Calculate marginal response
marginal <- robyn_marginal(
    InputCollect = InputCollect,
    OutputCollect = OutputCollect,
    select_model = "1_123_4",
    metric_name = "tv_spend",
    metric_value = 100000
)
```

## Cross-Model Budget Allocation (Experimental)

If you have multiple Robyn models (e.g., for different countries or
brands),
[`robyn_xmodels()`](https://laresbernardo.github.io/lares/reference/robyn_crossmmm.md)
and
[`robyn_xchannels()`](https://laresbernardo.github.io/lares/reference/robyn_crossmmm.md)
help you allocate budget across all of them simultaneously.

``` r
# Load multiple models
model_paths <- c("model_us.json", "model_uk.json", "model_de.json")
models <- lapply(model_paths, Robyn::robyn_recreate)
names(models) <- c("US", "UK", "DE")

# Allocate budget across models
cross_allocation <- robyn_xmodels(
    models = models,
    total_budget = 5000000,
    scenario = "max_response"
)

# View results
print(cross_allocation)
```

## Conclusion

These functions streamline the post-modeling analysis in Robyn, making
it easier to interpret results, select the best models, and make
data-driven budget allocation decisions.

## Additional Reading

For more information on Robyn and MMM, check out the following
resources:

- [Robyn GitHub
  Repository](https://github.com/facebookexperimental/Robyn)
- [Robyn: Analyst’s Guide to
  MMM](https://facebookexperimental.github.io/Robyn/docs/analysts-guide-to-MMM)
- [Select the right MMM candidate based on your specific criteria and
  business
  knowledge](https://medium.com/@laresbernardo/select-the-right-mmm-candidate-based-on-your-specific-criteria-and-business-knowledge-1f583c3cb97a)
- [Stop building MMMs without business-backed
  hypotheses](https://medium.com/@laresbernardo/stop-building-mmms-without-business-backed-hypotheses-0e8a9fa9a375)
- [Hitting ROAS Target Using Robyn’s Budget
  Allocator](https://medium.com/@gufengzhou/hitting-roas-target-using-robyns-budget-allocator-274ace3add4f)
- [The Convergence of Marginal ROAS in the Budget Allocation in
  Robyn](https://medium.com/@gufengzhou/the-convergence-of-marginal-roas-in-the-budget-allocation-in-robyn-5d407aebf021)
