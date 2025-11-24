# Calculate and Visualize Missingness

This function lets the user calculate the percentage of NAs or
missingness in a data.frame. It also plots the results if needed.

## Usage

``` r
missingness(df, plot = FALSE, full = FALSE, subtitle = NA, summary = TRUE)
```

## Arguments

- df:

  Dataframe. Dataframe to study

- plot:

  Boolean. Do you wish to plot results?

- full:

  Boolean. Return all variables (or only with missings)?

- subtitle:

  Character. Subtitle to show in plot

- summary:

  Boolean. Show numerical summary text?

## Value

data.frame with each variable, number of missing values and percentage.
If `plot=TRUE`, a plot with the same information reflected.

## See also

Other Exploratory:
[`corr_var()`](https://laresbernardo.github.io/lares/reference/corr_var.md),
[`crosstab()`](https://laresbernardo.github.io/lares/reference/crosstab.md),
[`df_str()`](https://laresbernardo.github.io/lares/reference/df_str.md),
[`distr()`](https://laresbernardo.github.io/lares/reference/distr.md),
[`freqs()`](https://laresbernardo.github.io/lares/reference/freqs.md),
[`freqs_df()`](https://laresbernardo.github.io/lares/reference/freqs_df.md),
[`freqs_list()`](https://laresbernardo.github.io/lares/reference/freqs_list.md),
[`freqs_plot()`](https://laresbernardo.github.io/lares/reference/freqs_plot.md),
[`lasso_vars()`](https://laresbernardo.github.io/lares/reference/lasso_vars.md),
[`plot_cats()`](https://laresbernardo.github.io/lares/reference/plot_cats.md),
[`plot_df()`](https://laresbernardo.github.io/lares/reference/plot_df.md),
[`plot_nums()`](https://laresbernardo.github.io/lares/reference/plot_nums.md),
[`tree_var()`](https://laresbernardo.github.io/lares/reference/tree_var.md)

Other Missing Values:
[`impute()`](https://laresbernardo.github.io/lares/reference/impute.md)

## Examples

``` r
Sys.unsetenv("LARES_FONT") # Temporal

# Dummy data
df <- data.frame(
  A = c(1:5),
  B = c(NA, NA, 1, 1, 1),
  C = rep(NA, 5),
  D = c(NA, LETTERS[1:4])
)

# Missing values summary
missingness(df)
#>   variable missing missingness
#> 1        C       5         100
#> 2        B       2          40
#> 3        D       1          20

# Visual results

missingness(df, plot = TRUE)


# Show all variables (including those with no missing values)
missingness(df, plot = TRUE, full = TRUE)
```
