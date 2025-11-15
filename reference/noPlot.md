# Plot Result with Nothing to Plot

This function lets the user print a plot without plot, with a
customizable message. It is quite useful for Shiny renderPlot when using
filters and no data is returned.

## Usage

``` r
noPlot(message = "Nothing to show here!", size = 4.5, ...)
```

## Arguments

- message:

  Character. What message do you wish to show?

- size:

  Numeric. Font size for `message` input.

- ...:

  Additional parameters passed to
  [`theme_lares()`](https://laresbernardo.github.io/lares/reference/theme_lares.md).

## Value

Empty ggplot2 object (with a `message` if set).

## See also

Other Visualization:
[`distr()`](https://laresbernardo.github.io/lares/reference/distr.md),
[`freqs()`](https://laresbernardo.github.io/lares/reference/freqs.md),
[`freqs_df()`](https://laresbernardo.github.io/lares/reference/freqs_df.md),
[`freqs_list()`](https://laresbernardo.github.io/lares/reference/freqs_list.md),
[`freqs_plot()`](https://laresbernardo.github.io/lares/reference/freqs_plot.md),
[`plot_chord()`](https://laresbernardo.github.io/lares/reference/plot_chord.md),
[`plot_survey()`](https://laresbernardo.github.io/lares/reference/plot_survey.md),
[`plot_timeline()`](https://laresbernardo.github.io/lares/reference/plot_timeline.md),
[`tree_var()`](https://laresbernardo.github.io/lares/reference/tree_var.md)

## Examples

``` r
Sys.unsetenv("LARES_FONT") # Temporal
noPlot(message = "No plot to show!")

noPlot(background = "#FF5500", size = 7)
```
