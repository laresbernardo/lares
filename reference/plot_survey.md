# Visualize Survey Results

This function lets the user plot a survey's result.

## Usage

``` r
plot_survey(answers, ignore = 1, title = NA, subtitle = NA)
```

## Arguments

- answers:

  Dataframe. Answers. Each row a different person. Each column a
  different answer.

- ignore:

  Numeric Vector. Which columns are NOT answers?

- title:

  Character. Title for your plot

- subtitle:

  Character. Subtitle for your plot.

## Value

ggplot2 object

## See also

Other Visualization:
[`distr()`](https://laresbernardo.github.io/lares/reference/distr.md),
[`freqs()`](https://laresbernardo.github.io/lares/reference/freqs.md),
[`freqs_df()`](https://laresbernardo.github.io/lares/reference/freqs_df.md),
[`freqs_list()`](https://laresbernardo.github.io/lares/reference/freqs_list.md),
[`freqs_plot()`](https://laresbernardo.github.io/lares/reference/freqs_plot.md),
[`noPlot()`](https://laresbernardo.github.io/lares/reference/noPlot.md),
[`plot_chord()`](https://laresbernardo.github.io/lares/reference/plot_chord.md),
[`plot_timeline()`](https://laresbernardo.github.io/lares/reference/plot_timeline.md),
[`tree_var()`](https://laresbernardo.github.io/lares/reference/tree_var.md)
