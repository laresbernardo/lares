# Chords Plot

This auxiliary function plots discrete and continuous values results

## Usage

``` r
plot_chord(
  origin,
  dest,
  weight = 1,
  mg = 3,
  title = "Chord Diagram",
  subtitle = "",
  pal = NA
)
```

## Arguments

- origin, dest:

  Vectors. Origin and destination vectors

- weight:

  Vector. Weight for each chord.

- mg:

  Numeric. Margin adjust for plot in case of need

- title:

  Character. Title for the plot

- subtitle:

  Character. Subtitle for the plot

- pal:

  Vector. Colour pallete. Order matters.

## Value

chordDiagram object

## See also

Other Visualization:
[`distr()`](https://laresbernardo.github.io/lares/reference/distr.md),
[`freqs()`](https://laresbernardo.github.io/lares/reference/freqs.md),
[`freqs_df()`](https://laresbernardo.github.io/lares/reference/freqs_df.md),
[`freqs_list()`](https://laresbernardo.github.io/lares/reference/freqs_list.md),
[`freqs_plot()`](https://laresbernardo.github.io/lares/reference/freqs_plot.md),
[`noPlot()`](https://laresbernardo.github.io/lares/reference/noPlot.md),
[`plot_survey()`](https://laresbernardo.github.io/lares/reference/plot_survey.md),
[`plot_timeline()`](https://laresbernardo.github.io/lares/reference/plot_timeline.md),
[`tree_var()`](https://laresbernardo.github.io/lares/reference/tree_var.md)

## Examples

``` r
# You must have "circlize" library to use this auxiliary function:
if (FALSE) { # \dontrun{
df <- data.frame(from = c(1, 1, 2, 3, 4, 1, 6), to = c(4, 4, 4, 2, 2, NA, NA))
plot_chord(df$from, df$to)
} # }
```
