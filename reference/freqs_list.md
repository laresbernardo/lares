# Frequencies on Lists and UpSet Plot

Visualize frequency of elements on a list, list vector, or vector with
comma separated values. Detect which combinations and elements are the
most frequent and how much they represent of your total observations.
This is similar to the [UpSet Plots](http://vcg.github.io/upset/) which
may be used as an alternative to Venn diagrams.

## Usage

``` r
freqs_list(
  df,
  var = NULL,
  wt = NULL,
  fx = "mean",
  rm.na = FALSE,
  min_elements = 1,
  limit = 10,
  limit_x = NA,
  limit_y = NA,
  tail = TRUE,
  size = 10,
  unique = TRUE,
  abc = FALSE,
  title = "",
  plot = TRUE
)
```

## Arguments

- df:

  Data.frame

- var:

  Variable. Variables you wish to process.

- wt:

  Variable, numeric. Select a numeric column to use in the colour scale,
  used as sum, mean... of those values for each of the combinations.

- fx:

  Character. Set operation: mean, sum

- rm.na:

  Boolean. Remove NA value from `wt`?

- min_elements:

  Integer. Exclude combinations with less than n elements

- limit, limit_x, limit_y:

  Integer. Show top n combinations (x) and/or elements (y). The rest
  will be grouped into a single element. Set argument to 0 to ignore.
  `limit_x`/`limit_y` answer to `limit`'s argument.

- tail:

  Boolean. Show tail grouped into "..." on the plots?

- size:

  Numeric. Text base size

- unique:

  Boolean. a,b = b,a?

- abc:

  Boolean. Do you wish to sort by alphabetical order?

- title:

  Character. Overwrite plot's title with.

- plot:

  Boolean. Plot viz? Will be generated anyways in the output object

## Value

List. data.frame with the data results, elements and combinations.

## See also

Other Frequency:
[`freqs()`](https://laresbernardo.github.io/lares/reference/freqs.md),
[`freqs_df()`](https://laresbernardo.github.io/lares/reference/freqs_df.md),
[`freqs_plot()`](https://laresbernardo.github.io/lares/reference/freqs_plot.md)

Other Exploratory:
[`corr_var()`](https://laresbernardo.github.io/lares/reference/corr_var.md),
[`crosstab()`](https://laresbernardo.github.io/lares/reference/crosstab.md),
[`df_str()`](https://laresbernardo.github.io/lares/reference/df_str.md),
[`distr()`](https://laresbernardo.github.io/lares/reference/distr.md),
[`freqs()`](https://laresbernardo.github.io/lares/reference/freqs.md),
[`freqs_df()`](https://laresbernardo.github.io/lares/reference/freqs_df.md),
[`freqs_plot()`](https://laresbernardo.github.io/lares/reference/freqs_plot.md),
[`lasso_vars()`](https://laresbernardo.github.io/lares/reference/lasso_vars.md),
[`missingness()`](https://laresbernardo.github.io/lares/reference/missingness.md),
[`plot_cats()`](https://laresbernardo.github.io/lares/reference/plot_cats.md),
[`plot_df()`](https://laresbernardo.github.io/lares/reference/plot_df.md),
[`plot_nums()`](https://laresbernardo.github.io/lares/reference/plot_nums.md),
[`tree_var()`](https://laresbernardo.github.io/lares/reference/tree_var.md)

Other Visualization:
[`distr()`](https://laresbernardo.github.io/lares/reference/distr.md),
[`freqs()`](https://laresbernardo.github.io/lares/reference/freqs.md),
[`freqs_df()`](https://laresbernardo.github.io/lares/reference/freqs_df.md),
[`freqs_plot()`](https://laresbernardo.github.io/lares/reference/freqs_plot.md),
[`noPlot()`](https://laresbernardo.github.io/lares/reference/noPlot.md),
[`plot_chord()`](https://laresbernardo.github.io/lares/reference/plot_chord.md),
[`plot_survey()`](https://laresbernardo.github.io/lares/reference/plot_survey.md),
[`plot_timeline()`](https://laresbernardo.github.io/lares/reference/plot_timeline.md),
[`tree_var()`](https://laresbernardo.github.io/lares/reference/tree_var.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- dplyr::starwars
head(df[, c(1, 4, 5, 12)], 10)

# Characters per movies combinations in a list column
head(df$films, 2)
freqs_list(df, films)

# Skin colours in a comma-separated column
head(df$skin_color)
x <- freqs_list(df, skin_color, min_elements = 2, limit = 5, plot = FALSE)
# Inside "x" we'll have:
names(x)

# Using the 'wt' argument to add a continuous value metric
# into an already one-hot encoded columns dataset (and hide tail)
csv <- "https://raw.githubusercontent.com/hms-dbmi/UpSetR/master/inst/extdata/movies.csv"
movies <- read.csv(csv, sep = ";")
head(movies)
freqs_list(movies,
  wt = AvgRating, min_elements = 2, tail = FALSE,
  title = "Movies\nMixed Genres\nRanking"
)
# So, please: no more Comedy+SciFi and more Drama+Horror films (based on ~50 movies)!
} # }
```
