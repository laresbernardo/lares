# Google Trends: Related and Time Plots

This function creates a plot with Google Trend's related topics and
queries, and let the user compare different keywords.

This function creates a plot with google trend's data on timelines and
let the user compare different keywords.

## Usage

``` r
gtrends_related(gtrend, top = NA, title = NA, note = NA, exclude = NULL)

gtrends_time(gtrend, title = NA)
```

## Arguments

- gtrend:

  List. Result from `gtrendsR::gtrends(keyword, geo, time)`

- top:

  Integer. Filter top n results only.

- title:

  Character. Custom title for the plot.

- note:

  Character. Add a note to the plot if needed.

- exclude:

  Character vector. Which observations do you wish to exclude?

## Value

plot for Google Trend's results input `gtrend`.

Plot for Google Trend's results input `gtrend`.

## See also

Other Scrapper:
[`filesGD()`](https://laresbernardo.github.io/lares/reference/filesGD.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`ip_data()`](https://laresbernardo.github.io/lares/reference/ip_data.md),
[`mp3_get()`](https://laresbernardo.github.io/lares/reference/mp3_get.md),
[`readGS()`](https://laresbernardo.github.io/lares/reference/google_sheets.md),
[`splot_summary()`](https://laresbernardo.github.io/lares/reference/stocks_plots.md),
[`stocks_quote()`](https://laresbernardo.github.io/lares/reference/stocks_hist.md)

Other Google:
[`filesGD()`](https://laresbernardo.github.io/lares/reference/filesGD.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`readGS()`](https://laresbernardo.github.io/lares/reference/google_sheets.md)
