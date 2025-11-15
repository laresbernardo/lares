# ETF's Sectors Breakdown

`etf_sector()` scraps etf.com data for sector breakdown on ETFs. Use
[`splot_etf()`](https://laresbernardo.github.io/lares/reference/stocks_plots.md)
for visualization.

## Usage

``` r
etf_sector(etf = "VTI", quiet = FALSE, cache = TRUE)
```

## Arguments

- etf:

  Character Vector. Which ETFs you wish to scrap?

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- cache:

  Boolean. Use daily cache if available?

## Value

data.frame with ETF break.down data by sector

## See also

Other Investment:
[`splot_summary()`](https://laresbernardo.github.io/lares/reference/stocks_plots.md),
[`stocks_file()`](https://laresbernardo.github.io/lares/reference/stocks_report.md),
[`stocks_quote()`](https://laresbernardo.github.io/lares/reference/stocks_hist.md)

## Examples

``` r
# \donttest{
etf_sector(etf = "VTI")
#> >>> Downloading sectors for each ETF...
#> Some ticks were not found as ETF: 'VTI'
#> No data found for given Tickers!
# }
```
