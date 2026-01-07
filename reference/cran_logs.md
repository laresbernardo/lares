# Download and plot daily downloads of CRAN packages

Download daily downloads stats from CRAN for any package, and plot. It
can also be used as an auxiliary function to plot
(`cranlogs::cran_downloads`) results.

## Usage

``` r
cran_logs(
  input = "lares",
  from = Sys.Date() - 31,
  to = Sys.Date() - 1,
  type = "daily",
  plot = TRUE
)
```

## Arguments

- input:

  Character vector with package names or data.frame product of
  `cranlogs::cran_downloads`.

- from, to:

  Dates. Range of dates to fetch downloads metrics.

- type:

  Character. Any of: "daily" or "total".

- plot:

  Boolean. Create a plot?

## Value

List with data.frame and plot if `plot=TRUE`.

## Examples

``` r
# \donttest{
cran_logs(c("lares", "dplyr"), from = "2021-05-31")
#> $df
#> # A tibble: 3,348 × 3
#>    date       count package
#>    <date>     <int> <chr>  
#>  1 2026-01-05   136 lares  
#>  2 2026-01-05 58412 dplyr  
#>  3 2026-01-04    49 lares  
#>  4 2026-01-04 36854 dplyr  
#>  5 2026-01-03    65 lares  
#>  6 2026-01-03 32671 dplyr  
#>  7 2026-01-02   197 lares  
#>  8 2026-01-02 30885 dplyr  
#>  9 2026-01-01    45 lares  
#> 10 2026-01-01 22980 dplyr  
#> # ℹ 3,338 more rows
#> 
#> $plot

#> 
# }
```
