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
#> # A tibble: 3,998 × 3
#>    date       count package
#>    <date>     <int> <chr>  
#>  1 2026-11-19   134 lares  
#>  2 2026-11-19 64167 dplyr  
#>  3 2026-11-18    99 lares  
#>  4 2026-11-18 66309 dplyr  
#>  5 2026-11-17    64 lares  
#>  6 2026-11-17 68530 dplyr  
#>  7 2026-11-16   108 lares  
#>  8 2026-11-16 65440 dplyr  
#>  9 2026-11-15    80 lares  
#> 10 2026-11-15 39125 dplyr  
#> # ℹ 3,988 more rows
#> 
#> $plot

#> 
# }
```
