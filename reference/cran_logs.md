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
#> # A tibble: 3,482 × 3
#>    date       count package
#>    <date>     <int> <chr>  
#>  1 2026-03-13    87 lares  
#>  2 2026-03-13 67533 dplyr  
#>  3 2026-03-12   154 lares  
#>  4 2026-03-12 77701 dplyr  
#>  5 2026-03-11   251 lares  
#>  6 2026-03-11 73951 dplyr  
#>  7 2026-03-10   160 lares  
#>  8 2026-03-10 80921 dplyr  
#>  9 2026-03-09   247 lares  
#> 10 2026-03-09 75030 dplyr  
#> # ℹ 3,472 more rows
#> 
#> $plot

#> 
# }
```
