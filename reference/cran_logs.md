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
#> # A tibble: 3,372 × 3
#>    date       count package
#>    <date>     <int> <chr>  
#>  1 2026-01-17   112 lares  
#>  2 2026-01-17 31850 dplyr  
#>  3 2026-01-16   222 lares  
#>  4 2026-01-16 64284 dplyr  
#>  5 2026-01-15   163 lares  
#>  6 2026-01-15 63255 dplyr  
#>  7 2026-01-14   223 lares  
#>  8 2026-01-14 64065 dplyr  
#>  9 2026-01-13   155 lares  
#> 10 2026-01-13 96397 dplyr  
#> # ℹ 3,362 more rows
#> 
#> $plot

#> 
# }
```
