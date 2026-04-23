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
#> # A tibble: 3,560 × 3
#>    date       count package
#>    <date>     <int> <chr>  
#>  1 2026-04-21   175 lares  
#>  2 2026-04-21 81777 dplyr  
#>  3 2026-04-20   144 lares  
#>  4 2026-04-20 78906 dplyr  
#>  5 2026-04-19    65 lares  
#>  6 2026-04-19 47659 dplyr  
#>  7 2026-04-18    86 lares  
#>  8 2026-04-18 49211 dplyr  
#>  9 2026-04-17   145 lares  
#> 10 2026-04-17 70131 dplyr  
#> # ℹ 3,550 more rows
#> 
#> $plot

#> 
# }
```
