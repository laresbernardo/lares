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
#> # A tibble: 3,244 × 3
#>    date       count package
#>    <date>     <int> <chr>  
#>  1 2025-11-14   244 lares  
#>  2 2025-11-14 67645 dplyr  
#>  3 2025-11-13   321 lares  
#>  4 2025-11-13 77061 dplyr  
#>  5 2025-11-12   325 lares  
#>  6 2025-11-12 87576 dplyr  
#>  7 2025-11-11   358 lares  
#>  8 2025-11-11 81530 dplyr  
#>  9 2025-11-10   328 lares  
#> 10 2025-11-10 74422 dplyr  
#> # ℹ 3,234 more rows
#> 
#> $plot

#> 
# }
```
