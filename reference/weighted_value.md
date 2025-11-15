# Calculate weighted stock values using FIFO/LIFO

Calculate weighted stock values using FIFO/LIFO

## Usage

``` r
weighted_value(
  value,
  n = rep(1, length(value)),
  technique = NULL,
  n_stocks = NULL,
  buy_only = TRUE,
  type = 1,
  ...
)
```

## Arguments

- value:

  Numeric vector. Representing the values of the stock.

- n:

  Numeric vector. Representing the volume of the operation. Positive for
  'Buy' and negative for 'Sale'.

- technique:

  Character. Pick any of FIFO or LIFO, or NULL to skip.

- n_stocks:

  Integer. Specify the number of stocks to consider. By default will sum
  positive values of `n`.

- buy_only:

  Boolean. Consider only buy (positive) values?

- type:

  Integer. 1 for returning the value, 2 for returning the data.frame
  with the details ("df" attribute)

- ...:

  Additional parameters.

## Value

The calculated weighted mean value.

## Examples

``` r
values <- c(10, 20, 30, 40, 50)
weights <- c(2, 3, -4, 5, 6)
mean(values)
#> [1] 30
weighted_value(values)
#> [1] 30
#> attr(,"df")
#>   value n total
#> 1    10 1     1
#> 2    20 1     1
#> 3    30 1     1
#> 4    40 1     1
#> 5    50 1     1
weighted.mean(values, weights)
#> [1] 38.33333
weighted_value(values, weights, buy_only = FALSE)
#> [1] 38.33333
#> attr(,"df")
#>   value  n total
#> 1    10  2     2
#> 2    20  3     3
#> 3    30 -4    -4
#> 4    40  5     5
#> 5    50  6     6
# Using FIFO and LIFO
weighted_value(values, weights, "FIFO")
#> [1] 31.66667
#> attr(,"df")
#>   value n total n_stocks
#> 1    10 2     2       12
#> 2    20 3     3       12
#> 3    40 5     5       12
#> 4    50 6     2       12
weighted_value(values, weights, "LIFO", n_stocks = 8)
#> [1] 47.5
#> attr(,"df")
#>   value n total n_stocks
#> 1    10 2     0        8
#> 2    20 3     0        8
#> 3    40 5     2        8
#> 4    50 6     6        8
```
