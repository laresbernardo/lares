# Surnames Order Sequence

Generate a sequence of numbers that determines the order in which
surnames should be listed based on the number of generations of
ancestors you wish to include. This sequence follows the traditional
Latin custom of assigning the father's surname first, followed by the
mother's surname. The same logic extends systematically to higher
generations, ensuring that the order of surnames remains consistent as
you move upward through the family tree.

## Usage

``` r
seq_surnames(n = 1)
```

## Arguments

- n:

  Integer. Number of generations to include in the sequence. Notice it
  will generate a vector with 2^(n-1) values.

## Value

Integer vector.

## Examples

``` r
seq_surnames(1)
#> [1] 1
seq_surnames(2)
#> [1] 1 2
seq_surnames(3)
#> [1] 1 5 3 7 2 6 4 8
seq_surnames(4)
#>  [1]  1  9  5 13  3 11  7 15  2 10  6 14  4 12  8 16
```
