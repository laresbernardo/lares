# Pattern Matching for Letters considering Blanks

Match pattern of letters considering blanks within each element of a
character vector, allowing counted characters between and around each
letter. Used as an auxiliary function for the Scrabble family of
functions.

## Usage

``` r
grepl_letters(x, pattern, blank = "_")
```

## Arguments

- x:

  Character vector

- pattern:

  Character. Character string containing a semi-regular expression which
  uses the following logic: "a_b" means any character that contains "a"
  followed by something followed by "b", anywhere in the string.

- blank:

  Character. String to use between letters.

## Value

Boolean check for each value on `x`.

## Examples

``` r
x <- c("aaaa", "bbbb", "baba", "aabb", "a", "ab")
grepl_letters(x, "ab")
#> [1] FALSE FALSE  TRUE  TRUE FALSE  TRUE
grepl_letters(x, "_ab")
#>  aaaa  bbbb  baba  aabb     a    ab 
#> FALSE FALSE  TRUE  TRUE FALSE FALSE 
grepl_letters(x, "a_a")
#>  aaaa  bbbb  baba  aabb     a    ab 
#>  TRUE FALSE  TRUE FALSE FALSE FALSE 
grepl_letters(x, "c")
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE
```
