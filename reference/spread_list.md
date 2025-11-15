# Spread list column into new columns

Spread an existing list column into new columns on a data.frame. Note
that every element on every observation must have a name for the
function to do its work. Original column will be automatically
suppressed but you can set the `replace` argument to avoid it.

## Usage

``` r
spread_list(df, col, str = NULL, replace = TRUE)
```

## Arguments

- df:

  Dataframe

- col:

  Variable name.

- str:

  Character. Start column names with. If set to `NULL`, original name of
  column will be used.

- replace:

  Boolean. Replace original values (delete column)

## Value

data.frame. Result of un-nesting named or un-named list columns.

## Examples

``` r
df <- dplyr::starwars
# Un-named list columns
spread_list(df, films, replace = FALSE) %>%
  dplyr::select(name, dplyr::starts_with("films")) %>%
  head(8)
#> # A tibble: 8 × 9
#>   name    films `films_A New Hope` films_Attack of the …¹ films_Return of the …²
#>   <chr>   <lis> <lgl>              <lgl>                  <lgl>                 
#> 1 Luke S… <chr> TRUE               FALSE                  TRUE                  
#> 2 C-3PO   <chr> TRUE               TRUE                   TRUE                  
#> 3 R2-D2   <chr> TRUE               TRUE                   TRUE                  
#> 4 Darth … <chr> TRUE               FALSE                  TRUE                  
#> 5 Leia O… <chr> TRUE               FALSE                  TRUE                  
#> 6 Owen L… <chr> TRUE               TRUE                   FALSE                 
#> 7 Beru W… <chr> TRUE               TRUE                   FALSE                 
#> 8 R5-D4   <chr> TRUE               FALSE                  FALSE                 
#> # ℹ abbreviated names: ¹​`films_Attack of the Clones`,
#> #   ²​`films_Return of the Jedi`
#> # ℹ 4 more variables: `films_Revenge of the Sith` <lgl>,
#> #   `films_The Empire Strikes Back` <lgl>, `films_The Force Awakens` <lgl>,
#> #   `films_The Phantom Menace` <lgl>
# Named (and un-nammed) list columns
df <- dplyr::tibble(id = 1:3, platform = list(
  list("fb" = 1, "ig" = 2),
  list("fb" = 3),
  list()
))
spread_list(df, platform, str = "ptf_")
#> # A tibble: 3 × 3
#>      id ptf_fb ptf_ig
#>   <int>  <dbl>  <dbl>
#> 1     1      1      2
#> 2     2      3      0
#> 3     3     NA     NA
```
