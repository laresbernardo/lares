# One Hot Encoding for a Vector with Comma Separated Values

This function lets the user do one hot encoding on a variable with comma
separated values

## Usage

``` r
ohe_commas(df, ..., sep = ",", noval = "NoVal", remove = FALSE)
```

## Arguments

- df:

  Dataframe. May contain one or more columns with comma separated values
  which will be separated as one hot encoding

- ...:

  Variables. Which variables to split into new columns?

- sep:

  Character. Which regular expression separates the elements?

- noval:

  Character. No value text

- remove:

  Boolean. Remove original variables?

## Value

data.frame on which all features are numerical by nature or transformed
with one hot encoding.

## See also

Other Data Wrangling:
[`balance_data()`](https://laresbernardo.github.io/lares/reference/balance_data.md),
[`categ_reducer()`](https://laresbernardo.github.io/lares/reference/categ_reducer.md),
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`date_cuts()`](https://laresbernardo.github.io/lares/reference/date_cuts.md),
[`date_feats()`](https://laresbernardo.github.io/lares/reference/date_feats.md),
[`file_name()`](https://laresbernardo.github.io/lares/reference/file_name.md),
[`formatHTML()`](https://laresbernardo.github.io/lares/reference/format_string.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`impute()`](https://laresbernardo.github.io/lares/reference/impute.md),
[`left()`](https://laresbernardo.github.io/lares/reference/left_right.md),
[`normalize()`](https://laresbernardo.github.io/lares/reference/normalize.md),
[`num_abbr()`](https://laresbernardo.github.io/lares/reference/num_abbr.md),
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md),
[`quants()`](https://laresbernardo.github.io/lares/reference/quants.md),
[`removenacols()`](https://laresbernardo.github.io/lares/reference/filterdata.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`replacefactor()`](https://laresbernardo.github.io/lares/reference/replacefactor.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`vector2text()`](https://laresbernardo.github.io/lares/reference/vector2text.md),
[`year_month()`](https://laresbernardo.github.io/lares/reference/year_month.md),
[`zerovar()`](https://laresbernardo.github.io/lares/reference/zerovar.md)

Other One Hot Encoding:
[`date_feats()`](https://laresbernardo.github.io/lares/reference/date_feats.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md)

## Examples

``` r
df <- data.frame(
  id = c(1:5),
  x = c("AA, D", "AA,B", "B,  D", "A,D,B", NA),
  z = c("AA+BB+AA", "AA", "BB,  AA", NA, "BB+AA")
)
ohe_commas(df, x, remove = TRUE)
#> # A tibble: 5 × 7
#>      id z        x_AA  x_D   x_B   x_A   x_NoVal
#>   <int> <chr>    <lgl> <lgl> <lgl> <lgl> <lgl>  
#> 1     1 AA+BB+AA TRUE  TRUE  FALSE FALSE FALSE  
#> 2     2 AA       TRUE  FALSE TRUE  FALSE FALSE  
#> 3     3 BB,  AA  FALSE TRUE  TRUE  FALSE FALSE  
#> 4     4 NA       FALSE TRUE  TRUE  TRUE  FALSE  
#> 5     5 BB+AA    FALSE FALSE FALSE FALSE TRUE   
ohe_commas(df, z, sep = "\\+")
#> # A tibble: 5 × 6
#>      id x     z        z_AA  z_BB  `z_AA, AA, BB,  AA, NoVal, BB`
#>   <int> <chr> <chr>    <lgl> <lgl> <lgl>                         
#> 1     1 AA, D AA+BB+AA TRUE  TRUE  FALSE                         
#> 2     2 AA,B  AA       TRUE  FALSE FALSE                         
#> 3     3 B,  D BB,  AA  FALSE FALSE FALSE                         
#> 4     4 A,D,B NA       FALSE FALSE FALSE                         
#> 5     5 NA    BB+AA    TRUE  TRUE  FALSE                         
ohe_commas(df, x, z)
#> # A tibble: 5 × 13
#>      id x     z        x_AA  z_D   x_B   z_A   x_NoVal `x_AA+BB+AA` z_AA  x_BB 
#>   <int> <chr> <chr>    <lgl> <lgl> <lgl> <lgl> <lgl>   <lgl>        <lgl> <lgl>
#> 1     1 AA, D AA+BB+AA TRUE  TRUE  FALSE FALSE FALSE   TRUE         FALSE FALSE
#> 2     2 AA,B  AA       TRUE  FALSE TRUE  FALSE FALSE   FALSE        TRUE  FALSE
#> 3     3 B,  D BB,  AA  FALSE TRUE  TRUE  FALSE FALSE   FALSE        TRUE  TRUE 
#> 4     4 A,D,B NA       FALSE TRUE  TRUE  TRUE  FALSE   FALSE        FALSE FALSE
#> 5     5 NA    BB+AA    FALSE FALSE FALSE FALSE TRUE    FALSE        FALSE FALSE
#> # ℹ 2 more variables: z_NoVal <lgl>, `x_BB+AA` <lgl>
```
