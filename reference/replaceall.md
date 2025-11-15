# Replace Values With

This function lets the user replace all specific values in a vector or
data.frame into another value. If replacing more than one value, order
matters so they will be replaced in the same order that you pass them to
the function. Factors will be refactored.

## Usage

``` r
replaceall(df, original, change, which = "all", fixclass = TRUE, quiet = TRUE)
```

## Arguments

- df:

  Data.frame or Vector

- original:

  String or Vector. Original text you wish to replace

- change:

  String or Vector. Values you wish to replace the originals with

- which:

  Character vector. Name of columns to use. Leave "all" for everything

- fixclass:

  Boolean. Try to detect logical classes after transformations (or leave
  as default classes as character)?

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## Value

data.frame with replaced values based on inputs.

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
[`ohe_commas()`](https://laresbernardo.github.io/lares/reference/ohe_commas.md),
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md),
[`quants()`](https://laresbernardo.github.io/lares/reference/quants.md),
[`removenacols()`](https://laresbernardo.github.io/lares/reference/filterdata.md),
[`replacefactor()`](https://laresbernardo.github.io/lares/reference/replacefactor.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`vector2text()`](https://laresbernardo.github.io/lares/reference/vector2text.md),
[`year_month()`](https://laresbernardo.github.io/lares/reference/year_month.md),
[`zerovar()`](https://laresbernardo.github.io/lares/reference/zerovar.md)

Other Text Mining:
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`ngrams()`](https://laresbernardo.github.io/lares/reference/ngrams.md),
[`remove_stopwords()`](https://laresbernardo.github.io/lares/reference/remove_stopwords.md),
[`sentimentBreakdown()`](https://laresbernardo.github.io/lares/reference/sentimentBreakdown.md),
[`textCloud()`](https://laresbernardo.github.io/lares/reference/textCloud.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`topics_rake()`](https://laresbernardo.github.io/lares/reference/topics_rake.md)

## Examples

``` r
df <- data.frame(
  one = c(1:4, NA),
  two = LETTERS[1:5],
  three = rep("A", 5),
  four = c(NA, "Aaa", 123, "B", "C")
)
print(df)
#>   one two three four
#> 1   1   A     A <NA>
#> 2   2   B     A  Aaa
#> 3   3   C     A  123
#> 4   4   D     A    B
#> 5  NA   E     A    C

replaceall(df, "A", NA)
#> # A tibble: 5 × 4
#>     one two   three four 
#>   <int> <chr> <lgl> <chr>
#> 1     1 NA    NA    NA   
#> 2     2 B     NA    NA   
#> 3     3 C     NA    123  
#> 4     4 D     NA    B    
#> 5    NA E     NA    C    

replaceall(df, "A", "a")
#> # A tibble: 5 × 4
#>     one two   three four 
#>   <int> <chr> <chr> <chr>
#> 1     1 a     a     NA   
#> 2     2 B     a     aaa  
#> 3     3 C     a     123  
#> 4     4 D     a     B    
#> 5    NA E     a     C    

replaceall(df, 1, "*")
#> # A tibble: 5 × 4
#>   one   two   three four 
#>   <chr> <chr> <chr> <chr>
#> 1 *     A     A     NA   
#> 2 2     B     A     Aaa  
#> 3 3     C     A     *23  
#> 4 4     D     A     B    
#> 5 NA    E     A     C    

replaceall(df, NA, "NotNA")
#> # A tibble: 5 × 4
#>   one   two   three four 
#>   <chr> <chr> <chr> <chr>
#> 1 1     A     A     NotNA
#> 2 2     B     A     Aaa  
#> 3 3     C     A     123  
#> 4 4     D     A     B    
#> 5 NotNA E     A     C    

replaceall(df, NA, 0)
#> # A tibble: 5 × 4
#>     one two   three four 
#>   <int> <chr> <chr> <chr>
#> 1     1 A     A     0    
#> 2     2 B     A     Aaa  
#> 3     3 C     A     123  
#> 4     4 D     A     B    
#> 5     0 E     A     C    

replaceall(df, c("A", "B"), c("'A'", "'B'"))
#> # A tibble: 5 × 4
#>     one two   three four 
#>   <int> <chr> <chr> <chr>
#> 1     1 'A'   'A'   NA   
#> 2     2 'B'   'A'   'A'aa
#> 3     3 C     'A'   123  
#> 4     4 D     'A'   'B'  
#> 5    NA E     'A'   C    

replaceall(df, "a", "*", which = "four")
#> # A tibble: 5 × 4
#>     one two   three four 
#>   <int> <chr> <chr> <chr>
#> 1     1 A     A     NA   
#> 2     2 B     A     A**  
#> 3     3 C     A     123  
#> 4     4 D     A     B    
#> 5    NA E     A     C    
```
