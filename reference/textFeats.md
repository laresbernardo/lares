# Create features out of text

This function creates a data.frame with features based on a text vector

## Usage

``` r
textFeats(text, auto = TRUE, contains = NA, prc = FALSE)
```

## Arguments

- text:

  Character vector

- auto:

  Boolean. Auto create some useful parameters?

- contains:

  Character vector. Which columns do you wish to add with a contains
  (counter) string validator?

- prc:

  Boolean. Also add percentage of each column compared with length?

## Value

data.frame with additional features based on `text`.

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
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`replacefactor()`](https://laresbernardo.github.io/lares/reference/replacefactor.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`vector2text()`](https://laresbernardo.github.io/lares/reference/vector2text.md),
[`year_month()`](https://laresbernardo.github.io/lares/reference/year_month.md),
[`zerovar()`](https://laresbernardo.github.io/lares/reference/zerovar.md)

Other Text Mining:
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`ngrams()`](https://laresbernardo.github.io/lares/reference/ngrams.md),
[`remove_stopwords()`](https://laresbernardo.github.io/lares/reference/remove_stopwords.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`sentimentBreakdown()`](https://laresbernardo.github.io/lares/reference/sentimentBreakdown.md),
[`textCloud()`](https://laresbernardo.github.io/lares/reference/textCloud.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`topics_rake()`](https://laresbernardo.github.io/lares/reference/topics_rake.md)

## Examples

``` r
textFeats("Bernardo Lares")
#> # A tibble: 1 × 12
#>   text     length  ncap  nvoc nexcl nquest  nats npunct  ndig nword nsymb nsmile
#>   <chr>     <int> <int> <int> <int>  <int> <int>  <int> <int> <dbl> <int>  <int>
#> 1 Bernard…     14     2     5     0      0     0      0     0     2     0      0
textFeats("Bernardo Lares 123!", prc = TRUE)
#> # A tibble: 1 × 23
#>   text     length  ncap  nvoc nexcl nquest  nats npunct  ndig nword nsymb nsmile
#>   <chr>     <int> <int> <int> <int>  <int> <int>  <int> <int> <dbl> <int>  <int>
#> 1 Bernard…     19     2     5     1      0     0      1     3     3     0      0
#> # ℹ 11 more variables: length_pct <dbl>, ncap_pct <dbl>, nvoc_pct <dbl>,
#> #   nexcl_pct <dbl>, nquest_pct <dbl>, nats_pct <dbl>, npunct_pct <dbl>,
#> #   ndig_pct <dbl>, nword_pct <dbl>, nsymb_pct <dbl>, nsmile_pct <dbl>
textFeats("I'm 100% Lares...", contains = c("Lares", "lares"))
#> # A tibble: 1 × 14
#>   text     length  ncap  nvoc nexcl nquest  nats npunct  ndig nword nsymb nsmile
#>   <chr>     <int> <int> <int> <int>  <int> <int>  <int> <int> <dbl> <int>  <int>
#> 1 I'm 100…     17     2     3     0      0     0      5     3     3     1      0
#> # ℹ 2 more variables: Lares <int>, lares <int>
textFeats(c("GREAT library!!", "Have you tried this 2?", "Happy faces :D :-)"))
#> # A tibble: 3 × 12
#>   text     length  ncap  nvoc nexcl nquest  nats npunct  ndig nword nsymb nsmile
#>   <chr>     <int> <int> <int> <int>  <int> <int>  <int> <int> <dbl> <int>  <int>
#> 1 GREAT l…     15     5     4     2      0     0      2     0     2     0      0
#> 2 Have yo…     22     1     7     0      1     0      1     1     5     0      0
#> 3 Happy f…     18     2     3     0      0     0      4     0     4     0      2
```
