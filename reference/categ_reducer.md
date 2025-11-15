# Reduce categorical values

This function lets the user reduce categorical values in a vector. It is
tidyverse friendly for use on pipelines

## Usage

``` r
categ_reducer(
  df,
  var,
  nmin = 0,
  pmin = 0,
  pcummax = 100,
  top = NA,
  pvalue_max = 1,
  cor_var = "tag",
  limit = 20,
  other_label = "other",
  ...
)
```

## Arguments

- df:

  Categorical Vector

- var:

  Variable. Which variable do you wish to reduce?

- nmin:

  Integer. Number of minimum times a value is repeated

- pmin:

  Numerical. Percentage of minimum times a value is repeated

- pcummax:

  Numerical. Top cumulative percentage of most repeated values

- top:

  Integer. Keep the n most frequently repeated values

- pvalue_max:

  Numeric (0-1\]. Max pvalue categories

- cor_var:

  Character. If pvalue_max \< 1, you must define which column name will
  be compared with (numerical or binary).

- limit:

  Integer. Limit one hot encoding to the n most frequent values of each
  column. Set to `NA` to ignore argument.

- other_label:

  Character. With which text do you wish to replace the filtered values
  with?

- ...:

  Additional parameters.

## Value

data.frame `df` on which `var` has been transformed

## See also

Other Data Wrangling:
[`balance_data()`](https://laresbernardo.github.io/lares/reference/balance_data.md),
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
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`vector2text()`](https://laresbernardo.github.io/lares/reference/vector2text.md),
[`year_month()`](https://laresbernardo.github.io/lares/reference/year_month.md),
[`zerovar()`](https://laresbernardo.github.io/lares/reference/zerovar.md)

## Examples

``` r
data(dft) # Titanic dataset
categ_reducer(dft, Embarked, top = 2) %>% freqs(Embarked)
#> # A tibble: 3 × 5
#>   Embarked     n     p order  pcum
#>   <chr>    <int> <dbl> <int> <dbl>
#> 1 S          644 72.3      1  72.3
#> 2 C          168 18.9      2  91.1
#> 3 other       79  8.87     3 100. 
categ_reducer(dft, Ticket, nmin = 7, other_label = "Other Ticket") %>% freqs(Ticket)
#> # A tibble: 4 × 5
#>   Ticket           n     p order  pcum
#>   <chr>        <int> <dbl> <int> <dbl>
#> 1 Other Ticket   870 97.6      1  97.6
#> 2 1601             7  0.79     2  98.4
#> 3 347082           7  0.79     3  99.2
#> 4 CA. 2343         7  0.79     4 100. 
categ_reducer(dft, Ticket, pvalue_max = 0.05, cor_var = "Survived") %>% freqs(Ticket)
#> Warning: Not a valid input: cor_var_temp was transformed or does not exist.
#>   >> Automatically using 'cor_var_temp_TRUE'
#> # A tibble: 6 × 5
#>   Ticket       n     p order  pcum
#>   <chr>    <int> <dbl> <int> <dbl>
#> 1 other      866 97.2      1  97.2
#> 2 347082       7  0.79     2  98.0
#> 3 CA. 2343     7  0.79     3  98.8
#> 4 113760       4  0.45     4  99.2
#> 5 2666         4  0.45     5  99.7
#> 6 110152       3  0.34     6 100. 
```
