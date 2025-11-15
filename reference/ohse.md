# One Hot Smart Encoding (Dummy Variables)

This function lets the user automatically transform a dataframe with
categorical columns into numerical by one hot encoding technic.

## Usage

``` r
ohse(
  df,
  redundant = FALSE,
  drop = TRUE,
  ignore = NULL,
  dates = FALSE,
  holidays = FALSE,
  country = "Venezuela",
  currency_pair = NA,
  trim = 0,
  limit = 10,
  variance = 0.9,
  other_label = "OTHER",
  sep = "_",
  quiet = FALSE,
  ...
)
```

## Arguments

- df:

  Dataframe

- redundant:

  Boolean. Should we keep redundant columns? i.e. If the column only has
  two different values, should we keep both new columns? Is set to
  `NULL`, only binary variables will dump redundant columns.

- drop:

  Boolean. Drop automatically some useless features?

- ignore:

  Vector or character. Which column should be ignored?

- dates:

  Boolean. Do you want the function to create more features out of the
  date/time columns?

- holidays:

  Boolean. Include holidays as new columns?

- country:

  Character or vector. For which countries should the holidays be
  included?

- currency_pair:

  Character. Which currency exchange do you wish to get the history
  from? i.e, USD/COP, EUR/USD...

- trim:

  Integer. Trim names until the nth character

- limit:

  Integer. Limit one hot encoding to the n most frequent values of each
  column. Set to `NA` to ignore argument.

- variance:

  Numeric. Drop columns with more than n variance. Range: 0-1. For
  example: if a variable contains 91 unique different values out of 100
  observations, this column will be suppressed if value is set to 0.9

- other_label:

  Character. With which text do you wish to replace the filtered values
  with?

- sep:

  Character. Separator's string

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- ...:

  Additional parameters.

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
[`ohe_commas()`](https://laresbernardo.github.io/lares/reference/ohe_commas.md),
[`quants()`](https://laresbernardo.github.io/lares/reference/quants.md),
[`removenacols()`](https://laresbernardo.github.io/lares/reference/filterdata.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`replacefactor()`](https://laresbernardo.github.io/lares/reference/replacefactor.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`vector2text()`](https://laresbernardo.github.io/lares/reference/vector2text.md),
[`year_month()`](https://laresbernardo.github.io/lares/reference/year_month.md),
[`zerovar()`](https://laresbernardo.github.io/lares/reference/zerovar.md)

Other Feature Engineering:
[`date_feats()`](https://laresbernardo.github.io/lares/reference/date_feats.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md)

Other One Hot Encoding:
[`date_feats()`](https://laresbernardo.github.io/lares/reference/date_feats.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`ohe_commas()`](https://laresbernardo.github.io/lares/reference/ohe_commas.md)

## Examples

``` r
data(dft)
dft <- dft[, c(2, 3, 5, 9, 11)]

ohse(dft, limit = 3) %>% head(3)
#> >>> One Hot Encoding applied to 3 variables: 'Pclass', 'Embarked', 'Survived'
#> # A tibble: 3 × 8
#>     Age  Fare Survived_TRUE Pclass_1 Pclass_2 Embarked_C Embarked_OTHER
#>   <dbl> <dbl>         <dbl>    <dbl>    <dbl>      <dbl>          <dbl>
#> 1    22  7.25             0        0        0          0              0
#> 2    38 71.3              1        1        0          1              0
#> 3    26  7.92             1        0        0          0              0
#> # ℹ 1 more variable: Embarked_Q <dbl>
ohse(dft, limit = 3, redundant = NULL) %>% head(3)
#> >>> One Hot Encoding applied to 3 variables: 'Pclass', 'Embarked', 'Survived'
#> # A tibble: 3 × 10
#>     Age  Fare Survived_TRUE Pclass_1 Pclass_2 Pclass_3 Embarked_C Embarked_OTHER
#>   <dbl> <dbl>         <dbl>    <dbl>    <dbl>    <dbl>      <dbl>          <dbl>
#> 1    22  7.25             0        0        0        1          0              0
#> 2    38 71.3              1        1        0        0          1              0
#> 3    26  7.92             1        0        0        1          0              0
#> # ℹ 2 more variables: Embarked_Q <dbl>, Embarked_S <dbl>

# Getting rid of columns with no (or too much) variance
dft$no_variance1 <- 0
dft$no_variance2 <- c("A", rep("B", nrow(dft) - 1))
dft$no_variance3 <- as.character(rnorm(nrow(dft)))
dft$no_variance4 <- c(rep("A", 20), round(rnorm(nrow(dft) - 20), 4))
ohse(dft, limit = 3) %>% head(3)
#> >>> One Hot Encoding applied to 4 variables: 'Pclass', 'Embarked', 'Survived', 'no_variance2'
#> # A tibble: 3 × 11
#>     Age  Fare no_variance3    no_variance4 Survived_TRUE no_variance2_B Pclass_1
#>   <dbl> <dbl> <chr>           <chr>                <dbl>          <dbl>    <dbl>
#> 1    22  7.25 0.153373117836… A                        0              0        0
#> 2    38 71.3  -1.13813693701… A                        1              1        1
#> 3    26  7.92 1.253814921069… A                        1              1        0
#> # ℹ 4 more variables: Pclass_2 <dbl>, Embarked_C <dbl>, Embarked_OTHER <dbl>,
#> #   Embarked_Q <dbl>
```
