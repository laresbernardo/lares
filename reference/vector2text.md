# Convert a vector into a comma separated text

Convert a vector into a comma separated text

## Usage

``` r
vector2text(vector, sep = ", ", quotes = TRUE, force_single = FALSE, and = "")

v2t(vector, sep = ", ", quotes = TRUE, force_single = FALSE, and = "")
```

## Arguments

- vector:

  Vector. Vector with more than 1 observation.

- sep:

  Character. String text wished to insert between values.

- quotes:

  Boolean. Bring simple quotes for each observation.

- force_single:

  Boolean. Force single quotes by replacing `\"`.

- and:

  Character. Add 'and' or something before last observation. Not boolean
  variable so it can be used on other languages. Note that the last
  comma will be suppressed if `Sys.getenv("LARES_NUMFORMAT")` is set to
  `1` and you have less than 3 values.

## Value

Vector pasting `vector` values into a single string

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
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`year_month()`](https://laresbernardo.github.io/lares/reference/year_month.md),
[`zerovar()`](https://laresbernardo.github.io/lares/reference/zerovar.md)

## Examples

``` r
vector2text(LETTERS[1:5])
#> [1] "'A', 'B', 'C', 'D', 'E'"
vector2text(c(1:5), quotes = FALSE)
#> [1] "1, 2, 3, 4, 5"
vector2text(c(1:5), quotes = FALSE, sep = "-")
#> [1] "1-2-3-4-5"
vector2text(c(1:5), and = "and also")
#> [1] "1, 2, 3, 4, and also 5"
vector2text(c("Text", "R's"), force_single = TRUE)
#> [1] "'Text', 'R's'"
# Shorter function with same purpose
v2t(LETTERS[1:5])
#> [1] "'A', 'B', 'C', 'D', 'E'"
```
