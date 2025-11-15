# Clean text strings automatically

`cleanText`: Clean character strings automatically. Options to keep
ASCII characters only, keep certain characters, lower caps, title
format, are available.

`cleanNames`: Resulting names are unique and consist only of the `_`
character, numbers, and ASCII letters. Capitalization preferences can be
specified using the `lower` parameter.

## Usage

``` r
cleanText(
  text,
  spaces = TRUE,
  keep = "",
  lower = TRUE,
  ascii = TRUE,
  title = FALSE
)

cleanNames(df, num = "x", keep = "_", ...)
```

## Arguments

- text:

  Character Vector

- spaces:

  Boolean. Keep spaces? If character input, spaces will be transformed
  into passed argument.

- keep:

  Character. String (concatenated or as vector) with all characters that
  are accepted and should be kept, in addition to alphanumeric.

- lower:

  Boolean. Transform all to lower case?

- ascii:

  Boolean. Only ASCII characters?

- title:

  Boolean. Transform to title format (upper case on first letters).

- df:

  data.frame/tibble.

- num:

  Add character before only-numeric names.

- ...:

  Additional parameters passed to `cleanText()`.

## Value

Character vector with transformed strings.

data.frame/tibble with transformed column names.

## Details

Inspired by `janitor::clean_names`.

## See also

Other Data Wrangling:
[`balance_data()`](https://laresbernardo.github.io/lares/reference/balance_data.md),
[`categ_reducer()`](https://laresbernardo.github.io/lares/reference/categ_reducer.md),
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

Other Text Mining:
[`ngrams()`](https://laresbernardo.github.io/lares/reference/ngrams.md),
[`remove_stopwords()`](https://laresbernardo.github.io/lares/reference/remove_stopwords.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`sentimentBreakdown()`](https://laresbernardo.github.io/lares/reference/sentimentBreakdown.md),
[`textCloud()`](https://laresbernardo.github.io/lares/reference/textCloud.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`topics_rake()`](https://laresbernardo.github.io/lares/reference/topics_rake.md)

## Examples

``` r
cleanText("Bernardo Lares 123")
#> [1] "bernardo lares 123"
cleanText("Bèrnärdo LáreS 123", lower = FALSE)
#> [1] "Bernardo LareS 123"
cleanText("Bernardo Lare$", spaces = ".", ascii = FALSE)
#> [1] "bernardo lare"
cleanText("\\@®ì÷å   %ñS  ..-X", spaces = FALSE)
#> [1] "riansx"
cleanText(c("maría", "€", "núñez_a."), title = TRUE)
#> [1] "Maria"  "Eur"    "Nuneza"
cleanText("29_Feb-92()#", keep = c("#", "_"), spaces = FALSE)
#> [1] "29_feb92#"

# For a data.frame directly:
df <- dft[1:5, 1:6] # Dummy data
colnames(df) <- c("ID.", "34", "x_2", "Num 123", "Nòn-äscì", "  white   Spaces  ")
print(df)
#>   ID.    34 x_2 Num 123 Nòn-äscì   white   Spaces  
#> 1   1 FALSE   3    male       22                  1
#> 2   2  TRUE   1  female       38                  1
#> 3   3  TRUE   3  female       26                  0
#> 4   4  TRUE   1  female       35                  1
#> 5   5 FALSE   3    male       35                  0
cleanNames(df)
#>   id   x34 x_2 num_123 nonasci white_spaces
#> 1  1 FALSE   3    male      22            1
#> 2  2  TRUE   1  female      38            1
#> 3  3  TRUE   3  female      26            0
#> 4  4  TRUE   1  female      35            1
#> 5  5 FALSE   3    male      35            0
cleanNames(df, lower = FALSE)
#>   ID   x34 x_2 Num_123 Nonasci white_Spaces
#> 1  1 FALSE   3    male      22            1
#> 2  2  TRUE   1  female      38            1
#> 3  3  TRUE   3  female      26            0
#> 4  4  TRUE   1  female      35            1
#> 5  5 FALSE   3    male      35            0
```
