# Tokenize Vectors into Words

This function transforms texts into words, calculate frequencies,
supress stop words in a given language.

## Usage

``` r
textTokenizer(
  text,
  exclude = NULL,
  lang = NULL,
  min_word_freq = 5,
  min_word_len = 2,
  keep_spaces = FALSE,
  lowercase = TRUE,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_lettt = TRUE,
  laughs = TRUE,
  utf = TRUE,
  df = FALSE,
  h2o = FALSE,
  quiet = FALSE
)
```

## Arguments

- text:

  Character vector. Sentences or texts you wish to tokenize.

- exclude:

  Character vector. Which words do you wish to exclude?

- lang:

  Character. Language in text (used for stop words). Example: "spanish"
  or "english". Set to `NA` to ignore.

- min_word_freq:

  Integer. This will discard words that appear less than \<int\> times.
  Defaults to 2. Set to `NA` to ignore.

- min_word_len:

  Integer. This will discard words that have less than \<int\>
  characters. Defaults to 5. Set to `NA` to ignore.

- keep_spaces:

  Boolean. If you wish to keep spaces in each line to keep unique
  compound words, separated with spaces, set to TRUE. For example, 'one
  two' will be set as 'one_two' and treated as a single word.

- lowercase, remove_numbers, remove_punct:

  Boolean.

- remove_lettt:

  Boolean. Repeated letters (more than 3 consecutive).

- laughs:

  Boolean. Try to unify all laughs texts.

- utf:

  Boolean. Transform all characters to UTF (no accents and crazy
  symbols)

- df:

  Boolean. Return a dataframe with a one-hot-encoding kind of results?
  Each word is a column and returns if word is contained.

- h2o:

  Boolean. Return `H2OFrame`?

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## Value

data.frame. Tokenized words with counters.

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
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`topics_rake()`](https://laresbernardo.github.io/lares/reference/topics_rake.md)
