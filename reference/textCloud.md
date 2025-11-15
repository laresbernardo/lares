# Wordcloud Plot

Study the distribution of a target variable vs another variable. This
function is quite similar to the funModeling's corrplot function.

## Usage

``` r
textCloud(
  text,
  lang = "english",
  exclude = NULL,
  seed = 0,
  keep_spaces = FALSE,
  min = 2,
  pal = NA,
  print = TRUE
)
```

## Arguments

- text:

  Character vector

- lang:

  Character. Language in text (used for stop words)

- exclude:

  Character vector. Which word do you wish to exclude?

- seed:

  Numeric. Seed for re-producible plots

- keep_spaces:

  Boolean. If you wish to keep spaces in each line to keep unique
  compount words, separated with spaces, set to TRUE. For example, 'LA
  ALAMEDA' will be set as 'LA_ALAMEDA' and treated as a single word.

- min:

  Integer. Words with less frequency will not be plotted

- pal:

  Character vector. Which colours do you wish to use

- print:

  Boolean. Plot results as textcloud?

## Value

wordcloud plot object

## See also

Other Text Mining:
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`ngrams()`](https://laresbernardo.github.io/lares/reference/ngrams.md),
[`remove_stopwords()`](https://laresbernardo.github.io/lares/reference/remove_stopwords.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`sentimentBreakdown()`](https://laresbernardo.github.io/lares/reference/sentimentBreakdown.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`topics_rake()`](https://laresbernardo.github.io/lares/reference/topics_rake.md)
