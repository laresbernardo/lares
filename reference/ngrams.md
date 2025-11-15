# Build N-grams and keep most frequent

Build out n-grams for multiple text inputs and keep the n most frequent
combinations.

## Usage

``` r
ngrams(text, ngram = c(2, 3), top = 10, stop_words = NULL, ...)
```

## Arguments

- text:

  Character vector

- ngram:

  Integer vector. Number of continuous n items in text.

- top:

  Integer. Keep n most frequent ngrams only.

- stop_words:

  Character vector. Words to exclude from text. Example: if you want to
  exclude "a", whenever that word appears it will be excluded, but when
  the letter "a" appears in a word, it will remain.

- ...:

  Additional parameters passed to `remove_stopwords`.

## Value

data.frame with ngrams and counters, sorted by frequency.

## See also

Other Text Mining:
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`remove_stopwords()`](https://laresbernardo.github.io/lares/reference/remove_stopwords.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`sentimentBreakdown()`](https://laresbernardo.github.io/lares/reference/sentimentBreakdown.md),
[`textCloud()`](https://laresbernardo.github.io/lares/reference/textCloud.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`topics_rake()`](https://laresbernardo.github.io/lares/reference/topics_rake.md)

## Examples

``` r
# You must have "tidytext" library to use this auxiliary function:
if (FALSE) { # \dontrun{
women <- read.csv("https://bit.ly/3mXJOOi")
x <- women$description
ngrams(x, ngram = c(2, 3), top = 3)
ngrams(x, ngram = 2, top = 6, stop_words = c("a", "is", "of", "the"))
} # }
```
