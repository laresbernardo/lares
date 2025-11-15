# Remove stop-words and patterns from character vector

Remove all stop-words and specific patterns from a character vector

## Usage

``` r
remove_stopwords(text, stop_words, exclude = NULL, sep = " ")
```

## Arguments

- text:

  Character vector

- stop_words:

  Character vector. Words to exclude from text. Example: if you want to
  exclude "a", whenever that word appears it will be excluded, but when
  the letter "a" appears in a word, it will remain.

- exclude:

  Character. Pattern to exclude using regex.

- sep:

  Character. String that separate the terms.

## Value

Character vector with removed texts.

## See also

Other Text Mining:
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`ngrams()`](https://laresbernardo.github.io/lares/reference/ngrams.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`sentimentBreakdown()`](https://laresbernardo.github.io/lares/reference/sentimentBreakdown.md),
[`textCloud()`](https://laresbernardo.github.io/lares/reference/textCloud.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`topics_rake()`](https://laresbernardo.github.io/lares/reference/topics_rake.md)

## Examples

``` r
x <- c("A brown fox jumps over a dog.", "Another brown dog.")
remove_stopwords(x, stop_words = c("dog", "brown", "a"), exclude = "\\.")
#> [1] "fox jumps over" "Another"       
```
