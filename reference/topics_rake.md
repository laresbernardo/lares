# Keyword/Topic identification using RAKE

RAKE is a basic algorithm which tries to identify keywords in text.
Based on `udpipe` library, model models, and keywords_rake function.

## Usage

``` r
topics_rake(text, file = "english-ewt-ud-2.4-190531.udpipe", lang = "english")
```

## Arguments

- text:

  Character vector

- file:

  Character. Name of `udpipe` model previously downloaded for a specific
  language

- lang:

  Character. If file does not exist, this language will be downloaded
  from `udpipe`'s models.

## Value

data.frame with topics for each `text` input.

## See also

Other Text Mining:
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`ngrams()`](https://laresbernardo.github.io/lares/reference/ngrams.md),
[`remove_stopwords()`](https://laresbernardo.github.io/lares/reference/remove_stopwords.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`sentimentBreakdown()`](https://laresbernardo.github.io/lares/reference/sentimentBreakdown.md),
[`textCloud()`](https://laresbernardo.github.io/lares/reference/textCloud.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md)
