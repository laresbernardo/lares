# Sentiment Breakdown on Text

This function searches for relevant words in a given text and adds
sentiments labels (joy, anticipation, surprise, positive, trust, anger,
sadness, fear, negative, disgust) for each of them, using NRC. Then,
makes a summary for all words and plot results.

## Usage

``` r
sentimentBreakdown(
  text,
  lang = "spanish",
  exclude = c("maduro", "que"),
  append_file = NA,
  append_words = NA,
  plot = TRUE,
  subtitle = NA
)
```

## Arguments

- text:

  Character vector

- lang:

  Character. Language in text (used for stop words)

- exclude:

  Character vector. Which word do you wish to exclude?

- append_file:

  Character. Add a dictionary to append. This file must contain at least
  two columns, first with words and second with the sentiment (consider
  sentiments on description).

- append_words:

  Dataframe. Same as append_file but appending data frame with word and
  sentiment directly

- plot:

  Boolean. Plot results summary?

- subtitle:

  Character. Add subtitle to the plot

## Value

List. Contains data.frame with words and sentiments, summary and plot.

## See also

Other Text Mining:
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`ngrams()`](https://laresbernardo.github.io/lares/reference/ngrams.md),
[`remove_stopwords()`](https://laresbernardo.github.io/lares/reference/remove_stopwords.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`textCloud()`](https://laresbernardo.github.io/lares/reference/textCloud.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`topics_rake()`](https://laresbernardo.github.io/lares/reference/topics_rake.md)
