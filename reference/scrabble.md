# Scrabble: Dictionaries

Download words from 4 different languages: English, Spanish, German, and
French. Words will be save into the `temp` directory. This is an
auxiliary function. You may want to use `scrabble_words` directly if you
are searching for the highest score words!

Get score for any word or list of words. You may set manually depending
on the rules and languages you are playing with. Check the examples for
Spanish and English values when I played Words With Friends.

Dataframe for every letter and points given a language.

Find highest score words given a set of letters, rules, and language to
win at Scrabble! You just have to find the best place to post your
tiles.

## Usage

``` r
scrabble_dictionary(lang_dic, quiet = FALSE)

scrabble_score(words, scores.df)

scrabble_points(lang)

scrabble_words(
  tiles = "",
  free = 0,
  force_start = "",
  force_end = "",
  force_str = "",
  force_exclude = "",
  exclude_here = "",
  force_n = 0,
  force_max = 0,
  pattern = "",
  repeated = FALSE,
  language = Sys.getenv("LARES_LANG"),
  scores = language,
  words = NULL,
  quiet = FALSE,
  print = TRUE
)
```

## Arguments

- lang_dic:

  Character. Any of "en","es","de","fr". Set to NULL if you wish to skip
  this step (and use `words` parameter in `scrabble_words` instead).

- quiet:

  Boolean. Do not print words as they are being searched.

- words:

  Character vector. Use if you wish to manually add words.

- scores.df:

  Dataframe. Must contain two columns: "tiles" with every letter of the
  alphabet and "scores" for each letter's score.

- lang:

  Character. Any of "en","es" or "chars". Set to NULL if you wish to
  skip this step (and use `words` parameter in `scrabble_words()`
  instead). The "chars" parameter will score the number of characters a
  word has.

- tiles:

  Character. The letters you wish to consider.

- free:

  Integer. How many free blank tiles you have?

- force_start, force_end:

  Character. Force words to start or end with a pattern of letters and
  position. Examples: "S" or "SO" or "\_\_S_O"... If the string contains
  tiles that were not specified in `tiles`, they will automatically be
  included.

- force_str:

  Character vector. Force words to contain strings. If the string
  contains tiles that were not specified in `tiles`, they will
  automatically be included.

- force_exclude, exclude_here:

  Character vector. Exclude words containing these tiles (and
  positions). Not very relevant on Scrabble but for Wordle.

- force_n, force_max:

  Integer. Force words to be n or max n characters long. Leave 0 to
  ignore parameter.

- pattern:

  Character string. Custom regex patterns you'd like to match.

- repeated:

  Boolean. By default, no replacement allowed. When activated, a single
  tile can be repeated and won't be "used and discarded".

- scores, language:

  Character. Any of "en","es","de","fr". If scores is not any of those
  languages, must be a data.frame that contains two columns: "tiles"
  with every letter of the alphabet and "scores" for each letter's
  score. If you wish to overwrite or complement this dictionaries other
  words you can set to `"none"` and/or use the `words` parameter. You
  might also want to set this parameter globally with
  `Sys.setenv("LARES_LANG" = "en")` and forget about it!

- print:

  Boolean. Print how many words are left by step.

## Value

data.frame with words and language columns.

data.frame with word, scores, and length values for each `word`.

data.frame with tiles and scores for each alphabet letter.

data.frame with matching words found, sorted by higher points.

## See also

Other Games:
[`maze_solve()`](https://laresbernardo.github.io/lares/reference/maze_solve.md),
[`sudoku_solver()`](https://laresbernardo.github.io/lares/reference/sudoku_solver.md),
[`wordle_check()`](https://laresbernardo.github.io/lares/reference/wordle.md)

## Examples

``` r
# \donttest{
if (haveInternet()) {
  # For Spanish words (default)
  es_scores <- scrabble_points("es")
  # Custom scores for each letter
  cu_scores <- data.frame(
    tiles = tolower(LETTERS),
    scores = c(
      1, 1, 1, 1, 1, 1, 1, 5, 1, 1, 5, 2, 4, 2, 1,
      4, 10, 1, 1, 1, 2, 5, 4, 8, 3, 10
    )
  )

  # Score values for each set of rules
  words <- c("Bernardo", "Whiskey", "R is great")
  scrabble_score(words, es_scores)
  scrabble_score(words, cu_scores)
}
#> >>> Points system: 'es'
#>         word scores length
#> 1    Whiskey     16      7
#> 2   Bernardo      8      8
#> 3 R is great      7     10
# }
scrabble_points("es")
#> >>> Points system: 'es'
#>    tiles scores
#> 1      a      1
#> 2      b      3
#> 3      c      2
#> 4      d      2
#> 5      e      1
#> 6      f      4
#> 7      g      3
#> 8      h      4
#> 9      i      1
#> 10     j      8
#> 11     k     10
#> 12     l      1
#> 13     m      3
#> 14     n      1
#> 15     ñ      8
#> 16     o      1
#> 17     p      3
#> 18     q      5
#> 19     r      1
#> 20     s      1
#> 21     t      1
#> 22     u      2
#> 23     v      4
#> 24     w     10
#> 25     x     10
#> 26     y      5
#> 27     z     10
scrabble_points("en")
#> >>> Points system: 'en'
#>    tiles scores
#> 1      a      1
#> 2      b      4
#> 3      c      4
#> 4      d      2
#> 5      e      1
#> 6      f      4
#> 7      g      3
#> 8      h      3
#> 9      i      1
#> 10     j     10
#> 11     k      5
#> 12     l      2
#> 13     m      4
#> 14     n      2
#> 15     o      1
#> 16     p      4
#> 17     q     10
#> 18     r      1
#> 19     s      1
#> 20     t      1
#> 21     u      2
#> 22     v      5
#> 23     w      4
#> 24     x      8
#> 25     y      3
#> 26     z     10
# Not yet available
scrabble_points("fr")
#> There are no points structure for this language/system yet
# \donttest{
if (haveInternet()) {
  # Automatic use of languages and scores
  Sys.setenv("LARES_LANG" = "es")
  scrabble_words(
    tiles = "hola",
    free = 2,
    force_start = "h",
    force_n = 4,
    force_str = "_o_a",
    exclude_here = "__z|j"
  )

  wordle <- c("board", "tempo", "shoes", "hoard")
  scrabble_words(
    language = NULL,
    words = wordle,
    force_n = 5,
    force_str = "O_R"
  )

  # Words considered for a language (you can custom it too!)
  es_words <- scrabble_dictionary("es")
}
#> >>> Points system: 'es'
#> >>> Setting up tiles...
#> Tiles: 'H', 'O', 'L', 'A', '_', '_'
#> >>> Downloading 'es' words. Source: github.com/lorenbrichter/Words
#> > Cache saved succesfully: lares_cache_es
#> >>> Saved (636,598 words) into cache
#> 39735 > 39735 > 2927 > 2927 > 853 > 71 > 71 > 6 > 4 > 4 
#> 
#> >>> Skipping points schema...
#> >>> Setting up tiles...
#> O and R were not in your tiles: now included
#> Tiles: 'O', 'R', '_', '_', '_'
#> >>> Added 4 custom words 
#> 4 > 4 > 4 > 4 > 2 > 2 > 2 > 2 > 2 
#> 
#> > Cache loaded succesfully: lares_cache_es
#> >>> Loaded 636,598 'es' words
# }
```
