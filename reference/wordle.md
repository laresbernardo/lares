# Wordle Game Validation

Given and input and a word, validate each letter based on Wordle's
rules: correct letter in correct placement (green), correct letter in
wrong placement (yellow), letter is not present (red).

## Usage

``` r
wordle_check(
  input,
  word,
  dictionary = NULL,
  lang_dic = "en",
  method = 3,
  print = TRUE
)

# S3 method for class 'wordle_check'
print(x, print = TRUE, ...)

wordle_dictionary(lang_dic = "en", method = 3, quiet = TRUE)

wordle_simulation(input, word, seed = NULL, quiet = FALSE, ...)

# S3 method for class 'wordle_simulation'
print(x, type = 1, ...)
```

## Arguments

- input:

  Character. Word to validate (5-letters)

- word:

  Character. Word actually answer (5-letters).

- dictionary:

  Character vector. List of valid words. If set to NULL then will use
  modified
  [`scrabble_dictionary()`](https://laresbernardo.github.io/lares/reference/scrabble.md)
  to fetch 5 letter words. Use `lang_dic` param to set language.

- lang_dic:

  Character. Any of: "en", "es". Only used when `dictionary` parameter
  is NULL. Requires internet connection the first time. Uses cache.

- method:

  Integer. 1 for
  [`scrabble_dictionary()`](https://laresbernardo.github.io/lares/reference/scrabble.md),
  3 for scrapping the words taken straight from the game's source code.

- print:

  Boolean. Print validation results?

- x:

  Object to print

- ...:

  Additional parameters.

- quiet:

  Boolean. Do not print words as they are being searched.

- seed:

  Numeric. For reproducibility. Accepts more than one: will run as many
  seeds there are.

- type:

  Integer. 1 for summary and 2 for coloured results.

## Value

Invisible vector with results by letter.

## See also

Other Games:
[`maze_solve()`](https://laresbernardo.github.io/lares/reference/maze_solve.md),
[`scrabble_dictionary()`](https://laresbernardo.github.io/lares/reference/scrabble.md),
[`sudoku_solver()`](https://laresbernardo.github.io/lares/reference/sudoku_solver.md)

## Examples

``` r
word <- "ABBEY"
# Or pick a random one:
# word <- sample(wordle_dictionary("en"), 1)
wordle_check("OPENS", word)
#> O P E N S
wordle_check("BABES", word)
#> B A B E S
wordle_check("KEBAB", word, print = FALSE)
wordle_check("ABYSS", word)
#> A B Y S S
wordle_check("ABBEY", word)
#> A B B E Y
# Feel free to use scrabble_words() for hints
# \donttest{
x <- wordle_simulation(input = "SAINT", word = "ABBEY", seed = 1:3)
#> S A I N T reduced from 14,855 to 946
#> 
#> A L G A E reduced from 946 to 85
#> 
#> A B O R D reduced from 85 to 7
#> 
#> A B A C A reduced from 7 to 2
#> 
#> A B U Z Z reduced from 2 to 1
#> 
#> A B B E Y reduced from 1 to 1
#> 
#> >> Iterations (seed = 1): 6
#> S A I N T
#> A R A B A reduced from 946 to 117
#> 
#> A F L O W reduced from 117 to 36
#> 
#> A G G A G reduced from 36 to 19
#> 
#> A C K E E reduced from 19 to 6
#> 
#> A D D E D reduced from 6 to 1
#> 
#> A B B E Y reduced from 1 to 1
#> 
#> >> Iterations (seed = 2): 7
#> S A I N T
#> C H E A P reduced from 946 to 362
#> 
#> A G L E Y reduced from 362 to 1
#> 
#> A B B E Y reduced from 1 to 1
#> 
#> >> Iterations (seed = 3): 4
#> Elapsed time: 1.71s
print(x)
#> Seed Word: SAINT
#> Objective Word: ABBEY
#> Iterations: 3
#>   Mean to succeed: 5.67
#>   Max to succeed: 7 [seed = 2]
# }
```
