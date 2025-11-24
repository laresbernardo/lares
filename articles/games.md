# Games & Puzzles

## Introduction

Beyond data analysis and machine learning, `lares` includes a collection
of fun game solvers and helpers! Challenge yourself with Wordle,
Scrabble, Sudoku, mazes…

``` r
library(lares)
```

## Wordle

Play or solve Wordle puzzles with validation, hints, and simulations.

### Basic Wordle Validation

``` r
# Check your guess against a word
wordle_check("OPENS", "ABBEY")
```

``` fansi
#> O P E N S
```

``` r
wordle_check("BABES", "ABBEY")
```

``` fansi
#> B A B E S
```

``` r
wordle_check("ABBEY", "ABBEY")
```

``` fansi
#> A B B E Y
```

### Get Wordle Hints

Use
[`scrabble_words()`](https://laresbernardo.github.io/lares/reference/scrabble.md)
to find possible words:

``` r
# After OPENS: O is not in word, P/E/N/S are not in positions 2/3/4/5
hints <- scrabble_words(
  tiles = "abcdefghijklmrtuvwxyz", # Available letters
  exclude_here = list("2" = "p", "3" = "e", "4" = "n", "5" = "s"),
  force_exclude = c("o"),
  force_n = 5, # 5-letter words
  language = "en"
)
```

``` fansi
#> 274869 > 145893 > 8857 > 8857 > 1604 > 1604 > 1604 > 1604 > 1577 > 1577 > 1577 > 1577 
#> 
```

``` r
head(hints, 10)
```

``` fansi
#> # A tibble: 10 × 3
#>    word  scores length
#>    <chr>  <int>  <int>
#>  1 jacky     23      5
#>  2 jumby     23      5
#>  3 mujik     22      5
#>  4 zymic     22      5
#>  5 jambu     21      5
#>  6 jaxie     21      5
#>  7 avyze     20      5
#>  8 bwazi     20      5
#>  9 furzy     20      5
#> 10 jakey     20      5
```

### Wordle Simulation

Simulate solving a Wordle puzzle:

``` r
# Simulate solving with different starting words
simulation <- wordle_simulation(
  input = "SAINT",
  word = "ABBEY",
  seed = 123
)
```

``` fansi
#> S A I N T reduced from 14,855 to 946
```

``` fansi
#> C L A M P reduced from 946 to 195
```

``` fansi
#> H O O K A reduced from 195 to 72
```

``` fansi
#> A R G U E reduced from 72 to 6
```

``` fansi
#> A D D E D reduced from 6 to 1
```

``` fansi
#> A B B E Y reduced from 1 to 1
```

``` r
print(simulation)
#> Seed Word: SAINT
#> Objective Word: ABBEY
#> Iterations: 1
#>   Mean to succeed: 6
#>   Max to succeed: 6 [seed = 1]
```

### Wordle Dictionary

Access word lists for different languages:

``` r
# Get English 5-letter words
en_words <- wordle_dictionary("en")
head(en_words, 20)

# Spanish words
es_words <- wordle_dictionary("es")
```

## Scrabble

Maximize your Scrabble score with word finders and calculators!

### Find Highest-Scoring Words

``` r
# Find best words from your tiles
scrabble_words(
  tiles = "aeiourtn",
  force_max = 8, # Max 8 letters
  language = "en",
  scores = "en"
)
```

``` fansi
#> 115977 > 115977 > 115977 > 115977 > 293 > 293 > 293 > 293 
#> 
```

``` fansi
#> # A tibble: 293 × 3
#>    word    scores length
#>    <chr>    <int>  <int>
#>  1 outearn      9      7
#>  2 rainout      9      7
#>  3 routine      9      7
#>  4 ruinate      9      7
#>  5 taurine      9      7
#>  6 uranite      9      7
#>  7 urinate      9      7
#>  8 aunter       8      6
#>  9 auntie       8      6
#> 10 nature       8      6
#> # ℹ 283 more rows
```

### With Board Constraints

``` r
# Must contain specific letters
scrabble_words(
  tiles = "bernardo",
  force_str = "arn",
  force_max = 7,
  language = "en"
)
```

``` fansi
#> 115977 > 115977 > 115977 > 75329 > 245 > 245 > 245 > 6 > 6 
#> 
```

``` fansi
#> # A tibble: 6 × 3
#>   word   scores length
#>   <chr>   <int>  <int>
#> 1 barned     11      6
#> 2 barn        8      4
#> 3 darner      8      6
#> 4 dearn       7      5
#> 5 darn        6      4
#> 6 earn        5      4
```

### Calculate Word Scores

``` r
# Get point values for each letter
en_scores <- scrabble_points("en")
print(en_scores)
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

# Calculate scores for words
words <- c("QUEEN", "QUIZ", "HELLO")
scrabble_score(words, en_scores)
#>    word scores length
#> 1 QUEEN      0      5
#> 2  QUIZ      0      4
#> 3 HELLO      0      5
```

### Multi-Language Support

``` r
# Spanish Scrabble
scrabble_words(
  tiles = "españa",
  language = "es",
  scores = "es"
)

# French Scrabble
scrabble_words(
  tiles = "bonjour",
  language = "fr",
  scores = "fr"
)
```

## Sudoku

Solve Sudoku puzzles automatically!

### Simple Sudoku

``` r
# Easy puzzle (0 represents empty cells)
trivial <- matrix(c(
  0, 9, 0, 7, 0, 0, 8, 6, 0,
  0, 3, 1, 0, 0, 5, 0, 2, 0,
  8, 0, 6, 0, 0, 0, 0, 0, 0,
  0, 0, 7, 0, 5, 0, 0, 0, 6,
  0, 0, 0, 3, 0, 7, 0, 0, 0,
  5, 0, 0, 0, 1, 0, 7, 0, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 9,
  0, 2, 0, 6, 0, 0, 3, 5, 0,
  0, 5, 4, 0, 0, 8, 0, 7, 0
), nrow = 9, byrow = TRUE)

solution <- sudoku_solver(trivial)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#>  [1,]    2    9    5    7    4    3    8    6    1
#>  [2,]    4    3    1    8    6    5    9    2    7
#>  [3,]    8    7    6    1    9    2    5    4    3
#>  [4,]    3    8    7    4    5    9    2    1    6
#>  [5,]    6    1    2    3    8    7    4    9    5
#>  [6,]    5    4    9    2    1    6    7    3    8
#>  [7,]    7    6    3    5    2    4    1    8    9
#>  [8,]    9    2    8    6    7    1    3    5    4
#>  [9,]    1    5    4    9    3    8    6    7    2
print(solution)
#> [1] TRUE
```

### Complex Sudoku

``` r
# Harder puzzle
difficult <- matrix(c(
  5, 3, 0, 0, 7, 0, 0, 0, 0,
  6, 0, 0, 1, 9, 5, 0, 0, 0,
  0, 9, 8, 0, 0, 0, 0, 6, 0,
  8, 0, 0, 0, 6, 0, 0, 0, 3,
  4, 0, 0, 8, 0, 3, 0, 0, 1,
  7, 0, 0, 0, 2, 0, 0, 0, 6,
  0, 6, 0, 0, 0, 0, 2, 8, 0,
  0, 0, 0, 4, 1, 9, 0, 0, 5,
  0, 0, 0, 0, 8, 0, 0, 7, 9
), nrow = 9, byrow = TRUE)

sudoku_solver(difficult)
```

## Maze Solver

Solve mazes using depth-first search algorithms!

### Basic Maze

``` r
# Create a simple maze (0 = path, 1 = wall)
simple_maze <- matrix(c(
  0, 1, 0, 0, 0,
  0, 1, 0, 1, 0,
  0, 0, 0, 1, 0,
  1, 1, 0, 0, 0,
  0, 0, 0, 1, 0
), nrow = 5, byrow = TRUE)

solution <- maze_solve(
  simple_maze,
  start = c(1, 1),
  end = c(5, 5)
)
#> Setup: Inertia (FALSE) | Aim (TRUE) | Random (FALSE)
#>   Total steps:  6 
#>   Total turns:  4 
#> 
#>    1  2 3  4 5
#> 1  ↓ []       
#> 2  ↘ []   []  
#> 3     ↘   []  
#> 4 [] [] →  ↘  
#> 5         [] X
print(solution)
#> Setup: Inertia (FALSE) | Aim (TRUE) | Random (FALSE)
#>   Total steps:  6 
#>   Total turns:  4 
#> 
#>    1  2 3  4 5
#> 1  ↓ []       
#> 2  ↘ []   []  
#> 3     ↘   []  
#> 4 [] [] →  ↘  
#> 5         [] X
```

### Micromouse Competition Maze

``` r
# Classic Micromouse-style maze
micromouse <- matrix(c(
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 0, 0, 0, 0, 0, 0, 1,
  1, 0, 1, 1, 1, 1, 0, 1,
  1, 0, 1, 0, 0, 0, 0, 1,
  1, 0, 1, 0, 1, 1, 0, 1,
  1, 0, 0, 0, 0, 1, 0, 1,
  1, 0, 1, 1, 0, 0, 0, 1,
  1, 1, 1, 1, 1, 1, 1, 1
), nrow = 8, byrow = TRUE)

maze_solve(
  micromouse,
  start = c(2, 2),
  end = c(7, 7),
  diagonal = FALSE
)
```

### Advanced Options

``` r
# With diagonal movement and aiming toward goal
maze_solve(
  micromouse,
  start = c(2, 2),
  end = c(7, 7),
  diagonal = TRUE, # Allow diagonal moves
  aim = TRUE, # Prefer directions toward goal
  inertia = TRUE, # Prefer continuing in same direction
  timeout = 5 # Max 5 seconds
)
```

## Combining Games & Data Science

Use game functions for:

**Text Analysis:**

``` r
# Find anagrams in your dataset
words <- c("listen", "silent", "hello")
scrabble_words(tiles = "listen", language = "en")
```

**Algorithm Teaching:**

``` r
# Demonstrate recursion with maze solving
maze_solve(simple_maze, start = c(1, 1), end = c(5, 5))
```

**Pattern Recognition:**

``` r
# Wordle simulations for optimal starting words
seeds <- 1:100
results <- lapply(seeds, function(s) {
  wordle_simulation("SAINT", "ABBEY", seed = s, quiet = TRUE)
})
```

## Further Reading

### Package Resources

- **Package documentation:** <https://laresbernardo.github.io/lares/>
- **GitHub repository:** <https://github.com/laresbernardo/lares>
- **Report issues:** <https://github.com/laresbernardo/lares/issues>

### Game References

- **Wordle:** The viral word game by Josh Wardle
- **Sudoku:** Japanese number puzzle
- **Scrabble:** Classic word board game
- **Micromouse:** Autonomous maze-solving competition

## Next Steps

- Explore data analysis features (see Data Wrangling vignette)
- Learn machine learning (see Machine Learning vignette)
- Try API integrations (see API Integrations vignette)
- Check game function documentation:
  [`?wordle_check`](https://laresbernardo.github.io/lares/reference/wordle.md),
  [`?scrabble_words`](https://laresbernardo.github.io/lares/reference/scrabble.md),
  [`?sudoku_solver`](https://laresbernardo.github.io/lares/reference/sudoku_solver.md),
  [`?maze_solve`](https://laresbernardo.github.io/lares/reference/maze_solve.md)
