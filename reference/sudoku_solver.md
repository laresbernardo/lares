# Solve Sudoku Puzzles

Solve a Sudoku puzzle, where empty values are represented by 0s into a
matrix object.

## Usage

``` r
sudoku_solver(board, needed_cells = NULL, index = 1, quiet = FALSE)
```

## Arguments

- board:

  Matrix. 9x9 matrix or vector length 81, with only digits from 0 to 9.

- needed_cells, index:

  Auxiliary parameters to auto-iterate using this same fx.

- quiet:

  Boolean. Keep quiet? If not, plot results.

## Value

Logical output answering of the input board can be solved. The actual
solved solution will be created as an object named `solved` in your
`.GlobalEnv`.

## See also

Other Games:
[`maze_solve()`](https://laresbernardo.github.io/lares/reference/maze_solve.md),
[`scrabble_dictionary()`](https://laresbernardo.github.io/lares/reference/scrabble.md),
[`wordle_check()`](https://laresbernardo.github.io/lares/reference/wordle.md)

## Examples

``` r
# \donttest{
# board <- c(0,0,0,0,0,6,0,0,0,
#            0,9,5,7,0,0,3,0,0,
#            4,0,0,0,9,2,0,0,5,
#            7,6,4,0,0,0,0,0,3,
#            0,0,0,0,0,0,0,0,0,
#            2,0,0,0,0,0,9,7,1,
#            5,0,0,2,1,0,0,0,9,
#            0,0,7,0,0,5,4,8,0,
#            0,0,0,8,0,0,0,0,0)
# sudoku_solver(board)

# Trivial input (everything)
trivial <- matrix(rep(0, 81), byrow = TRUE, ncol = 9)
trivial
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#>  [1,]    0    0    0    0    0    0    0    0    0
#>  [2,]    0    0    0    0    0    0    0    0    0
#>  [3,]    0    0    0    0    0    0    0    0    0
#>  [4,]    0    0    0    0    0    0    0    0    0
#>  [5,]    0    0    0    0    0    0    0    0    0
#>  [6,]    0    0    0    0    0    0    0    0    0
#>  [7,]    0    0    0    0    0    0    0    0    0
#>  [8,]    0    0    0    0    0    0    0    0    0
#>  [9,]    0    0    0    0    0    0    0    0    0
sudoku_solver(trivial)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#>  [1,]    1    4    7    2    3    8    5    6    9
#>  [2,]    2    5    8    1    6    9    3    4    7
#>  [3,]    3    6    9    4    5    7    1    2    8
#>  [4,]    4    7    1    3    8    2    6    9    5
#>  [5,]    5    8    2    6    9    1    4    7    3
#>  [6,]    6    9    3    5    7    4    2    8    1
#>  [7,]    7    1    4    8    2    3    9    5    6
#>  [8,]    8    2    5    9    1    6    7    3    4
#>  [9,]    9    3    6    7    4    5    8    1    2
#> [1] TRUE

# Wrong / Impossible to solve input
imp <- matrix(c(rep(1, 72), rep(0, 9)), byrow = TRUE, ncol = 9)
imp
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#>  [1,]    1    1    1    1    1    1    1    1    1
#>  [2,]    1    1    1    1    1    1    1    1    1
#>  [3,]    1    1    1    1    1    1    1    1    1
#>  [4,]    1    1    1    1    1    1    1    1    1
#>  [5,]    1    1    1    1    1    1    1    1    1
#>  [6,]    1    1    1    1    1    1    1    1    1
#>  [7,]    1    1    1    1    1    1    1    1    1
#>  [8,]    1    1    1    1    1    1    1    1    1
#>  [9,]    0    0    0    0    0    0    0    0    0
sudoku_solver(imp)
#> [1] FALSE
# }
```
