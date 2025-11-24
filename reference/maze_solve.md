# Maze Solver, inspired by Micromouse competitions

Modified recursive depth-first search (DFS) algorithm to solve mazes. It
explores the maze by recursively moving to adjacent cells until it finds
a path from the starting point to the destination. Contains options to
maximize paths by trying to turn less, allowing diagonal turns,
prioritizing turns that chooses next step pointing towards the end
point, and a grid search combining parameters to find best route.

## Usage

``` r
maze_solve(
  maze,
  start = c(1, 1),
  end = dim(maze),
  inertia = FALSE,
  aim = TRUE,
  diagonal = TRUE,
  random = FALSE,
  timeout = 4,
  quiet = FALSE,
  seed = NULL,
  ...
)

# S3 method for class 'maze_solve'
print(x, ...)

maze_gridsearch(
  maze,
  start = c(2, 2),
  end = round(dim(maze)/2),
  quiet = TRUE,
  seed = 123,
  ...
)
```

## Arguments

- maze:

  Matrix. Using 0 for open space and 1 for walls.

- start, end:

  Integer vector, length 2. Start and end coordinates.

- inertia:

  Boolean. When enabled, algorithm will check for new directions only
  when impossible to continue in a straight line.

- aim:

  Boolean. When enabled, algorithm will try first the directions closer
  to the `end` point, ranked and sorted by shorter distances.

- diagonal:

  Boolean. When enabled, algorithm will have 8 degrees of freedom to
  move, if not, only 4 (up, down, left, right).

- random:

  Boolean. When enabled, algorithm will pick next direction randomly.

- timeout:

  Numeric. How many seconds set for timeout to force algorithm to stop
  trying new paths?

- quiet:

  Boolean. Keep quiet? If not, print results

- seed:

  Numeric. Seed to replicate random results.

- ...:

  Additional parameters passed to `corr`

- x:

  maze_solve object

## Value

List with data.frame containing solved solution, data.frame with path
coordinates and directions, steps counter and turns counter.

## See also

Other Games:
[`scrabble_dictionary()`](https://laresbernardo.github.io/lares/reference/scrabble.md),
[`sudoku_solver()`](https://laresbernardo.github.io/lares/reference/sudoku_solver.md),
[`wordle_check()`](https://laresbernardo.github.io/lares/reference/wordle.md)

## Examples

``` r
micromouse <- matrix(c(
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1,
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1,
  1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1,
  1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1,
  1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
  1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1,
  1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
), nrow = 12, byrow = TRUE)
maze_solve(micromouse, start = c(2, 2), end = c(7, 7))
#> Setup: Inertia (FALSE) | Aim (TRUE) | Random (FALSE)
#>   Total steps:  31 
#>   Total turns:  17 
#> 
#>     1  2  3  4  5  6  7  8  9 10 11 12
#> 1  [] [] [] [] [] [] [] [] [] [] [] []
#> 2  []  →  →  →  →  →  →  →  ↘       []
#> 3  []    [] [] [] [] [] [] []  ↙    []
#> 4  []           ↙  ←  ←  ←  ↓       []
#> 5  [] [] []  ↘ [] [] [] []  ↖ [] [] []
#> 6  []           ↙ [] [] []    []    []
#> 7  [] [] []  ↘ [] []  X []    []    []
#> 8  []  ↓  ←  ←  ← []  ↑ []    []    []
#> 9  []  ↘ [] [] [] []  ↑ [] [] []    []
#> 10 []     →  →  ↘ []  ↑       []    []
#> 11 [] []           ↗                []
#> 12 [] [] [] [] [] [] [] [] [] [] [] []
```
