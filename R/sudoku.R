####################################################################
#' Solve Sudoku Puzzles
#'
#' Solve a Sudoku puzzle, where empty values are represented by 0s
#' into a matrix object.
#'
#' @param board Matrix. 9x9 matrix or vector length 81, with only digits from 0 to 9.
#' @param needed_cells,index Auxiliary parameters to auto-iterate using this same fx.
#' @param quiet Boolean. Keep quiet? If not, plot results.
#' @return Logical output answering of the input board can be solved. The
#' actual solved solution will be created as an object named \code{solved}
#' in your \code{.GlobalEnv}.
#' @examples
#' \donttest{
#' # board <- c(0,0,0,0,0,6,0,0,0,
#' #            0,9,5,7,0,0,3,0,0,
#' #            4,0,0,0,9,2,0,0,5,
#' #            7,6,4,0,0,0,0,0,3,
#' #            0,0,0,0,0,0,0,0,0,
#' #            2,0,0,0,0,0,9,7,1,
#' #            5,0,0,2,1,0,0,0,9,
#' #            0,0,7,0,0,5,4,8,0,
#' #            0,0,0,8,0,0,0,0,0)
#' # sudoku_solver(board)
#'
#' # Trivial input (everything)
#' trivial <- matrix(rep(0, 81), byrow = TRUE, ncol = 9)
#' trivial
#' sudoku_solver(trivial)
#'
#' # Wrong / Impossible to solve input
#' imp <- matrix(c(rep(1, 72), rep(0, 9)), byrow = TRUE, ncol = 9)
#' imp
#' sudoku_solver(imp)
#' }
#' @export
sudoku_solver <- function(board, needed_cells = NULL, index = 1, quiet = FALSE) {
  # Convert vector to matrix
  if (is.vector(board)) {
    ints_split <- as.integer(unlist(str_split(board, pattern = "", n = 9 * 9)))
    board <- matrix(ints_split, byrow = TRUE, ncol = 9)
  }

  if (!all(dim(board) == 9)) {
    stop("Check your input's dimensions. 9x9 digits needed.")
  }

  # Determine empty cells to fill
  if (is.null(needed_cells)) {
    needed_cells <- which(board == 0, arr.ind = TRUE)
  }

  # Base case: all cells filled
  if (index > nrow(needed_cells)) {
    if (!quiet) print(board)
    TRUE
  } else {
    row <- needed_cells[index, 1]
    col <- needed_cells[index, 2]

    # Try digits 1 through 9
    solved <- FALSE
    for (num in 1:9) {
      if (.sudoku_valid_input(board, num, row, col)) {
        board2 <- board
        board2[row, col] <- num
        solved <- sudoku_solver(board2, needed_cells, index + 1, quiet)
        if (isTRUE(solved)) break
      }
    }
    solved
  }
}

.sudoku_valid_input <- function(board, i, row, col) {
  invalid_row <- any(board[row, ] == i)
  invalid_col <- any(board[, col] == i)

  box_x <- floor((row - 1) / 3) + 1
  box_y <- floor((col - 1) / 3) + 1
  box <- board[(3 * box_x - 2):(3 * box_x), (3 * box_y - 2):(3 * box_y)]
  invalid_box <- any(box == i)

  !(invalid_row || invalid_col || invalid_box)
}
