####################################################################
#' Solve Sudoku Puzzles
#'
#' Solve a Sudoku puzzle, where empty values are represented by 0s
#' into a matrix object.
#' 
#' @param board Matrix. 9x9 matrix.
#' @param needed_cells,index Auxiliary parameters used to iterate 
#' and solve the input board.
#' @param quiet Boolean. Keep quiet? If not, plot results.
#' @examples 
#' # Valid input board
#' board <- matrix(
#'   c(0,0,0,0,0,6,0,0,0,
#'     0,9,5,7,0,0,3,0,0,
#'     4,0,0,0,9,2,0,0,5,
#'     7,6,4,0,0,0,0,0,3,
#'     0,0,0,0,0,0,0,0,0,
#'     2,0,0,0,0,0,9,7,1,
#'     5,0,0,2,1,0,0,0,9,
#'     0,0,7,0,0,5,4,8,0,
#'     0,0,0,8,0,0,0,0,0),
#'   byrow = TRUE, ncol = 9); board
#' sudoku_solver(board)
#' 
#' # Trivial input (everything)
#' trivial <- matrix(rep(0, 81), byrow = TRUE, ncol = 9); trivial
#' sudoku_solver(trivial)
#' 
#' # Wrong / Impossible to solve input
#' imp <- matrix(c(rep(1, 72), rep(0, 9)), byrow = TRUE, ncol = 9); imp
#' sudoku_solver(imp)
#' @return Logical output answering of the input board can be solved. The
#' actual solved solution will be created as an object named \code{solved}
#' in your \code{.GlobalEnv}.
#' @export
sudoku_solver <- function(board, needed_cells = NULL, index = 1, quiet = FALSE) {
  
  # 1. Find all empty cells
  if (is.null(needed_cells)) 
    needed_cells <- which(board == 0, arr.ind = TRUE)
  
  # 2. Iterate on each empty cell
  if (index > nrow(needed_cells)) {
    solved <<- solved <- board
    if (!quiet) print(solved)
    return(invisible(TRUE))
  } else {
    row <- needed_cells[index, 1]
    col <- needed_cells[index, 2]
  }
  # Test for valid answers
  for (num in 1:9) {
    if (!sudoku_valid_input(board, num, row, col)) {
      next
    } else {
      board2 = board
      board2[row, col] <- num
      # Retest with input
      if (sudoku_solver(board2, needed_cells, index + 1)) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

sudoku_valid_input <- function(board, i, row, col) {
  
  # 1. Check if any cell in the same row has value = i
  if (any(board[row, ] == i)) return(FALSE)
  
  # 2. Check if any cell in the same column has value = i
  if (any(board[, col] == i)) return(FALSE)
  
  # 3. Check boxes
  box_x <- floor((row - 1) / 3) + 1
  box_y <- floor((col - 1) / 3) + 1
  box <- board[(3 * box_x - 2):(3 * box_x), (3 * box_y - 2):(3 * box_y)]
  if (any(box == i)) return(FALSE)
  
  # If everything passes, then valid digit
  return(TRUE)
  
}
