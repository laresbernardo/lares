####################################################################
#' Maze Solver, inspired by Micromouse competitions
#'
#' Modified recursive depth-first search (DFS) algorithm to solve mazes.
#' It explores the maze by recursively moving to adjacent cells until it finds a
#' path from the starting point to the destination. Contains options to
#' maximize paths by trying to turn less, allowing diagonal turns, prioritizing
#' turns that chooses next step pointing towards the end point, and a grid
#' search combining parameters to find best route.
#'
#' @family Maze
#' @inheritParams corr_cross
#' @param maze Matrix. Using 0 for open space and 1 for walls.
#' @param start,end Integer vector, length 2. Start and end coordinates.
#' @param inertia Boolean. When enabled, algorithm will check for new
#' directions only when impossible to continue in a straight line.
#' @param aim Boolean. When enabled, algorithm will try first the directions
#' closer to the \code{end} point, ranked and sorted by shorter distances.
#' @param diagonal Boolean. When enabled, algorithm will have 8 degrees of
#' freedom to move, if not, only 4 (up, down, left, right).
#' @param random Boolean. When enabled, algorithm will pick next direction
#' randomly.
#' @param timeout Numeric. How many seconds set for timeout to force
#' algorithm to stop trying new paths?
#' @param seed Numeric. Seed to replicate random results.
#' @return List with data.frame containing solved solution, data.frame with
#' path coordinates and directions, steps counter and turns counter.
#' @examples
#' micromouse <- matrix(c(
#'   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'   1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
#'   1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1,
#'   1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
#'   1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1,
#'   1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1,
#'   1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1,
#'   1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
#'   1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1,
#'   1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
#'   1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
#'   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#' ), nrow = 12, byrow = TRUE)
#' maze_solve(micromouse, start = c(2, 2), end = c(7, 7))
#' @export
maze_solve <- function(
    maze, start = c(1, 1), end = dim(maze),
    inertia = FALSE, aim = TRUE, diagonal = TRUE, random = FALSE,
    timeout = 4,
    quiet = FALSE,
    seed = NULL,
    ...) {
  
  # Inputs validation
  if (!is.matrix(maze)) {
    stop("Input 'maze' must be a matrix or a data.frame")
  }
  if (!all(start %in% which(maze == 0, arr.ind = TRUE)) ||
    !all(end %in% which(maze == 0, arr.ind = TRUE)) ||
    any(start < 1) || any(end > dim(maze))) {
    stop("Invalid start or end point")
  }
  if (maze[start[1], start[1]] != 0) stop("Starting point must be a 0")
  if (maze[end[1], end[1]] != 0) stop("Ending point must be a 0")
  if (is.null(seed)) seed <- round(1000 * runif(1))
  set.seed(seed)
  if (!random) seed <- NA
  
  # Initialize data frame to store path coordinates
  tic("maze_solve_timeout")
  result <- maze_solve_recursive(maze, start, end, aim, inertia, diagonal, random, timeout)

  # Process results
  if (!isFALSE(result)) {
    colnames(result$path_coords) <- c("row", "col")
    result$path_coords <- rbind(result$path_coords, end)
    result$path_coords$direction <- c(sapply(1:(nrow(result$path_coords) - 1), function(i) {
      calculate_direction(
        result$path_coords$row[i + 1] - result$path_coords$row[i],
        result$path_coords$col[i + 1] - result$path_coords$col[i]
      )
    }), "X")

    colnames(result$maze) <- seq_len(ncol(result$maze))
    result$maze <- as.data.frame(result$maze)
    result$maze[result$maze == 0] <- " "
    result$maze[result$maze == 1] <- "[]"
    for (x in seq_along(result$path_coords$direction)) {
      result$maze[result$path_coords$row[x], result$path_coords$col[x]] <- result$path_coords$direction[x]
    }
    result$steps_counter <- nrow(result$path_coords)
    result$turns_counter <- count_direction_changes(result$path_coords) + 1
  } else {
    result <- list(maze = NULL)  
  }
  result$start <- paste(start, collapse = ",")
  result$end <- paste(end, collapse = ",")
  result$inertia <- inertia
  result$aim <- aim
  result$random <- random
  result$seed <- seed
  
  class(result) <- c("maze_solve", class(result))
  if (!quiet) print(result)
  return(invisible(result))
}

#' @rdname maze_solve
#' @param x maze_solve object
#' @export
print.maze_solve <- function(x, ...) {
  cat(sprintf(
    "Setup: Inertia (%s) | Aim (%s) | Random (%s)%s\n",
    x$inertia, x$aim, x$random,
    ifelse(x$random, sprintf(" | Seed (%s)", x$seed), "")))
  if (!is.null(x$maze)) {
    if (isTRUE(x$coords_inv))
      cat("  [Inverted start and end points]\n")
    cat("  Total steps: ", x$steps_counter, "\n")
    cat("  Total turns: ", x$turns_counter, "\n\n")
    print(x$maze)
  } else {
    cat("No solution found for this maze\n")
  }
}

# Solve Maze recursive function with diagonal movement
maze_solve_recursive <- function(
    maze, current, end,
    aim = TRUE,
    inertia = TRUE,
    diagonal = FALSE,
    random = FALSE,
    timeout = 10,
    path_coords = data.frame(row = integer(0), col = integer(0)),
    prev_direction = NULL) {

  # When solution found or timeout reached, return results
  toci <- toc("maze_solve_timeout", quiet = TRUE)
  timeout_reached <- any(toci$toc - toci$tic > timeout)
  if (any(c(identical(current, end), timeout_reached))) {
    if (timeout_reached) {
      return(FALSE)
    }
    return(list(maze = maze, path_coords = path_coords))
  }

  # Mark the current cell as part of the solution path
  row <- current[1]
  col <- current[2]
  maze[row, col] <- "X"

  # Update path coordinates
  path_coords <- rbind(path_coords, c(row, col))

  # Rank next positions based on minimum distance to goal
  temp <- if (aim) end else c(row, col)
  positions <- rank_positions(row, col, temp[1], temp[2], diagonal)
  if (random) positions <- positions[sample(seq_len(nrow(positions))), ]

  # Ensure that the direction it came from is the first move if inertia is TRUE
  if (!is.null(prev_direction) & isTRUE(inertia)) {
    first_point <- linear_extrapolation(prev_direction[1], prev_direction[2], row, col)
    if (all(!is.na(first_point))) {
      skip <- which(positions$x == first_point[1] & positions$y == first_point[2])
      if (length(skip) > 0) {
        positions <- rbind(first_point, positions[-skip, -3])
      }
    }
  }

  for (i in seq_len(nrow(positions))) {
    next_row <- positions$x[i]
    next_col <- positions$y[i]

    # Check if the next cell is within bounds and is an open path
    if (next_row > 0 && next_row <= nrow(maze) &&
      next_col > 0 && next_col <= ncol(maze) &&
      maze[next_row, next_col] == 0) {
      # Recursively explore the next cell
      nexti <- c(next_row, next_col)
      result <- maze_solve_recursive(
        maze, nexti, end, aim, inertia, diagonal, random, 
        timeout, path_coords,
        prev_direction = c(row, col)
      )
      if (!is.logical(result)) {
        return(result)
      }
    }
  }
  return(FALSE) # No solution found from this point
}

#' @rdname maze_solve
#' @export
maze_gridsearch <- function(
    maze,
    start = c(2, 2),
    end = round(dim(maze) / 2),
    quiet = TRUE,
    seed = 123, ...) {
  results <- list()
  for (a in c(TRUE, FALSE)) {
    for (b in c(TRUE, FALSE)) {
      for (c in c(TRUE, FALSE)) {
        for (d in c(TRUE, FALSE)) {
          this <- maze_solve(
            maze,
            start = start, end = end,
            inertia = a, diagonal = b, random = c, aim = d,
            quiet = quiet, seed = seed, ...
          )
          if (!is.logical(this)) {
            this$coords_inv <- FALSE
            results <- append(results, list(this))   
          }
          this <- maze_solve(
            maze,
            start = end, end = start,
            inertia = a, diagonal = b, random = c, aim = d,
            quiet = quiet, seed = seed, ...
          )
          if (!is.logical(this)) {
            this$coords_inv <- TRUE
            results <- append(results, list(this))   
          }
        }
      }
    }
  }
  counters <- dplyr::bind_rows(lapply(results, function(y)
    y[unlist(lapply(y, function(x) !is.data.frame(x) && !is.null(x)))]))
  counters <- data.frame(id = seq_along(results), counters) %>%
    arrange(.data$steps_counter, .data$turns_counter) %>%
    select(.data$id, contains("counter"), everything(), .data$start, .data$end) %>%
    data.frame()
  return(list(solutions = results, results = counters))
}

# Function to calculate the number of direction changes
count_direction_changes <- function(path_coords) {
  if (!is.null(path_coords)) {
    # Calculate the direction for each step
    directions <- atan2(diff(path_coords$row), diff(path_coords$col)) * (180 / pi)
    # Adjust angles to be between 0 and 360 degrees
    directions <- (directions + 360) %% 360
    # Calculate the absolute difference between consecutive directions
    direction_diff <- abs(diff(directions))
    # Count the number of direction changes greater than a threshold (e.g., 45 degrees)
    num_changes <- sum(direction_diff > 30)
    return(num_changes)
  } else {
    return(0)
  }
}

# Function to calculate the direction of point in path
calculate_direction <- function(row, col) {
  if (is.na(row) || is.na(col) || length(row) == 0 || length(col) == 0) {
    return("\u003F")  # ?
  }
  if (row == 0 && col == 1) {
    return("\u2192")  # Right
  } else if (row == 0 && col == -1) {
    return("\u2190")  # Left
  } else if (row == 1 && col == 0) {
    return("\u2193")  # Down
  } else if (row == -1 && col == 0) {
    return("\u2191")  # Up
  } else if (row == 1 && col == 1) {
    return("\u2198")  # Diagonal down-right
  } else if (row == -1 && col == 1) {
    return("\u2197")  # Diagonal up-right
  } else if (row == 1 && col == -1) {
    return("\u2199")  # Diagonal down-left
  } else if (row == -1 && col == -1) {
    return("\u2196")  # Diagonal up-left
  } else if (row == 0 && col == 0) {
    return("\u2022")  # Center (no movement)
  } else {
    return("Invalid direction")
  }
}

# Order of directions based on minimum distance
rank_positions <- function(x1, y1, x2, y2, diagonal = TRUE) {
  if (diagonal) {
    positions <- data.frame(
      x = c(x1 - 1, x1, x1 + 1, x1 - 1, x1 + 1, x1 - 1, x1, x1 + 1),
      y = c(y1 + 1, y1 + 1, y1 + 1, y1, y1, y1 - 1, y1 - 1, y1 - 1)
    )
  } else {
    positions <- data.frame(
      x = c(x1, x1 + 1, x1, x1 - 1),
      y = c(y1 + 1, y1, y1 - 1, y1)
    )
  }
  positions$d <- sqrt((positions$x - x2)^2 + (positions$y - y2)^2)
  ranked_positions <- positions[order(positions$d), ]
  return(ranked_positions)
}

# Function to perform linear extrapolation and return the next integer point
linear_extrapolation <- function(x1, y1, x2, y2) {
  # Calculate the slope
  slope <- (y2 - y1) / (x2 - x1)
  # Extrapolate to the next integer x value
  next_x <- x2 + 1 # Assuming you want to extrapolate to the next integer x value
  # Calculate the corresponding y value
  next_y <- y2 + slope * (next_x - x2)
  return(c(round(next_x), round((next_y))))
}

# micromouse <- matrix(c(
#   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#   1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
#   1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1,
#   1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
#   1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1,
#   1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1,
#   1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1,
#   1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
#   1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1,
#   1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
#   1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
#   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
# ), nrow = 12, byrow = TRUE)

# temp1 <- maze_gridsearch(micromouse, end = c(7, 7))
# counters <- data.frame(
#   steps_counter = unlist(lapply(temp1, function(x) x$steps_counter)),
#   turns_counter = unlist(lapply(temp1, function(x) x$turns_counter)))
# counters[order(counters$steps_counter), ]
# temp1[[6]]
