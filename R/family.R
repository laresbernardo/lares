####################################################################
#' Family Tree Surnames Sequence
#'
#' @param n Integer. Number of generations. Notice this will generate sequences
#' of 2^n integer values.
#' @return Integer vector.
#' @examples
#' seq_surnames(1) # Expected: 1
#' seq_surnames(2) # Expected: 1, 2
#' seq_surnames(3) # Expected: 1, 5, 3, 7, 2, 6, 4, 8
#' seq_surnames(4) # Expected: 1, 9, 5, 13, 3, 11, 7, 15, 2, 10, 6, 14, 4, 12, 8, 16
#' @export
seq_surnames <- function(n = 1) {
  stopifnot(!is.integer(n))
  ni <- n - 1 # We calculate only one and then sum 1 to the other half
  for (k in seq(2^ni)) {
    # Always start with 1
    if (k == 1) {
      vals <- NULL
      vals[1] <- 1
    } else {
      # Handle the specific case for n = 2
      if (n == 2) {
        vals[k] <- k
      } else {
        # Generate the first 4 values of seq for larger n
        if (k %in% seq(4)) {
          if (is_odd(k)) {
            vals[k] <- vals[k - 1] - 2^(ni - 1)
          } else {
            vals[k] <- vals[k - 1] + 2^(ni)
          }
        }
      }
    }
  }
  # Now sum the rest of the sequence based on vals
  if (n >= 3) {
    for (i in (n - 2):1) {
      vals <- c(vals, vals + 2^(i - 1))
    }
  }
  return(vals)
}
