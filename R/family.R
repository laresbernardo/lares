####################################################################
#' Surnames Order Sequence
#'
#' Generate a sequence of numbers that determines the order in which
#' surnames should be listed based on the number of generations of
#' ancestors you wish to include. This sequence follows the traditional
#' Latin custom of assigning the father's surname first, followed by
#' the mother's surname. The same logic extends systematically to higher
#' generations, ensuring that the order of surnames remains consistent
#' as you move upward through the family tree.
#'
#' @param n Integer. Number of generations to include in the sequence.
#' Notice it will generate a vector with 2^(n-1) values.
#' @return Integer vector.
#' @examples
#' seq_surnames(1)
#' seq_surnames(2)
#' seq_surnames(3)
#' seq_surnames(4)
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
  vals
}
