LARES_DIGITS <- 0:9
LARES_OPERATORS <- c("+", "-", "*", "/")

numble_valid <- function(x,
                         result = eval(parse(text = x)),
                         len = 7, ...) {
  temp <- unlist(strsplit(x, ""))
  if (length(temp) != len) stop(paste("Provide only", len, "elements"))
  digits_check <- temp %in% as.character(LARES_DIGITS)
  ops_check <- temp %in% as.character(LARES_OPERATORS)
  not_valid <- !(digits_check | ops_check)
  if (any(not_valid)) stop("Not valid input values: ", v2t(temp[not_valid]))
  total <- eval(parse(text = x))
  if (total != result) stop(paste("Your operation must equal to", result, "not", total))
}

numble_check <- function(input, solution, len = 7, print = TRUE) {
  numble_valid(solution, len = len)
  result <- eval(parse(text = solution))
  numble_valid(input, result = result, len = len)
  out <- pos_check(input, solution, len = len, print = FALSE)
  if (print) {
    print(out)
    cat(paste(" =", result))
  }
  invisible(out)
}
