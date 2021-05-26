####################################################################
#' Validate options within vector
#' 
#' This function validates if inputs match all/any of your options
#' and return error/message with possible options to use.
#'
#' @param inputs Vector character
#' @param opts Vector character
#' @param type Character. Options: all, any
#' @param not Character. Options: stop, message, print, return
#' @param quiet Boolean. Keep quiet? If not, returns TRUE or FALSE
#' @return Boolean. Result of \code{inputs} in \code{opts} (options).
#' Depending on \code{type} and/or \code{stop} arguments,
#' errors or messages will be shown.
#' @examples 
#' opts <- c("A", "B", "C")
#' # Let's check the "all" logic
#' check_opts(inputs = c("A", "B"), opts, quiet = FALSE)
#' check_opts(inputs = c("X"), opts, not = "message", quiet = FALSE)
#' check_opts(inputs = c("A","X"), opts, not = "warning")
#' # Now let's check the "any" logic
#' check_opts(inputs = c("A","X"), opts, type = "any")
#' check_opts(inputs = c("X"), opts, type = "any", not = "message")
#' check_opts(inputs = c("A", NA), opts, type = "any")
#' # Final trick: just ignore results
#' check_opts(inputs = "X", opts, not = "invisible")
#' @export
check_opts <- function(inputs, opts, 
                       type = "all", not = "stop", 
                       quiet = TRUE) {
  aux <- base::get(type)
  not <- base::get(not)
  isit <- aux(inputs %in% opts)
  if (!isit) {
    if (type == "all")
      inputs <- inputs[which(!inputs %in% opts)]
    not(paste("Your input", vector2text(inputs), 
              "is not valid;", toupper(type),
              "of the inputs should match these options:", 
              vector2text(opts))) 
  }
  if (!quiet) return(isit)
}

####################################################################
#' Attribute checker
#' 
#' This function checks if an object has a specific attribute and
#' stops if not
#' 
#' @param object Object of any kind
#' @param attr Character. Attribute to check
#' @param check Character. Attribute value
#' @param stop Boolean. Stop if doesn't check?
#' @return No return value, called for side effects.
#' @export
check_attr <- function(object, attr, check, stop = TRUE) {
  aux <- attr(object, attr)
  if (is.null(aux)) aux <- "Noclass"
  if (aux != check) {
    msg <- paste("Your object must be", attr, check)
    if (stop) stop(msg) else message(msg)
  }
}

####################################################################
#' Check if input is_* or are_*
#' 
#' Check whether a value or vector is or is not following a set
#' of rules. For example: is an URL, is an ID vector, are non-variant or 
#' constant values, are binary values... Notice that \code{is_} will return 
#' the result for each observation and \code{are_} for the whole vector.
#'
#' @param x Vector
#' @param ... Additional parameters
#' @return \code{is_url}. Boolean. Result of checking if \code{x} is a valid URL string.
#' @examples 
#' is_url(c("google.com","http://google.com"))
#' 
#' is_ip(c("163.114.132.0","7.114.132", "0.0.0.0", "1.1.1.1."))
#' 
#' are_id(1:10)
#' are_id(LETTERS[1:10])
#' 
#' are_constant(rep(1,10))
#' are_constant(1:10)
#' 
#' are_binary(c("A","B","A"))
#' @export
is_url <- function(x, ...) {
  return(grepl("(http|https)://[a-zA-Z0-9./?=_%:-]*", x, ...))
}

#' @rdname is_url
#' @return \code{is_ip}. Boolean. Result of checking if \code{x} is a valid IP string.
#' @export
is_ip <- function(x, ...) {
  regex <- paste0("^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.)",
                  "{3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$")
  return(grepl(regex, x, ...))
}

#' @rdname is_url
#' @return \code{are_id}. Boolean. Result of checking if \code{x} is a potential ID vector
#' @export
are_id = function(x) {
  return(is.character(x) & length(unique(x)) == length(x))
}

#' @rdname is_url
#' @return \code{are_constant}. Boolean. Result of checking if \code{x} is a constant vector
#' @export
are_constant = function(x) {
  return(length(unique(x)) == 1)
}

#' @rdname is_url
#' @return \code{are_binary}. Boolean. Result of checking if \code{x} is a binary vector
#' @export
are_binary = function(x) {
  return(length(unique(x)) == 2)
}
