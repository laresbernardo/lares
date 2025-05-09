% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{check_opts}
\alias{check_opts}
\alias{check_attr}
\alias{is_url}
\alias{is_ip}
\alias{are_id}
\alias{are_constant}
\alias{are_binary}
\alias{is_even}
\alias{is_odd}
\title{Validate inputs (attributions, options, ...)}
\usage{
check_opts(
  inputs,
  opts,
  input_name = "input",
  type = "all",
  not = "stop",
  quiet = TRUE
)

check_attr(object, attr = "type", check = NULL, stop = TRUE)

is_url(x, ...)

is_ip(x, ...)

are_id(x)

are_constant(x)

are_binary(x)

is_even(x)

is_odd(x)
}
\arguments{
\item{inputs}{Vector character. Check options.}

\item{opts}{Vector character. Valid options.}

\item{input_name}{Character. Custom your message and change "input"
for any other string. For example: "column names".}

\item{type}{Character. Options: "all", "any."}

\item{not}{Character. Options: "stop", "message", "print", "return".}

\item{quiet}{Boolean. Keep quiet? If not, returns logical value.}

\item{object}{Object of any kind}

\item{attr}{Character. Attribute to check}

\item{check}{Character. Attribute value}

\item{stop}{Boolean. Stop if doesn't check?}

\item{x}{Vector}

\item{...}{Additional parameters passed to \code{grepl()}}
}
\value{
Boolean. Result of \code{inputs} in \code{opts} (options).
Depending on \code{type} and/or \code{stop} arguments,
errors or messages will be shown.

No return value, called for side effects.

\code{is_url}. Boolean. Result of checking if \code{x} is a valid URL string.

\code{is_ip}. Boolean. Result of checking if \code{x} is a valid IP string.

\code{are_id}. Boolean. Result of checking if \code{x} is a potential ID vector

\code{are_constant}. Boolean. Result of checking if \code{x} is a constant vector

\code{are_binary}. Boolean. Result of checking if \code{x} is a binary vector
}
\description{
This function validates if inputs match all/any of your options
and return error/message with possible options to use. Similar
to \code{match.arg()} but more flexible.

This function checks if an object has a specific attribute and
stops if not.

Check whether a value or vector is or is not following a set
of rules. For example: is an URL, is an ID vector, are non-variant or
constant values, are binary values... Notice that \code{is_} will return
the result for each observation and \code{are_} for the whole vector.
}
\examples{
opts <- c("A", "B", "C")
# Let's check the "all" logic
check_opts(inputs = c("A", "B"), opts, quiet = FALSE)
check_opts(inputs = c("X"), opts, not = "message", quiet = FALSE)
check_opts(inputs = c("A", "X"), opts, input_name = "value", not = "warning")
# Now let's check the "any" logic
check_opts(inputs = c("A", "X"), opts, type = "any")
check_opts(inputs = c("X"), opts, type = "any", not = "message")
check_opts(inputs = c("A", NA), opts, type = "any")
# Final trick: just ignore results
check_opts(inputs = "X", opts, not = "invisible")
test <- data.frame()
attributes(test)
check_attr(test, "class", "data.frame")
# check_attr(test, "class", "not.data.frame")
is_url(c("google.com", "http://google.com"))

is_ip(c("163.114.132.0", "7.114.132", "0.0.0.0", "1.1.1.1."))

are_id(1:10)
are_id(LETTERS[1:10])

are_constant(rep(1, 10))
are_constant(1:10)

are_binary(c("A", "B", "A"))

is_even(1:5)
is_odd(1:5)
is_odd(c(0, 1.5, 2.5, NA, Inf, NULL))
}
\concept{Checks}
