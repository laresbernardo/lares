# Validate inputs (attributions, options, ...)

This function validates if inputs match all/any of your options and
return error/message with possible options to use. Similar to
[`match.arg()`](https://rdrr.io/r/base/match.arg.html) but more
flexible.

This function checks if an object has a specific attribute and stops if
not.

Check whether a value or vector is or is not following a set of rules.
For example: is an URL, is an ID vector, are non-variant or constant
values, are binary values... Notice that `is_` will return the result
for each observation and `are_` for the whole vector.

## Usage

``` r
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
```

## Arguments

- inputs:

  Vector character. Check options.

- opts:

  Vector character. Valid options.

- input_name:

  Character. Custom your message and change "input" for any other
  string. For example: "column names".

- type:

  Character. Options: "all", "any."

- not:

  Character. Options: "stop", "message", "print", "return".

- quiet:

  Boolean. Keep quiet? If not, returns logical value.

- object:

  Object of any kind

- attr:

  Character. Attribute to check

- check:

  Character. Attribute value

- stop:

  Boolean. Stop if doesn't check?

- x:

  Vector

- ...:

  Additional parameters passed to
  [`grepl()`](https://rdrr.io/r/base/grep.html)

## Value

Boolean. Result of `inputs` in `opts` (options). Depending on `type`
and/or `stop` arguments, errors or messages will be shown.

No return value, called for side effects.

`is_url`. Boolean. Result of checking if `x` is a valid URL string.

`is_ip`. Boolean. Result of checking if `x` is a valid IP string.

`are_id`. Boolean. Result of checking if `x` is a potential ID vector

`are_constant`. Boolean. Result of checking if `x` is a constant vector

`are_binary`. Boolean. Result of checking if `x` is a binary vector

## Examples

``` r
opts <- c("A", "B", "C")
# Let's check the "all" logic
check_opts(inputs = c("A", "B"), opts, quiet = FALSE)
#> [1] TRUE
check_opts(inputs = c("X"), opts, not = "message", quiet = FALSE)
#> Your input 'X' is NOT valid; ALL inputs should match these options: 'A', 'B', 'C'
#> [1] FALSE
check_opts(inputs = c("A", "X"), opts, input_name = "value", not = "warning")
#> Warning: Your value 'X' is NOT valid; ALL inputs should match these options: 'A', 'B', 'C'
# Now let's check the "any" logic
check_opts(inputs = c("A", "X"), opts, type = "any")
check_opts(inputs = c("X"), opts, type = "any", not = "message")
#> Your input 'X' is NOT valid; ANY inputs should match these options: 'A', 'B', 'C'
check_opts(inputs = c("A", NA), opts, type = "any")
# Final trick: just ignore results
check_opts(inputs = "X", opts, not = "invisible")
test <- data.frame()
attributes(test)
#> $names
#> character(0)
#> 
#> $row.names
#> integer(0)
#> 
#> $class
#> [1] "data.frame"
#> 
check_attr(test, "class", "data.frame")
# check_attr(test, "class", "not.data.frame")
is_url(c("google.com", "http://google.com"))
#> [1] FALSE  TRUE

is_ip(c("163.114.132.0", "7.114.132", "0.0.0.0", "1.1.1.1."))
#> [1]  TRUE FALSE  TRUE FALSE

are_id(1:10)
#> [1] FALSE
are_id(LETTERS[1:10])
#> [1] TRUE

are_constant(rep(1, 10))
#> [1] TRUE
are_constant(1:10)
#> [1] FALSE

are_binary(c("A", "B", "A"))
#> [1] TRUE

is_even(1:5)
#> [1] FALSE  TRUE FALSE  TRUE FALSE
is_odd(1:5)
#> [1]  TRUE FALSE  TRUE FALSE  TRUE
is_odd(c(0, 1.5, 2.5, NA, Inf, NULL))
#> [1] FALSE FALSE FALSE    NA    NA
```
