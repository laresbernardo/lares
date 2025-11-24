####################################################################
#' New Line Feed for Long Strings (Wrapper)
#'
#' Add a break or new line without breaking words. Automatically,
#' the function can detect your plot's width and will dynamically
#' set an auto width. You can adjust the relation (rel) parameter
#' for different fonts and sizes until perfect harmony found.
#' Quite similar to \code{stringr::str_wrap} but, if the text vector
#' is a factor, the levels will be kept in order and transformed.
#'
#' @family Tools
#' @param text Character or factor vector.
#' @param top Integer. How many characters aprox. should be on each line?
#' @param rel Numeric. Relation of pixels and characters per line
#' @return Character. String (vector) including some \code{\\n} within.
#' @examples
#' cat(autoline("This is a long text that may not fit into a single line", 8))
#'
#' text <- factor(c("First value", "Second value", "First value"),
#'   levels = c("First value", "Second value")
#' )
#' autoline(text, 1)
#'
#' path <- file.path(R.home("doc"), "THANKS")
#' text <- paste(readLines(path), collapse = " ")
#' cat(autoline(text))
#' @export
autoline <- function(text, top = "auto", rel = 9) {
  # Auto-maximum
  if (top == "auto") top <- round(dev.size("px")[1] / rel)

  # Keep factors
  is_factor <- is.factor(text)
  if (is_factor) {
    levs <- levels(text)
    text <- as.character(text)
  }

  ret <- stringr::str_wrap(text, top)

  if (is_factor) {
    aux <- data.frame(ret, text) %>%
      mutate(text = factor(text, levels = levs)) %>%
      arrange(text) %>%
      unique()
    ret <- factor(ret, levels = aux$ret)
  }
  ret
}


####################################################################
#' Format a string text as markdown/HTML
#'
#' Format any character string to HTML or markdown format. We
#' recommend using this format with the \code{ggtext::geom_richtext}
#' function to format text in \code{ggplot2} objects.
#'
#' @family Tools
#' @param text Character. Strings to format.
#' @param color Character. Hex colour code.
#' @param size Numeric. Text size.
#' @param bold Boolean. Should the text be bold?
#' @return String with format characters included.
#' @examples
#' formatHTML("Text test", color = "#000000")
#' formatHTML(c(123, 456), color = "orange", size = 120, bold = TRUE)
#'
#' # If you want to use it with \code{ggtext}:
#' \dontrun{
#' col1 <- "grey"
#' col2 <- "orange"
#' pt <- data.frame(
#'   label = paste0(
#'     formatHTML(123, color = col2, size = 120, bold = TRUE), "<br/>",
#'     formatHTML("of children had a", col1), "<br/>",
#'     formatHTML("traditional stay-at-home mom", color = col2, bold = TRUE), "<br/>",
#'     formatHTML(paste0("in 2012, compared to ", 321, " in 1970"), color = col1)
#'   )
#' )
#' ggplot(pt, aes(x = 0, y = 0)) +
#'   ggtext::geom_richtext(
#'     aes(label = label),
#'     hjust = 0,
#'     label.color = NA,
#'     lineheight = 1.5
#'   ) +
#'   xlim(0, 0.01) +
#'   theme_void()
#' }
#' @export
#' @rdname format_string
formatHTML <- function(text, color = "black", size = 20, bold = FALSE) {
  opening_span <- paste0("<span style='font-size:", size, "px; color:", color, "'>")
  if (bold) text <- paste0("**", text, "**")
  closing_span <- "</span>"
  text <- paste(text, collapse = "<br/>")
  ret <- paste0(opening_span, text, closing_span)
  ret
}


####################################################################
#' Interpolate a string [glue wrapper]
#'
#' Format and interpolate a string using a \code{glue} wrapper. Allows
#' simple operations, \code{NULL} values as input, and interactions with
#' internal (created within \code{glued}) and external (environment) objects.
#'
#' @family Tools
#' @inheritParams stringr::str_glue
#' @param empty_lines Character. Set to \code{"keep"} to keep or
#' \code{"drop"} to drop empty lines.
#' @return Same as input but transformed (glued).
#' @examples
#' name <- "Bernardo"
#' age <- 29
#' anniversary <- as.Date("2016-04-30")
#' glued("
#'   My name is {name},
#'   my age next year will be {age + 1},
#'   and I got married on {format(anniversary, '%A, %B %d, %Y')}.")
#'
#' # Single braces can be inserted by doubling them
#' glued("My name is {name}, not {{name}}.")
#'
#' # You can also used named arguments
#' glued(
#'   "Her name is {name}, ",
#'   "and her age next year will be {age + 1}.",
#'   name = "Maru",
#'   age = 6
#' )
#'
#' # And run operations with memories (beware!)
#' glued("My name, {name}, has {n <- nchar(name); n} characters.
#'        If we multiply by ten, we'll have {10 * n} characters!")
#'
#' # If you pass a vector, the operation will be repeated for each element
#' glued("Here's the value #{1:3}")
#' @export
glued <- function(..., .sep = "", empty_lines = "keep", .envir = parent.frame()) {
  null_transformer <- function(text, envir) {
    out <- eval(parse(text = text, keep.source = FALSE), envir)
    if (is.null(out)) out <- ""
    out
  }
  output <- stringr::str_glue(
    ...,
    .sep = .sep,
    .transformer = null_transformer,
    .envir = .envir
  )
  if (empty_lines == "drop") {
    lines <- stringr::str_split(output, "\n")[[1]]
    output <- glued(paste(lines[trimws(lines) != ""], collapse = "\n"))
  }
  output
}


####################################################################
#' Pattern Matching for Any or All Multiple Matches
#'
#' This function returns a boolean vector of the same length as `x`,
#' each element of which is the result of applying the `type` of matches
#' to the corresponding element of `x`, using regular expressions.
#'
#' @family Tools
#' @inheritParams base::grep
#' @param x Character vector. Text where matches are sought, or an object
#' which can be coerced by as.character to a character vector.
#' Long vectors are supported.
#' @param type Character. Type of match. Choose one of:
#' \code{any}, \code{all}
#' @param ... Additional arguments to pass to \code{grepl}
#' @return Boolean of same length as \code{x}
#' @examples
#' x <- c(123, 876, 18761)
#' patterns <- c(1, 2)
#' grepm(patterns, x, type = "any")
#' grepm(patterns, x, type = "all")
#' @export
grepm <- function(pattern, x, type = "all", ...) {
  lapply(x, function(a) lapply(pattern, function(i) grepl(i, a, ...))) %>%
    lapply(get(type)) %>%
    unlist() %>%
    suppressWarnings()
}

####################################################################
#' Print Coloured Messages
#'
#' @family Tools
#' @param txt Character. Text to print or transform.
#' @param colour Character. Any of: grey, red, green, yellow, blue, or purple.
#' @param bold Boolean. Set bold text?
#' @param cat Boolean. Print with cat? If not, raw string
#' @return Depends on \code{cat}: NULL if TRUE or character string if FALSE.
#' @examples
#' opts <- c("GREY", "RED", "GREEN", "YELLOW", "BLUE", "PURPLE")
#' for (colour in opts) formatColoured(paste("Colour:", colour, "\n"), colour)
#' formatColoured("my bold coloured text", bold = TRUE, cat = TRUE)
#' @export
formatColoured <- function(txt, colour = c("yellow", "blue", "grey"), bold = FALSE, cat = TRUE) {
  colour <- toupper(colour)[1]
  opts <- c("GREY", "RED", "GREEN", "YELLOW", "BLUE", "PURPLE", "CYAN", "WHITE")
  check_opts(colour, opts)
  if (colour == opts[1]) code <- 30
  if (colour == opts[2]) code <- 31
  if (colour == opts[3]) code <- 32
  if (colour == opts[4]) code <- 33
  if (colour == opts[5]) code <- 34
  if (colour == opts[6]) code <- 35
  if (colour == opts[7]) code <- 36
  if (colour == opts[8]) code <- 37
  out <- paste0("\033[", ifelse(!bold, 0, 1), ";", code, "m", txt, "\033[0m")
  if (cat) cat(out) else out
}

####################################################################
#' Test the Truth of R Expressions and Warn
#'
#' If the expression in ... is not \code{TRUE}, \code{warning} is called,
#' producing a warning message indicating the expression which was not true.
#'
#' @family Tools
#' @param ... any R expression, which should evaluate to TRUE
#' @examples
#' warnifnot(TRUE)
#' warnifnot(FALSE)
#' warnifnot(1 + 1 == 3)
#' @export
warnifnot <- function(...) if (!isTRUE(...)) warning(paste(deparse(...), "is not TRUE"))
