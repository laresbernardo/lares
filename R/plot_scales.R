####################################################################
#' Axis scales format
#'
#' The \code{_comma} ones set comma format for axis text, the \code{_percent}
#' ones set percent format for axis text, \code{_dollar} for collar currency,
#' and \code{_abbr} for abbreviated format. Lastly, use \code{_formatNum} to
#' further customize your numerical scales with \code{formatNum}.
#'
#' @param ... Arguments passed to \code{ggplot2::continuous_scale} or
#' \code{formatNum} depending on the function.
#' @return Reformatted scales on ggplot2 object
#' @examples
#' library(ggplot2)
#' df <- ggplot2::txhousing %>% removenarows(all = FALSE)
#'
#' ggplot(df, aes(x = sales, y = volume)) +
#'   geom_point() +
#'   scale_x_dollar() +
#'   scale_y_abbr()
#'
#' # Use any argument from scale_x/y_continuous
#' ggplot(df, aes(x = listings, y = log(inventory))) +
#'   geom_point() +
#'   scale_x_comma() +
#'   scale_y_percent(limits = c(0, 3))
#'
#' # Use any argument from scale_x/y_continuous AND formatNum
#' ggplot(df, aes(x = median, y = inventory)) +
#'   geom_point() +
#'   scale_x_formatNum(n.breaks = 3, pre = "@", abbr = TRUE) +
#'   scale_y_formatNum(position = "right", decimals = 0, pos = " X")
#' @export
scale_x_comma <- function(...) {
  scale_x_continuous(..., labels = function(x) {
    formatNum(x, decimals = NULL, signif = 3)
  })
}

#' @rdname scale_x_comma
#' @export
scale_y_comma <- function(...) {
  scale_y_continuous(..., labels = function(x) {
    formatNum(x, decimals = NULL, signif = 3)
  })
}

#' @rdname scale_x_comma
#' @export
scale_x_percent <- function(...) {
  scale_x_continuous(..., labels = function(x) {
    formatNum(x * 100, pos = "%", decimals = NULL, signif = 3)
  })
}

#' @rdname scale_x_comma
#' @export
scale_y_percent <- function(...) {
  scale_y_continuous(..., labels = function(x) {
    formatNum(x * 100, pos = "%", decimals = NULL, signif = 3)
  })
}

#' @rdname scale_x_comma
#' @export
scale_x_dollar <- function(...) {
  scale_x_continuous(..., labels = function(x) {
    formatNum(x, pre = "$", decimals = NULL, signif = 3)
  })
}

#' @rdname scale_x_comma
#' @export
scale_y_dollar <- function(...) {
  scale_y_continuous(..., labels = function(x) {
    formatNum(x, pre = "$", decimals = NULL, signif = 3)
  })
}

#' @rdname scale_x_comma
#' @export
scale_x_abbr <- function(...) scale_x_continuous(..., labels = num_abbr)

#' @rdname scale_x_comma
#' @export
scale_y_abbr <- function(...) scale_y_continuous(..., labels = num_abbr)

#' @rdname scale_x_comma
#' @inheritParams formatNum
#' @export
scale_x_formatNum <- function(..., decimals = 2, signif = NULL,
                              type = Sys.getenv("LARES_NUMFORMAT"),
                              pre = "", pos = "", sign = FALSE, abbr = FALSE) {
  scale_x_continuous(..., labels = function(x) {
    formatNum(x,
      decimals = decimals, signif = signif, type = type,
      pre = pre, pos = pos, sign = sign, abbr = abbr
    )
  })
}

#' @rdname scale_x_comma
#' @export
scale_y_formatNum <- function(..., decimals = 2, signif = NULL,
                              type = Sys.getenv("LARES_NUMFORMAT"),
                              pre = "", pos = "", sign = FALSE, abbr = FALSE) {
  scale_y_continuous(..., labels = function(x) {
    formatNum(x,
      decimals = decimals, signif = signif, type = type,
      pre = pre, pos = pos, sign = sign, abbr = abbr
    )
  })
}
