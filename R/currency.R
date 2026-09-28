####################################################################
#' Download Historical Currency Exchange Rate
#'
#' This function lets the user download historical currency exchange
#' rate between two currencies.
#'
#' @family Currency
#' @inheritParams cache_write
#' @param currency_pair Character. Which currency exchange do you
#' wish to get the history from? i.e, USD/COP, EUR/USD...
#' @param from Date. From date
#' @param to Date. To date
#' @param fill Boolean. Fill weekends and non-quoted dates with
#' previous values?
#' @return data.frame. Result of fetching online data for \code{currency_pair}
#' grouped by date. A single-day request returns the last available quote
#' within the previous 14 days when that day is not quoted. If the provider
#' cannot be reached or has no quotes, returns an empty data.frame with
#' \code{date} and \code{rate} columns and an informative message.
#' No cached quotes are used when the provider is unavailable.
#' @examples
#' \dontrun{
#' # Requires an external currency provider; do not run in automated checks.
#' # For today (or any one single date)
#' get_currency("USD/COP", from = Sys.Date())
#' # For multiple dates
#' get_currency("EUR/USD", from = Sys.Date() - 7, fill = TRUE)
#' }
#' @export
get_currency <- function(currency_pair,
                         from = Sys.Date() - 99,
                         to = Sys.Date(),
                         fill = FALSE, ...) {
  if (!haveInternet()) {
    message("Currency quotes unavailable: no internet connection")
    data.frame(date = as.Date(character()), rate = numeric())
  } else {
    try_require("quantmod")

    string <- paste0(toupper(cleanText(currency_pair)), "=X")

    if (length(from) != 1 || length(to) != 1 || is.na(from) || is.na(to)) {
      stop("You must insert a valid date")
    }

    from <- as.Date(from)
    to <- as.Date(to)

    # Yahoo may not publish today's quote on weekends or before market close.
    # Include a short lookback only for a single requested date; do not return
    # an older quote outside the requested range for historical series.
    single_day <- from == to
    lookup_from <- if (single_day) from - 14 else from
    lookup_to <- if (single_day) to + 1 else to
    if (lookup_to > Sys.Date() + 1) lookup_to <- Sys.Date() + 1

    empty <- data.frame(date = as.Date(character()), rate = numeric())
    x <- tryCatch(
      suppressWarnings(getSymbols(string, env = NULL,
                                  from = lookup_from, to = lookup_to, ...)),
      error = function(e) {
        message("Currency quotes unavailable for ", currency_pair, ": ",
                conditionMessage(e))
        NULL
      }
    )
    if (is.null(x)) return(empty)
    x <- tryCatch(data.frame(x), error = function(e) NULL)
    if (is.null(x) || !ncol(x) || !nrow(x)) {
      message("No currency quotes available for ", currency_pair)
      return(empty)
    }
    dates <- suppressWarnings(as.Date(gsub("\\.", "-", gsub("X", "", rownames(x)))))
    if (length(dates) != nrow(x) || all(is.na(dates))) {
      message("No usable currency quote dates for ", currency_pair)
      return(empty)
    }
    rate <- data.frame(date = dates, rate = x[, 1])
    rate <- rate[!is.na(rate$date) & rate$date >= lookup_from &
                   rate$date <= to, , drop = FALSE]
    if (single_day && nrow(rate)) {
      rate <- tail(rate[order(rate$date), , drop = FALSE], 1)
    }
    if (!nrow(rate)) {
      message("No currency quotes available for ", currency_pair)
      return(empty)
    }
    # Sometimes, the last date is repeated.
    if (nrow(rate) > 1 && tail(rate$date, 1) == tail(rate$date, 2)[1]) {
      rate <- rate[-nrow(rate), , drop = FALSE]
    }

    if (fill && !single_day) {
      rate <- data.frame(date = as.character(
        as.Date(as.Date(from):as.Date(to), origin = "1970-01-01")
      )) %>%
        left_join(rate %>% mutate(date = as.character(date)), "date") %>%
        tidyr::fill(rate, .direction = "down") %>%
        tidyr::fill(rate, .direction = "up") %>%
        mutate(date = as.Date(date)) %>%
        filter(date >= as.Date(from))
    }
    rate
  }
}
