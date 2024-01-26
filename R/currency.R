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
#' grouped by date.
#' @examples
#' \donttest{
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
  try_require("quantmod")

  string <- paste0(toupper(cleanText(currency_pair)), "=X")

  if (is.na(from) || is.na(to)) {
    stop("You must insert a valid date")
  }

  from <- as.Date(from)
  to <- as.Date(to)

  if (from == to) to <- from + 1
  if (to > Sys.Date()) to <- Sys.Date()

  x <- try(data.frame(suppressWarnings(getSymbols(
    string,
    env = NULL,
    from = from,
    to = to,
    ...
  ))))
  if ("try-error" %in% class(x)) {
    warning(x)
    return(x)
  }
  dates <- as.Date(gsub("\\.", "\\-", gsub("X", "", rownames(x))))
  rate <- data.frame(date = dates, rate = x[, 1])
  # Sometimes, the last date is repeated
  if (tail(rate$date, 1) == tail(rate$date, 2)[1] && nrow(rate) > 1) {
    rate <- rate[-nrow(rate), ]
  }

  if (fill) {
    rate <- data.frame(date = as.character(
      as.Date(as.Date(from):Sys.Date(), origin = "1970-01-01")
    )) %>%
      left_join(rate %>% mutate(date = as.character(date)), "date") %>%
      tidyr::fill(rate, .direction = "down") %>%
      tidyr::fill(rate, .direction = "up") %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= as.Date(from))
  }

  return(rate)
}
