# Network-free regression tests for external providers. Run with Rscript.
source("R/currency.R", local = TRUE)
source("R/utils_data.R", local = TRUE)
haveInternet <- function() TRUE
try_require <- function(...) TRUE
cleanText <- function(x) gsub("/", "", x)

getSymbols <- function(...) stop("provider unavailable")
result <- suppressWarnings(get_currency("USD/COP", as.Date("2026-09-27"),
                                        as.Date("2026-09-27")))
stopifnot(is.data.frame(result), identical(names(result), c("date", "rate")),
          nrow(result) == 0)

seen_from <- NULL
getSymbols <- function(Symbols, env, from, to, ...) {
  seen_from <<- from
  structure(matrix(c(4000, 4010), ncol = 1,
                   dimnames = list(c("2026-09-24", "2026-09-25"), "USD.COP")),
            class = "matrix")
}
result <- get_currency("USD/COP", as.Date("2026-09-27"),
                       as.Date("2026-09-27"))
stopifnot(nrow(result) == 1, result$date == as.Date("2026-09-25"),
          result$rate == 4010, seen_from == as.Date("2026-09-13"))

getSymbols <- function(...) matrix(numeric(), ncol = 1)
result <- get_currency("USD/COP", as.Date("2026-09-27"),
                       as.Date("2026-09-27"))
stopifnot(nrow(result) == 0)

is_ip <- function(ip) TRUE
GET <- function(...) stop("db-ip unavailable")
cleanNames <- identity
result <- ip_data("163.114.132.0", quiet = TRUE)
stopifnot(is.data.frame(result), nrow(result) == 0)
cat("Network-free provider regression tests passed\n")
