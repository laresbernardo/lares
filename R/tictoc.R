####################################################################
#' Stopwatch to measure timings in R
#'
#' Start a stopwatch.
#'
#' @family Time
#' @family Tools
#' @param id Define ID if multiple \code{tic} & \code{toc} are being used.
#' @param start Start time. Now is default.
#' @param quiet Boolean. Quiet messages?
#' @return Invisible list. Contains tic (start time), toc (stop time),
#' elapsed time and message printed.
#' @examples
#' # Basic use (global stopwatch)
#' tic()
#' Sys.sleep(0.1)
#' toc()
#'
#' # Multiple tic tocs
#' tic(id = "two", quiet = FALSE)
#' Sys.sleep(0.2)
#' toc(id = "two")
#'
#' # Global is still working (id = 1)
#' toc(msg = "The function finished its work in")
#' @export
#' @rdname tic
tic <- function(id = 1, start = proc.time()["elapsed"], quiet = TRUE) {
  tic <- .set.tictoc("TIC", id, start = start)
  if (!quiet) message(sprintf("Tic `id = %s` start time: %s", id, Sys.time()))
  invisible(tic)
}


####################################################################
#' Stopwatch Stop
#'
#' Stop a stopwatch.
#'
#' @param msg Character. Custom message shown
#' @param type Character. Output format for \code{time} list element.
#' Choose any of: \code{units, clock, seconds}.
#' @param signif Integer. Significant digits.
#' @param ... Additional parameters.
#' @return \code{toc} returns an (invisible) list containing the time-stamps
#' \code{tic} and \code{toc}, \code{time} in seconds and the message \code{msg}.
#' @export
#' @rdname tic
toc <- function(id = 1, msg = "Elapsed time:", type = "units", signif = 3, quiet = FALSE, ...) {
  check_opts(type, c("units", "clock", "seconds"))
  current <- getOption("LARES_TICTOC")
  id <- as.character(id)
  if (!id %in% names(current)) {
    stop(sprintf("You need to tic(id = '%s') before you toc(id = '%s')", id, id))
  }

  toc <- .set.tictoc("TOC", id)
  tic <- getOption("LARES_TICTOC")[[id]][["TIC"]]
  toc <- getOption("LARES_TICTOC")[[id]][["TOC"]]
  time <- as.numeric(toc - tic)

  if (type == "units") {
    x <- time
    u <- ifelse(x < 60, "s", ifelse(x < 3600, "m", ifelse(x < 86400, "h", "d")))
    d <- ifelse(x < 60, 1, ifelse(x < 3600, 60, ifelse(x < 86400, 3600, 86400)))
    out <- paste0(signif(time / d, signif), u)
  }

  if (type == "clock") {
    td <- lubridate::seconds_to_period(round(time))
    out <- sprintf("%02d:%02d:%02d", td@hour, minute(td), second(td))
  }

  if (type == "seconds") {
    out <- signif(time, signif)
  }

  msg <- sprintf("%s %s", msg, out)
  if (!quiet) message(msg)
  res <- list(tic = tic, toc = toc, time = out, msg = msg)
  invisible(res)
}

.set.tictoc <- function(which = c("TIC", "TOC"), id = 1,
                        start = proc.time()["elapsed"]) {
  aux <- getOption("LARES_TICTOC")
  if (is.null(aux)) aux <- list()
  id <- as.character(id)
  if (which[1] == "TIC") temp <- data.frame(TIC = start, TOC = NA)
  if (which[1] == "TOC") temp <- data.frame(TIC = aux[[id]]$TIC, TOC = start)
  aux[[id]] <- temp
  options("LARES_TICTOC" = aux)
  return(aux[id])
}
