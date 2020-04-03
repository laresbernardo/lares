####################################################################
#' Stopwatch Start
#' 
#' Measure timings between a tic and a toc.
#' 
#' @family Time
#' @param id Define ID if multiple tic() & toc() are being used
#' @param quiet Boolean. Quiet messages?
#' @examples 
#' \dontrun{
#' # Basic use
#' tic(quiet = FALSE)
#' toc()
#' getOption("tic")
#' 
#' # Multiple tic toc
#' tic(id = 123, quiet = FALSE)
#' toc(id = 123)
#' getOption("tic")
#' }
#' @export
#' @rdname tic
tic <- function(id = 1, quiet = TRUE) { 
  now <- proc.time()["elapsed"]
  tic <- getOption("tic")
  name <- sprintf("tic_%s", id)
  if (is.null(tic)) tic <- list()
  tic[[name]] <- now
  options("tic" = tic)
  if (!quiet)
    message(sprintf("Tic `id = %s` start time: %s", id, Sys.time()))
  invisible(tic)
}


####################################################################
#' Stopwatch Stop
#' 
#' Measure timings between a tic and a toc.
#' 
#' @family Time
#' @param id Define ID if multiple tic() & toc() are being used
#' @param msg Character. Custom message shown
#' @param quiet Boolean. Quiet messages?
#' @return \code{toc} returns an (invisible) list containing the timestamps
#' \code{tic} and \code{toc}, \code{time} in seconds and the message \code{msg}.
#' @export
#' @rdname tic
toc <- function(id = 1, msg = "Elapsed time:", quiet = FALSE) {
  tic <- getOption("tic")[[paste0("tic_", id)]]
  if (is.null(tic)) 
    stop("You need to tic() before you toc()")
  toc <- proc.time()["elapsed"]
  options("toc" = toc)
  time <- as.numeric(toc - tic)
  msg <- sprintf("%s %ss", msg, signif(time, 3))
  if (!quiet)
    message(msg)
  res <- list(tic = tic, toc = toc, time = time, msg = msg)
  invisible(res)
}
