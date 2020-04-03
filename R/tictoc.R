####################################################################
# Auxiliary function for tic() and toc()
####################################################################
set.tictoc <- function(which, id) {
  now <- proc.time()["elapsed"]
  aux <- getOption(which)
  if (!sprintf("tic_%s", id) %in% names(getOption("tic")))
    stop(sprintf("You need to tic(id = '%s') before you toc(id = '%s')", id, id))
  name <- sprintf("%s_%s", which, id)
  if (is.null(aux)) aux <- list()
  aux[[name]] <- now
  return(aux)
}


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
#' # Multiple tic tocs
#' tic(id = 123, quiet = FALSE)
#' toc(id = 123)
#' toc(id = 1)
#' }
#' @export
#' @rdname tic
tic <- function(id = 1, quiet = TRUE) { 
  tic <- set.tictoc("tic", id)
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
  
  toc <- set.tictoc("toc", id)
  options("toc" = toc)
  
  tic <- getOption("tic")[[paste0("tic_", id)]]
  toc <- getOption("toc")[[paste0("toc_", id)]]
  time <- as.numeric(toc - tic)
  
  msg <- sprintf("%s %ss", msg, signif(time, 3))
  if (!quiet) message(msg)
  res <- list(tic = tic, toc = toc, time = time, msg = msg)
  invisible(res)
}

####################################################################
#' Stopwatch Reset
#' 
#' Measure timings between a tic and a toc. Reset all values
#' 
#' @family Time
#' @param which Character. Select: both, tic, toc
#' @export
#' @rdname tic
resettictoc <- function(which = "both") {
  if ("tic" %in% which)
    options("tic" = NULL)
  if ("toc" %in% which)
    options("toc" = NULL)
  if ("both" == which)
    options("tic" = NULL, "toc" = NULL)
}
