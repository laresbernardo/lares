####################################################################
# Auxiliary function for tic() and toc()
####################################################################
set.tictoc <- function(which, id) {
  now <- proc.time()["elapsed"]
  aux <- getOption(which)
  name <- sprintf("%s_%s", which, id)
  if (is.null(aux)) aux <- list()
  aux[[name]] <- now
  return(aux)
}


####################################################################
#' Stopwatch to measure timings in R
#' 
#' Start a stopwatch.
#' 
#' @family Time
#' @family Tools
#' @param id Define ID if multiple \code{tic} & \code{toc} are being used
#' @param quiet Boolean. Quiet messages?
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
#' Stop a stopwatch.
#' 
#' @param msg Character. Custom message shown
#' @param units Boolean. Do you want nice format for the time units? 
#' If not, seconds elapsed as numerical values
#' @param signif Integer. Significant digits
#' @return \code{toc} returns an (invisible) list containing the timestamps
#' \code{tic} and \code{toc}, \code{time} in seconds and the message \code{msg}.
#' @export
#' @rdname tic
toc <- function(id = 1, msg = "Elapsed time:", units = TRUE, signif = 3, quiet = FALSE) {
  
  if (!sprintf("tic_%s", id) %in% names(getOption("tic")))
    stop(sprintf("You need to tic(id = '%s') before you toc(id = '%s')", id, id))
  
  toc <- set.tictoc("toc", id)
  options("toc" = toc)
  
  tic <- getOption("tic")[[paste0("tic_", id)]]
  toc <- getOption("toc")[[paste0("toc_", id)]]
  time <- as.numeric(toc - tic)
  
  if (units) {
    x <- time
    u <- ifelse(x < 60, "s", ifelse(x < 3600, "m", ifelse(x < 86400, "h", "d")))
    d <- ifelse(x < 60, 1, ifelse(x < 3600, 60, ifelse(x < 86400, 3600, 86400)))
    timer <- paste0(signif(time/d, signif), u)
  } else timer <- paste0(signif(time, signif), "s")
  
  msg <- sprintf("%s %s", msg, timer)
  if (!quiet) message(msg)
  res <- list(tic = tic, toc = toc, time = time, msg = msg)
  invisible(res)
}

####################################################################
#' Stopwatch Reset
#' 
#' Reset all tic and toc values in your environment.
#' 
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
