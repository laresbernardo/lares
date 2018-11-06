####################################################################
#' Update the library
#'
#' This function lets the user update from repository or local source.
#'
#' @param local Boolean. Install package with local files (TRUE) or Github repository
#' @param force Boolean. Force install if needed
#' @param restart Boolean. Restart session after re-installing the library
#' @export
updateLares <- function(local = FALSE, force = FALSE, restart = FALSE) {
  
  # require(devtools)
  # require(config)
  
  start <- Sys.time()
  message(paste(start,"| Started installation..."))
  
  if (local == TRUE) {
    devtools::install("~/Dropbox (Personal)/Documentos/R/Github/lares")
  } else {
    devtools::install_github("laresbernardo/lares", force = force) 
  }
  if (restart == TRUE) {
    .rs.restartR()
  }
  message(paste(Sys.time(), "| Duration:", round(difftime(Sys.time(), start, units="secs"), 2), "s"))
}
