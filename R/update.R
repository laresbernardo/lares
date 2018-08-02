####################################################################
#' Update the library
#'
#' This function lets the user update from repository or local source.
#'
#' @param local Boolean. Install package with local files (TRUE) or Github repository
#' @export
updateLares <- function(local = FALSE) {
  
  suppressMessages(require(devtools))
  suppressMessages(require(config))
  
  start <- Sys.time()
  message(paste(start,"| Started installation..."))
  
  if (local == TRUE) {
    devtools::install("~/Dropbox (Personal)/Documentos/R/Github/lares")
  } else {
    devtools::install_github("laresbernardo/lares") 
  }
  
  message(paste(Sys.time(), "| Duration:", round(difftime(Sys.time(), start, units="secs"), 2), "s"))
}
