####################################################################
#' Update the library (dev or CRAN version)
#'
#' This auxiliary function lets the user update \code{lares} to latest
#' CRAN or developer version.
#'
#' @family Tools
#' @param force Boolean. Force install.
#' @param dev Boolean. Developer version (Github)? If not, CRAN version.
#' @param all Boolean. Install other recommended libraries? Kinda Docker install!
#' @param local Boolean. Install package with local files? (or Github repo).
#' @param fb Boolean. From FB instance? Personal internal use.
#' @return No return value, called for side effects.
#' @export
updateLares <- function(force = FALSE, dev = TRUE, all = FALSE, local = FALSE, fb = FALSE) {
  
  try_require("devtools")
  
  tic(id = "updateLares")
  message(paste(Sys.time(), "| Started update..."))
  
  # Auxiliary proxy fx when on devservers
  auxfx <- if (fb) with_proxy else function(x) x
  
  if (local) {
    install("~/Dropbox (Personal)/Documentos/R/Github/lares")
  } else {
    if (!dev) {
      auxfx(install.packages("lares"))
    } else {
      auxfx(install_github("laresbernardo/lares", force = force))
    } 
  }
  
  if (all) auxfx(install_recommended())
  
  toc(id = "updateLares", msg = paste(Sys.time(), "|"))  

}
