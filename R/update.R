####################################################################
#' Update the library
#'
#' This function lets the user update from repository or local source.
#'
#' @family Tools
#' @param force Boolean. Force install
#' @param all Boolean. Install other recommended libraries? Kinda Docker install!
#' @param local Boolean. Install package with local files? (or Github repo)
#' @param fb Boolean. From FB instance? Personal use
#' @return No return value, called for side effects.
#' @export
updateLares <- function(force = FALSE, all = FALSE, local = FALSE, fb = FALSE) {
  
  try_require("devtools")
  
  tic(id = "updateLares")
  message(paste(Sys.time(), "| Started update..."))
  
  if (local) {
    install("~/Dropbox (Personal)/Documentos/R/Github/lares")
  } else {
    if (fb) {
      try_require("fbr", stop = TRUE)
      n <- FALSE
      # Personal access token
      aux <- get_credentials("github")$fb
      with_proxy(install_github(
        "laresbernardo/laresfb", force = force, auth_token = aux))
    } else install_github("laresbernardo/lares", force = force) 
  }

  # if (n) {
  #   aux <- paste("User updated:", Sys.info()[["user"]])
  #   slackSend(aux, title = "New lares update")
  # }
  
  if (all) install_recommended()
  
  # if ("lares" %in% names(utils::sessionInfo()$otherPkgs)) { 
  #   message("Reloading library...")
  #   detach(package:lares, unload = TRUE)
  #   library(lares)
  # }
  
  toc(id = "updateLares", msg = paste(Sys.time(), "|"))  

}
