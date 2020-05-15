####################################################################
#' Update the library
#'
#' This function lets the user update from repository or local source.
#'
#' @family Tools
#' @param local Boolean. Install package with local files (TRUE) or Github repository
#' @param force Boolean. Force install if needed
#' @param n Boolean. Notify update for lares' stats
#' @param fb Boolean. From FB instance?
#' @param all Boolean. Install other recommended libraries? Kinda Docker install!
#' @export
updateLares <- function(local = FALSE, force = FALSE, n = TRUE, fb = FALSE, all = FALSE) {
  
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
      # aux <- lares::get_credentials("github")$laresfb
      aux <- paste0("b7b59665f63ad91b6c577", "17c3148c63d12db63f5")
      with_proxy(install_github(
        "laresbernardo/laresfb", force = force, auth_token = aux))
    } else install_github("laresbernardo/lares", force = force) 
  }

  if (n) {
    aux <- paste("User:", Sys.info()[["user"]])
    slackSend(aux, title = "New lares update", quiet = TRUE)
  }
  
  if (all) install_recommended()
  
  toc(id = "updateLares", msg = paste(Sys.time(), "|"))  
  
  if ("lares" %in% names(utils::sessionInfo()$otherPkgs)) { 
    message("Reloading library... ready to use!")
    detach(package:lares, unload = TRUE)
    library(lares)
  }

}

####################################################################
#' Install latest version of H2O
#' 
#' This function lets the user un-install the current version of
#' H2O installed and update to latest stable version.
#' 
#' @family Tools
#' @param run Boolean. Do you want to run and start an H2O cluster?
#' @param lib Character. Library directories where to install h2o
#' @export
h2o_update <- function(run = TRUE, lib = .libPaths()){
  tic(id = "h2o_update")
  url <- "http://h2o-release.s3.amazonaws.com/h2o/latest_stable.html"
  end <- xml2::read_html(url) %>% rvest::html_node("head") %>% 
    as.character() %>% gsub(".*url=","",.) %>% gsub("/index.html.*","",.)
  newurl <- paste0(gsub("/h2o/.*","",url), end, "/R")
  # The following commands remove any previously installed H2O version
  if ("package:h2o" %in% search()) { detach("package:h2o", unload = TRUE) }
  if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
  # Now we download, install and initialize the H2O package for R.
  message(paste("Installing h2o from", newurl))
  install.packages("h2o", type = "source", repos = newurl, lib = lib)
  if (run) h2o.init()
  toc(id = "h2o_update", msg = paste(Sys.time(), "|"))
}
