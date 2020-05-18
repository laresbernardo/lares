####################################################################
#' Load parameters and credentials from YML file
#' 
#' Load credentials from a local YML file. Uses your \code{.Renviron} file
#' and the \code{LARES_CREDS} parameter to remember (forever) the directory 
#' of your credentials' file and uses it when \code{dir = NA}. You may use
#' this function as well for external (non-\code{lares}) code.
#' 
#' @section Set the default directory:
#' The first time you use any function that has the \code{creds} parameter, if
#' the \code{dir} parameter is set to \code{NA}, this function will ask you to
#' set the directory where you save your YML local file with your credentials.
#' This will be asked once and will be set for further R sessions. Remember to
#' reset your session for this setup to start working properly.
#' 
#' @family Tools
#' @family Credentials
#' @param from Character. Family of values to import from the YML file.
#' If you don't know these names, set \code{from = NA}
#' and a warning will display all possible values, depending on your YML file.
#' @param dir Character. Credentials directory where your YML file is.
#' If used frequently, set your directory by using the \code{.Renviron} file. 
#' To do so, leave \code{dir} as \code{NA} and follow the steps.
#' @param filename Character. YML filename with your credentials.
#' @param env Character. Environment variable name. No need to set differently
#' for any function that uses this library. Only for external use
#' @export
get_credentials <- function(from = NA, dir = NA, 
                            filename = "config.yml", 
                            env = "LARES_CREDS") {
  
  if (is.na(dir)) { 
    dir <- Sys.getenv(env)
    if (dir == "") {
      message(sprintf(
        "Please, set your creds directory (one-time only step to set %s):", env))
      dir <- readline(sprintf("Set directory where your %s file is saved: ", filename))
      if (!dir.exists(dir))
        stop(sprintf("Directory: %s. \nCan't find or does not exist. Please try again...", dir))
      line <- sprintf("%s=%s", env, dir)
      write(line, file = "~/.Renviron", append = TRUE)
      message("ALL's SET! But, you must reset your session for it to work!")
      return(invisible(NULL))
    }
  }
  
  file <- paste0(dir, "/", filename)
  if (!file.exists(file)) {
    message(sprintf("YML file with credentials not found in %s", dir))
  } else {
    wd <- getwd()
    setwd(dir)
    credentials <- config::get(from)
    if (is.null(credentials)) {
      trues <- names(config::get())
      warning(paste("No credentials for", from, "found in your YML file.",
                    "\nTry any of the following:", v2t(trues)))
    }
    setwd(wd)
    return(credentials)
  }
}
#' @rdname get_credentials
#' @export
get_creds <- get_credentials
