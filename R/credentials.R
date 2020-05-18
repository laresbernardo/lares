####################################################################
#' Load parameters and credentials from YML file
#' 
#' This function lets the user load parameters and credentials
#' 
#' @family Tools
#' @family Credentials
#' @param from Character. Family of values to import from the YML file.
#' If you don't know these names, you can run 
#' \code{get_credentials(dir = "your/dir")} and a warning will display these values.
#' @param dir Character. Credentials directory where your YML file is.
#' If used frequently, set your directory by using the `.Renvironment` file. To do so,
#' leave \code{dir} as \code{NA} and follow the steps. Next time, everytime you need 
#' to fetch your credentials' YML this directory will be used automatically.
#' Remember to reset your session the first time you set this parameter.
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
