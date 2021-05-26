####################################################################
#' Load Credentials from a YML File
#' 
#' Load credentials from a local YML file. You can set your \code{.Renviron}
#' and the \code{LARES_CREDS} parameter to remember (forever) the directory 
#' of your credentials' file. To use it later, you may leave \code{dir = NA}. 
#' You may also use this function for external (non-\code{lares}) code/use.
#' 
#' @section Set the default directory:
#' The first time you use any function that has the \code{creds} parameter, if
#' the \code{dir} parameter is set to \code{NA}, this function will ask you to
#' set the directory where you save your YML local file with your credentials.
#' This will be asked once and will be set for further R sessions. Remember to
#' reset your session for this setup to start working properly.
#' 
#' @section YML file format:
#' A YML file is a text file, with \code{.yml} file format. You may start from 
#' the dummy YML file shared which shows the structure you must follow to set your 
#' credentials file. Check it out
#' \href{https://raw.githubusercontent.com/laresbernardo/lares/master/inst/docs/config.yml}{here}
#' or find it locally using \code{system.file("docs", "config.yml", package = "lares")}.
#' 
#' @family Tools
#' @family Credentials
#' @param from Character. Family of values to import from the YML file.
#' If you don't know these names, set \code{from = NA}
#' and a warning will display all possible values, depending on your YML file.
#' @param dir Character. Credentials directory where your YML file is.
#' If used frequently, set your directory by using the \code{.Renviron} file. 
#' To do so, leave \code{dir} as \code{NA} and follow the steps.
#' If \code{dir} is a list, it'll return \code{dir} (manual credentials input).
#' @param filename Character. YML filename with your credentials.
#' @param env Character. Environment variable name. No need to set differently
#' for any function that uses this library. Only for external use.
#' @return List. Result of reading your credential's YML file, filtered by your
#' \code{from} input if provided.
#' @examples 
#' \dontrun{
#' # Load dummy config.yml file from the library
#' # Recommendation: set dir with NA (read documentation)
#' # We need the directory, not the file
#' yml <- dirname(system.file("docs", "config.yml", package = "lares"))
#' 
#' # Let's see which credentials we have in our file
#' get_credentials(dir = yml)
#' # Warning message: No credentials for NA found in your YML file. 
#' # Try any of the following: 'service1', 'service2', 'service3'
#'   
#' # Get credentials for service2
#' get_credentials("service2", dir = yml)
#' }
#' @export
get_credentials <- function(from = NA, dir = NA,
                            filename = "config.yml",
                            env = "LARES_CREDS") {
  
  if (is.list(dir)) return(dir)
  
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
    credentials <- yaml::read_yaml(file)$default
    if (is.null(credentials)) {
      trues <- names(credentials)
      warning(paste("No credentials for", from, "found in your YML file.",
                    "\nTry any of the following:", v2t(trues)))
      return(invisible(NULL))
    }
    if (!is.na(from)) credentials <- credentials[[from]]
    return(credentials)
  }
}

#' @rdname get_credentials
#' @export
get_creds <- get_credentials
