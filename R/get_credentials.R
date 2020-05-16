####################################################################
#' Load parameters and credentials from YML file
#' 
#' This function lets the user load parameters and credentials
#' 
#' @family Tools
#' @param from Character. Family of values to import from the YML file.
#' If you don't know these names, you can run 
#' \code{get_credentials(dir = "yourdir")} and a warning will display these values.
#' @param dir Character. Credentials directory where your YML file is.
#' If used with frequency, set your directory with 
#' \code{Sys.setenv(LARES_CREDS = "/your/creds/dir")} once and leave `dir` 
#' as `NA` to fetch this directory automatically. Remember to reset your
#' session the first time you set this parameter for it to work.
#' @param filename Character. YML filename with your credentials.
#' @export
get_credentials <- function(from = NA, dir = NA, filename = "config.yml") {
  
  if (is.na(dir)) { 
    dir <- Sys.getenv("LARES_CREDS")
    if (dir == "") {
      stop(paste('Please, set your creds directory once with:',
                 'Sys.setenv(LARES_CREDS = "/your/creds/dir")',
                 'After doing so, reset your session for it to work...', sep = "\n"))
    }
  }
  
  file <- paste0(dir,"/",filename)
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
