# Update lares' library

updateLares <- function(user=NA, creds=NA, local=F) {
  
  suppressMessages(require(devtools))
  suppressMessages(require(config))
  
  start <- Sys.time()
  message(paste(start,"| Started installation..."))
  
  if (local == TRUE) {
    devtools::install("~/Dropbox (ID)/CM Data Science/Bitbucket/analytics/lares")
    
  } else {
    
    if (!is.na(user)) {
      pass <- readline("Provide your Bitbucket user's password: ")
    } else {
      credentials <- lares::get_credentials("bitbucket", dir = creds)
      user <- credentials$username
      pass <- credentials$password
    }
    
    devtools::install_bitbucket("comparamejorteam/analytics",
                                subdir = "lares",
                                auth_user = user,
                                password = pass)
    
    message(paste(Sys.time(), "| Duration:", round(difftime(Sys.time(), start, units="secs"), 2), "s"))
  }
}

