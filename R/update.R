# Update the library
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
