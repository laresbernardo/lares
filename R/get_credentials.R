####################################################################
#' Load personal parameters and credentials
#' 
#' This function lets the user load parameters and credentials
#' 
#' @param from Character. Which account do we need to import
#' @param dir Character. Credentials directory for specific user
#' @param filename Character. YML with credentials to read
#' @export
get_credentials <- function(from=NA, dir=NA, filename="config.yml") {
  
  # require(config)
  
  froms <- c("dummy",
             "production",
             "warehouse",
             "dummy_cm",
             "production_cm",
             "warehouse_cm",
             "sendgrid",
             "mailgun",
             "redshift",
             "hubspot",
             "github",
             "bitbucket",
             "typeform",
             "google_api",
             "google_analytics_somosf1",
             "google_analytics_comparamejor",
             "twitter")

  if (is.na(dir)) { dir <- "~/Dropbox (Personal)/Documentos/Docs/Data" }
  if (dir == "personal") { dir <- "~/Dropbox (Personal)/Documentos/Docs/Data" }
  if (dir == "juan") { dir <- "~/Documents/somosf1/analitica" }
  if (dir == "matrix") { dir <- "/srv/creds" }
  if (dir == "alex") { dir <- "/Users/alexswift/Documents" }
  if (dir == "fer") { dir <- "/Users/fernandosucre/Documents" }

  file <- paste0(dir,"/",filename)

  # Check if the credential type asked exists
  if (from %in% froms) {
    # Check if the cretendials .yml file exists
    if (!file.exists(file)) {
      message("Please, try again by defining where your YML file with the credentials is!")
    } else {
      wd <- getwd()
      setwd(dir)
      credentials <- config::get(from)
      setwd(wd)
      return(credentials)
    }
  } else {
    message(paste("Not a valid 'from' value. Try any of the following:\n", 
                  paste(shQuote(froms), collapse="\n ")))
  }
}
