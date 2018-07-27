# Load parameters and credentials
get_credentials <- function(from=NA, dir=NA, filename="config.yml") {

  froms <- c("dummy",
             "production",
             "warehouse",
             "sendgrid",
             "redshift",
             "hubspot",
             "github",
             "bitbucket",
             "typeform",
             "google_api",
             "google_analytics_somosf1",
             "google_analytics_comparamejor",
             "twitter")

  if (is.na(dir)) { dir <- "~/Dropbox (ID)/CM Data Science/Library" }
  if (dir == "personal") { dir <- "~/Dropbox (Personal)/Documentos/Docs/Data" }
  if (dir == "juan") { dir <- "C:/Users/1107051878/Desktop/Personal/Estudio JER/R" }
  if (dir == "matrix") { dir <- "/srv/creds" }

  file <- paste0(dir,"/",filename)

  # Check if the credential type asked exists
  if (from %in% froms) {
    # Check if the cretendials .yml file exists
    if (!file.exists(file)) {
      message("Please, try again by defining where your YML file with the credentials is!")
    } else {
      # Bring credentials
      require(config)
      wd <- getwd()
      setwd(dir)
      credentials <- config::get(from)
      setwd(wd)
      return(credentials)
    }
  } else {
    message(paste("Not a valid 'from' value. Try any of the following:\n", paste(shQuote(froms), collapse="\n ")))
  }
}
