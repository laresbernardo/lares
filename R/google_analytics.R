# Authenticate
ga_auth <- function(account = "comparamejor", creds = NA) {
  
  if(account == "comparamejor") {
    credentials <- lares::get_credentials(from = "google_analytics", dir = creds)
    client_id <- credentials$client_id
    client_secret <- credentials$client_secret
    message(paste("Client ID:",credentials$client_id))
    message(paste("Client Secret:",credentials$client_secret))
  } else {
    stop("You must provide a valid account!")
  }
  
  require(googleAuthR)
  options(googleAuthR.client_id = client_id, 
          googleAuthR.client_secret = client_secret,
          googleAuthR.scopes.selected = "https://analyticsreporting.googleapis.com")
  gar_auth()
}

# Queries
ga_query <- function(account = "comparamejor", creds = NA,
                     metrics = "sessions",
                     start = as.character(lubridate::floor_date(Sys.Date(), "month")), 
                     end = as.character(Sys.Date())) {
  
  if(account == "comparamejor") {
    ga_id <- "174483551"
  } else {
    stop("You must provide a valid account!")
  }
  
  require(googleAuthR)
  require(googleAnalyticsR)
  
  if(creds == "matrix") {
    gar_auth(token = "~/creds/ga.httr-oauth")
  }
  
  google_analytics(
    ga_id, 
    start = start,
    end = as.character(Sys.Date()),
    metrics = metrics)
  
}
