# Queries on Google Analytics
queryGA <- function(account = "comparamejor",
                    creds = NA, token_dir = NA,
                    metrics = "sessions",
                    dimensions = "date",
                    start = lubridate::floor_date(Sys.Date(), "month"),
                    end = Sys.Date()){
  
  account <- paste("google_analytics", account, sep="_")
  message(paste0("Account:", account))
  vars <- lares::get_credentials(from = account, dir = creds)
  
  # Authenticate with local file
  require(googleAuthR)
  token <- paste0(token_dir, "/", vars$token_name)
  message(paste("Token:", token))
  gar_auth(token)
  
  # Query on Google Analytics
  require(googleAnalyticsR)
  google_analytics(
    vars$ga_id, 
    date_range = c(start, end),
    metrics = metrics,
    dimensions = dimensions)
}

# queryGA(creds = "creds", token_dir = "creds")