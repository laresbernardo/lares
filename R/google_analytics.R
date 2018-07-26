# Queries
ga_query <- function(account = "comparamejor", 
                     creds = NA,
                     metrics = "sessions",
                     dimensions = "date",
                     start = lubridate::floor_date(Sys.Date(), "month"), 
                     end = Sys.Date()) {
  
  account <- paste("google_analytics", account, sep="_")
  message(paste("Account:", account))
  
  require(googleAuthR)
  require(googleAnalyticsR)
  
  vars <- lares::get_credentials(from = account, dir = creds)
  ga_id <- vars$ga_id
  
  if (is.na(creds)) {
    gar_auth(token = vars$token_name) 
  } else {
    if (creds == "matrix") {
      gar_auth(token = paste0(creds, substr(vars$token_name, 2, 100)))
    } else {
      gar_auth(token = vars$token_name) 
    }
  }
  
  return(
    googleAnalyticsR::google_analytics(
      ga_id, 
      date_range = c(start, end),
      metrics = metrics,
      dimensions = dimensions)
  )
  
  googleAuth(revoke = TRUE)
  
}
