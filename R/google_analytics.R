# Queries
ga_query <- function(account = "comparamejor", 
                     creds = NA,
                     metrics = "sessions",
                     dimensions = "date",
                     start = lubridate::floor_date(Sys.Date(), "month"), 
                     end = Sys.Date()) {
  message("1")
  account <- paste("google_analytics", account, sep="_")
  message(paste("Account:", account))
  message("2")
  require(googleAuthR)
  require(googleAnalyticsR)
  message("3")
  vars <- lares::get_credentials(from = account, dir = creds)
  ga_id <- vars$ga_id
  gar_auth()
  
  return(
    googleAnalyticsR::google_analytics(
      ga_id, 
      date_range = c(start, end),
      metrics = metrics,
      dimensions = dimensions)
  )
  
  googleAuth(revoke = TRUE)
  
}
