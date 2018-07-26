# Queries
ga_query <- function(account = "comparamejor", 
                     creds = NA,
                     metrics = "sessions",
                     dimensions = "date",
                     start = as.character(lubridate::floor_date(Sys.Date(), "month")), 
                     end = as.character(Sys.Date())) {
  
  account <- paste("google_analytics", account, sep="_")
  message(paste("Account:", account))
  
  require(googleAuthR)
  require(googleAnalyticsR)
  vars <- lares::get_credentials(from = account, dir = creds)
  ga_id <- vars$ga_id
  if (creds == "matrix") {
    gar_auth(token = sub("(.{2})(.*)", "\\1creds\\/\\2", vars$token_name))
  } else {
    gar_auth(token = vars$token_name) 
  }
  
  return(
    google_analytics(
    ga_id, 
    start = start,
    end = end,
    metrics = metrics,
    dimensions = dimensions)
  )
  
  googleAuth(revoke = TRUE)

}
