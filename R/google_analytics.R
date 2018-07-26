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
  creds <- ifelse(grepl("matrix", creds), "matrix", creds)
  
  vars <- lares::get_credentials(from = account, dir = creds)
  ga_id <- vars$ga_id
  
  if (is.na(creds)) {
    token <- vars$token_name
  } else {
    if (creds == "matrix_test") {
      token <- paste0("~/creds", substr(vars$token_name, 2, 100))
    } else {
      if (creds == "matrix") {
        token <- paste0(creds, substr(vars$token_name, 2, 100))
      } else
        token <- vars$token_name
    }
  }
  message(paste("Token to use:", token))
  googleAuthR::gar_auth(token)
  
  return(
    googleAnalyticsR::google_analytics(
      ga_id, 
      date_range = c(start, end),
      metrics = metrics,
      dimensions = dimensions)
  )
  
  googleAuth(revoke = TRUE)
  
}
