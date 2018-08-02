####################################################################
#' Queries on Google Analytics
#' 
#' This function lets the user query Google Analytics with its API
#' 
#' @param account Character. Personal named accounts
#' @param creds Character. Credential's user (see get_credentials)
#' @param metrics Character. Which metrics we sih to bring
#' @param dimensions Character. Which dimensions we sih to bring
#' @param start Date. Start date for the report
#' @param end Date. End date for the report
#' @export
queryGA <- function(account = "comparamejor",
                    creds = NA, token_dir = NA,
                    metrics = "sessions",
                    dimensions = "date",
                    start = lubridate::floor_date(Sys.Date(), "month"),
                    end = Sys.Date()){
  
  account <- paste("google_analytics", account, sep="_")
  vars <- lares::get_credentials(from = account, dir = creds)

  # Authenticate with local file
  require(googleAuthR)
  if (!is.na(token_dir)) { token <- paste0(token_dir, "/", vars$token_name) } else { token <- vars$token_name }
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
