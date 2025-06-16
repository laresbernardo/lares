####################################################################
#' Get Tweets
#'
#' This function downloads tweets with personal credentials
#'
#' @family Credentials
#' @family Twitter
#' @param q Query. Check for ?rtweet::search_tweets()
#' @param n Integer. Total of tweets to return
#' @param creds Character. Credential's user (see \code{get_creds()})
#' @return data.frame with API response results.
#' @export
get_tweets <- function(q, n = 10000, creds = NA) {
  try_require("rtweet")
  c <- get_credentials(from = "twitter", dir = creds)
  token <- create_token(
    app = c$app,
    consumer_key = c$consumer_key,
    consumer_secret = c$consumer_secret,
    access_token = c$access_token,
    access_secret = c$access_secret
  )
  search_tweets(q = as.character(q), n = n, retryonratelimit = T)
}
