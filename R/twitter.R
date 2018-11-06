get_tweets <- function(q, n = 10000, creds = NA) {
  
  # require(config)
  # require(rtweet)
  
  c <- lares::get_credentials(from = "twitter", dir = creds)
  
  token <- create_token(
    app = c$app,
    consumer_key = c$consumer_key,
    consumer_secret = c$consumer_secret,
    access_token = c$access_token,
    access_secret = c$access_secret)
  
  out <- search_tweets(q = as.character(q), n = n, retryonratelimit = T)
  
  return(out)
  
}
