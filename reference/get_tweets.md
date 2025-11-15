# Get Tweets

This function downloads tweets with personal credentials

## Usage

``` r
get_tweets(q, n = 10000, creds = NA)
```

## Arguments

- q:

  Query. Check for ?rtweet::search_tweets()

- n:

  Integer. Total of tweets to return

- creds:

  Character. Credential's user (see
  [`get_creds()`](https://laresbernardo.github.io/lares/reference/get_credentials.md))

## Value

data.frame with API response results.

## See also

Other Credentials:
[`db_download()`](https://laresbernardo.github.io/lares/reference/db_download.md),
[`db_upload()`](https://laresbernardo.github.io/lares/reference/db_upload.md),
[`encrypt_file()`](https://laresbernardo.github.io/lares/reference/encrypt_file.md),
[`get_credentials()`](https://laresbernardo.github.io/lares/reference/get_credentials.md),
[`mail_send()`](https://laresbernardo.github.io/lares/reference/mail_send.md),
[`queryDB()`](https://laresbernardo.github.io/lares/reference/queryDB.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md),
[`stocks_file()`](https://laresbernardo.github.io/lares/reference/stocks_report.md)
