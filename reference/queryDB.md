# PostgreSQL Queries on Database (Read)

This function lets the user query a PostgreSQL database. Previously was
called `queryDummy` but was replaced and deprecated for a more general
function by using the `from` parameter.

## Usage

``` r
queryDB(query, from, creds = NA)
```

## Arguments

- query:

  Character. SQL Query

- from:

  Character. Credential's user (see
  [`get_creds()`](https://laresbernardo.github.io/lares/reference/get_credentials.md))

- creds:

  Character. Credential's directory (see
  [`get_creds()`](https://laresbernardo.github.io/lares/reference/get_credentials.md))

## Value

data.frame. Result of fetching the `query` data.

## See also

Other Credentials:
[`db_download()`](https://laresbernardo.github.io/lares/reference/db_download.md),
[`db_upload()`](https://laresbernardo.github.io/lares/reference/db_upload.md),
[`encrypt_file()`](https://laresbernardo.github.io/lares/reference/encrypt_file.md),
[`get_credentials()`](https://laresbernardo.github.io/lares/reference/get_credentials.md),
[`get_tweets()`](https://laresbernardo.github.io/lares/reference/get_tweets.md),
[`mail_send()`](https://laresbernardo.github.io/lares/reference/mail_send.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md),
[`stocks_file()`](https://laresbernardo.github.io/lares/reference/stocks_report.md)
