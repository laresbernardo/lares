# Queries on Google Analytics

This function lets the user query Google Analytics with its API. More
about the documentation and parameters in
`googleAnalyticsR::google_analytics()` or Google Analytics' API.

## Usage

``` r
queryGA(
  account,
  creds = NA,
  token_dir = NA,
  metrics = "sessions",
  dimensions = "date",
  met_filters = NULL,
  dim_filters = NULL,
  start = lubridate::floor_date(Sys.Date(), "month"),
  end = Sys.Date()
)
```

## Arguments

- account:

  Character. Personal named accounts

- creds:

  Character. Credential's user (see
  [`get_creds()`](https://laresbernardo.github.io/lares/reference/get_credentials.md))

- token_dir:

  Character. Credential's directory (see
  [`get_creds()`](https://laresbernardo.github.io/lares/reference/get_credentials.md))

- metrics:

  Character. Which metrics we wish to bring

- dimensions:

  Character. Which dimensions we wish to bring

- met_filters, dim_filters:

  A `filter_clause_ga4` for filtering metrics/dimensions. Check
  `googleAnalyticsR::google_analytics()`.

- start:

  Date. Start date for the report

- end:

  Date. End date for the report

## Value

data.frame with the API GET request tabulated results.

## See also

Other Credentials:
[`db_download()`](https://laresbernardo.github.io/lares/reference/db_download.md),
[`db_upload()`](https://laresbernardo.github.io/lares/reference/db_upload.md),
[`encrypt_file()`](https://laresbernardo.github.io/lares/reference/encrypt_file.md),
[`get_credentials()`](https://laresbernardo.github.io/lares/reference/get_credentials.md),
[`get_tweets()`](https://laresbernardo.github.io/lares/reference/get_tweets.md),
[`mail_send()`](https://laresbernardo.github.io/lares/reference/mail_send.md),
[`queryDB()`](https://laresbernardo.github.io/lares/reference/queryDB.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md),
[`stocks_file()`](https://laresbernardo.github.io/lares/reference/stocks_report.md)

Other Google:
[`filesGD()`](https://laresbernardo.github.io/lares/reference/filesGD.md),
[`gtrends_related()`](https://laresbernardo.github.io/lares/reference/google_trends.md),
[`readGS()`](https://laresbernardo.github.io/lares/reference/google_sheets.md)

Other API:
[`bring_api()`](https://laresbernardo.github.io/lares/reference/bring_api.md),
[`fb_accounts()`](https://laresbernardo.github.io/lares/reference/fb_accounts.md),
[`fb_ads()`](https://laresbernardo.github.io/lares/reference/fb_ads.md),
[`fb_creatives()`](https://laresbernardo.github.io/lares/reference/fb_creatives.md),
[`fb_insights()`](https://laresbernardo.github.io/lares/reference/fb_insights.md),
[`fb_process()`](https://laresbernardo.github.io/lares/reference/fb_process.md),
[`fb_report_check()`](https://laresbernardo.github.io/lares/reference/fb_report_check.md),
[`fb_rf()`](https://laresbernardo.github.io/lares/reference/fb_rf.md),
[`fb_token()`](https://laresbernardo.github.io/lares/reference/fb_token.md),
[`gemini_ask()`](https://laresbernardo.github.io/lares/reference/gemini_ask.md),
[`gpt_ask()`](https://laresbernardo.github.io/lares/reference/gpt_ask.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md)
