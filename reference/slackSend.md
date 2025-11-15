# Send Slack Message (Webhook)

This function send a Slack message using its Webhooks.

## Usage

``` r
slackSend(text, title = "", pretext = "", hook = NA, creds = NA)
```

## Arguments

- text, title, pretext:

  Character. Content on you Slack message.

- hook:

  Character. Web hook URL. Ths value will be overwritten by creds if
  correctly used.

- creds:

  Character. Credential's dir (see
  [`get_creds()`](https://laresbernardo.github.io/lares/reference/get_credentials.md)).
  Set hook URL into the "slack" list in your YML file. Will use first
  value.

## Value

Invisible POST response

## Details

For more help, you can follow the [Sending messages using Incoming
Webhooks](https://api.slack.com/messaging/webhooks#posting_with_webhooks)
original documentarion.

## See also

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
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md)

Other Credentials:
[`db_download()`](https://laresbernardo.github.io/lares/reference/db_download.md),
[`db_upload()`](https://laresbernardo.github.io/lares/reference/db_upload.md),
[`encrypt_file()`](https://laresbernardo.github.io/lares/reference/encrypt_file.md),
[`get_credentials()`](https://laresbernardo.github.io/lares/reference/get_credentials.md),
[`get_tweets()`](https://laresbernardo.github.io/lares/reference/get_tweets.md),
[`mail_send()`](https://laresbernardo.github.io/lares/reference/mail_send.md),
[`queryDB()`](https://laresbernardo.github.io/lares/reference/queryDB.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`stocks_file()`](https://laresbernardo.github.io/lares/reference/stocks_report.md)

## Examples

``` r
if (FALSE) { # \dontrun{
slackSend(text = "This is a message", title = "TEST", pretext = Sys.info()["user"])
} # }
```
