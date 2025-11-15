# Facebook Ads API

This returns all available FB ads for any account, campaign, or ad set
id. For more information on Ad' API, go to the [original
documentaion](https://developers.facebook.com/docs/marketing-api/reference/adgroup)

## Usage

``` r
fb_ads(
  token,
  which,
  start_date = Sys.Date() - 31,
  end_date = Sys.Date(),
  fields = NA,
  api_version = NULL,
  process = TRUE,
  ...
)
```

## Arguments

- token:

  Character. Valid access token with sufficient privileges. Visit the
  [Facebook API Graph
  Explorer](https://developers.facebook.com/tools/explorer) to acquire
  one.

- which:

  Character vector. This is the accounts, campaigns, adsets, or ads IDs
  to be queried. Remember: if `report_level = "account"`, you must start
  the ID with `act_`.

- start_date, end_date:

  Character. The first and last full day to report, in the format
  `"YYYY-MM-DD"`.

- fields:

  Character, json format. Leave `NA` for default fields OR `NULL` to
  ignore.

- api_version:

  Character. Facebook API version.

- process:

  Boolean. Process GET results to a more friendly format?

- ...:

  Additional parameters.

## Value

data.frame with un-nested processed results if `process=TRUE` or raw API
results as list when `process=FALSE`.

## Details

This function was based on FBinsightsR.

## See also

Other API:
[`bring_api()`](https://laresbernardo.github.io/lares/reference/bring_api.md),
[`fb_accounts()`](https://laresbernardo.github.io/lares/reference/fb_accounts.md),
[`fb_creatives()`](https://laresbernardo.github.io/lares/reference/fb_creatives.md),
[`fb_insights()`](https://laresbernardo.github.io/lares/reference/fb_insights.md),
[`fb_process()`](https://laresbernardo.github.io/lares/reference/fb_process.md),
[`fb_report_check()`](https://laresbernardo.github.io/lares/reference/fb_report_check.md),
[`fb_rf()`](https://laresbernardo.github.io/lares/reference/fb_rf.md),
[`fb_token()`](https://laresbernardo.github.io/lares/reference/fb_token.md),
[`gemini_ask()`](https://laresbernardo.github.io/lares/reference/gemini_ask.md),
[`gpt_ask()`](https://laresbernardo.github.io/lares/reference/gpt_ask.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md)

Other Meta:
[`fb_accounts()`](https://laresbernardo.github.io/lares/reference/fb_accounts.md),
[`fb_creatives()`](https://laresbernardo.github.io/lares/reference/fb_creatives.md),
[`fb_insights()`](https://laresbernardo.github.io/lares/reference/fb_insights.md),
[`fb_process()`](https://laresbernardo.github.io/lares/reference/fb_process.md),
[`fb_report_check()`](https://laresbernardo.github.io/lares/reference/fb_report_check.md),
[`fb_rf()`](https://laresbernardo.github.io/lares/reference/fb_rf.md),
[`fb_token()`](https://laresbernardo.github.io/lares/reference/fb_token.md)

## Examples

``` r
if (FALSE) { # \dontrun{
token <- YOURTOKEN
account <- act_ADACCOUNT

# Query all ads for "which" (account) with results in the last 10 days
ads <- fb_ads(token, account, start_date = Sys.Date() - 10)
} # }
```
