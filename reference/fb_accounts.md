# Facebook Ad Accounts

This returns all ad accounts for a FB Business Account FB. For more
information on Ad Insights' API, go to the [original
documentaion](https://developers.facebook.com/docs/marketing-api/insights/)

## Usage

``` r
fb_accounts(
  token,
  business_id = "904189322962915",
  type = c("owned", "client"),
  limit = 100,
  api_version = NULL,
  ...
)
```

## Arguments

- token:

  Character. Valid access token with sufficient privileges. Visit the
  [Facebook API Graph
  Explorer](https://developers.facebook.com/tools/explorer) to acquire
  one.

- business_id:

  Character. Business ID.

- type:

  Character vector. Values: owned, client.

- limit:

  Integer. Query limit by pagination.

- api_version:

  Character. Facebook API version.

- ...:

  Additional parameters.

## Value

data.frame with un-nested processed results fetched with API.

## See also

Other API:
[`bring_api()`](https://laresbernardo.github.io/lares/reference/bring_api.md),
[`fb_ads()`](https://laresbernardo.github.io/lares/reference/fb_ads.md),
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
[`fb_ads()`](https://laresbernardo.github.io/lares/reference/fb_ads.md),
[`fb_creatives()`](https://laresbernardo.github.io/lares/reference/fb_creatives.md),
[`fb_insights()`](https://laresbernardo.github.io/lares/reference/fb_insights.md),
[`fb_process()`](https://laresbernardo.github.io/lares/reference/fb_process.md),
[`fb_report_check()`](https://laresbernardo.github.io/lares/reference/fb_report_check.md),
[`fb_rf()`](https://laresbernardo.github.io/lares/reference/fb_rf.md),
[`fb_token()`](https://laresbernardo.github.io/lares/reference/fb_token.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Query all accounts (owned and with permissions) of a Business ID
accounts <- fb_accounts(YOURTOKEN, YOURBUSINESS)
} # }
```
