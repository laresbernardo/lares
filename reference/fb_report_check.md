# Facebook API Report Status Check

This returns all available FB insights per day including any given
breakdown to the specified report level, and place into a data frame.
For more information on Ad Insights' API, go to the original
[documentaion](https://developers.facebook.com/docs/marketing-api/insights/).

## Usage

``` r
fb_report_check(
  token,
  report_run_id,
  api_version = NULL,
  live = FALSE,
  sleep = 10,
  quiet = FALSE
)
```

## Arguments

- token:

  Character. Valid access token with sufficient privileges. Visit the
  [Facebook API Graph
  Explorer](https://developers.facebook.com/tools/explorer) to acquire
  one.

- report_run_id:

  Integer. Report ID to check status.

- api_version:

  Character. Facebook API version.

- live:

  Boolean. Run until status report is finished?

- sleep:

  Boolean. If `live=TRUE`, then how many seconds should we wait until
  next check?

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## Value

List with API status results.

## See also

Other API:
[`bring_api()`](https://laresbernardo.github.io/lares/reference/bring_api.md),
[`fb_accounts()`](https://laresbernardo.github.io/lares/reference/fb_accounts.md),
[`fb_ads()`](https://laresbernardo.github.io/lares/reference/fb_ads.md),
[`fb_creatives()`](https://laresbernardo.github.io/lares/reference/fb_creatives.md),
[`fb_insights()`](https://laresbernardo.github.io/lares/reference/fb_insights.md),
[`fb_process()`](https://laresbernardo.github.io/lares/reference/fb_process.md),
[`fb_rf()`](https://laresbernardo.github.io/lares/reference/fb_rf.md),
[`fb_token()`](https://laresbernardo.github.io/lares/reference/fb_token.md),
[`gemini_ask()`](https://laresbernardo.github.io/lares/reference/gemini_ask.md),
[`gpt_ask()`](https://laresbernardo.github.io/lares/reference/gpt_ask.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md)

Other Meta:
[`fb_accounts()`](https://laresbernardo.github.io/lares/reference/fb_accounts.md),
[`fb_ads()`](https://laresbernardo.github.io/lares/reference/fb_ads.md),
[`fb_creatives()`](https://laresbernardo.github.io/lares/reference/fb_creatives.md),
[`fb_insights()`](https://laresbernardo.github.io/lares/reference/fb_insights.md),
[`fb_process()`](https://laresbernardo.github.io/lares/reference/fb_process.md),
[`fb_rf()`](https://laresbernardo.github.io/lares/reference/fb_rf.md),
[`fb_token()`](https://laresbernardo.github.io/lares/reference/fb_token.md)

## Examples

``` r
if (FALSE) { # \dontrun{
token <- "YOURTOKEN"
report_run_id <- "123456789"
fb_report_check(token, report_run_id, live = TRUE, quiet = FALSE)
} # }
```
