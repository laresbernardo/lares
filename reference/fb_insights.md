# Facebook Insights API

This returns all available FB insights per day including any given
breakdown to the specified report level, and place into a data frame.
For more information on Ad Insights' API, go to the original
[documentaion](https://developers.facebook.com/docs/marketing-api/insights/).

## Usage

``` r
fb_insights(
  token,
  which,
  start_date = Sys.Date() - 7,
  end_date = Sys.Date(),
  time_increment = "1",
  report_level = "campaign",
  ad_object = "insights",
  breakdowns = NA,
  fields = NA,
  filtering = NULL,
  limit = 100,
  api_version = NULL,
  process = TRUE,
  async = FALSE,
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

- time_increment:

  Character. Group by months ("monthly"), everything together
  ("all_days") or an integer per days \[1-90\]. Default: each day
  separately (i.e. "1").

- report_level:

  Character. One of "ad", "adset", "campaign", or "account"

- ad_object:

  Character. One of: "insights" (default), "adsets", ...

- breakdowns:

  Character Vector. One or more of breakdowns for segmentation results.
  Set to NA for no breakdowns

- fields:

  Character, json format. Leave `NA` for default fields OR `NULL` to
  ignore.

- filtering:

  List. Each filter will be a list containing "field", "operator", and
  "value". Read more about the operators in the official
  [docs](https://developers.facebook.com/docs/marketing-api/insights).
  Example:
  `dplyr::tibble(field = "country", operator = "IN", value = list("PE")))`.

- limit:

  Integer. Query limit by pagination.

- api_version:

  Character. Facebook API version.

- process:

  Boolean. Process GET results to a more friendly format?

- async:

  Boolean. Run an async query. When set to `TRUE`, instead of making a
  GET query, it'll run a POST query and will return a report run ID.

- ...:

  Additional parameters.

## Value

data.frame with un-nested processed results if `process=TRUE` or raw API
results as list when `process=FALSE`.

## See also

Other API:
[`bring_api()`](https://laresbernardo.github.io/lares/reference/bring_api.md),
[`fb_accounts()`](https://laresbernardo.github.io/lares/reference/fb_accounts.md),
[`fb_ads()`](https://laresbernardo.github.io/lares/reference/fb_ads.md),
[`fb_creatives()`](https://laresbernardo.github.io/lares/reference/fb_creatives.md),
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
[`fb_ads()`](https://laresbernardo.github.io/lares/reference/fb_ads.md),
[`fb_creatives()`](https://laresbernardo.github.io/lares/reference/fb_creatives.md),
[`fb_process()`](https://laresbernardo.github.io/lares/reference/fb_process.md),
[`fb_report_check()`](https://laresbernardo.github.io/lares/reference/fb_report_check.md),
[`fb_rf()`](https://laresbernardo.github.io/lares/reference/fb_rf.md),
[`fb_token()`](https://laresbernardo.github.io/lares/reference/fb_token.md)

## Examples

``` r
if (FALSE) { # \dontrun{
token <- "YOURTOKEN"
which <- "act_20846447"

# Platforms' Insights: all ad-sets platforms of "which" account,
# aggregated, for the last 30 days
platforms <- fb_insights(
  token, which,
  start_date = Sys.Date() - 30,
  time_increment = "all_days",
  report_level = "adset",
  fields = c(
    "account_name",
    "adset_id",
    "adset_start",
    "adset_end"
  ),
  breakdowns = c(
    "publisher_platform",
    "platform_position",
    "device_platform"
  )
)

# Daily results for all campaigns of "which" account,
# with custom performance fields with no breakdowns.
insights_adset <- fb_insights(
  token, which,
  time_increment = "1",
  report_level = "campaign",
  fields = c(
    "adset_id",
    "reach",
    "frequency",
    "spend",
    "cpm",
    "objective",
    "optimization_goal"
  )
)
} # }
```
