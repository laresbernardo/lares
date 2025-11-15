# Facebook Reach and Frequency API

Create or query reach and frequency predictions using Facebook's Reach
and Frequency API. For more information on the API and its parameters,
go to the [original
documentaion](https://developers.facebook.com/docs/marketing-api/insights).

## Usage

``` r
fb_rf(
  token,
  ad_account = NA,
  prediction = NA,
  objective = "REACH",
  days = 28,
  budget = 2e+06,
  destination_ids = NA,
  countries = "MX",
  frequency_cap = 8,
  prediction_mode = 1,
  curve = TRUE,
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

- ad_account:

  Character. Ad Account. Remember to start with `act_`. If you use the
  `prediction` argument, no need to provide this parameter.

- prediction:

  Integer. Prediction ID if you already created the prediction and wish
  to query the curve's data. As this prediction already exists, the rest
  of arguments of this function will be ignored.

- objective:

  Character. Any of: "BRAND_AWARENESS", "LINK_CLICKS",
  "POST_ENGAGEMENT", "MOBILE_APP_INSTALLS", "CONVERSIONS", "REACH", or
  "VIDEO_VIEWS".

- days:

  Integer. Amount of days for your campaign's predictions.

- budget:

  Integer. The budget in the Ad Account currency in cents.

- destination_ids:

  Integer vector. Page ID and/or Instagram Account ID.

- countries:

  Character vector. Country's acronyms.

- frequency_cap:

  Integer. Frequency cap over all the campaign duration.

- prediction_mode:

  Integer. "1" for predicting Reach by providing budget, "2" is for
  predicting Budget given a specific Reach.

- curve:

  Boolean. Return curve data? If not, only prediction will be created.

- api_version:

  Character. Facebook API version.

- process:

  Boolean. Process GET results to a more friendly format?

- ...:

  Additional parameters passed to target specs.

## Value

data.frame with un-nested processed results if `process=TRUE` or raw API
results as list when `process=FALSE`.

## See also

Other API:
[`bring_api()`](https://laresbernardo.github.io/lares/reference/bring_api.md),
[`fb_accounts()`](https://laresbernardo.github.io/lares/reference/fb_accounts.md),
[`fb_ads()`](https://laresbernardo.github.io/lares/reference/fb_ads.md),
[`fb_creatives()`](https://laresbernardo.github.io/lares/reference/fb_creatives.md),
[`fb_insights()`](https://laresbernardo.github.io/lares/reference/fb_insights.md),
[`fb_process()`](https://laresbernardo.github.io/lares/reference/fb_process.md),
[`fb_report_check()`](https://laresbernardo.github.io/lares/reference/fb_report_check.md),
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
[`fb_report_check()`](https://laresbernardo.github.io/lares/reference/fb_report_check.md),
[`fb_token()`](https://laresbernardo.github.io/lares/reference/fb_token.md)

## Examples

``` r
if (FALSE) { # \dontrun{
token <- "YOURTOKEN"
account_id <- "act_20846447"

# BASIC 1: Create and return data for a new prediction
basic1 <- fb_rf(token, account_id, destination_ids = 187071108930, countries = "AR")

# BASIC 2: Fetch data for an existing prediction ID
basic2 <- fb_rf(token, account_id, prediction = 6317720998974)

# ADVANCED (Fully custom prediction)
advanced <- fb_rf(token, account_id,
  objective = "REACH",
  days = 28,
  budget = 2000000,
  destination_ids = c(187071108930, 1142958119078556),
  age_min = 15,
  age_max = 65,
  genders = 2,
  countries = "MX",
  publisher_platforms = c(
    "facebook",
    "instagram",
    #' audience_network',
    "messenger"
  ),
  # interests_ids = NA,
  facebook_positions = c(
    "feed",
    #' instant_article',
    "marketplace",
    "video_feeds",
    "story",
    "search",
    "instream_video"
  ),
  instagram_positions = c(
    "stream",
    "story",
    "explore"
  ),
  # audience_network_positions = c(
  #  'classic',
  #  'instream_video')
  messenger_positions = c(
    "messenger_home",
    "sponsored_messages",
    "story"
  ),
  device_platforms = c(
    "mobile",
    "desktop"
  )
)
} # }
```
