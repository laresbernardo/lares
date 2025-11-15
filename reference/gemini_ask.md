# Gemini API Interaction with R

This function lets the user interact with Google's Gemini LLM Model
using its API, and returns the rendered reply.

## Usage

``` r
gemini_ask(
  ask,
  secret_key = get_creds("gemini")$api_key,
  url = Sys.getenv("LARES_GEMINI_API"),
  temperature = 0.5,
  max_tokens = 1024,
  quiet = FALSE,
  ...
)

gemini_image(
  ask,
  image,
  secret_key = get_creds("gemini")$api_key,
  url = Sys.getenv("LARES_GEMINI_API"),
  temperature = 0.5,
  max_tokens = 1024,
  quiet = FALSE,
  ...
)
```

## Arguments

- ask:

  Character. Redacted prompt to ask. If multiple asks are requested,
  they will be concatenated with "+" into a single request.

- secret_key:

  Character. Secret Key. Get yours in: platform.openai.com for OpenAI or
  makersuite.google.com/app/apikey for Gemini.

- url:

  Character. Base API URL.

- temperature:

  Numeric. The temperature to use for generating the response. Default
  is 0.5. The lower the `temperature`, the more deterministic the
  results in the sense that the highest probable next token is always
  picked. Increasing temperature could lead to more randomness, which
  encourages more diverse or creative outputs. You are essentially
  increasing the weights of the other possible tokens. In terms of
  application, you might want to use a lower temperature value for tasks
  like fact-based QA to encourage more factual and concise responses.
  For poem generation or other creative tasks, it might be beneficial to
  increase the temperature value.

- max_tokens:

  Integer. The maximum number of tokens in the response.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- ...:

  Additional parameters.

- image:

  Character. Data to be encoded/decoded. It can be a raw vector, text
  connection or file name.

## Value

(Invisible) list. Content returned from API POST and processed.

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
[`gpt_ask()`](https://laresbernardo.github.io/lares/reference/gpt_ask.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md)

Other LLM:
[`gpt_ask()`](https://laresbernardo.github.io/lares/reference/gpt_ask.md),
[`gpt_prompter()`](https://laresbernardo.github.io/lares/reference/gpt_prompter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_key <- get_credentials()$gemini$api_key
# Open question:
gemini_ask("Can you write an R function to plot a dummy histogram?", api_key)
# Image question
image <- "man/figures/automl_map.png"
gemini_image("Can you explain this flow with more detail?", image, api_key)
} # }
```
