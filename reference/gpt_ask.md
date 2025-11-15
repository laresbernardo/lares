# ChatGPT API Interaction with R

This function lets the user ask ChatGPT via its API, and returns the
rendered reply. There are a couple of specific verbs (functions) with a
preset prompt to help fetch the data in specific formats. We also store
the prompts and replies in current session with their respective
time-stamps so user can gather historical results.

## Usage

``` r
gpt_ask(
  ask,
  secret_key = get_credentials()$openai$secret_key,
  url = Sys.getenv("LARES_GPT_URL"),
  model = Sys.getenv("LARES_GPT_MODEL"),
  num_retries = 3,
  temperature = 0.5,
  max_tokens = NULL,
  pause_base = 1,
  quiet = FALSE,
  ...
)

gpt_history(quiet = TRUE, ...)

gpt_table(x, cols = NULL, quiet = TRUE, ...)

gpt_classify(x, categories, quiet = TRUE, ...)

gpt_tag(x, tags, quiet = TRUE, ...)

gpt_extract(x, extract, quiet = TRUE, ...)

gpt_format(x, format, quiet = TRUE, ...)

gpt_convert(x, unit, quiet = TRUE, ...)

gpt_translate(x, language, quiet = TRUE, ...)
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

- model:

  Character. OpenAI model to use. This can be adjusted according to the
  available models in the OpenAI API (such as "gpt-4").

- num_retries:

  Integer. Number of times to retry the request in case of failure.
  Default is 3.

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

- pause_base:

  Numeric. The number of seconds to wait between retries. Default is 1.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- ...:

  Additional parameters.

- x:

  Vector. List items you wish to process in your instruction

- cols:

  Vector. Force column names for your table results.

- categories, tags:

  Vector. List of possible categories/tags to consider.

- extract, format, unit:

  Character. Length 1 or same as x to extract/format/unit information
  from x. For example: email, country of phone number, country, amount
  as number, currency ISO code, ISO, Fahrenheit, etc.

- language:

  Character. Language to translate to

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
[`gemini_ask()`](https://laresbernardo.github.io/lares/reference/gemini_ask.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md)

Other ChatGPT:
[`gpt_prompter()`](https://laresbernardo.github.io/lares/reference/gpt_prompter.md)

Other LLM:
[`gemini_ask()`](https://laresbernardo.github.io/lares/reference/gemini_ask.md),
[`gpt_prompter()`](https://laresbernardo.github.io/lares/reference/gpt_prompter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_key <- get_credentials()$openai$secret_key
# Open question:
gpt_ask("Can you write an R function to plot a dummy histogram?", api_key)

##### The following examples return dataframes:
# Classify each element based on categories:
gpt_classify(1:10, c("odd", "even"))

# Add all tags that apply to each element based on tags:
gpt_tag(
  c("I love chocolate", "I hate chocolate", "I like Coke"),
  c("food", "positive", "negative", "beverage")
)

# Extract specific information:
gpt_extract(
  c("My mail is 123@test.com", "30 Main Street, Brooklyn, NY, USA", "+82 2-312-3456", "$1.5M"),
  c("email", "full state name", "country of phone number", "amount as number")
)

# Format values
gpt_format(
  c("March 27th, 2021", "12-25-2023 3:45PM", "01.01.2000", "29 Feb 92"),
  format = "ISO Date getting rid of time stamps"
)

# Convert temperature units
gpt_convert(c("50C", "300K"), "Fahrenheit")

# Create a table with data
gpt_table("5 random people's address in South America, email, phone, age between 18-30")
gpt_table(
  ask = "5 largest cities, their countries, and population",
  cols = c("city_name", "where", "POP")
)

# Translate text to any language
gpt_translate(
  rep("I love you with all my heart", 5),
  language = c("spanish", "chinese", "japanese", "russian", "german")
)

# Now let's read the historical prompts, replies, ano more from current session
gpt_history()
} # }
```
