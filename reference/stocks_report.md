# Build a Personal Investing Portfolio Report

`stocks_file()` lets the user download his personal Excel with his
Portfolio's data, locally or from Dropbox.

`daily_stocks()` creates a dataframe with all relevant metrics and
values, for each ticker or symbol, for every day since inception.

`daily_portfolio()` creates a data.frame with all relevant metrics and
values, for the overall portfolio, for every day since inception.

`stocks_obj()` lets the user create his portfolio's calculations and
plots for further study.

`stocks_report()` lets the user create his portfolio's full report with
plots and send it to an email with the HTML report attached

## Usage

``` r
stocks_file(
  file = NA,
  creds = NA,
  auto = TRUE,
  sheets = c("Portafolio", "Fondos", "Transacciones"),
  keep_old = TRUE,
  cache = TRUE,
  quiet = FALSE,
  ...
)

daily_stocks(hist, trans, tickers = NA, window = "MAX", ...)

daily_portfolio(hist, trans, cash, cash_fix = 0, window = "MAX")

stocks_obj(
  data = stocks_file(),
  cash_fix = 0,
  tax = 30,
  sectors = FALSE,
  parg = FALSE,
  window = c("1M", "YTD", "1Y", "MAX"),
  cache = TRUE,
  quiet = FALSE
)

stocks_report(
  data = NA,
  keep_old = TRUE,
  dir = NA,
  mail = FALSE,
  attachment = TRUE,
  to = "laresbernardo@gmail.com",
  sectors = FALSE,
  keep = FALSE,
  creds = NA,
  cache = TRUE
)
```

## Arguments

- file:

  Character. Import an Excel file, local or from URL.

- creds:

  Character. Credential's user (see
  [`get_creds()`](https://laresbernardo.github.io/lares/reference/get_credentials.md))
  for sending mail and Dropbox interaction.

- auto:

  Boolean. Automatically use my local personal file? You might want to
  set in into your .Renviron `LARES_PORTFOLIO=~/dir/to/your/file.xlsx`
  so you can leave all other parameters as `NA` and use it every time.

- sheets:

  Character Vector. Names of each sheet containing Portfolio summary,
  Cash, and Transactions information. Please, keep the order of these
  tabs.

- keep_old:

  Boolean. Include sold tickers even though not currently in portfolio?

- cache:

  Boolean. Use daily cache if available?

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- ...:

  Additional parameters.

- hist:

  Dataframe. Result from
  [`stocks_hist()`](https://laresbernardo.github.io/lares/reference/stocks_hist.md)

- trans:

  Dataframe. Result from `stocks_file()$transactions`

- tickers:

  Dataframe. Result from `stocks_file()$portfolio`

- window:

  Character. Choose any of: "1W", "1M", "6M", "1Y", "YTD", "5Y", "MAX"

- cash:

  Dataframe. Result from `stocks_file()$cash`

- cash_fix:

  Numeric. If, for some reason, you need to fix your cash amount for all
  reports, set the amount here

- data:

  Character. `stocks_obj()` output. If NA, automatic parameters and
  `stocks_file()` defaults will be used.

- tax:

  Numeric. How much \[0-99\] of your dividends are gone with taxes?

- sectors:

  Boolean. Return sectors segmentation for ETFs?

- parg:

  Boolean. Personal argument. Used to personalize stuff, in this case,
  taxes changed from A to B in given date (hard-coded)

- dir:

  Character. Directory for HTML report output. If set to NA, current
  working directory will be used. If mail sent, file will be erased

- mail:

  Boolean. Do you want to send an email with the report attached? If
  not, an HTML file will be created in dir

- attachment:

  Boolean. Create and add report as attachment if `mail=TRUE`? If not,
  no report will be rendered and only tabulated summaries will be
  included on email's body.

- to:

  Character. Email to send the report to

- keep:

  Boolean. Keep HTML file when sent by email?

## Value

List with portfolio, transactions, and cash data.frames.

data.frame. Processed at date and symbol level.

data.frame. Processed at date and portfolio level.

List. Aggregated results and plots.

Invisible list. Aggregated results and plots.

## See also

Other Investment:
[`etf_sector()`](https://laresbernardo.github.io/lares/reference/etf_sector.md),
[`splot_summary()`](https://laresbernardo.github.io/lares/reference/stocks_plots.md),
[`stocks_quote()`](https://laresbernardo.github.io/lares/reference/stocks_hist.md)

Other Credentials:
[`db_download()`](https://laresbernardo.github.io/lares/reference/db_download.md),
[`db_upload()`](https://laresbernardo.github.io/lares/reference/db_upload.md),
[`encrypt_file()`](https://laresbernardo.github.io/lares/reference/encrypt_file.md),
[`get_credentials()`](https://laresbernardo.github.io/lares/reference/get_credentials.md),
[`get_tweets()`](https://laresbernardo.github.io/lares/reference/get_tweets.md),
[`mail_send()`](https://laresbernardo.github.io/lares/reference/mail_send.md),
[`queryDB()`](https://laresbernardo.github.io/lares/reference/queryDB.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load lares dummy portfolio XLSX
file <- system.file("inst/docs", "dummyPortfolio.xlsx", package = "lares")
df <- stocks_file(
  file = file,
  sheets = c("Portafolio", "Fondos", "Transacciones"),
  keep_old = FALSE
)
} # }
if (FALSE) { # \dontrun{
list <- stocks_obj()
stocks_report(list, dir = "~/Desktop")
} # }
```
