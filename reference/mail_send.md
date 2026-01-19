# Send Emails with Attachments (POST)

This function lets the user send Emails with Attachments using MailGun's
API service.

## Usage

``` r
mail_send(
  from = "RMail <laresbernardo@gmail.com>",
  to = "laresbernardo@gmail.com",
  cc = NULL,
  bcc = NULL,
  subject = "Mail from R",
  text = " \n",
  html = NULL,
  attachment = NULL,
  service = "mailgun",
  creds = NULL,
  quiet = FALSE,
  ...
)
```

## Arguments

- from, to, cc, bcc:

  Character. Emails

- subject:

  Character. Subject for the email.

- text, html:

  Character. Text or HTML to send in the body.

- attachment:

  Character, plot or data.frame. Will send the file, plot as PNG or
  data.frame as CSV, respectively.

- service:

  Character. Service platform to search on `creds`.

- creds:

  Character. Credential's user (see
  [`get_creds()`](https://laresbernardo.github.io/lares/reference/get_credentials.md)).
  Must contain: url (POST address), api (API key).

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- ...:

  Additional parameters.

## Value

No return value, called for side effects.

## See also

Other Tools:
[`autoline()`](https://laresbernardo.github.io/lares/reference/autoline.md),
[`bind_files()`](https://laresbernardo.github.io/lares/reference/bind_files.md),
[`bring_api()`](https://laresbernardo.github.io/lares/reference/bring_api.md),
[`cal_split()`](https://laresbernardo.github.io/lares/reference/cal_split.md),
[`chr2num()`](https://laresbernardo.github.io/lares/reference/chr2num.md),
[`db_download()`](https://laresbernardo.github.io/lares/reference/db_download.md),
[`db_upload()`](https://laresbernardo.github.io/lares/reference/db_upload.md),
[`dont_sleep()`](https://laresbernardo.github.io/lares/reference/dont_sleep.md),
[`export_plot()`](https://laresbernardo.github.io/lares/reference/export_plot.md),
[`export_results()`](https://laresbernardo.github.io/lares/reference/export_results.md),
[`files_functions()`](https://laresbernardo.github.io/lares/reference/files_functions.md),
[`font_exists()`](https://laresbernardo.github.io/lares/reference/font_exists.md),
[`formatColoured()`](https://laresbernardo.github.io/lares/reference/formatColoured.md),
[`formatHTML()`](https://laresbernardo.github.io/lares/reference/format_string.md),
[`glued()`](https://laresbernardo.github.io/lares/reference/glued.md),
[`grepm()`](https://laresbernardo.github.io/lares/reference/grepm.md),
[`h2o_selectmodel()`](https://laresbernardo.github.io/lares/reference/h2o_selectmodel.md),
[`haveInternet()`](https://laresbernardo.github.io/lares/reference/haveInternet.md),
[`image_metadata()`](https://laresbernardo.github.io/lares/reference/image_metadata.md),
[`importxlsx()`](https://laresbernardo.github.io/lares/reference/importxlsx.md),
[`ip_data()`](https://laresbernardo.github.io/lares/reference/ip_data.md),
[`json2vector()`](https://laresbernardo.github.io/lares/reference/json2vector.md),
[`list_cats()`](https://laresbernardo.github.io/lares/reference/list_cats.md),
[`listfiles()`](https://laresbernardo.github.io/lares/reference/listfiles.md),
[`markdown2df()`](https://laresbernardo.github.io/lares/reference/markdown2df.md),
[`move_files()`](https://laresbernardo.github.io/lares/reference/move_files.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md),
[`myip()`](https://laresbernardo.github.io/lares/reference/myip.md),
[`quiet()`](https://laresbernardo.github.io/lares/reference/quiet.md),
[`read.file()`](https://laresbernardo.github.io/lares/reference/read.file.md),
[`statusbar()`](https://laresbernardo.github.io/lares/reference/statusbar.md),
[`tic()`](https://laresbernardo.github.io/lares/reference/tic.md),
[`try_require()`](https://laresbernardo.github.io/lares/reference/try_require.md),
[`updateLares()`](https://laresbernardo.github.io/lares/reference/updateLares.md),
[`warnifnot()`](https://laresbernardo.github.io/lares/reference/warnifnot.md),
[`what_size()`](https://laresbernardo.github.io/lares/reference/what_size.md)

Other Credentials:
[`db_download()`](https://laresbernardo.github.io/lares/reference/db_download.md),
[`db_upload()`](https://laresbernardo.github.io/lares/reference/db_upload.md),
[`encrypt_file()`](https://laresbernardo.github.io/lares/reference/encrypt_file.md),
[`get_credentials()`](https://laresbernardo.github.io/lares/reference/get_credentials.md),
[`get_tweets()`](https://laresbernardo.github.io/lares/reference/get_tweets.md),
[`queryDB()`](https://laresbernardo.github.io/lares/reference/queryDB.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md),
[`stocks_file()`](https://laresbernardo.github.io/lares/reference/stocks_report.md)

## Examples

``` r
if (FALSE) { # \dontrun{
myPlot <- noPlot("My plot")
mail_send(
  from = "BLV <myuser@mail.com>",
  to = "youruser@mail.com",
  cc = "myuser@mail.com",
  subject = paste("Daily report:", Sys.Date()),
  attachment = myPlot
)
} # }
```
