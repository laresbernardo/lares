# Load Credentials from a YML File

Load credentials from a local YML file. You can set your `.Renviron` and
the `LARES_CREDS` parameter to remember (forever) the directory of your
credentials' file. To use it later, you may leave `dir = NA`. You may
also use this function for external (non-`lares`) code/use.

## Usage

``` r
get_credentials(
  from = NA,
  dir = NA,
  filename = "config.yml",
  env = "LARES_CREDS",
  ...
)

get_creds(
  from = NA,
  dir = NA,
  filename = "config.yml",
  env = "LARES_CREDS",
  ...
)
```

## Arguments

- from:

  Character. Family of values to import from the YML file. If you don't
  know these names, set `from = NA` and a warning will display all
  possible values, depending on your YML file.

- dir:

  Character. Credentials directory where your YML file is. If used
  frequently, set your directory by using the `.Renviron` file. To do
  so, leave `dir` as `NA` and follow the steps. If `dir` is a list,
  it'll return `dir` (manual credentials input).

- filename:

  Character. YML filename with your credentials.

- env:

  Character. Environment variable name. No need to set differently for
  any function that uses this library. Only for external use.

- ...:

  Additional parameters.

## Value

List. Result of reading your credential's YML file, filtered by your
`from` input if provided.

## Set the default directory

The first time you use any function that has the `creds` parameter, if
the `dir` parameter is set to `NA`, this function will ask you to set
the directory where you save your YML local file with your credentials.
This will be asked once and will be set for further R sessions. Remember
to reset your session for this setup to start working properly.

## YML file format

A YML file is a text file, with `.yml` file format. You may start from
the dummy YML file shared which shows the structure you must follow to
set your credentials file. Check it out
[here](https://raw.githubusercontent.com/laresbernardo/lares/master/inst/docs/config.yml)
or find it locally using
`system.file("docs", "config.yml", package = "lares")`.

## See also

Other Credentials:
[`db_download()`](https://laresbernardo.github.io/lares/reference/db_download.md),
[`db_upload()`](https://laresbernardo.github.io/lares/reference/db_upload.md),
[`encrypt_file()`](https://laresbernardo.github.io/lares/reference/encrypt_file.md),
[`get_tweets()`](https://laresbernardo.github.io/lares/reference/get_tweets.md),
[`mail_send()`](https://laresbernardo.github.io/lares/reference/mail_send.md),
[`queryDB()`](https://laresbernardo.github.io/lares/reference/queryDB.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md),
[`stocks_file()`](https://laresbernardo.github.io/lares/reference/stocks_report.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load dummy config.yml file from the library
# Recommendation: set dir with NA (read documentation)
# We need the directory, not the file
yml <- dirname(system.file("docs", "config.yml", package = "lares"))

# Let's see which credentials we have in our file
get_credentials(dir = yml)
# Warning message: No credentials for NA found in your YML file.
# Try any of the following: 'service1', 'service2', 'service3'

# Get credentials for service2
get_credentials("service2", dir = yml)
} # }
```
