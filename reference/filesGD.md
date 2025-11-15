# Google Drive Files (API v4)

Authenticate and find Google Drive files and IDs by name.

## Usage

``` r
filesGD(title, server = FALSE, json = NULL, api_key = NULL, email = NULL)
```

## Arguments

- title:

  Character. Title of Google Drive file. Uses regular expressions so you
  may fetch with patterns instead of names.

- server:

  Boolean. Force interacting auth process?

- json:

  Character. JSON filename with service auth

- email, api_key:

  Character. If you have multiple pre-authorized accounts in your
  machine, you may non-interactively select which one you wish to use by
  email and/or api_key.

## Value

Vector with found file names based on `title` on Google Drive.

## See also

Other Scrapper:
[`gtrends_related()`](https://laresbernardo.github.io/lares/reference/google_trends.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`ip_data()`](https://laresbernardo.github.io/lares/reference/ip_data.md),
[`mp3_get()`](https://laresbernardo.github.io/lares/reference/mp3_get.md),
[`readGS()`](https://laresbernardo.github.io/lares/reference/google_sheets.md),
[`splot_summary()`](https://laresbernardo.github.io/lares/reference/stocks_plots.md),
[`stocks_quote()`](https://laresbernardo.github.io/lares/reference/stocks_hist.md)

Other Google:
[`gtrends_related()`](https://laresbernardo.github.io/lares/reference/google_trends.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`readGS()`](https://laresbernardo.github.io/lares/reference/google_sheets.md)
