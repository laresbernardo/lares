# Google Sheets Reading and Writing (API v4)

Read and write data from Google Sheets knowing the file's title. You may
use a single value from a cell or a data.frame from a cell range.

## Usage

``` r
readGS(
  title,
  sheet = "Hoja 1",
  range = NULL,
  drop_nas = TRUE,
  json = NULL,
  email = NULL,
  api_key = NULL,
  server = FALSE,
  ...
)

writeGS(
  data,
  title,
  sheet = "Hoja 1",
  range = "A1",
  reformat = FALSE,
  append = FALSE,
  json = NULL,
  email = NULL,
  api_key = NULL,
  server = FALSE,
  ...
)
```

## Arguments

- title:

  Character. Title of Google Drive file. Uses regular expressions so you
  may fetch with patterns instead of names.

- sheet:

  Character. Working sheet to import

- range:

  Character. A cell range to read from

- drop_nas:

  Boolean. Remove columns and rows that contain only NAs?

- json:

  Character. JSON filename with service auth

- email, api_key:

  Character. If you have multiple pre-authorized accounts in your
  machine, you may non-interactively select which one you wish to use by
  email and/or api_key.

- server:

  Boolean. Force interacting auth process?

- ...:

  Additional parameters passed to `read_sheet()`.

- data:

  Object (value, vector, data.frame, list).

- reformat:

  Boolean. Reformat the affected cells?

- append:

  Boolean.

## Value

For reading, data.frame with the results of your Google Sheets file
based on its `title`, specificially the `sheet` and `range` requested.
For writing, no return value.

## See also

Other Scrapper:
[`filesGD()`](https://laresbernardo.github.io/lares/reference/filesGD.md),
[`gtrends_related()`](https://laresbernardo.github.io/lares/reference/google_trends.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`ip_data()`](https://laresbernardo.github.io/lares/reference/ip_data.md),
[`mp3_get()`](https://laresbernardo.github.io/lares/reference/mp3_get.md),
[`splot_summary()`](https://laresbernardo.github.io/lares/reference/stocks_plots.md),
[`stocks_quote()`](https://laresbernardo.github.io/lares/reference/stocks_hist.md)

Other Google:
[`filesGD()`](https://laresbernardo.github.io/lares/reference/filesGD.md),
[`gtrends_related()`](https://laresbernardo.github.io/lares/reference/google_trends.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md)
