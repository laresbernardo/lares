# Prevent Computer from Sleeping by Simulating Mouse Activity

Keeps the computer awake by moving the mouse cursor and clicking
periodically with a human-like behavior. It automatically detects the
operating system (Windows or macOS) and runs the appropriate underlying
logic.

## Usage

``` r
dont_sleep(sec_range = c(20, 60), off_time = 18.5, click = TRUE, quiet = FALSE)

dont_sleep_time(quiet = FALSE)
```

## Arguments

- sec_range:

  Numeric vector of length 2. Range (in seconds) of random intervals
  between mouse movements and clicks. Default is `c(20, 60)`.

- off_time:

  Numeric. Decimal hour (24h format) to stop the function automatically,
  e.g. 18.5 means 18:30 (6:30 PM). Default is `18.5`.

- click:

  Logical. If `TRUE`, clicks the mouse. Default is `TRUE`.

- quiet:

  Logical. If `TRUE`, suppresses progress messages. Default is `FALSE`.

## Value

Invisibly returns `NULL` when the function exits.

## Details

The cursor initially moves to the upper-left corner (0,0), then performs
small random movements and clicks periodically to avoid system sleep.
The function exits if the user moves the mouse away from the corner or
when the specified off-time is reached.

Requires `cliclick` installed on macOS (install via Homebrew:
`brew install cliclick`), and the `KeyboardSimulator` package on
Windows.

## See also

Other Tools:
[`autoline()`](https://laresbernardo.github.io/lares/reference/autoline.md),
[`bind_files()`](https://laresbernardo.github.io/lares/reference/bind_files.md),
[`bring_api()`](https://laresbernardo.github.io/lares/reference/bring_api.md),
[`chr2num()`](https://laresbernardo.github.io/lares/reference/chr2num.md),
[`db_download()`](https://laresbernardo.github.io/lares/reference/db_download.md),
[`db_upload()`](https://laresbernardo.github.io/lares/reference/db_upload.md),
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
[`mail_send()`](https://laresbernardo.github.io/lares/reference/mail_send.md),
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

## Examples

``` r
if (FALSE) { # \dontrun{
# Keep the computer awake until 8:00 PM, with mouse actions every 30 to 60 seconds
dont_sleep(sec_range = c(30, 60), off_time = 20)
# Summary of amount of daily time running function
dont_sleep_time()
} # }
```
