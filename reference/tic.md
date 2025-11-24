# Stopwatch to measure timings in R

Start a stopwatch.

Stop a stopwatch.

## Usage

``` r
tic(id = 1, start = proc.time()["elapsed"], quiet = TRUE)

toc(
  id = 1,
  msg = "Elapsed time:",
  type = "units",
  signif = 3,
  quiet = FALSE,
  ...
)
```

## Arguments

- id:

  Define ID if multiple `tic` & `toc` are being used.

- start:

  Start time. Now is default.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- msg:

  Character. Custom message shown

- type:

  Character. Output format for `time` list element. Choose any of:
  `units, clock, seconds`.

- signif:

  Integer. Significant digits.

- ...:

  Additional parameters.

## Value

Invisible list. Contains tic (start time), toc (stop time), elapsed time
and message printed.

`toc` returns an (invisible) list containing the time-stamps `tic` and
`toc`, `time` in seconds and the message `msg`.

## See also

Other Tools:
[`autoline()`](https://laresbernardo.github.io/lares/reference/autoline.md),
[`bind_files()`](https://laresbernardo.github.io/lares/reference/bind_files.md),
[`bring_api()`](https://laresbernardo.github.io/lares/reference/bring_api.md),
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
[`mail_send()`](https://laresbernardo.github.io/lares/reference/mail_send.md),
[`markdown2df()`](https://laresbernardo.github.io/lares/reference/markdown2df.md),
[`move_files()`](https://laresbernardo.github.io/lares/reference/move_files.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md),
[`myip()`](https://laresbernardo.github.io/lares/reference/myip.md),
[`quiet()`](https://laresbernardo.github.io/lares/reference/quiet.md),
[`read.file()`](https://laresbernardo.github.io/lares/reference/read.file.md),
[`statusbar()`](https://laresbernardo.github.io/lares/reference/statusbar.md),
[`try_require()`](https://laresbernardo.github.io/lares/reference/try_require.md),
[`updateLares()`](https://laresbernardo.github.io/lares/reference/updateLares.md),
[`warnifnot()`](https://laresbernardo.github.io/lares/reference/warnifnot.md),
[`what_size()`](https://laresbernardo.github.io/lares/reference/what_size.md)

## Examples

``` r
# Basic use (global stopwatch)
tic()
Sys.sleep(0.1)
toc()
#> Elapsed time: 0.101s

# Multiple tic tocs
tic(id = "two", quiet = FALSE)
#> Tic `id = two` start time: 2025-11-24 19:29:51.888147
Sys.sleep(0.2)
toc(id = "two")
#> Elapsed time: 0.202s

# Global is still working (id = 1)
toc(msg = "The function finished its work in")
#> The function finished its work in 0.306s
```
