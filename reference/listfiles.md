# List files in a directory

This function lets the user list all files on a given directory. It also
lets filter files which contains a string.

## Usage

``` r
listfiles(folder = getwd(), recursive = TRUE, regex = NA, images = FALSE)
```

## Arguments

- folder:

  Character. Directory which contains files

- recursive:

  Boolean. Should the listing recurse into directories?

- regex:

  Character. String to use for filtering files

- images:

  Boolean. Bring only image files?

## Value

data.frame with relevant data for each file on `folder` directory.

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
# All files in current directory (without recursive files)
df <- listfiles(recursive = TRUE)
head(df, 3)
#>            filename size isdir mode               mtime               ctime
#> 1          ROC.html   17 FALSE  644 2025-11-24 20:26:27 2025-11-24 20:26:27
#> 2     autoline.html   21 FALSE  644 2025-11-24 20:26:28 2025-11-24 20:26:28
#> 3 balance_data.html   10 FALSE  644 2025-11-24 20:26:28 2025-11-24 20:26:28
#>                 atime  uid  gid  uname grname
#> 1 2025-11-24 20:26:27 1001 1001 runner runner
#> 2 2025-11-24 20:26:28 1001 1001 runner runner
#> 3 2025-11-24 20:26:28 1001 1001 runner runner

# All files in current directory (with recursive files)
df <- listfiles(recursive = TRUE)
tail(df, 3)
#>            filename size isdir mode               mtime               ctime
#> 128 lasso_vars.html   12 FALSE  644 2025-11-24 20:27:12 2025-11-24 20:27:12
#> 129 left_right.html    8 FALSE  644 2025-11-24 20:27:12 2025-11-24 20:27:12
#> 130  list_cats.html   11 FALSE  644 2025-11-24 20:27:12 2025-11-24 20:27:12
#>                   atime  uid  gid  uname grname
#> 128 2025-11-24 20:27:12 1001 1001 runner runner
#> 129 2025-11-24 20:27:12 1001 1001 runner runner
#> 130 2025-11-24 20:27:12 1001 1001 runner runner

# Check R files using regex
df <- listfiles(regex = "\\.R$")
```
