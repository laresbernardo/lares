# List categorical values for data.frame

Make a list with all categorical values and

## Usage

``` r
list_cats(df, ..., abc = TRUE)
```

## Arguments

- df:

  data.frame

- ...:

  Variables to segment counters

- abc:

  Boolean. Sort alphabetically?

## Value

List. Length same as number of categorical columns, each with a
frequency data.frame using
[`freqs()`](https://laresbernardo.github.io/lares/reference/freqs.md).

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
data(dft) # Titanic dataset
df <- dft[, 1:5]
head(df)
#>   PassengerId Survived Pclass    Sex Age
#> 1           1    FALSE      3   male  22
#> 2           2     TRUE      1 female  38
#> 3           3     TRUE      3 female  26
#> 4           4     TRUE      1 female  35
#> 5           5    FALSE      3   male  35
#> 6           6    FALSE      3   male  NA
list_cats(df)
#> $Pclass
#> # A tibble: 3 × 3
#>   Pclass     n     p
#>   <chr>  <int> <dbl>
#> 1 1        216  24.2
#> 2 2        184  20.6
#> 3 3        491  55.1
#> 
#> $Sex
#> # A tibble: 2 × 3
#>   Sex        n     p
#>   <chr>  <int> <dbl>
#> 1 female   314  35.2
#> 2 male     577  64.8
#> 
```
