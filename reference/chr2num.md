# Check character values for date/numeric/logical and change datatype

Automatically check a vector, data.frame or list for numeric, logical,
date content and change their datatype. Note that factors are skipped in
case the user requires character numeric values to be kept as they are.

## Usage

``` r
chr2num(data)

chr2logical(data)

chr2date(data)
```

## Arguments

- data:

  Vector, data.frame or list

## See also

Other Tools:
[`autoline()`](https://laresbernardo.github.io/lares/reference/autoline.md),
[`bind_files()`](https://laresbernardo.github.io/lares/reference/bind_files.md),
[`bring_api()`](https://laresbernardo.github.io/lares/reference/bring_api.md),
[`cal_split()`](https://laresbernardo.github.io/lares/reference/cal_split.md),
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
[`tic()`](https://laresbernardo.github.io/lares/reference/tic.md),
[`try_require()`](https://laresbernardo.github.io/lares/reference/try_require.md),
[`updateLares()`](https://laresbernardo.github.io/lares/reference/updateLares.md),
[`warnifnot()`](https://laresbernardo.github.io/lares/reference/warnifnot.md),
[`what_size()`](https://laresbernardo.github.io/lares/reference/what_size.md)

## Examples

``` r
str(chr2num(c("1", "2", "3")))
#>  num [1:3] 1 2 3
df <- data.frame(A = c("1", "3"), B = c("A", "B"), c = c(pi, pi * 2))
str(chr2num(df))
#> 'data.frame':    2 obs. of  3 variables:
#>  $ A: num  1 3
#>  $ B: chr  "A" "B"
#>  $ c: num  3.14 6.28
lst <- list(A = c("1", "2", "3"), B = c("A", "B", "3"), C = pi, D = 3L)
str(chr2num(lst))
#> List of 4
#>  $ A: num [1:3] 1 2 3
#>  $ B: chr [1:3] "A" "B" "3"
#>  $ C: num 3.14
#>  $ D: int 3
lst2 <- list(layer1 = ":D", layer2 = lst)
str(chr2num(lst2))
#> List of 2
#>  $ layer1: chr ":D"
#>  $ layer2:List of 4
#>   ..$ A: chr [1:3] "1" "2" "3"
#>   ..$ B: chr [1:3] "A" "B" "3"
#>   ..$ C: num 3.14
#>   ..$ D: int 3
str(chr2logical(c(NA, "true", FALSE)))
#>  logi [1:3] NA TRUE FALSE
```
