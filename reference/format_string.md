# Format a string text as markdown/HTML

Format any character string to HTML or markdown format. We recommend
using this format with the `ggtext::geom_richtext` function to format
text in `ggplot2` objects.

This function lets the user format numerical values nicely

## Usage

``` r
formatHTML(text, color = "black", size = 20, bold = FALSE)

formatNum(
  x,
  decimals = 2,
  signif = NULL,
  type = Sys.getenv("LARES_NUMFORMAT"),
  pre = "",
  pos = "",
  sign = FALSE,
  abbr = FALSE,
  ...
)
```

## Arguments

- text:

  Character. Strings to format.

- color:

  Character. Hex colour code.

- size:

  Numeric. Text size.

- bold:

  Boolean. Should the text be bold?

- x:

  Numerical Vector

- decimals:

  Integer. Amount of decimals to display. If set to `NULL`, then
  `getOption("digits")` will be used.

- signif:

  Integer. Rounds the values in its first argument to the specified
  number of significant digits.

- type:

  Integer. `1` for International standards. `2` for American Standards.
  Use `Sys.setenv("LARES_NUMFORMAT" = 2)` to set this parameter
  globally.

- pre, pos:

  Character. Add string before or after number.

- sign:

  Boolean. Add `+` sign to positive values.

- abbr:

  Boolean. Abbreviate using num_abbr()? You can use the \`decimals\`
  parameter to set abbr's `n`(-1) parameter.

- ...:

  Additional lazy eval parameters.

## Value

String with format characters included.

Character. String vector with reformatted continuous numbers

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

Other Data Wrangling:
[`balance_data()`](https://laresbernardo.github.io/lares/reference/balance_data.md),
[`categ_reducer()`](https://laresbernardo.github.io/lares/reference/categ_reducer.md),
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`date_cuts()`](https://laresbernardo.github.io/lares/reference/date_cuts.md),
[`date_feats()`](https://laresbernardo.github.io/lares/reference/date_feats.md),
[`file_name()`](https://laresbernardo.github.io/lares/reference/file_name.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`impute()`](https://laresbernardo.github.io/lares/reference/impute.md),
[`left()`](https://laresbernardo.github.io/lares/reference/left_right.md),
[`normalize()`](https://laresbernardo.github.io/lares/reference/normalize.md),
[`num_abbr()`](https://laresbernardo.github.io/lares/reference/num_abbr.md),
[`ohe_commas()`](https://laresbernardo.github.io/lares/reference/ohe_commas.md),
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md),
[`quants()`](https://laresbernardo.github.io/lares/reference/quants.md),
[`removenacols()`](https://laresbernardo.github.io/lares/reference/filterdata.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`replacefactor()`](https://laresbernardo.github.io/lares/reference/replacefactor.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`vector2text()`](https://laresbernardo.github.io/lares/reference/vector2text.md),
[`year_month()`](https://laresbernardo.github.io/lares/reference/year_month.md),
[`zerovar()`](https://laresbernardo.github.io/lares/reference/zerovar.md)

## Examples

``` r
formatHTML("Text test", color = "#000000")
#> [1] "<span style='font-size:20px; color:#000000'>Text test</span>"
formatHTML(c(123, 456), color = "orange", size = 120, bold = TRUE)
#> [1] "<span style='font-size:120px; color:orange'>**123**<br/>**456**</span>"

# If you want to use it with \code{ggtext}:
if (FALSE) { # \dontrun{
col1 <- "grey"
col2 <- "orange"
pt <- data.frame(
  label = paste0(
    formatHTML(123, color = col2, size = 120, bold = TRUE), "<br/>",
    formatHTML("of children had a", col1), "<br/>",
    formatHTML("traditional stay-at-home mom", color = col2, bold = TRUE), "<br/>",
    formatHTML(paste0("in 2012, compared to ", 321, " in 1970"), color = col1)
  )
)
ggplot(pt, aes(x = 0, y = 0)) +
  ggtext::geom_richtext(
    aes(label = label),
    hjust = 0,
    label.color = NA,
    lineheight = 1.5
  ) +
  xlim(0, 0.01) +
  theme_void()
} # }
formatNum(1.23456, decimals = 3)
#> [1] "1.235"
formatNum(1.23456, type = 1)
#> [1] "1,23"
formatNum(1.23456, pre = "$", pos = "/person")
#> [1] "$1.23/person"
formatNum(123456, abbr = TRUE)
#> [1] "123K"
formatNum(c(123123, 123.123, 0.123123), signif = 2)
#> [1] "120,000" "120"     "0.12"   
formatNum(1234567890, abbr = TRUE, signif = 3)
#> [1] "1.23B"
formatNum(1234567890, decimals = 0, abbr = TRUE)
#> [1] "1B"
formatNum(c(-3:3), sign = TRUE)
#> [1] "-3" "-2" "-1" "0"  "+1" "+2" "+3"
```
