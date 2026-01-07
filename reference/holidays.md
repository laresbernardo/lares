# Holidays in your Country

This function lets the user automatically scrap holiday dates from any
country and year within +- 5 years. Thanks to timeanddate.com!

## Usage

``` r
holidays(
  countries = "Venezuela",
  years = year(Sys.Date()),
  quiet = FALSE,
  include_regions = FALSE
)
```

## Arguments

- countries:

  Character or vector. For which country(ies) should the holidays be
  imported?

- years:

  Character or vector. For which year(s) do you wish to import holiday
  dates?

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- include_regions:

  Boolean. Default FALSE. If TRUE, for countries with internal
  subdivisions, it will provide details on which sub-state the found
  holidays apply.

## Value

`data.frame` with holidays data for given `countries` and `years`.

## See also

Other Data Wrangling:
[`balance_data()`](https://laresbernardo.github.io/lares/reference/balance_data.md),
[`categ_reducer()`](https://laresbernardo.github.io/lares/reference/categ_reducer.md),
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`date_cuts()`](https://laresbernardo.github.io/lares/reference/date_cuts.md),
[`date_feats()`](https://laresbernardo.github.io/lares/reference/date_feats.md),
[`file_name()`](https://laresbernardo.github.io/lares/reference/file_name.md),
[`formatHTML()`](https://laresbernardo.github.io/lares/reference/format_string.md),
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

Other Feature Engineering:
[`date_feats()`](https://laresbernardo.github.io/lares/reference/date_feats.md),
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md)

Other Scrapper:
[`filesGD()`](https://laresbernardo.github.io/lares/reference/filesGD.md),
[`gtrends_related()`](https://laresbernardo.github.io/lares/reference/google_trends.md),
[`ip_data()`](https://laresbernardo.github.io/lares/reference/ip_data.md),
[`mp3_get()`](https://laresbernardo.github.io/lares/reference/mp3_get.md),
[`readGS()`](https://laresbernardo.github.io/lares/reference/google_sheets.md),
[`splot_summary()`](https://laresbernardo.github.io/lares/reference/stocks_plots.md),
[`stocks_quote()`](https://laresbernardo.github.io/lares/reference/stocks_hist.md)

Other One Hot Encoding:
[`date_feats()`](https://laresbernardo.github.io/lares/reference/date_feats.md),
[`ohe_commas()`](https://laresbernardo.github.io/lares/reference/ohe_commas.md),
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md)

## Examples

``` r
# \donttest{
holidays(countries = "Argentina")
#> >>> Extracting Argentina's holidays for 2026
#> Warning: All formats failed to parse. No formats found.
#> # A tibble: 0 × 10
#> # ℹ 10 variables: holiday <date>, holiday_name <chr>, holiday_type <chr>,
#> #   national <lgl>, observance <lgl>, bank <lgl>, nonwork <lgl>, season <lgl>,
#> #   hother <lgl>, county <fct>
year <- as.integer(format(Sys.Date(), format = "%Y"))
holidays(countries = c("Spain", "Venezuela"), years = year)
#> >>> Extracting Spain's holidays for 2026
#> Warning: All formats failed to parse. No formats found.
#> >>> Extracting Venezuela's holidays for 2026
#> Warning: All formats failed to parse. No formats found.
#> # A tibble: 0 × 11
#> # ℹ 11 variables: holiday <date>, holiday_name <chr>, holiday_type <chr>,
#> #   national <lgl>, observance <lgl>, bank <lgl>, nonwork <lgl>, season <lgl>,
#> #   hother <lgl>, country <fct>, county <fct>
holidays(countries = "Germany", include_regions = TRUE)
#> >>> Extracting Germany's holidays for 2026
#> Warning: All formats failed to parse. No formats found.
#> # A tibble: 0 × 11
#> # ℹ 11 variables: holiday <date>, holiday_name <chr>, holiday_type <chr>,
#> #   holiday_details <chr>, national <lgl>, observance <lgl>, bank <lgl>,
#> #   nonwork <lgl>, season <lgl>, hother <lgl>, county <fct>
# }
```
