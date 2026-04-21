# Holidays in your Country

This function lets the user automatically retrieve public holiday dates
for any country supported by the Nager.Date API. Accepts country names
(e.g., "Portugal") or ISO 3166-1 alpha-2 codes (e.g., "PT"). Thanks to
[Nager.Date](https://date.nager.at)!

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
  imported? Accepts country names or ISO 3166-1 alpha-2 codes.

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
#> # A tibble: 16 × 10
#>    holiday    holiday_name holiday_type national observance bank  nonwork season
#>    <date>     <chr>        <chr>        <lgl>    <lgl>      <lgl> <lgl>   <lgl> 
#>  1 2026-01-01 New Year's … Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  2 2026-02-16 Carnival     Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  3 2026-02-17 Carnival     Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  4 2026-03-24 Day of Reme… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  5 2026-04-02 Day of the … Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  6 2026-04-03 Good Friday  Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  7 2026-05-01 Labour Day   Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  8 2026-05-25 May Revolut… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  9 2026-06-15 Anniversary… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 10 2026-06-20 General Man… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 11 2026-07-09 Independenc… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 12 2026-08-17 General Jos… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 13 2026-10-12 Day of Resp… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 14 2026-11-23 National So… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 15 2026-12-08 Immaculate … Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 16 2026-12-25 Christmas D… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> # ℹ 2 more variables: hother <lgl>, county <chr>
year <- as.integer(format(Sys.Date(), format = "%Y"))
holidays(countries = c("Spain", "Venezuela"), years = year)
#> >>> Extracting Spain's holidays for 2026
#> >>> Extracting Venezuela's holidays for 2026
#> # A tibble: 66 × 11
#>    holiday    holiday_name holiday_type national observance bank  nonwork season
#>    <date>     <chr>        <chr>        <lgl>    <lgl>      <lgl> <lgl>   <lgl> 
#>  1 2026-01-01 New Year's … Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  2 2026-01-06 Epiphany     Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  3 2026-02-28 Day of Anda… Public       FALSE    FALSE      FALSE FALSE   FALSE 
#>  4 2026-03-01 Day of the … Public       FALSE    FALSE      FALSE FALSE   FALSE 
#>  5 2026-04-02 Maundy Thur… Public       FALSE    FALSE      FALSE FALSE   FALSE 
#>  6 2026-04-03 Good Friday  Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  7 2026-04-06 Easter Mond… Public       FALSE    FALSE      FALSE FALSE   FALSE 
#>  8 2026-04-23 Castile and… Public       FALSE    FALSE      FALSE FALSE   FALSE 
#>  9 2026-04-23 Day of Arag… Public       FALSE    FALSE      FALSE FALSE   FALSE 
#> 10 2026-05-01 Labour Day   Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> # ℹ 56 more rows
#> # ℹ 3 more variables: hother <lgl>, country <chr>, county <chr>
holidays(countries = "Germany", include_regions = TRUE)
#> >>> Extracting Germany's holidays for 2026
#> # A tibble: 19 × 11
#>    holiday    holiday_name      holiday_type holiday_details national observance
#>    <date>     <chr>             <chr>        <chr>           <lgl>    <lgl>     
#>  1 2026-01-01 New Year's Day    Public       NA              TRUE     FALSE     
#>  2 2026-01-06 Epiphany          Public       DE-BW, DE-BY, … FALSE    FALSE     
#>  3 2026-03-08 International Wo… Public       DE-BE, DE-MV    FALSE    FALSE     
#>  4 2026-04-03 Good Friday       Public       NA              TRUE     FALSE     
#>  5 2026-04-05 Easter Sunday     Public       DE-BB           FALSE    FALSE     
#>  6 2026-04-06 Easter Monday     Public       NA              TRUE     FALSE     
#>  7 2026-05-01 Labour Day        Public       NA              TRUE     FALSE     
#>  8 2026-05-14 Ascension Day     Public       NA              TRUE     FALSE     
#>  9 2026-05-24 Pentecost         Public       DE-BB           FALSE    FALSE     
#> 10 2026-05-25 Whit Monday       Public       NA              TRUE     FALSE     
#> 11 2026-06-04 Corpus Christi    Public       DE-BW, DE-BY, … FALSE    FALSE     
#> 12 2026-08-15 Assumption Day    Public       DE-SL           FALSE    FALSE     
#> 13 2026-09-20 World Children's… Public       DE-TH           FALSE    FALSE     
#> 14 2026-10-03 German Unity Day  Public       NA              TRUE     FALSE     
#> 15 2026-10-31 Reformation Day   Public       DE-BB, DE-MV, … FALSE    FALSE     
#> 16 2026-11-01 All Saints' Day   Public       DE-BW, DE-BY, … FALSE    FALSE     
#> 17 2026-11-18 Repentance and P… Public       DE-SN           FALSE    FALSE     
#> 18 2026-12-25 Christmas Day     Public       NA              TRUE     FALSE     
#> 19 2026-12-26 St. Stephen's Day Public       NA              TRUE     FALSE     
#> # ℹ 5 more variables: bank <lgl>, nonwork <lgl>, season <lgl>, hother <lgl>,
#> #   county <chr>
holidays(countries = "PT") # Also accepts ISO country codes
#> >>> Extracting PT's holidays for 2026
#> # A tibble: 17 × 10
#>    holiday    holiday_name holiday_type national observance bank  nonwork season
#>    <date>     <chr>        <chr>        <lgl>    <lgl>      <lgl> <lgl>   <lgl> 
#>  1 2026-01-01 New Year's … Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  2 2026-02-17 Carnival     Optional     FALSE    FALSE      FALSE TRUE    FALSE 
#>  3 2026-04-03 Good Friday  Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  4 2026-04-05 Easter Sund… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  5 2026-04-25 Freedom Day  Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  6 2026-05-01 Labour Day   Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  7 2026-06-01 Azores Day   Public       FALSE    FALSE      FALSE FALSE   FALSE 
#>  8 2026-06-04 Corpus Chri… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#>  9 2026-06-10 National Day Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 10 2026-07-01 Madeira Day  Public       FALSE    FALSE      FALSE FALSE   FALSE 
#> 11 2026-08-15 Assumption … Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 12 2026-10-05 Republic Day Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 13 2026-11-01 All Saints … Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 14 2026-12-01 Restoration… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 15 2026-12-08 Immaculate … Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 16 2026-12-25 Christmas D… Public       TRUE     FALSE      FALSE FALSE   FALSE 
#> 17 2026-12-26 St. Stephen… Public       FALSE    FALSE      FALSE FALSE   FALSE 
#> # ℹ 2 more variables: hother <lgl>, county <chr>
# }
```
