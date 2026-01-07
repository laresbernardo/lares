# Scrap data based on IP address

This function lets the user scrap https://db-ip.com/ given IP
address(es) to get their associated address type, ASN, ISP,
organization, country, state or region, county, city, ZIP postal code,
weather station, coordinates, Timezone, local time, languages, and
currency.

## Usage

``` r
ip_data(ip = myip(), quiet = FALSE)
```

## Arguments

- ip:

  Vector. Vector with all IP's we wish to search.

- quiet:

  Boolean. Do not show the loading `statusbar`?

## Value

data.frame. Each row is an unique `ip` address, and columns will bee
created for all the additional information found.

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

Other Scrapper:
[`filesGD()`](https://laresbernardo.github.io/lares/reference/filesGD.md),
[`gtrends_related()`](https://laresbernardo.github.io/lares/reference/google_trends.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`mp3_get()`](https://laresbernardo.github.io/lares/reference/mp3_get.md),
[`readGS()`](https://laresbernardo.github.io/lares/reference/google_sheets.md),
[`splot_summary()`](https://laresbernardo.github.io/lares/reference/stocks_plots.md),
[`stocks_quote()`](https://laresbernardo.github.io/lares/reference/stocks_hist.md)

## Examples

``` r
# \donttest{
ip_data("163.114.132.0")
#>              id addresstype                   asn          isp connection
#> 1 163.114.132.0        IPv4 54115 - FACEBOOK-CORP Facebook Inc    Hosting
#>    organization       country stateregion districtcounty           city
#> 1 Facebook Corp United States  California      San Mateo East Palo Alto
#>         weatherstation       coordinates                    timezone localtime
#> 1 USCA0830 - Palo Alto 37.4688, -122.141 America/Los_Angeles (UTC-8)          
#>               languages     currency
#> 1 en-US, es-US, haw, fr Dollar (USD)
ip_data(ip = c(myip(), "201.244.197.199"), quiet = TRUE)
#>                id addresstype                                asn
#> 1    52.225.25.96        IPv4 8075 - MICROSOFT-CORP-MSN-AS-BLOCK
#> 2 201.244.197.199        IPv4                    19429 - AS19429
#>                     isp connection       country stateregion districtcounty
#> 1 Microsoft Corporation    Hosting United States  California    Santa Clara
#> 2        ETB - Colombia       <NA>      Colombia Bogota D.C.   Bogotá  D.C.
#>              city zippostalcode            weatherstation       coordinates
#> 1        San Jose         95141       USCA0993 - San Jose 37.3387, -121.885
#> 2 Barrio San Luis          <NA> COXX7742 - Vereda El Hato 4.66779, -74.0215
#>                      timezone localtime             languages     currency
#> 1 America/Los_Angeles (UTC-8)           en-US, es-US, haw, fr Dollar (USD)
#> 2      America/Bogota (UTC-5)                           es-CO   Peso (COP)
#>                                     hostname
#> 1                                       <NA>
#> 2 dynamic-201-244-197-199.dynamic.etb.net.co
# }
```
