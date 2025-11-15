# Check if Font is Installed

This function checks if a font is installed in your machine. To list all
available fonts, set `font = NULL`.

## Usage

``` r
font_exists(font = "Arial Narrow", font_dirs = NULL, quiet = FALSE, ...)
```

## Arguments

- font:

  Character. Which font to check. No need to add .TFF.

- font_dirs:

  Character vector. Additional directories to check for fonts.

- quiet:

  Boolean. Keep quiet? If not, show message

- ...:

  Additional parameters.

## Value

Boolean result of the existing fonts check.

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
font_exists(font = "Arial")
#> [1] FALSE
font_exists(font = "arial")
#> [1] FALSE
font_exists(font = "")
#> Maybe you meant one of these:
#> 'C059-BdIta', 'C059-Bold', 'C059-Italic', 'C059-Roman', 'D050000L', 'DejaVuMathTeXGyre', 'DejaVuSans', 'DejaVuSans-Bold', 'DejaVuSans-BoldOblique', 'DejaVuSans-ExtraLight', 'DejaVuSans-Oblique', 'DejaVuSansCondensed', 'DejaVuSansCondensed-Bold', 'DejaVuSansCondensed-BoldOblique', 'DejaVuSansCondensed-Oblique', 'DejaVuSansMono', 'DejaVuSansMono-Bold', 'DejaVuSansMono-BoldOblique', 'DejaVuSansMono-Oblique', 'DejaVuSerif', 'DejaVuSerif-Bold', 'DejaVuSerif-BoldItalic', 'DejaVuSerif-Italic', 'DejaVuSerifCondensed', 'DejaVuSerifCondensed-Bold', 'DejaVuSerifCondensed-BoldItalic', 'DejaVuSerifCondensed-Italic', 'DroidSansFallbackFull', 'Lato-Black', 'Lato-BlackItalic', 'Lato-Bold', 'Lato-BoldItalic', 'Lato-Hairline', 'Lato-HairlineItalic', 'Lato-Heavy', 'Lato-HeavyItalic', 'Lato-Italic', 'Lato-Light', 'Lato-LightItalic', 'Lato-Medium', 'Lato-MediumItalic', 'Lato-Regular', 'Lato-Semibold', 'Lato-SemiboldItalic', 'Lato-Thin', 'Lato-ThinItalic', 'LiberationMono-Bold', 'LiberationMono-BoldItalic', 'LiberationMono-Italic', 'LiberationMono-Regular', 'LiberationSans-Bold', 'LiberationSans-BoldItalic', 'LiberationSans-Italic', 'LiberationSans-Regular', 'LiberationSerif-Bold', 'LiberationSerif-BoldItalic', 'LiberationSerif-Italic', 'LiberationSerif-Regular', 'NimbusMonoPS-Bold', 'NimbusMonoPS-BoldItalic', 'NimbusMonoPS-Italic', 'NimbusMonoPS-Regular', 'NimbusRoman-Bold', 'NimbusRoman-BoldItalic', 'NimbusRoman-Italic', 'NimbusRoman-Regular', 'NimbusSans-Bold', 'NimbusSans-BoldItalic', 'NimbusSans-Italic', 'NimbusSans-Regular', 'NimbusSansNarrow-Bold', 'NimbusSansNarrow-BoldOblique', 'NimbusSansNarrow-Oblique', 'NimbusSansNarrow-Regular', 'NotoColorEmoji', 'NotoMono-Regular', 'NotoSansMono-Bold', 'NotoSansMono-Regular', 'P052-Bold', 'P052-BoldItalic', 'P052-Italic', 'P052-Roman', 'StandardSymbolsPS', 'URWBookman-Demi', 'URWBookman-DemiItalic', 'URWBookman-Light', 'URWBookman-LightItalic', 'URWGothic-Book', 'URWGothic-BookOblique', 'URWGothic-Demi', 'URWGothic-DemiOblique', 'Z003-MediumItalic'
#> [1] FALSE
font_exists(font = NULL)
#>  [1] "C059-BdIta"                      "C059-Bold"                      
#>  [3] "C059-Italic"                     "C059-Roman"                     
#>  [5] "D050000L"                        "NimbusMonoPS-Bold"              
#>  [7] "NimbusMonoPS-BoldItalic"         "NimbusMonoPS-Italic"            
#>  [9] "NimbusMonoPS-Regular"            "NimbusRoman-Bold"               
#> [11] "NimbusRoman-BoldItalic"          "NimbusRoman-Italic"             
#> [13] "NimbusRoman-Regular"             "NimbusSans-Bold"                
#> [15] "NimbusSans-BoldItalic"           "NimbusSans-Italic"              
#> [17] "NimbusSans-Regular"              "NimbusSansNarrow-Bold"          
#> [19] "NimbusSansNarrow-BoldOblique"    "NimbusSansNarrow-Oblique"       
#> [21] "NimbusSansNarrow-Regular"        "P052-Bold"                      
#> [23] "P052-BoldItalic"                 "P052-Italic"                    
#> [25] "P052-Roman"                      "StandardSymbolsPS"              
#> [27] "URWBookman-Demi"                 "URWBookman-DemiItalic"          
#> [29] "URWBookman-Light"                "URWBookman-LightItalic"         
#> [31] "URWGothic-Book"                  "URWGothic-BookOblique"          
#> [33] "URWGothic-Demi"                  "URWGothic-DemiOblique"          
#> [35] "Z003-MediumItalic"               "DejaVuMathTeXGyre"              
#> [37] "DejaVuSans-Bold"                 "DejaVuSans-BoldOblique"         
#> [39] "DejaVuSans-ExtraLight"           "DejaVuSans-Oblique"             
#> [41] "DejaVuSans"                      "DejaVuSansCondensed-Bold"       
#> [43] "DejaVuSansCondensed-BoldOblique" "DejaVuSansCondensed-Oblique"    
#> [45] "DejaVuSansCondensed"             "DejaVuSansMono-Bold"            
#> [47] "DejaVuSansMono-BoldOblique"      "DejaVuSansMono-Oblique"         
#> [49] "DejaVuSansMono"                  "DejaVuSerif-Bold"               
#> [51] "DejaVuSerif-BoldItalic"          "DejaVuSerif-Italic"             
#> [53] "DejaVuSerif"                     "DejaVuSerifCondensed-Bold"      
#> [55] "DejaVuSerifCondensed-BoldItalic" "DejaVuSerifCondensed-Italic"    
#> [57] "DejaVuSerifCondensed"            "DroidSansFallbackFull"          
#> [59] "Lato-Black"                      "Lato-BlackItalic"               
#> [61] "Lato-Bold"                       "Lato-BoldItalic"                
#> [63] "Lato-Hairline"                   "Lato-HairlineItalic"            
#> [65] "Lato-Heavy"                      "Lato-HeavyItalic"               
#> [67] "Lato-Italic"                     "Lato-Light"                     
#> [69] "Lato-LightItalic"                "Lato-Medium"                    
#> [71] "Lato-MediumItalic"               "Lato-Regular"                   
#> [73] "Lato-Semibold"                   "Lato-SemiboldItalic"            
#> [75] "Lato-Thin"                       "Lato-ThinItalic"                
#> [77] "LiberationMono-Bold"             "LiberationMono-BoldItalic"      
#> [79] "LiberationMono-Italic"           "LiberationMono-Regular"         
#> [81] "LiberationSans-Bold"             "LiberationSans-BoldItalic"      
#> [83] "LiberationSans-Italic"           "LiberationSans-Regular"         
#> [85] "LiberationSerif-Bold"            "LiberationSerif-BoldItalic"     
#> [87] "LiberationSerif-Italic"          "LiberationSerif-Regular"        
#> [89] "NotoColorEmoji"                  "NotoMono-Regular"               
#> [91] "NotoSansMono-Bold"               "NotoSansMono-Regular"           
```
