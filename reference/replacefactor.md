# Replace Factor Values

This function lets the user replace levels on a factor vector.

## Usage

``` r
replacefactor(x, original, change)
```

## Arguments

- x:

  Factor (or Character) Vector

- original:

  String or Vector. Original text you wish to replace

- change:

  String or Vector. Values you wish to replace the originals with

## Value

Factor vector with transformed levels.

## See also

Other Data Wrangling:
[`balance_data()`](https://laresbernardo.github.io/lares/reference/balance_data.md),
[`categ_reducer()`](https://laresbernardo.github.io/lares/reference/categ_reducer.md),
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`date_cuts()`](https://laresbernardo.github.io/lares/reference/date_cuts.md),
[`date_feats()`](https://laresbernardo.github.io/lares/reference/date_feats.md),
[`file_name()`](https://laresbernardo.github.io/lares/reference/file_name.md),
[`formatHTML()`](https://laresbernardo.github.io/lares/reference/format_string.md),
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
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`vector2text()`](https://laresbernardo.github.io/lares/reference/vector2text.md),
[`year_month()`](https://laresbernardo.github.io/lares/reference/year_month.md),
[`zerovar()`](https://laresbernardo.github.io/lares/reference/zerovar.md)

## Examples

``` r
library(dplyr)
#> 
#> ######################### Warning from 'xts' package ##########################
#> #                                                                             #
#> # The dplyr lag() function breaks how base R's lag() function is supposed to  #
#> # work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
#> # source() into this session won't work correctly.                            #
#> #                                                                             #
#> # Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
#> # conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
#> # dplyr from breaking base R's lag() function.                                #
#> #                                                                             #
#> # Code in packages is not affected. It's protected by R's namespace mechanism #
#> # Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
#> #                                                                             #
#> ###############################################################################
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:xts’:
#> 
#>     first, last
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
data(dft)
# Replace a single value
dft <- mutate(dft, Pclass = replacefactor(Pclass, original = "1", change = "First"))
levels(dft$Pclass)
#> [1] "First" "2"     "3"    
# Replace multiple values
dft <- mutate(dft, Pclass = replacefactor(Pclass, c("2", "3"), c("Second", "Third")))
levels(dft$Pclass)
#> [1] "First"  "Second" "Third" 
```
