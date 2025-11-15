# Personal Colours Palette

Fetch customizable palettes for the library's usage. The package has its
own default colour-blind friendly colours but can be customized using R
internal options (i.e.
`options("lares.palette" = c("#FF8303" = "#000", "#40A4D8" = "#FFF", ...))`.
There are 3 options you can use to customize all colour palletes:
"lares.palette" (vector, will be used in the same order as passed, and
must have a counter colour defined), "lares.colours" (vector, simple
colour names and their HEX codes), and "lares.colours.custom"
(data.frame, containing "values" to use dynamically, "fill" for main
colour, and "colour" (not obligatory) for counter colour).

## Usage

``` r
lares_pal(return = "list")
```

## Arguments

- return:

  Character. Get only what you need. Select any of: "all" or "list"
  (list), "colors" or "colours" (vector), "pal" or "palette" (named
  vector), "simple" (named vector), "custom" or "personal" (data.frame)

## Value

Depending on the `return` input, we get a:

- `vector` with `palette` results vector

- `vector` with `palette` results vector's names

- `list` with `palette` results vector, `labels` results data.frame, and
  `simple` results named vector

## See also

Other Themes:
[`gg_fill_customs()`](https://laresbernardo.github.io/lares/reference/gg_fill_customs.md),
[`plot_palette()`](https://laresbernardo.github.io/lares/reference/plot_palette.md),
[`theme_lares()`](https://laresbernardo.github.io/lares/reference/theme_lares.md)

## Examples

``` r
# Simple colour-named palette
lares_pal("simple")
#>    orange      blue    purple       red     green      navy    yellow      grey 
#> "#EA9A28" "#00B1DA" "#6A2C70" "#E63946" "#00C9AE" "#125D98" "#F8D962" "#8D99AE" 
#>      pink   fuchsia     brown     black     white 
#> "#F6B8B8" "#FF2EB5" "#884F3E" "#0c0c0c" "#F6F5F5" 

# Raw colours and counter-colours
# OR simply: lares_pal("palette")
nice_palette <- lares_pal("colours")
nice_palette_ctr <- as.vector(lares_pal()$palette)
lapply(list(nice_palette, nice_palette_ctr), head)
#> [[1]]
#> [1] "#EA9A28" "#00B1DA" "#6A2C70" "#E63946" "#00C9AE" "#F8D962"
#> 
#> [[2]]
#> [1] "#000000" "#000000" "#FFFFFF" "#000000" "#000000" "#000000"
#> 

# Personal colours by name
df <- lares_pal("custom")
df[sample(nrow(df), 5), ]
#>      values    fill  colour
#> 25   summer #E63946 #F6F5F5
#> 21 negative #E5586E #0c0c0c
#> 45 facebook #4267B2 #0c0c0c
#> 18  rechaza #E5586E #0c0c0c
#> 42      fb1 #405996 #F6F5F5
```
