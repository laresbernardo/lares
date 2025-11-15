# Plot Palette Colours

This function plots a list of colours

## Usage

``` r
plot_palette(fill, colour = "black", id = NA, limit = 12, ...)
```

## Arguments

- fill:

  Vector. List of colours for fills.

- colour:

  Vector. List of colours for colours.

- id:

  Vector. ID for each color.

- limit:

  Integer. Show only first n values.

- ...:

  Additional parameters.

## Value

Plot with `fill` colours and `colour` counter-colours if provided.

## See also

Other Themes:
[`gg_fill_customs()`](https://laresbernardo.github.io/lares/reference/gg_fill_customs.md),
[`lares_pal()`](https://laresbernardo.github.io/lares/reference/lares_pal.md),
[`theme_lares()`](https://laresbernardo.github.io/lares/reference/theme_lares.md)

## Examples

``` r
# Simply pass a vector
pal <- lares_pal("simple")
plot_palette(pal)
#> Limited to 12 colours. Overwrite with 'limit' parameter

# Or fill + color named vector
pal <- lares_pal("pal")
plot_palette(fill = names(pal), colour = as.vector(pal))
#> Limited to 12 colours. Overwrite with 'limit' parameter
```
