# Calculate Character Reduction from R Object to TOON Format

Calculates the percentage reduction in the number of characters when
converting an R object to its TOON string representation compared to its
JSON representation (or the string itself if the input is a single
string).

## Usage

``` r
toon_reduction(obj, ...)
```

## Arguments

- obj:

  Any R object (list, vector, data frame, or string).

- ...:

  Additional arguments passed to jsonlite::toJSON()

## Value

A numeric value representing the character reduction ratio (0 to 1).
