# Cache Save and Load (Write and Read)

This function lets the user save and load a cache of any R object to
improve timings and UX.

## Usage

``` r
cache_write(
  data,
  base = "temp",
  cache_dir = getOption("LARES_CACHE_DIR"),
  ask = FALSE,
  overwrite = TRUE,
  quiet = FALSE,
  ...
)

cache_read(
  base,
  cache_dir = getOption("LARES_CACHE_DIR"),
  ask = FALSE,
  overwrite = TRUE,
  quiet = FALSE,
  ...
)

cache_exists(base = NULL, cache_dir = getOption("LARES_CACHE_DIR"), ...)

cache_clear(cache_dir = getOption("LARES_CACHE_DIR"), quiet = FALSE, ...)

cache_pipe(data, base = "cache_pipe", read = TRUE, write = TRUE, ...)
```

## Arguments

- data:

  Object

- base:

  Character vector. Unique name for your cache file. You can pass a
  character vector with multiple elements that will be concatenated. All
  cache files with start with `lares_cache_*` automatically to quickly
  detect these cache files.

- cache_dir:

  Character. Where do you want to save you cache files? By default
  they'll be stored on
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) but you can change
  it using this parameter or setting a global option called
  `"LARES_CACHE_DIR"`.

- ask:

  Boolean. If cache exists, when reading: (interactive) ask the user if
  the cache should be used to proceed or ignored; when writing,
  (interactive) ask the user if the cache should be overwritten. Note
  that you can only ask for one cache file at a time because vectors are
  concatenated.

- overwrite:

  Boolean. Set to overwrite existing cache file. When reading, this
  parameter answers to ask prompt instead.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- ...:

  Additional parameters.

- read, write:

  Boolean. Do you wish to read and or write cache?

## Value

`cache_write`. No return value, called for side effects.

`cache_read`. R object. Data from cache file or NULL if no cache found.

`cache_exists`. Boolean. Result of `base` existence.

`cache_clear`. Invisible vector containing cache file names removed.

`cache_pipe`. Same as `x` or cached result.

## Examples

``` r
x <- list(a = 1, b = 2:4)
base <- c(as.character(Sys.Date()), "A", "B")
cache_write(x, base)
#> > Cache saved succesfully: lares_cache_2026-01-07.A.B
cache_read(base, ask = FALSE)
#> > Cache loaded succesfully: lares_cache_2026-01-07.A.B
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2 3 4
#> 
cache_exists(base)
#> [1] TRUE
#> attr(,"filename")
#> [1] "/tmp/RtmpQyMTFT/lares_cache_2026-01-07.A.B.RDS"
#> attr(,"base")
#> [1] "lares_cache_2026-01-07.A.B"
#> attr(,"cache_dir")
#> [1] "/tmp/RtmpQyMTFT"
cache_clear()
#> Removed 1 cache files succesfully!

# Use cache_pipe() for automatic use (saves and loads if exists)
cache_pipe({
  x <- 1
  x + 1
})
#> > Cache saved succesfully: lares_cache_cache_pipe
#> [1] 2
cache_pipe({
  x <- 1
  x + 1
})
#> > Cache loaded succesfully: lares_cache_cache_pipe
#> [1] 2
```
