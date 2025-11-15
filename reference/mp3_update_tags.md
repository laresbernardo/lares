# Update MP3 Metadata Tags

Updates the ID3 metadata tags of an MP3 file using the Python \`eyeD3\`
library via the \`reticulate\` package. You can modify standard fields
such as `title`, `artist`, `album`, and `genre`, as well as additional
tags passed through `...` if they exist in the MP3 file.

## Usage

``` r
mp3_update_tags(
  filename,
  title = NULL,
  artist = NULL,
  album = NULL,
  genre = NULL,
  ...
)
```

## Arguments

- filename:

  Character. Path to the MP3 file to update.

- title:

  Character. New title of the track (optional).

- artist:

  Character. New artist name (optional).

- album:

  Character. New album name (optional).

- genre:

  Character. New genre name (optional).

- ...:

  Additional named arguments corresponding to other ID3 tags. Only tags
  that exist in the MP3 file will be updated.

## Value

Invisibly returns `NULL`. The MP3 file is updated in-place.

## Details

The function requires Python and the `eyeD3` package installed. It will
automatically initialize ID3 tags if they do not exist. Tags provided
via `...` are checked against the existing tag names to avoid errors.

## See also

Other Audio:
[`mp3_get()`](https://laresbernardo.github.io/lares/reference/mp3_get.md),
[`mp3_trim()`](https://laresbernardo.github.io/lares/reference/mp3_trim.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mp3_update_tags(
"song.mp3",
title = "My Jazz Song",
artist = "Bernardo",
album = "Smooth Album",
genre = "Jazz"
)
} # }
```
