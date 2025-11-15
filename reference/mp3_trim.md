# Trim MP3 Audio File

This function trims MP3 files given a start and/or end numeric
timestamp. Requires `ffmpeg` installed in your machine.

## Usage

``` r
mp3_trim(
  file,
  start_time = 0,
  end_time = NA,
  overwrite = FALSE,
  ext = "mp3",
  quiet = FALSE
)
```

## Arguments

- file:

  Character. File name to trim.

- start_time, end_time:

  Numeric. Start and end time to trim the audio output in seconds.

- overwrite:

  Boolean. Overwrite original file?

- ext:

  Character. File extension/type.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## See also

Other Audio:
[`mp3_get()`](https://laresbernardo.github.io/lares/reference/mp3_get.md),
[`mp3_update_tags()`](https://laresbernardo.github.io/lares/reference/mp3_update_tags.md)
