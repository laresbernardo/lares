# Download MP3 from URL

This function downloads YouTube videos or Soundcloud or any other
platform supported by the youtube-dl library, and converts them into
high quality MP3 files. The URL can be for a single video or a whole
playlist. It also returns metadata into an (invisible) list.

## Usage

``` r
mp3_get(
  id,
  mp3 = TRUE,
  repo = "yt-dlp",
  params = "--no-check-certificate",
  start_time = 0,
  end_time = NA,
  overwrite = TRUE,
  open = FALSE,
  delete = open,
  info = TRUE,
  title = NULL,
  cover = FALSE,
  metadata = FALSE,
  quiet = FALSE
)
```

## Arguments

- id:

  Character. YouTube URL or ID to search for.

- mp3:

  Boolean. Add mp3 optimal parameters?

- repo:

  Character. Chose repository you installed youtube-dl from. Any of:
  "youtube-dl" (latest stable version), "yt-dlp" (latest dev version).

- params:

  Character. Additional parameters.

- start_time, end_time:

  Numeric. Start and end time to trim the audio output in seconds.

- overwrite:

  Boolean. Overwrite original file?

- open, delete:

  Boolean. After everything's done, should the file be opened? Should it
  be deleted?

- info:

  Boolean. Import and return metadata?

- title:

  Character. Title of the track to search for cover, metadata, and name
  the file.

- cover:

  Boolean. Google Search its squared cover? Uses `title` input when
  provided.

- metadata:

  Boolean. Use "spotifyr" to extract "track" data using Spotify's API.
  Needs credentials. Uses `title` input when provided.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## Value

(Invisible) list with id's meta-data.

## youtube-dl

More info from the original developers and its code: [youtube-dl's
Github](https://github.com/ytdl-org/youtube-dl/)

## See also

Other Scrapper:
[`filesGD()`](https://laresbernardo.github.io/lares/reference/filesGD.md),
[`gtrends_related()`](https://laresbernardo.github.io/lares/reference/google_trends.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`ip_data()`](https://laresbernardo.github.io/lares/reference/ip_data.md),
[`readGS()`](https://laresbernardo.github.io/lares/reference/google_sheets.md),
[`splot_summary()`](https://laresbernardo.github.io/lares/reference/stocks_plots.md),
[`stocks_quote()`](https://laresbernardo.github.io/lares/reference/stocks_hist.md)

Other Audio:
[`mp3_trim()`](https://laresbernardo.github.io/lares/reference/mp3_trim.md),
[`mp3_update_tags()`](https://laresbernardo.github.io/lares/reference/mp3_update_tags.md)

## Examples

``` r
# You must have "youtube-dl" installed in your OS:
if (FALSE) { # \dontrun{
# Download video from YouTube and convert to MP3
mp3_get("https://www.youtube.com/watch?v=lrlKcCdVw9Q")
# OR simply
mp3_get("lrlKcCdVw9Q")
# For dev version, use:
mp3_get("m3RX4LJh0iI", repo = "yt-dlp")
} # }
```
