####################################################################
#' Download MP3 from URL
#' 
#' This function downloads a YouTube video or Soundcloud or any other 
#' platform supported by the youtube-dl library, and converts it into a 
#' high quality MP3 file.
#' 
#' @section youtube-dl:
#' More info from the original developers and its code: 
#' \href{https://github.com/ytdl-org/youtube-dl/}{youtube-dl's Github}
#' 
#' @family Scrapper
#' @family Audio
#' @param url Characeter. YouTube URL
#' @param mp3 Boolean. Add mp3 parameters?
#' @param params Character. Additional parameters
#' @param quiet Boolean. Keep quiet (or show query)?
#' @examples 
#' \dontrun{
#'  # Download video from YouTube and convert to MP3
#'  get_mp3("https://www.youtube.com/watch?v=lrlKcCdVw9Q")
#' }
#' @export
get_mp3 <- function(url, mp3 = TRUE, params = "", quiet = FALSE) {

  # Build query's parameters
  query <- "--rm-cache-dir"
  if (mp3)
    query <- c(query, 
               "-f bestaudio",
               "--extract-audio",
               "--audio-format mp3",
               "--audio-quality 0")
  query <- c(query, '-o "%(title)s.%(ext)s"', params)
  if (!quiet)
    message(vector2text(c("Query parameters:", query), quotes = FALSE, sep = " "))
  query <- vector2text(c("youtube-dl", query, url), quotes = FALSE, sep = " ")
  
  # Run youtube-dl
  tryCatch({
    if (!quiet)
      message(">>> Downloading and converting...")
    system(query)
    
  }, error = function(err) {
    msg <- "You must have youtube-dl installed!"
    if (grepl("^darwin", R.version$os))
      msg <- paste(msg, "Run in Terminal: brew install youtube-dl", 
                   "Then restart and try again", sep = "\n")
    stop(msg)
  })
}
