####################################################################
#' Get Personal Portfolio's Data
#' 
#' This function lets me download my personal Excel with my Portfolio 
#' data, locally or from Dropbox. You can also use it for local files
#' 
#' @family Scrapper
#' @family Audio
#' @param url Characeter. YouTube URL
#' @export
yt2mp3 <- function(url = "https://www.youtube.com/watch?v=lrlKcCdVw9Q") {
  
  try_require("av")
  
  # txt <- read_html(url) %>%
  #   html_nodes("head title") %>% 
  #   html_text() %>% 
  #   gsub(" - YouTube", "", .)
  # message("Video: ", txt)
  # filename <- ifelse(is.na(filename), txt, filename)
  
  tryCatch({
    message(">>> Downloading and converting video...")
    system(paste("youtube-dl",
                 "-f bestaudio",
                 "--extract-audio",
                 "--audio-format mp3",
                 "--audio-quality 0", 
                 '-o "%(title)s.%(ext)s"',
                 url))
    # message(">>> Converting to audio...")
    # files <- listfiles(recursive = FALSE)
    # file <- as.character(files$filename[grepl("tempvid", files$filename)])
    # mp3 <- paste0(filename,".mp3")
    # if (file.exists(mp3)) invisible(file.remove(mp3))
    # av_audio_convert(file, mp3, start_time = start) 
    # invisible(file.remove(file))
  }, error = function(err) {
    msg <- "You need to have youtube-dl installed"
    if (grepl("^darwin", R.version$os))
      msg <- paste(msg, "Run in Terminal: brew install youtube-dl", sep = "\n")
    stop(msg)
  })
  message(">>> MP3 created succesfully!")
}
