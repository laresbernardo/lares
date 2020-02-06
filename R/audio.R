####################################################################
#' Get Personal Portfolio's Data
#' 
#' This function lets me download my personal Excel with my Portfolio 
#' data, locally or from Dropbox. You can also use it for local files
#' 
#' @family Scrapper
#' @family Audio
#' @param url Characeter. YouTube URL
#' @param filename Character. Specify output filename. If NA, video's name 
#' will be automatically used
#' @param start Numeric. Seeks in the input file to position (Must be 
#' greater than 0) and crops output
#' @export
yt2mp3 <- function(url = "https://www.youtube.com/watch?v=lrlKcCdVw9Q", 
                   filename = NA, start = 0) {
  
  try_require("av")
  
  txt <- read_html(url) %>%
    html_nodes("head title") %>% 
    html_text() %>% 
    gsub(" - YouTube", "", .)
  message("Video: ", txt)
  filename <- ifelse(is.na(filename), txt, filename)
  
  tryCatch({
    message(">>> Downloading video...")
    system(paste("youtube-dl", url, "-o tempvid"))
    message(">>> Converting to audio...")
    files <- listfiles(recursive = FALSE)
    file <- as.character(files$filename[grepl("tempvid", files$filename)])
    mp3 <- paste0(filename,".mp3")
    if (file.exists(mp3)) invisible(file.remove(mp3))
    av_audio_convert(file, mp3, start_time = start) 
    invisible(file.remove(file))
  }, error = function(err) {
    stop("You need youtube-dl. MacOS: brew install youtube-dl")
  })
  message(">>> MP3 created succesfully!")
}
