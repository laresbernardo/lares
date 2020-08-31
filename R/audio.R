####################################################################
#' Download MP3 from URL
#' 
#' This function downloads YouTube videos or Soundcloud or any other 
#' platform supported by the youtube-dl library, and converts them into 
#' high quality MP3 files. The URL can be for a single video or a whole 
#' playlist. It also returns metadata into an (invisible) list.
#' 
#' @section youtube-dl:
#' More info from the original developers and its code: 
#' \href{https://github.com/ytdl-org/youtube-dl/}{youtube-dl's Github}
#' 
#' @family Scrapper
#' @family Audio
#' @param id Characeter. YouTube URL or ID to search for
#' @param mp3 Boolean. Add mp3 optimal parameters?
#' @param params Character. Additional parameters
#' @param info Boolean. Import and return metadata?
#' @param cover Boolean. Google Search its squared cover?
#' @examples 
#' \dontrun{
#'  # Download video from YouTube and convert to MP3
#'  get_mp3("https://www.youtube.com/watch?v=lrlKcCdVw9Q")
#' }
#' @export
get_mp3 <- function(id, 
                    mp3 = TRUE, 
                    params = "", 
                    info = TRUE,
                    cover = FALSE) {
  
  # Build query's parameters
  query <- "--rm-cache-dir"
  if (mp3)
    query <- c(query, 
               "-f bestaudio",
               "--extract-audio",
               "--audio-format mp3",
               "--audio-quality 0")
  if (info) query <- c(query, '--write-info-json')
  query <- c(query, '-o "%(title)s.%(ext)s"', params)
  query <- v2t(c("youtube-dl", query, id), quotes = FALSE, sep = " ")
  message(v2t(c("Query:", query), quotes = FALSE, sep = " "))
  
  # Run youtube-dl  
  tryCatch({
    
    system(query)
    
    if (info) {
      f <- listfiles(getwd(), recursive = FALSE) %>%
        filter(grepl("\\.info\\.json", .data$filename)) %>%
        arrange(desc(.data$mtime)) %>% .[1,1]
      infox <- jsonlite::read_json(f)
      invisible(file.remove(f)) 
      infox[["formats"]] <- NULL
    }
    
    if (cover & mp3 & info) {
      aux <- gsub("\\.mp3", "", infox$title)
      aux <- gsub("lyrics|lyric|official|video", "", tolower(aux))
      aux <- gsub(" ", "\\+", cleanText(aux))
      url <- glued("https://www.google.com/search?q={aux}&tbm=isch&tbs=iar%3As")
      browseURL(url)
    }
    
    return(invisible(infox))
    
  }, error = function(err) {
    msg <- "Something went wrong. Do you have youtube-dl installed?"
    if (grepl("^darwin", R.version$os))
      msg <- paste(msg, "Run in Terminal: brew install youtube-dl",
                   "Then restart and try again", sep = "\n")
    msg <- paste(msg, "If already installed, try updating it with:",
                 "sudo pip3 install --upgrade youtube_dl", sep = "\n")
    stop(msg)
  })
}
