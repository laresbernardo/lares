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
#' @param id Character. YouTube URL or ID to search for.
#' @param mp3 Boolean. Add mp3 optimal parameters?
#' @param params Character. Additional parameters.
#' @param start_time,end_time Numeric. Start and end time
#' to trim the audio output in seconds.
#' @param overwrite Boolean. Overwrite original file?
#' @param info Boolean. Import and return metadata?
#' @param cover Boolean. Google Search its squared cover?
#' @param quiet Boolean. Keep quiet? If not, print messages.
#' @examples 
#' \dontrun{
#'  # Download video from YouTube and convert to MP3
#'  get_mp3("https://www.youtube.com/watch?v=lrlKcCdVw9Q")
#' }
#' @export
get_mp3 <- function(id, 
                    mp3 = TRUE, 
                    params = "", 
                    start_time = NA,
                    end_time = NA,
                    overwrite = TRUE,
                    info = TRUE,
                    cover = FALSE,
                    quiet = FALSE) {
  
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
  if (!quiet) message(v2t(c("Query:", query), quotes = FALSE, sep = " "))
  
  # Run youtube-dl  
  tryCatch({
    
    system(query)
    
  }, error = function(err) {
    msg <- "Something went wrong. Do you have youtube-dl installed?"
    if (grepl("^darwin", R.version$os))
      msg <- paste(msg, "Run in Terminal: brew install youtube-dl",
                   "Then restart and try again", sep = "\n")
    msg <- paste(msg, "If already installed, try updating it with:",
                 "sudo pip3 install --upgrade youtube_dl", sep = "\n")
    stop(msg)
  })
  
  f <- listfiles(getwd(), recursive = FALSE) %>%
    filter(grepl("\\.info\\.json", .data$filename)) %>%
    arrange(desc(.data$mtime)) %>% .[1,1]
  infox <- jsonlite::read_json(f)
  invisible(file.remove(f)) 
  infox[["formats"]] <- NULL
  
  if (cover & mp3 & info) {
    aux <- gsub("\\.mp3", "", infox$title)
    aux <- gsub("lyrics|lyric|official|video", "", tolower(aux))
    aux <- gsub(" ", "\\+", cleanText(aux))
    url <- glued("https://www.google.com/search?q={aux}&tbm=isch&tbs=iar%3As")
    browseURL(url)
  }
  
  # TRIM START AND/OR END OF AUDIO FILE
  if (any(c(!is.na(start_time)), !is.na(end_time))) {
    message(">>> Trimming audio file")
    trim_mp3(sprintf("%s.mp3", infox$title), 
             start_time, end_time, 
             overwrite = FALSE,
             quiet = quiet)
    if (overwrite) file.remove(sprintf("%s.mp3", infox$title))
  }
  
  return(invisible(infox))
  
}


####################################################################
#' Trim MP3 Audio File
#' 
#' This function trims MP3 files given a start and/or end numeric 
#' timestamp. Requires \code{ffmpeg} installed in your machine.
#' 
#' @family Audio
#' @inheritParams get_mp3
#' @param file Character. File name to trim
#' @export
trim_mp3 <- function(file, start_time = 0, end_time, overwrite = FALSE, quiet = FALSE) {
  start <- paste("-ss", start_time)
  end <- ifelse(!is.na(end_time), paste("-to", end_time), "")
  file_out <- ifelse(overwrite, file, paste0(gsub("\\.mp3.*","",file), "_trimmed.mp3"))
  query2 <- paste("ffmpeg -hide_banner -loglevel panic -y", 
                  start, end, "-i",
                  sprintf("'%s'", file), 
                  sprintf("'%s'", file_out))
  if (!quiet) message("Query: ", query2)
  system(query2)
}
