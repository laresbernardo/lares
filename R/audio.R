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
#' @param repo Character. Chose repository you installed youtube-dl from.
#' Any of: "youtube-dl" (latest stable version), "yt-dlp"
#' (latest dev version).
#' @param params Character. Additional parameters.
#' @param start_time,end_time Numeric. Start and end time
#' to trim the audio output in seconds.
#' @param overwrite Boolean. Overwrite original file?
#' @param open,delete Boolean. After everything's done, should the
#' file be opened? Should it be deleted?
#' @param info Boolean. Import and return metadata?
#' @param cover Boolean. Google Search its squared cover?
#' @param quiet Boolean. Keep quiet? If not, informative messages will be shown.
#' @return (Invisible) list with id's meta-data.
#' @examples
#' # You must have "youtube-dl" installed in your OS:
#' \dontrun{
#' # Download video from YouTube and convert to MP3
#' get_mp3("https://www.youtube.com/watch?v=lrlKcCdVw9Q")
#' # OR simply
#' get_mp3("lrlKcCdVw9Q")
#' # For dev version, use:
#' get_mp3("m3RX4LJh0iI", repo = "yt-dlp")
#' }
#' @export
get_mp3 <- function(id,
                    mp3 = TRUE,
                    repo = "yt-dlp",
                    params = "--no-check-certificate",
                    start_time = 0,
                    end_time = NA,
                    overwrite = TRUE,
                    open = FALSE,
                    delete = open,
                    info = TRUE,
                    cover = FALSE,
                    quiet = FALSE) {
  # Build query's parameters
  query <- "--rm-cache-dir"
  if (mp3) {
    query <- c(
      query,
      "-f bestaudio",
      "--extract-audio",
      "--audio-format mp3",
      "--audio-quality 0"
    )
  }
  if (info) query <- c(query, "--write-info-json")
  query <- c(query, '-o "%(title)s.%(ext)s"', params)
  query <- v2t(c(repo, query, id), quotes = FALSE, sep = " ")
  if (!quiet) message(v2t(c("Query:", query), quotes = FALSE, sep = " "))

  # Run youtube-dl
  ret <- tryCatch(
    {
      system(query)
    },
    error = function(err) {
      msg <- "Something went wrong. Do you have youtube-dl installed?"
      if (grepl("^darwin", R.version$os)) {
        msg <- paste(msg, "Run in Terminal: brew install youtube-dl",
          "Then restart and try again",
          sep = "\n"
        )
      }
      msg <- paste(msg, "If already installed, try updating it with:",
        "sudo pip3 install --upgrade youtube_dl",
        sep = "\n"
      )
      msg <- paste(msg, "Our you could also try latest dev version that contains new features:",
        "https://github.com/yt-dlp/yt-dlp/wiki/Installation",
        sep = "\n"
      )
      stop(msg)
    }
  )

  if (1 %in% ret) {
    invisible(NULL)
  } else {
    f <- listfiles(getwd(), recursive = FALSE) %>%
      filter(grepl("\\.info\\.json", .data$filename)) %>%
      arrange(desc(.data$mtime)) %>%
      .[1, 1]

    infox <- jsonlite::read_json(f)
    invisible(file.remove(f))
    infox[["formats"]] <- NULL

    if (cover && mp3 && info) {
      aux <- gsub("\\.mp3", "", infox$title)
      aux <- gsub("lyrics|lyric|official|video", "", tolower(aux))
      aux <- gsub(" ", "\\+", cleanText(aux))
      url <- glued("https://www.google.com/search?q={aux}&tbm=isch&tbs=iar%3As")
      browseURL(url)
    }

    # TRIM START AND/OR END OF AUDIO FILE
    file <- sprintf("%s.mp3", infox$title)
    if (any(c(start_time > 0, !is.na(end_time)))) {
      message(">>> Trimming audio file: ", file)
      trim_mp3(file,
        start_time = start_time,
        end_time = end_time,
        overwrite = overwrite,
        quiet = quiet
      )
    }

    # Open file once everything is done
    mp3_file <- gsub(".info.json", ".mp3", f)
    if (open) {
      if (file.exists(mp3_file)) {
        message("Opening file: ", mp3_file)
        browseURL(mp3_file)
      } else {
        warning("Can't open file; possibly due to strange characters in title: ", mp3_file)
      }
    }
    # Delete file if delete
    if (delete) {
      if (file.exists(mp3_file)) {
        message("Deleting file: ", mp3_file)
        if (open) Sys.sleep(5)
        file.remove(mp3_file)
      } else {
        warning("Can't delete file; possibly due to strange characters in title: ", mp3_file)
      }
    }
    invisible(infox)
  }
}


####################################################################
#' Trim MP3 Audio File
#'
#' This function trims MP3 files given a start and/or end numeric
#' timestamp. Requires \code{ffmpeg} installed in your machine.
#'
#' @family Audio
#' @inheritParams get_mp3
#' @param file Character. File name to trim.
#' @param ext Character. File extension/type.
#' @export
trim_mp3 <- function(file, start_time = 0, end_time = NA,
                     overwrite = FALSE, ext = "mp3",
                     quiet = FALSE) {
  start <- paste("-ss", start_time)
  end <- ifelse(!is.na(end_time), paste("-to", end_time), "")
  for (i in file) {
    file <- ifelse(endsWith(i, ext), i, sprintf("%s.%s", file_name(i), ext))
    if (!file.exists(file)) {
      message(paste("File", file, "does not exist or can't be found."))
      next
    }
    query2 <- paste(
      "ffmpeg -hide_banner -loglevel panic -y",
      start, end, "-i",
      sprintf("'%s'", file),
      sprintf("'%s'", paste0(file_name(file), "_trimmed.mp3"))
    )
    if (!quiet) message("Query: ", query2)
    system(query2)
    if (overwrite) file.remove(gsub("_trimmed", "", file))
  }
}
