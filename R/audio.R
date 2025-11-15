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
#' @param title Character. Title of the track to search for cover, metadata, and
#' name the file.
#' @param cover Boolean. Google Search its squared cover? Uses \code{title}
#' input when provided.
#' @param metadata Boolean. Use "spotifyr" to extract "track" data using Spotify's
#' API. Needs credentials. Uses \code{title} input when provided.
#' @param quiet Boolean. Keep quiet? If not, informative messages will be shown.
#' @return (Invisible) list with id's meta-data.
#' @examples
#' # You must have "youtube-dl" installed in your OS:
#' \dontrun{
#' # Download video from YouTube and convert to MP3
#' mp3_get("https://www.youtube.com/watch?v=lrlKcCdVw9Q")
#' # OR simply
#' mp3_get("lrlKcCdVw9Q")
#' # For dev version, use:
#' mp3_get("m3RX4LJh0iI", repo = "yt-dlp")
#' }
#' @export
mp3_get <- function(id,
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
    
    # The actual file path created by youtube-dl
    mp3_file_original <- gsub(".info.json", ".mp3", f)
    mp3_file <- mp3_file_original
    
    if (!is.null(title)) {
      infox$title <- title
      safe_title_for_file <- trimws(title)
      safe_title_for_file <- gsub("\\s+", " ", safe_title_for_file)
      new_mp3_file_base <- paste0(safe_title_for_file, ".mp3")
      new_mp3_path <- file.path(dirname(mp3_file_original), new_mp3_file_base)
      if (file.exists(mp3_file_original)) {
        if (file.exists(new_mp3_path) && overwrite) {
          file.remove(new_mp3_path)
        } else if (file.exists(new_mp3_path) && !overwrite) {
          if (!quiet) warning(paste("File already exists:", basename(new_mp3_path), "- skipping rename"))
          mp3_file <- mp3_file_original 
        }
        if (file.exists(mp3_file_original) && !file.exists(new_mp3_path)) {
          file.rename(mp3_file_original, new_mp3_path)
          mp3_file <- new_mp3_path
          if (!quiet) message(sprintf("File renamed to: %s", basename(mp3_file)))
        }
      } else {
        if (!quiet) warning("Could not find the original MP3 file for renaming. Proceeding with original filename.")
      }
    }
    
    filename <- basename(mp3_file)
    
    if (metadata) {
      try_require("spotifyr")
      message(">>> Adding metadata: ", infox$title)
      authorization <- get_spotify_access_token(
        client_id = get_creds("spotify")$SPOTIFY_CLIENT_ID,
        client_secret = get_creds("spotify")$SPOTIFY_CLIENT_SECRET
      )
      sp <- search_spotify(
        q = infox$title,
        type = "track",
        limit = 3,
        authorization = authorization
      ) %>%
        rowwise() %>%
        mutate(matching = grepl(.data$name, infox$title) |
                 grepl(.data$album.name, infox$title)) %>%
        arrange(desc(.data$matching)) %>%
        head(1)
      album <- get_album(sp$album.id, authorization = authorization)
      
      # Fetch (first) artist's genre if no genre album genres available
      if (length(album$genres) == 0) {
        artist <- search_spotify(
          sp$artists[[1]]$name[1],
          type = "artist", limit = 1,
          authorization = authorization
        )
        genres <- cleanText(artist$genres[[1]][1],
                            spaces = TRUE, keep = c("&", "/"), title = TRUE
        )
        if (genres == "Null") genres <- ""
      } else {
        genres <- v2t(album$genres, sep = " / ")
        artist <- NULL
      }
      
      # Update all metadata
      infox$audio <- mp3_update_tags(
        mp3_file,
        title = sp$name,
        artist = v2t(sp$artists[[1]]$name, quotes = FALSE),
        album = sp$album.name,
        release_date = sp$album.release_date,
        recording_date = sp$album.release_date,
        track_num = tuple(count = sp$track_number, total = sp$album.total_tracks),
        genre = genres,
        label = album$label
      )
      
      # Handle cover art from URL
      cover_url <- sp$album.images[[1]]$url[1]
      if (!is.null(cover_url)) {
        message(">>> Adding cover...")
        tmp_img <- tempfile(fileext = ".jpg")
        httr::GET(cover_url, httr::write_disk(tmp_img, overwrite = TRUE))
        img_bytes <- readBin(tmp_img, what = "raw", n = file.info(tmp_img)$size)
        infox$audio$tag$images$set(3L, img_bytes, "image/jpeg", "Cover")
        infox$audio$tag$save()
        cover <- FALSE
      }
      infox$metadata <- list(track = sp, artist = artist, album = album)
    }
    
    if (cover && mp3 && info) {
      aux <- gsub("\\.mp3", "", infox$title)
      aux <- gsub("lyrics|lyric|official|video", "", tolower(aux))
      aux <- gsub(" ", "\\+", cleanText(aux))
      url <- glued("https://www.google.com/search?q={aux}&tbm=isch&tbs=iar%3As")
      browseURL(url)
    }
    
    # TRIM START AND/OR END OF AUDIO FILE
    if (any(c(start_time > 0, !is.na(end_time)))) {
      message(">>> Trimming audio file: ", filename)
      mp3_trim(file = mp3_file,
               start_time = start_time,
               end_time = end_time,
               overwrite = overwrite,
               quiet = quiet
      )
    }
    
    # Open file once everything is done
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
#' @inheritParams mp3_get
#' @param file Character. File name to trim.
#' @param ext Character. File extension/type.
#' @export
mp3_trim <- function(file, start_time = 0, end_time = NA,
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


####################################################################
#' Update MP3 Metadata Tags
#'
#' Updates the ID3 metadata tags of an MP3 file using the Python `eyeD3` library
#' via the `reticulate` package. You can modify standard fields such as
#' \code{title}, \code{artist}, \code{album}, and \code{genre}, as well as
#' additional tags passed through \code{...} if they exist in the MP3 file.
#' @details
#' The function requires Python and the \code{eyeD3} package installed. It will
#' automatically initialize ID3 tags if they do not exist. Tags provided via
#' \code{...} are checked against the existing tag names to avoid errors.
#'
#' @family Audio
#' @param filename Character. Path to the MP3 file to update.
#' @param title Character. New title of the track (optional).
#' @param artist Character. New artist name (optional).
#' @param album Character. New album name (optional).
#' @param genre Character. New genre name (optional).
#' @param ... Additional named arguments corresponding to other ID3 tags.
#' Only tags that exist in the MP3 file will be updated.
#' @examples
#' \dontrun{
#' mp3_update_tags(
#' "song.mp3",
#' title = "My Jazz Song",
#' artist = "Bernardo",
#' album = "Smooth Album",
#' genre = "Jazz"
#' )
#' }
#' @return Invisibly returns \code{NULL}. The MP3 file is updated in-place.
#' @export
mp3_update_tags <- function(filename, title = NULL, artist = NULL, album = NULL, genre = NULL, ...) {
  if (!file.exists(filename)) {
    message("File does not exist. Check filename input and working directory....")
  } else {
    try_require("reticulate")
    eyeD3 <- NULL
    eyeD3 <- import("eyed3")
    audio <- eyeD3$load(normalizePath(filename))
    if (is.null(audio$tag)) audio$initTag()
    tag_args <- list(title = title, artist = artist, album = album, genre = genre, ...)
    valid_tags <- names(audio$tag)
    for (n in names(tag_args)) {
      if (!is.null(tag_args[[n]]) && n %in% valid_tags) {
        audio$tag[[n]] <- tag_args[[n]]
      }
    }
    audio$tag$save()
  }
  invisible(audio)
}