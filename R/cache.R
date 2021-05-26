####################################################################
#' Cache Save and Load (Write and Read)
#' 
#' This function lets the user save and load a cache of any R object to
#' improve timings and UX.
#' 
#' @family Cache
#' @param data Object
#' @param base Character vector. Unique name for your cache file. You can pass
#' a character vector with multiple elements that will be concatenated. 
#' @param cache_dir Character. Where do you want to save you cache files?
#' By default they'll be stored on \code{tempdir()} but you can change it
#' using this parameter or setting a global option called \code{"LARES_CACHE_DIR"}.
#' @param quiet Boolean. Keep quiet? If not, message will be shown.
#' @param ask Boolean. If cache exists, when reading: (interactive) ask the user 
#' if the cache should be used to proceed or ignored; when writing, (interactive)
#' ask the user if the cache should be overwritten.
#' @return \code{cache_write}. No return value, called for side effects.
#' @examples
#' x = list(a = 1, b = 2:4)
#' base <- c(as.character(Sys.Date()), "A","B","C")
#' cache_write(x, base)
#' cache_read(base, ask = FALSE)
#' @export
cache_write <- function(data,
                        base = "temp",
                        cache_dir = getOption("LARES_CACHE_DIR"),
                        ask = FALSE,
                        quiet = FALSE) {
  if (is.null(getOption("LARES_CACHE_DIR")))
    cache_dir <- tempdir()
  base <- paste(base, collapse = ".")
  base <- paste0("lares_cache_", base)
  file <- sprintf("%s/%s.RDS", cache_dir, base)
  if (nchar(file) >= 252)
    stop("Your file name can't contain more than 250 characters.")
  if (dir.exists(cache_dir)) {
    if (cache_exists(filename = file) & ask) {
      message("> Cache found: ", base)
      answer <- readline("Press ENTER to rewrite cache or type [i] to ignore: ")
    } else answer <- "use"
    if (answer != "i")  {
      saveRDS(data, file = file)
      if (!quiet) message("> Cache saved succesfully: ", base)
    } else {
      if (!quiet) message("> Skipped cache for: ", base)
      return(invisible(NULL))
    }
  } else {
    stop("Not a valid directory: ", cache_dir)
  }
}

#' @rdname cache_write
#' @return \code{cache_read}. R object. Data from cache file or NULL if no cache found.
#' @export
cache_read <- function(base,
                       cache_dir = getOption("LARES_CACHE_DIR"),
                       ask = FALSE,
                       quiet = FALSE) {
  base <- paste(base, collapse = ".")
  base <- paste0("lares_cache_", base)
  exists <- cache_exists(base, cache_dir)
  if (exists) {
    file <- attr(exists, "filename")
    base <- attr(exists, "base")
    if (ask == TRUE) {
      message("> Cache found: ", base)
      answer <- readline("Press ENTER to use cache or type [i] to ignore: ")
    } else answer <- "use"
    if (answer != "i")  {
      data <- readRDS(file)
      if (!quiet) message("> Cache loaded succesfully: ", base)
      return(data)
    }
  } else {
    if (!quiet) message("No cache found for: ", file)
    return(invisible(NULL))
  }
}

#' @rdname cache_write
#' @param filename Character. File name to check existence.
#' @return \code{cache_exists}. Boolean. Result of \code{base} existence.
#' @export
cache_exists <- function(base = NULL,
                         cache_dir = getOption("LARES_CACHE_DIR"),
                         filename = NULL) {
  if (is.null(getOption("LARES_CACHE_DIR")))
    cache_dir <- tempdir()
  if (is.null(filename)) {
    base <- paste(base, collapse = ".")
    filename <- sprintf("%s/%s.RDS", cache_dir, base) 
  }
  exists <- file.exists(filename)
  attr(exists, "filename") <- filename
  attr(exists, "base") <- base
  attr(exists, "cache_dir") <- cache_dir
  return(exists)
}

#' @rdname cache_write
#' @return \code{cache_clear}. Invisible vector containing cache file names removed.
#' @export
cache_clear <- function(cache_dir = getOption("LARES_CACHE_DIR")) {
  files <- list.files(cache_dir)
  caches <- files[grepl("lares_cache", files)]
  invisible(file.remove(sprintf("%s/%s", cache_dir, caches)))
  message(paste("Removed", length(caches), "cache files succesfully!"))
  return(invisible(caches))
}
