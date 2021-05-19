####################################################################
#' Cache Write and Read
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
  base <- v2t(base, quotes = FALSE, sep = ".")
  file <- sprintf("%s/%s.RDS", cache_dir, base)
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
#' @export
cache_read <- function(base,
                       cache_dir = getOption("LARES_CACHE_DIR"),
                       ask = FALSE,
                       quiet = FALSE) {
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
#' @export
cache_exists <- function(base = NULL,
                         cache_dir = getOption("LARES_CACHE_DIR"),
                         filename = NULL) {
  if (is.null(getOption("LARES_CACHE_DIR")))
    cache_dir <- tempdir()
  if (is.null(filename)) {
    base <- v2t(base, quotes = FALSE, sep = ".")
    filename <- sprintf("%s/%s.RDS", cache_dir, base) 
  }
  exists <- file.exists(filename)
  attr(exists, "filename") <- filename
  attr(exists, "base") <- base
  attr(exists, "cache_dir") <- cache_dir
  return(exists)
}
