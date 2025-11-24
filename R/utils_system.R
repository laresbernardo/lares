####################################################################
#' Check if Specific Package is Installed
#'
#' This function checks library dependencies
#'
#' @family Tools
#' @inheritParams cache_write
#' @param package Character. Name of the library
#' @param stop Boolean. Stop if not installed. If \code{FALSE} and
#' library is not available, warning will be shown.
#' @param load Boolean. Load library?
#' @param lib.loc Character vector. Location of R library trees
#' to search through, or \code{NULL}. The default value of \code{NULL}
#' corresponds to all libraries currently known to \code{.libPaths()}.
#' Non-existent library trees are silently ignored.
#' @return No return value, called for side effects.
#' @examples
#' # Check if library base is installed. If not, stop and show error
#' try_require("base", stop = TRUE)
#' # Check if library xxx is installed. If not, show warning
#' try_require("xxx", stop = FALSE)
#' @export
try_require <- function(package, stop = TRUE, load = TRUE, lib.loc = NULL, ...) {
  present <- length(find.package(package, quiet = TRUE)) > 0
  if (present && load) {
    # Be careful: ... parameter is not enabled in library()
    suppressPackageStartupMessages(library(package, character.only = TRUE, lib.loc = lib.loc))
  } else {
    if (stop) {
      stop(paste0("Package '", package, "' required. Install and try again."), call. = FALSE)
    } else {
      warning(paste0("Package '", package, "' recommended. Install for better results."), call. = FALSE)
    }
  }
}


####################################################################
#' Get Meta Data from Image Files
#'
#' This function lets the user get meta data from image files or directory.
#'
#' @family Tools
#' @param files Character vector. Files or directory which contains files.
#' @return data.frame with meta-data for each image file.
#' @export
image_metadata <- function(files) {
  try_require("exifr")

  files <- as.character(files)
  if (length(files) == 1) {
    if (dir.exists(files)) {
      files <- listfiles(files, recursive = TRUE)
    }
  }

  df <- data.frame(file = files)
  tags <- c(
    "FileName", "SourceFile",
    "CreateDate", "DateTimeOriginal", "FileModifyDate",
    "FileTypeExtension", "Megapixels",
    "ImageSize", "ImageWidth", "ImageHeight",
    "GPSLongitude", "GPSLatitude",
    "GPSLatitudeRef", "GPSLongitudeRef",
    "Rotation", "Flash", "Duration",
    "Make", "Model"
  )

  aux <- ceiling(nrow(df) / 500)
  for (i in 1:aux) {
    if (i == 1) {
      ret <- NULL
      if (aux > 2) {
        message(paste(
          "This might take a while... Analizing",
          formatNum(nrow(df), decimals = 0), "files!"
        ))
      }
    }
    from <- (i - 1) * 500 + 1
    to <- i * 500
    x <- slice(df, from:to)
    temp <- read_exif(as.character(x$file), tags = tags)
    if (nrow(temp) > 0) {
      if ("DateTimeOriginal" %in% colnames(temp)) {
        ret <- bind_rows(ret, select(temp, one_of(tags)))
      }
    }
    statusbar(i, aux, label = paste(formatNum(from, 0), "-", formatNum(to, 0)))
  }

  if (length(ret) > 0) {
    if ("DateTimeOriginal" %in% colnames(ret)) {
      df <- ret %>%
        mutate(
          DateTimeOriginal = ymd_hms(.data$DateTimeOriginal),
          CreateDate = ymd_hms(.data$CreateDate),
          FileModifyDate = ymd_hms(.data$FileModifyDate)
        )
      df <- df[, colSums(is.na(df)) < nrow(df)]
      if (aux > 10) {
        tryCatch({
          try_require("beepr", stop = FALSE)
          beep()
        })
      }
      df
    }
  } else {
    message("No images found to process...")
  }
}


####################################################################
#' List files in a directory
#'
#' This function lets the user list all files on a given directory.
#' It also lets filter files which contains a string.
#'
#' @family Tools
#' @param folder Character. Directory which contains files
#' @param recursive Boolean. Should the listing recurse into directories?
#' @param regex Character. String to use for filtering files
#' @param images Boolean. Bring only image files?
#' @return data.frame with relevant data for each file on
#' \code{folder} directory.
#' @examples
#' # All files in current directory (without recursive files)
#' df <- listfiles(recursive = TRUE)
#' head(df, 3)
#'
#' # All files in current directory (with recursive files)
#' df <- listfiles(recursive = TRUE)
#' tail(df, 3)
#'
#' # Check R files using regex
#' df <- listfiles(regex = "\\.R$")
#' @export
listfiles <- function(folder = getwd(),
                      recursive = TRUE,
                      regex = NA,
                      images = FALSE) {
  if (!file.exists(folder)) {
    stop("That directory doesn't exist; please try again!")
  }

  files <- list.files(folder, recursive = recursive)
  address <- paste0(folder, "/", files)
  info <- file.info(address)

  df <- data.frame(filename = files, address, info)
  df$size <- as.integer(df$size / 1024)
  # imgs <- "jpg|JPG|jpeg|JPEG|png|PNG|gif|GIF"

  if (!is.na(regex)) df <- df[grep(regex, df$filename), ]

  if (images) {
    df <- image_metadata(df$address)
  }

  row.names(df) <- NULL
  df$address <- NULL
  df
}


####################################################################
#' What's my IP?
#'
#' Reveal your current IP address.
#'
#' @family Tools
#' @return Character. Result of your IP address based on ipify.org
#' @examples
#' \donttest{
#' myip()
#' }
#' @export
myip <- function() {
  if (!haveInternet()) {
    message("No internet connetion...")
    invisible(NULL)
  } else {
    ipify <- "https://api.ipify.org/"
    try(content(GET(ipify), encoding = "UTF-8"))
  }
}


####################################################################
#' Quiet prints and verbose noise
#'
#' This function silences (verbose) output prints. Thanks to Hadley Wickham
#' for bringing the idea.
#'
#' @family Tools
#' @param fx Function to quiet
#' @param quiet Quiet outputs? If not, skip quietness.
#' @return Same as \code{fx} but with no messages or prints.
#' @export
quiet <- function(fx, quiet = TRUE) {
  if (!quiet) {
    fx
  } else {
    invisible(capture.output(fx))
  }
}


####################################################################
#' Internet Connection Check
#'
#' This function checks if your R session currently have Wifi or
#' Internet connection.
#'
#' @family Tools
#' @param thresh Numeric. How many seconds to consider a slow connection?
#' @param url Character. URL to test the readLines 1 command
#' @return Boolean. Result of checking if device has internet connection.
#' @export
haveInternet <- function(thresh = 3, url = "http://www.google.com") {
  start <- Sys.time()
  if (!capabilities(what = "http/ftp")) {
    FALSE
  } else {
    test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
    if (as.numeric(Sys.time() - start) > thresh) {
      message("Slow internet connection but available...")
    }
    !inherits(test, "try-error")
  }
}


####################################################################
#' Check if Font is Installed
#'
#' This function checks if a font is installed in your machine.
#' To list all available fonts, set \code{font = NULL}.
#'
#' @family Tools
#' @inheritParams cache_write
#' @param font Character. Which font to check. No need to add .TFF.
#' @param font_dirs Character vector. Additional directories to check for fonts.
#' @param quiet Boolean. Keep quiet? If not, show message
#' @return Boolean result of the existing fonts check.
#' @examples
#' font_exists(font = "Arial")
#' font_exists(font = "arial")
#' font_exists(font = "")
#' font_exists(font = NULL)
#' @export
font_exists <- function(font = "Arial Narrow", font_dirs = NULL, quiet = FALSE, ...) {
  if (isTRUE(font[1] == "")) {
    FALSE
  }
  if (isTRUE(is.na(font[1]))) {
    FALSE
  }
  tryCatch(
    check_font(font, font_dirs, quiet),
    error = function(err) {
      if (!quiet) message(paste("Font issue:", err))
      FALSE
    }
  )
}

# Thanks to extrafont for the idea for this code
ttf_find_default_path <- function(font_dirs = NULL) {
  os <- R.version$os
  paths <- if (grepl("^darwin", os)) {
    c(
      "/Library/Fonts/",
      "/System/Library/Fonts",
      "/System/Library/Fonts/Supplemental",
      "~/Library/Fonts/",
      font_dirs
    )
  } else if (grepl("^linux-gnu", os)) {
    c(
      "/usr/share/fonts/",
      "/usr/local/share/fonts/",
      "/usr/X11R6/lib/X11/fonts/TrueType/",
      "~/.local/share/fonts/",
      "~/.fonts/",
      font_dirs
    )
  } else if (grepl("^freebsd", os)) {
    c(
      "/usr/local/share/fonts/truetype/",
      "/usr/local/lib/X11/fonts/",
      "~/.fonts/",
      font_dirs
    )
  } else if (grepl("^mingw", os)) {
    c(
      file.path(Sys.getenv("SystemRoot"), "Fonts"),
      file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft", "Windows", "Fonts"),
      font_dirs
    )
  } else {
    stop("Unknown platform. Don't know where to look for truetype fonts. Sorry!")
  }
  paths[file.exists(paths)]
}

check_font <- function(font, font_dirs = NULL, quiet = FALSE) {
  pattern <- "\\.ttf$|\\.otf"
  fonts_path <- ttf_find_default_path(font_dirs)
  ttfiles <- list.files(fonts_path,
    pattern = pattern, recursive = TRUE,
    full.names = TRUE, ignore.case = TRUE
  )
  font_names <- basename(ttfiles)
  nice_names <- gsub(pattern, "", font_names, ignore.case = TRUE)
  if (is.null(font)) {
    nice_names
  } else {
    if (grepl("^mingw", R.version$os)) {
      try_require("grDevices")
      win_fonts <- unlist(windowsFonts())
      ret <- font %in% win_fonts
      # font_names <- c(font_names, win_fonts)
    } else {
      ret <- font %in% nice_names
    }
    if (!quiet && !all(ret)) {
      if (!is.null(font)) {
        font_names <- font_names[
          grepl(tolower(paste(font, collapse = "|")), tolower(font_names))
        ]
      }
      if (length(font_names) > 0) {
        message("Maybe you meant one of these:\n", v2t(sort(gsub("\\..*", "", font_names))))
      }
    }
    ret
  }
}


####################################################################
#' List all functions used in R script files by package
#'
#' Parses all functions called by an R script and then lists
#' them by package. Wrapper for 'getParseData'. May be of great
#' use for those developing a package to help see what
#' namespace 'importsFrom' calls will be required.
#'
#' @family Tools
#' @param filename Character. Path to an R file (or directory)
#' containing R code files.
#' @param abc Boolean. List functions alphabetically.
#' If FALSE, will list in order of frequency.
#' @param quiet Boolean. Keep quiet? If not, print messages and
#' \code{statusbar}.
#' @return data.frame. Each row is a function and columns stating
#' number of appearances, percentage, packages, and files searched.
#' @examples
#' \dontrun{
#' # Choose an R script file with functions
#' rfile <- file.choose()
#' files_functions(rfile)
#' }
#' @export
files_functions <- function(filename, abc = TRUE, quiet = FALSE) {
  if (dir.exists(filename)) {
    if (!quiet) message("Importing R files from directory")
    filename <- sprintf("%s/%s", filename, listfiles(filename, regex = "\\.R")$filename)
  }
  results <- data.frame()
  for (i in seq_along(filename)) {
    if (!file.exists(filename[i])) {
      stop("Couldn't find file ", filename[i])
    }
    tmp <- getParseData(parse(filename[i], keep.source = TRUE))
    funs <- tmp$text[which(tmp$token == "SYMBOL_FUNCTION_CALL")] %>%
      freqs(abc = abc) %>%
      rename(fun = 1)
    pkgs <- lapply(funs$fun, find)
    pkgs[unlist(lapply(pkgs, function(x) length(x) == 0))] <- NA
    pkgs <- lapply(pkgs, function(x) v2t(x, quotes = FALSE))
    funs$package <- gsub("package:", "", unlist(pkgs))
    funs$file <- filename[i]
    results <- rbind(results, funs)
    if (length(filename) > 1 && !quiet) {
      statusbar(i, length(filename), filename[i])
    }
  }
  results
}


####################################################################
#' Move files from A to B
#'
#' Move one or more files from a directory to another using R.
#'
#' @family Tools
#' @param from Character. File names and directories. All files
#' will be moved recursively.
#' @param to Character. File names for each \code{from} file or
#' directory. If directory does not exist, it will be created.
#' @return No return value, called for side effects.
#' @export
move_files <- function(from, to) {
  froms <- dirs <- NULL
  for (i in seq_along(from)) {
    fromi <- from[i]
    if (isTRUE(file.info(fromi)$isdir)) {
      fromi <- list.files(fromi, recursive = TRUE)
      fromi <- paste(from[i], fromi, sep = "/")
      dirs <- c(dirs, from[i])
    }
    froms <- c(froms, fromi)
  }
  froms <- froms[grepl("\\.", basename(froms))]
  froms <- gsub(paste0(getwd(), "/"), "", froms)

  tos <- to
  # If it is a directory
  if (length(tos) == 1) {
    if (!isTRUE(file.info(tos)$isdir)) {
      dir.create(tos, recursive = FALSE)
      message(sprintf("Directory '%s' did not exist and was created", tos))
    }
    tos <- paste(to, sub(".*?/", "", froms), sep = "/")
  }

  # Final check for all files
  if (length(froms) == 0) {
    warning(sprintf("No files to move from %s...", from))
    invisible(NULL)
  } else {
    if (length(tos) != length(froms)) {
      stop("Every 'from' must have a respective 'to' filename")
    }
    # Now move/rename all files
    for (i in seq_along(froms)) {
      todir <- dirname(tos[i])
      if (!isTRUE(file.info(todir)$isdir)) {
        dir.create(todir, recursive = FALSE)
      }
      file.rename(from = froms[i], to = basename(tos[i]))
    }
  }
}


####################################################################
#' Calculate the size of any R object or directory
#'
#' @family Tools
#' @inheritParams base::format
#' @inheritParams base::list.files
#' @param x Object
#' @param units Character. Specify which unit to use,
#' i.e. "Gb", "Mb", "Kb".
#' @examples
#' what_size(seq(1:1e3), "Kb")
#' what_size(seq(1:1e6))
#' what_size(as.character(seq(1:1e6)))
#' # what_size(path = ".")
#' @export
what_size <- function(x = NULL, units = "Mb", path = NULL, recursive = TRUE, ...) {
  if (!is.null(path)) {
    size <- dir_size(path, recursive, ...)
    format(size, units = units, ...)
  } else {
    format(object.size(x), units = units, ...)
  }
}

dir_size <- function(path = getwd(), recursive = TRUE, pattern = NULL, ...) {
  stopifnot(is.character(path))
  stopifnot(dir.exists(path))
  # files <- list.files(
  #   path = path, pattern = pattern,
  #   recursive = recursive, full.names = TRUE)
  # size_files <- 0
  # if (length(files) > 0) {
  #   vect_size <- sapply(files, function(x) file.size(x))
  #   size_files <- sum(vect_size, na.rm = TRUE)
  # }
  cmd <- paste("du -hs", path)
  size_files <- system(cmd, intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  size_files <- gsub("\\\t.*", "", as.numeric(size_files))
  size_files <- num_abbr(size_files, numeric = TRUE)
  class(size_files) <- "object_size"
  size_files
}
