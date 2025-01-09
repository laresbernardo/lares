#' Pipe operator
#' @name lares-exports
NULL

#' @name %>%
#' @export
#' @rdname lares-exports
NULL

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
#' Scrap data based on IP address
#'
#' This function lets the user scrap https://db-ip.com/ given
#' IP address(es) to get their associated address type, ASN, ISP,
#' organization, country, state or region, county, city, ZIP postal code,
#' weather station, coordinates, Timezone, local time, languages, and currency.
#'
#' @family Tools
#' @family Scrapper
#' @param ip Vector. Vector with all IP's we wish to search.
#' @param quiet Boolean. Do not show the loading \code{statusbar}?
#' @return data.frame. Each row is an unique \code{ip} address,
#' and columns will bee created for all the additional information found.
#' @examples
#' \donttest{
#' ip_data("163.114.132.0")
#' ip_data(ip = c(myip(), "201.244.197.199"), quiet = TRUE)
#' }
#' @export
ip_data <- function(ip = myip(), quiet = FALSE) {
  ip <- ip[!is.na(ip)]
  ip <- ip[is_ip(ip)]
  ip <- unique(ip)
  output <- data.frame()
  for (i in seq_along(ip)) {
    url <- paste0("https://db-ip.com/", ip[i])
    scrap <- content(GET(url)) %>% html_table()
    clean <- bind_rows(scrap[[1]], scrap[[3]])
    row <- data.frame(t(clean[, 2]))
    colnames(row) <- clean$X1
    row <- data.frame(id = ip[i], row)
    output <- bind_rows(output, row)
    if (length(ip) > 1 && !quiet) statusbar(i, length(ip), ip[i])
  }
  output <- cleanNames(output)
  row.names(output) <- NULL
  return(output)
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
      return(df)
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

  return(df)
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
  ipify <- "https://api.ipify.org/"
  content(GET(ipify), encoding = "UTF-8")
}


####################################################################
#' Convert Python JSON string to R vector (data.frame with 1 row)
#'
#' This function lets the user transform a JSON string into vector
#' (data.frame with 1 row). You can also pass a Python's dictionary.
#' For any other JSON transformation, \code{jsonlite} is recommended.
#'
#' @family Tools
#' @param json Character. JSON string.
#' @return List, data.frame, or vector. Depends on the \code{json} string.
#' @examples
#' json2vector('{"id": 1, "nodata": null, "gender": "M"}')
#' @export
json2vector <- function(json) {
  string <- paste0("[", gsub('"', "\"", json), "]")
  string <- gsub("'", '"', string)
  string <- gsub("None", "null", string)
  string <- gsub("True", "true", string)
  string <- gsub("False", "false", string)
  vector <- fromJSON(string)
  data.frame(t(unlist(vector)))
}


####################################################################
#' Import Excel File with All Its Tabs
#'
#' This function lets the user import an Excel file's tabs into a list
#'
#' @family Tools
#' @param file String. Local Excel file name
#' @return List or data.frame. If single tab is found, a data.frame; if
#' multiple tabs are found on file, a list of data.frames.
#' @export
importxlsx <- function(file) {
  sheets <- getSheetNames(file)
  mylist <- list()
  for (i in seq_along(sheets)) {
    sheet <- read.xlsx(file,
      sheet = i,
      skipEmptyRows = TRUE,
      skipEmptyCols = TRUE,
      startRow = 1,
      detectDates = TRUE
    )
    mylist[[sheets[i]]] <- sheet
  }
  if (length(sheets) == 1) {
    mylist <- mylist[[1]][[1]]
  }
  mylist
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
    return(fx)
  }
  invisible(capture.output(fx))
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
    return(FALSE)
  }
  test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
  if (as.numeric(Sys.time() - start) > thresh) {
    message("Slow internet connection but available...")
  }
  return(!inherits(test, "try-error"))
}

####################################################################
#' Read Files Quickly (Auto-detected)
#'
#' This function lets the user import csv, xlsx, xls, sav files.
#'
#' @family Tools
#' @param filename Character. File name to import.
#' @param current_wd Boolean. Use current working directory before
#' the file's name? Use this param to NOT get absolute root directory.
#' @param sheet Character. Name or index of the sheet to read data
#' from if file is xlsx or xls.
#' @param quiet Boolean. Quiet summary message?
#' @return List or data.frame, depending on \code{filename}'s data.
#' @export
read.file <- function(filename, current_wd = TRUE, sheet = 1, quiet = FALSE) {
  if (current_wd) filename <- paste0(getwd(), "/", filename)
  if (!file.exists(filename)) {
    stop("That file doesn't exist.. try with another!")
  } else {
    filetype <- gsub("\\.", "", right(filename, 4))

    if (filetype == "csv") {
      results <- data.frame(read.csv(filename))
    }
    if (filetype == "xlsx") {
      results <- read.xlsx(filename, sheet)
    }
    if (filetype == "xls") {
      try_require("gdata")
      results <- read.xls(filename, read.xls)
    }
    # if (filetype == "sav") {
    #   try_require("foreign")
    #   results <- quiet(read.spss(filename, to.data.frame = T))
    # }
    # if (filetype == "dta") {
    #   # Stata version 5-12 .dta file
    #   # results <- foreign::read.dta(filename)
    #   # Stata version 13 .dta file
    #   try_require("readstata13")
    #   results <- read.dta13(filename)
    # }
    if (filetype == "dat") {
      results <- read.table(filename, header = TRUE)
    }

    if (!quiet) {
      message(paste(
        "Imported", filetype, "file with",
        formatNum(nrow(results), 0), "rows x",
        formatNum(ncol(results), 0), "columns, succesfully!"
      ))
    }
  }
  if (nrow(results) == 0) warning("There is no data in that file...")
  return(results)
}


####################################################################
#' Bind Files into Dataframe
#'
#' This function imports and binds multiple files into a single
#' data.frame. Files must be inserted with absolute roots files names.
#'
#' @family Tools
#' @param files Character vector. Files names.
#' @return data.frame with data joined from all \code{files} passed.
#' @export
bind_files <- function(files) {
  alldat <- data.frame()
  for (i in seq_along(files)) {
    file <- files[i]
    dfi <- read.file(file, current_wd = FALSE)
    alldat <- bind_rows(alldat, dfi)
    statusbar(i, length(files))
  }
  as_tibble(alldat)
}


####################################################################
#' New Line Feed for Long Strings (Wrapper)
#'
#' Add a break or new line without breaking words. Automatically,
#' the function can detect your plot's width and will dynamically
#' set an auto width. You can adjust the relation (rel) parameter
#' for different fonts and sizes until perfect harmony found.
#' Quite similar to \code{stringr::str_wrap} but, if the text vector
#' is a factor, the levels will be kept in order and transformed.
#'
#' @family Tools
#' @param text Character or factor vector.
#' @param top Integer. How many characters aprox. should be on each line?
#' @param rel Numeric. Relation of pixels and characters per line
#' @return Character. String (vector) including some \code{\\n} within.
#' @examples
#' cat(autoline("This is a long text that may not fit into a single line", 8))
#'
#' text <- factor(c("First value", "Second value", "First value"),
#'   levels = c("First value", "Second value")
#' )
#' autoline(text, 1)
#'
#' path <- file.path(R.home("doc"), "THANKS")
#' text <- paste(readLines(path), collapse = " ")
#' cat(autoline(text))
#' @export
autoline <- function(text, top = "auto", rel = 9) {
  # Auto-maximum
  if (top == "auto") top <- round(dev.size("px")[1] / rel)

  # Keep factors
  is_factor <- is.factor(text)
  if (is_factor) {
    levs <- levels(text)
    text <- as.character(text)
  }

  ret <- stringr::str_wrap(text, top)

  if (is_factor) {
    aux <- data.frame(ret, text) %>%
      mutate(text = factor(text, levels = levs)) %>%
      arrange(text) %>%
      unique()
    ret <- factor(ret, levels = aux$ret)
  }
  return(ret)
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
  if (grepl("^darwin", R.version$os)) {
    paths <-
      c(
        "/Library/Fonts/", # System fonts
        "/System/Library/Fonts", # More system fonts
        "/System/Library/Fonts/Supplemental", # More system fonts
        "~/Library/Fonts/", # User fonts
        font_dirs
      )
    return(paths[file.exists(paths)])
  } else if (grepl("^linux-gnu", R.version$os)) {
    paths <-
      c(
        "/usr/share/fonts/", # Ubuntu/Debian/Arch/Gentoo
        "/usr/local/share/fonts/", # system-admin-guide/stable/fonts.html.en
        "/usr/X11R6/lib/X11/fonts/TrueType/", # RH 6
        "~/.local/share/fonts/", # Added with Gnome font viewer
        "~/.fonts/", # User fonts
        font_dirs
      )
    return(paths[file.exists(paths)])
  } else if (grepl("^freebsd", R.version$os)) {
    # Possible font paths, depending on installed ports
    paths <-
      c(
        "/usr/local/share/fonts/truetype/",
        "/usr/local/lib/X11/fonts/",
        "~/.fonts/", # User fonts
        font_dirs
      )
    return(paths[file.exists(paths)])
  } else if (grepl("^mingw", R.version$os)) {
    paths <-
      c(
        file.path(Sys.getenv("SystemRoot"), "Fonts"),
        file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft", "Windows", "Fonts"),
        font_dirs
      )
    return(paths[file.exists(paths)])
  } else {
    stop("Unknown platform. Don't know where to look for truetype fonts. Sorry!")
  }
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
    return(nice_names)
  }
  if (grepl("^mingw", R.version$os)) {
    try_require("grDevices")
    win_fonts <- unlist(windowsFonts())
    ret <- font %in% win_fonts
    # font_names <- c(font_names, win_fonts)
  } else {
    ret <- font %in% nice_names
  }
  if (!quiet & !all(ret)) {
    if (!is.null(font)) {
      font_names <- font_names[
        grepl(tolower(paste(font, collapse = "|")), tolower(font_names))
      ]
    }
    if (length(font_names) > 0) {
      message("Maybe you meant one of these:\n", v2t(sort(gsub("\\..*", "", font_names))))
    }
  }
  return(ret)
}


####################################################################
#' List categorical values for data.frame
#'
#' Make a list with all categorical values and
#'
#' @family Tools
#' @param df data.frame
#' @param ... Variables to segment counters
#' @param abc Boolean. Sort alphabetically?
#' @return List. Length same as number of categorical columns, each with a
#' frequency data.frame using \code{freqs()}.
#' @examples
#' data(dft) # Titanic dataset
#' df <- dft[, 1:5]
#' head(df)
#' list_cats(df)
#' @export
list_cats <- function(df, ..., abc = TRUE) {
  is.categorical <- function(x) is.character(x) | is.factor(x)
  category <- which(unlist(lapply(df, is.categorical)))
  ret <- list()
  for (i in seq_along(category)) {
    which <- as.character(names(category)[i])
    df[, which] <- as.character(df[, which])
    aux <- freqs(df, cats = base::get(which), ...) %>%
      select(-.data$pcum, -.data$order)
    if (abc) aux <- arrange(aux, .data$cats)
    colnames(aux)[1] <- which
    ret[[which]] <- aux
  }
  return(ret)
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
  return(results)
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
    return(invisible(NULL))
  }
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


####################################################################
#' Spread list column into new columns
#'
#' Spread an existing list column into new columns on a data.frame. Note
#' that every element on every observation must have a name for the function
#' to do its work. Original column will be automatically suppressed but you
#' can set the \code{replace} argument to avoid it.
#'
#' @param df Dataframe
#' @param col Variable name.
#' @param str Character. Start column names with. If set to \code{NULL},
#' original name of column will be used.
#' @param replace Boolean. Replace original values (delete column)
#' @return data.frame. Result of un-nesting named or un-named list columns.
#' @examples
#' df <- dplyr::starwars
#' # Un-named list columns
#' spread_list(df, films, replace = FALSE) %>%
#'   dplyr::select(name, dplyr::starts_with("films")) %>%
#'   head(8)
#' # Named (and un-nammed) list columns
#' df <- dplyr::tibble(id = 1:3, platform = list(
#'   list("fb" = 1, "ig" = 2),
#'   list("fb" = 3),
#'   list()
#' ))
#' spread_list(df, platform, str = "ptf_")
#' @export
spread_list <- function(df, col, str = NULL, replace = TRUE) {
  var <- enquo(col)
  col <- gsub('"', "", as_label(var))
  cols <- colnames(df)

  if (!"list" %in% unlist(lapply(df[, cols == col], class))) {
    warning("The variable provided is not a list variable")
    return(df)
  }

  # If all values are NA, no need to proceed but to change into vector with NAs
  if (all(unlist(lapply(df[, cols == col], is.na)))) {
    df[, cols == col] <- rep(NA, nrow(df))
    return(df)
  }

  # Automatic naming based on original
  if (is.null(str)) str <- sprintf("%s_", col)

  # Add character NAs name to those observations with no data, thus no names
  nonames <- rowwise(df) %>%
    mutate(len = length(names(!!var))) %>%
    pull(.data$len) == 0
  # Non-named list columns
  if (sum(nonames) == nrow(df)) {
    binded <- select(df, !!var) %>%
      mutate(temp_cross_id = row_number()) %>%
      tidyr::unnest_longer(!!var) %>%
      mutate(key = TRUE) %>%
      tidyr::spread(key = !!var, value = .data$key) %>%
      replace(is.na(.), FALSE)
  } else {
    # Named list columns
    binded <- lapply(df[!nonames, cols == col], bind_rows) %>%
      bind_rows() %>%
      mutate(temp_cross_id = which(!nonames))
    if (is.numeric(binded[[1]])) binded <- binded %>% replace(is.na(.), 0)
  }

  if (is.na(str)) str <- paste0(col, "_")
  ids <- which(colnames(binded) == "temp_cross_id")
  colnames(binded)[-ids] <- paste0(str, colnames(binded))[-ids]
  done <- df %>%
    mutate(temp_cross_id = row_number()) %>%
    left_join(binded, "temp_cross_id") %>%
    select(-.data$temp_cross_id)

  original <- which(cols == col)
  done <- done %>% select(1:original, starts_with(str), (original + 1):ncol(done))
  if (replace) done <- done[, -original]
  return(as_tibble(done))
}


####################################################################
#' Format a string text as markdown/HTML
#'
#' Format any character string to HTML or markdown format. We
#' recommend using this format with the \code{ggtext::geom_richtext}
#' function to format text in \code{ggplot2} objects.
#'
#' @family Tools
#' @param text Character. Strings to format.
#' @param color Character. Hex colour code.
#' @param size Numeric. Text size.
#' @param bold Boolean. Should the text be bold?
#' @return String with format characters included.
#' @examples
#' formatHTML("Text test", color = "#000000")
#' formatHTML(c(123, 456), color = "orange", size = 120, bold = TRUE)
#'
#' # If you want to use it with \code{ggtext}:
#' \dontrun{
#' col1 <- "grey"
#' col2 <- "orange"
#' pt <- data.frame(
#'   label = paste0(
#'     formatHTML(123, color = col2, size = 120, bold = TRUE), "<br/>",
#'     formatHTML("of children had a", col1), "<br/>",
#'     formatHTML("traditional stay-at-home mom", color = col2, bold = TRUE), "<br/>",
#'     formatHTML(paste0("in 2012, compared to ", 321, " in 1970"), color = col1)
#'   )
#' )
#' ggplot(pt, aes(x = 0, y = 0)) +
#'   ggtext::geom_richtext(
#'     aes(label = label),
#'     hjust = 0,
#'     label.color = NA,
#'     lineheight = 1.5
#'   ) +
#'   xlim(0, 0.01) +
#'   theme_void()
#' }
#' @export
#' @rdname format_string
formatHTML <- function(text, color = "black", size = 20, bold = FALSE) {
  opening_span <- paste0("<span style='font-size:", size, "px; color:", color, "'>")
  if (bold) text <- paste0("**", text, "**")
  closing_span <- "</span>"
  text <- paste(text, collapse = "<br/>")
  ret <- paste0(opening_span, text, closing_span)
  return(ret)
}


####################################################################
#' Interpolate a string [glue wrapper]
#'
#' Format and interpolate a string using a \code{glue} wrapper. Allows
#' simple operations, \code{NULL} values as input, and interactions with
#' internal (created within \code{glued}) and external (environment) objects.
#'
#' @family Tools
#' @inheritParams stringr::str_glue
#' @param empty_lines Character. Set to \code{"keep"} to keep or
#' \code{"drop"} to drop empty lines.
#' @return Same as input but transformed (glued).
#' @examples
#' name <- "Bernardo"
#' age <- 29
#' anniversary <- as.Date("2016-04-30")
#' glued("
#'   My name is {name},
#'   my age next year will be {age + 1},
#'   and I got married on {format(anniversary, '%A, %B %d, %Y')}.")
#'
#' # Single braces can be inserted by doubling them
#' glued("My name is {name}, not {{name}}.")
#'
#' # You can also used named arguments
#' glued(
#'   "Her name is {name}, ",
#'   "and her age next year will be {age + 1}.",
#'   name = "Maru",
#'   age = 6
#' )
#'
#' # And run operations with memories (beware!)
#' glued("My name, {name}, has {n <- nchar(name); n} characters.
#'        If we multiply by ten, we'll have {10 * n} characters!")
#'
#' # If you pass a vector, the operation will be repeated for each element
#' glued("Here's the value #{1:3}")
#' @export
glued <- function(..., .sep = "", empty_lines = "keep", .envir = parent.frame()) {
  null_transformer <- function(text, envir) {
    out <- eval(parse(text = text, keep.source = FALSE), envir)
    if (is.null(out)) out <- ""
    return(out)
  }
  output <- stringr::str_glue(
    ...,
    .sep = .sep,
    .transformer = null_transformer,
    .envir = .envir
  )
  if (empty_lines == "drop") {
    lines <- stringr::str_split(output, "\n")[[1]]
    output <- glued(paste(lines[trimws(lines) != ""], collapse = "\n"))
  }
  return(output)
}


####################################################################
#' Pattern Matching for Any or All Multiple Matches
#'
#' This function returns a boolean vector of the same length as `x`,
#' each element of which is the result of applying the `type` of matches
#' to the corresponding element of `x`, using regular expressions.
#'
#' @family Tools
#' @inheritParams base::grep
#' @param x Character vector. Text where matches are sought, or an object
#' which can be coerced by as.character to a character vector.
#' Long vectors are supported.
#' @param type Character. Type of match. Choose one of:
#' \code{any}, \code{all}
#' @param ... Additional arguments to pass to \code{grepl}
#' @return Boolean of same length as \code{x}
#' @examples
#' x <- c(123, 876, 18761)
#' patterns <- c(1, 2)
#' grepm(patterns, x, type = "any")
#' grepm(patterns, x, type = "all")
#' @export
grepm <- function(pattern, x, type = "all", ...) {
  lapply(x, function(a) lapply(pattern, function(i) grepl(i, a, ...))) %>%
    lapply(get(type)) %>%
    unlist() %>%
    suppressWarnings()
}

####################################################################
#' Print Coloured Messages
#'
#' @family Tools
#' @param txt Character. Text to print or transform.
#' @param colour Character. Any of: grey, red, green, yellow, blue, or purple.
#' @param bold Boolean. Set bold text?
#' @param cat Boolean. Print with cat? If not, raw string
#' @return Depends on \code{cat}: NULL if TRUE or character string if FALSE.
#' @examples
#' opts <- c("GREY", "RED", "GREEN", "YELLOW", "BLUE", "PURPLE")
#' for (colour in opts) formatColoured(paste("Colour:", colour, "\n"), colour)
#' formatColoured("my bold coloured text", bold = TRUE, cat = TRUE)
#' @export
formatColoured <- function(txt, colour = c("yellow", "blue", "grey"), bold = FALSE, cat = TRUE) {
  colour <- toupper(colour)[1]
  opts <- c("GREY", "RED", "GREEN", "YELLOW", "BLUE", "PURPLE", "CYAN", "WHITE")
  check_opts(colour, opts)
  if (colour == opts[1]) code <- 30
  if (colour == opts[2]) code <- 31
  if (colour == opts[3]) code <- 32
  if (colour == opts[4]) code <- 33
  if (colour == opts[5]) code <- 34
  if (colour == opts[6]) code <- 35
  if (colour == opts[7]) code <- 36
  if (colour == opts[8]) code <- 37
  out <- paste0("\033[", ifelse(!bold, 0, 1), ";", code, "m", txt, "\033[0m")
  if (cat) cat(out) else return(out)
}

####################################################################
#' Test the Truth of R Expressions and Warn
#'
#' If the expression in ... is not \code{TRUE}, \code{warning} is called,
#' producing a warning message indicating the expression which was not true.
#'
#' @family Tools
#' @param ... any R expression, which should evaluate to TRUE
#' @examples
#' warnifnot(TRUE)
#' warnifnot(FALSE)
#' warnifnot(1 + 1 == 3)
#' @export
warnifnot <- function(...) if (!isTRUE(...)) warning(paste(deparse(...), "is not TRUE"))


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
  return(size_files)
}

####################################################################
#' Convert markdown string tables to data.frame
#'
#' @family Tools
#' @param text Character. Markdown text representing a table.
#' @param autoformat Boolean. Automatically format numerical,
#' logical and date values to their classes?
#' @examples
#' txt <- "| Item | Value |\n|------|-------|\n| 50C  | 122F  |\n| 300K | 80.33F |"
#' markdown2df(txt)
#' @export
markdown2df <- function(text, autoformat = TRUE) {
  df <- removenacols(read.table(
    text = text, sep = "|", header = TRUE, strip.white = TRUE, quote = "\""
  ))
  # Get rid of potential first row with all values set as --- or :---
  if (all(stringr::str_split(df[1, 1], "-")[[1]] == "")) df <- df[-1, ]
  if (substr(df[1, 1], 1, 4) == ":---") df <- df[-1, ]
  rownames(df) <- NULL
  df <- as_tibble(df)
  if (autoformat) {
    df <- df %>%
      chr2num() %>%
      chr2logical() %>%
      chr2date()
  }
  return(df)
}

####################################################################
#' Check character values for date/numeric/logical and change datatype
#'
#' Automatically check a vector, data.frame or list for numeric, logical,
#' date content and change their datatype. Note that factors are skipped in
#' case the user requires character numeric values to be kept as they are.
#'
#' @family Tools
#' @param data Vector, data.frame or list
#' @examples
#' str(chr2num(c("1", "2", "3")))
#' df <- data.frame(A = c("1", "3"), B = c("A", "B"), c = c(pi, pi * 2))
#' str(chr2num(df))
#' lst <- list(A = c("1", "2", "3"), B = c("A", "B", "3"), C = pi, D = 3L)
#' str(chr2num(lst))
#' lst2 <- list(layer1 = ":D", layer2 = lst)
#' str(chr2num(lst2))
#' str(chr2logical(c(NA, "true", FALSE)))
#' @export
chr2num <- function(data) {
  pattern <- "^-?\\d+(\\.\\d+)?$"
  if (is.list(data)) {
    char_elements <- sapply(data, is.character)
    num_elements <- sapply(data, function(element) {
      all(grepl(pattern, element[!is.na(element)]))
    })
    elements_to_convert <- char_elements & num_elements
    data[elements_to_convert] <- lapply(data[elements_to_convert], as.numeric)
  } else {
    if (is.character(data) && all(grepl(pattern, data))) {
      data <- as.numeric(data)
    }
  }
  data
}
#' @rdname chr2num
#' @export
chr2logical <- function(data) {
  if (is.list(data)) {
    char_elements <- sapply(data, is.character)
    log_elements <- sapply(data, function(element) {
      all(toupper(element) %in% c("TRUE", "FALSE", NA))
    })
    elements_to_convert <- char_elements & log_elements
    data[elements_to_convert] <- lapply(toupper(data[elements_to_convert]), as.logical)
  } else {
    if (is.character(data) && all(toupper(data) %in% c("TRUE", "FALSE", NA))) {
      data <- as.logical(toupper(data))
    }
  }
  data
}
#' @rdname chr2num
#' @export
chr2date <- function(data) {
  pattern <- "^[0-9-]+$"
  if (is.list(data)) {
    char_elements <- sapply(data, is.character)
    log_elements <- sapply(data, function(element) {
      all(grepl(pattern, element) | is.na(element))
    })
    elements_to_convert <- char_elements & log_elements
    data[elements_to_convert] <- lapply(
      data[elements_to_convert], function(x) as.Date(x, origin = "1970-01-01")
    )
  } else {
    if (is.character(data) && all(grepl(pattern, data) | is.na(data))) {
      data <- as.Date(data, origin = "1970-01-01")
    }
  }
  return(data)
}
