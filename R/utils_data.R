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
  if (!haveInternet()) {
    message("No internet connetion...")
    invisible(NULL)
  } else {
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
    output
  }
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
  results
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
  ret
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
    df
  } else {
    # If all values are NA, no need to proceed but to change into vector with NAs
    if (all(unlist(lapply(df[, cols == col], is.na)))) {
      df[, cols == col] <- rep(NA, nrow(df))
      df
    } else {
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
      as_tibble(done)
    }
  }
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
  df
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
  data
}

####################################################################
#' Calculate Character Reduction from R Object to TOON Format
#'
#' Calculates the percentage reduction in the number of characters when
#' converting an R object to its TOON string representation compared to
#' its JSON representation (or the string itself if the input is a single string).
#'
#' @param obj Any R object (list, vector, data frame, or string).
#' @param ... Additional arguments passed to jsonlite::toJSON()
#' @return A numeric value representing the character reduction ratio (0 to 1).
#' @export
toon_reduction <- function(obj, ...) {
  toon_str <- as_toon(obj)
  toon_nchar <- nchar(toon_str)
  if (length(obj) == 1 && is.character(obj) && !is.na(obj)) {
    original_str <- obj
  } else {
    original_str <- jsonlite::toJSON(obj, ...)
  }
  original_nchar <- nchar(original_str)
  if (original_nchar == 0) {
    return(0)
  }
  reduction_amount <- original_nchar - toon_nchar
  reduction_ratio <- reduction_amount / original_nchar
  return(reduction_ratio)
}
