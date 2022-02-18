####################################################################
#' Google Sheets Reading and Writing (API v4)
#'
#' Read and write data from Google Sheets knowing the file's title.
#' You may use a single value from a cell or a data.frame from a cell range.
#'
#' @family Scrapper
#' @family Google
#' @param title Character. Title of Google Drive file. Uses regular
#' expressions so you may fetch with patterns instead of names.
#' @param sheet Character. Working sheet to import
#' @param range Character. A cell range to read from
#' @param drop_nas Boolean. Remove columns and rows that contain only NAs?
#' @param json Character. JSON filename with service auth
#' @param email,api_key Character. If you have multiple pre-authorized
#' accounts in your machine, you may non-interactively select
#' which one you wish to use by email and/or api_key.
#' @param server Boolean. Force interacting auth process?
#' @param ... Additional parameters passed to \code{read_sheet()}.
#' @return For reading, data.frame with the results of your Google Sheets file
#' based on its \code{title}, specificially the \code{sheet} and \code{range}
#' requested. For writing, no return value.
#' @aliases readGS4
#' @export
#' @rdname google_sheets
readGS <- function(title, sheet = "Hoja 1", range = NULL, drop_nas = TRUE,
                   json = NULL, email = NULL, api_key = NULL, server = FALSE, ...) {
  files <- filesGD(title = title, server = server, json = json, email = email)
  if (nrow(files) > 0) {
    message(sprintf("Using: %s (%s)", files$name[1], files$id[1]))
    df <- read_sheet(files$id[1], sheet = sheet, range = range, ...)
    if (drop_nas & isTRUE(ncol(df) > 0) & isTRUE(nrow(df) > 0)) {
      df <- df %>%
        removenacols() %>%
        removenarows()
    }
    if (length(df) > 0 & nrow(df) == 0) {
      df <- names(df)
    }
    return(df)
  }
}

#' @param data Object (value, vector, data.frame, list).
#' @param reformat Boolean. Reformat the affected cells?
#' @param append Boolean.
#' @aliases writeGS4
#' @export
#' @rdname google_sheets
writeGS <- function(data, title, sheet = "Hoja 1", range = "A1",
                    reformat = FALSE, append = FALSE,
                    json = NULL, email = NULL, api_key = NULL, server = FALSE, ...) {
  files <- filesGD(title = title, server = server, json = json, email = email)

  if (nrow(files) > 0) {
    if (is.vector(data) & !is.list(data)) {
      data <- data.frame(data)
      col_names <- FALSE
    } else {
      col_names <- TRUE
    }

    if (nrow(files) == 0) {
      message("Google Sheet filename not found: created one for you!")
      gs4_create(title, sheets = data, col_names = col_names)
      return(invisible(NULL))
    }

    message(sprintf("Using: %s (%s)", files$name[1], files$id[1]))

    if (append) {
      invisible(
        sheet_append(files$id[1], sheet = sheet, data = data)
      )
    } else {
      invisible(
        range_write(files$id[1],
          sheet = sheet, data = data,
          range = range, reformat = reformat, col_names = col_names, ...
        )
      )
    }
  }
}


####################################################################
#' Google Drive Files (API v4)
#'
#' Authenticate and find Google Drive files and IDs by name.
#'
#' @family Scrapper
#' @family Google
#' @inheritParams readGS
#' @return Vector with found file names based on \code{title} on Google Drive.
#' @export
filesGD <- function(title, server = FALSE, json = NULL, api_key = NULL, email = NULL) {
  try_require("googledrive")
  try_require("googlesheets4")
  options("gargle_oob_default" = server)

  if (!is.null(json)) {
    if (file.exists(json)) {
      drive_auth(path = json)
      gs4_auth(path = json)
    } else {
      stop("No credentials found on ", json)
    }
  } else {
    if (!is.null(api_key)) {
      gs4_auth_configure(api_key = api_key)
    }
    if (!is.null(email)) {
      drive_auth(email = email)
      gs4_auth(email = email)
    }
  }

  local_drive_quiet()
  files <- drive_find(
    pattern = title,
    n_max = 199,
    type = "spreadsheet"
  )

  nfiles <- nrow(files)
  if (nfiles != 1) message(glued("{nfiles} files found with pattern: {title}"))
  return(files)
}
