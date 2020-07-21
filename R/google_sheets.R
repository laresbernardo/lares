####################################################################
#' Google Sheets Reading (API v4)
#' 
#' Read data from a Google Sheets.
#' 
#' @family Scrapper
#' @family Google
#' @param title Character. Textual title of Google Sheet
#' @param sheet Character. Working sheet to import
#' @param range Character. A cell range to read from
#' @param creds Character. JSON filename with service auth
#' @param server Boolean. Force interacting auth process?
#' @param ... Further read_sheet parameters
#' @aliases readGS4
#' @export
readGS <- function(title, sheet = "Hoja 1", range = NULL, 
                   creds = NULL, server = FALSE, ...) {
  files <- get_drive_files(title, server, creds)
  if (nrow(files) > 0) {
    message(sprintf("Using: %s (%s)", files$name[1], files$id[1]))
    df <- read_sheet(files$id[1], sheet = sheet, range = range, ...)   
    return(df)
  }
}

####################################################################
#' Google Sheets Writing (API v4)
#' 
#' Write data into Google Sheets.
#' 
#' @family Scrapper
#' @family Google
#' @inheritParams readGS
#' @param data Object (value, vector, dataframe, list)
#' @param reformat Boolean. Reformat the affected cells?
#' @aliases writeGS4
#' @export
writeGS <- function(data, title, sheet = "Hoja 1", range = 'A1', reformat = FALSE,
                    creds = NULL, server = FALSE, ...) {
  files <- get_drive_files(title, server, creds)
  if (nrow(files) > 0) {
    if (nrow(files) == 0) {
      message("Google Sheet filename not found: created one for you!")
      gs4_create(title, sheets = data)
    }
    message(sprintf("Using: %s (%s)", files$name[1], files$id[1]))
    invisible(
      range_write(files$id[1], sheet = sheet, data = data, 
                  range = range, reformat = reformat, ...))
  }
}

get_drive_files <- function(title, server, creds, api_key = NULL) {
  
  try_require("googledrive")
  try_require("googlesheets4")
  options(gargle_oob_default = server)
  
  if (!is.null(api_key))
    gs4_auth_configure(api_key = api_key)
  
  if (!is.null(creds)) {
    if (file.exists(creds)) {
      sheets_auth(path = creds)
      drive_auth(path = creds)  
    } else {
      stop("No credentials found on ", creds)
    } 
  }
  
  files <- drive_find(pattern = title,
                      n_max = 199, 
                      type = "spreadsheet", 
                      verbose = FALSE)
  
  if (nrow(files) > 1) {
    message(paste(nrow(files), "files found with pattern:", title))
  }
  
  return(files)
  
}