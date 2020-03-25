####################################################################
#' Google Sheets Reading
#' 
#' This function lets the user read any Google Sheet's data
#' 
#' @family Connection
#' @family Scrapper
#' @param title Character. Textual title of Google Sheet
#' @param ws Character. Working sheet to import
#' @param first_time Boolean. Authenticate manualy
#' @export
readGS <- function(title, ws = "Hoja 1", first_time = FALSE) {
  try_require("googlesheets")
  if (first_time) {
    options(httr_oob_default = TRUE)
    gs_auth(new_user = TRUE)
  }
  gs <- gs_title(title)
  gs <- invisible(gs_read(gs, ws = ws, verbose = FALSE) %>% data.frame())
  return(gs)
}

####################################################################
#' Google Sheets Writing
#' 
#' This function lets the user write on any Google Sheet
#' 
#' @family Connection
#' @family Scrapper
#' @param data Data Frame. Data to export to Google Sheet 
#' @param title Character. Textual title of Google Sheet
#' @param ws Character. Working sheet to export to
#' @param cell Character. In which cell should we paste the data (upper left cell)
#' @param first_time Boolean. Authenticate manualy
#' @export
writeGS <- function(data, title, ws = "Hoja 1", cell = 'A1', first_time = FALSE) {
  try_require("googlesheets")
  if (first_time) {
    options(httr_oob_default = TRUE)
    gs_auth(new_user = TRUE)
  }
  gs <- gs_title(title)
  invisible(gs_edit_cells(
    gs, ws = ws, input = data, anchor = cell, verbose = FALSE) %>% 
      data.frame())
}


####################################################################
#' Google Sheets Reading (API v4)
#' 
#' This function lets the user read any Google Sheet's data
#' 
#' @family Connection
#' @family Scrapper
#' @param title Character. Textual title of Google Sheet
#' @param sheet Character. Working sheet to import
#' @param range Character. A cell range to read from
#' @param creds Character. JSON filename with service auth
#' @param ... Further read_sheet parameters
#' @export
readGS4 <- function(title, sheet = "Hoja 1", range = NULL, creds = NULL, ...) {
  
  try_require("googledrive")
  try_require("googlesheets4")
  
  if (!is.null(creds)) {
    if (file.exists(creds)) {
      sheets_auth(path = creds)
      drive_auth(path = creds)  
    } else stop("No credentials found on ", creds)
  }
  aux <- drive_find(pattern = title, n_max = 199, 
                    type = "spreadsheet", verbose = FALSE)
  message(paste(nrow(aux), "files found with pattern:", title))
  if (nrow(aux) > 0) {
    if (nrow(aux) > 1)
      message(sprintf("Using: %s (%s)", aux$name[1], aux$id[1]))
    df <- read_sheet(aux$id[1], sheet = sheet, range = range, ...)   
    return(df)
  } else return(invisible(NULL))
}
