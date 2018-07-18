# Fast read and write Google Sheets functions

# Read in a Google Sheet
readGS = function(title, ws, server=FALSE) {
  suppressMessages(require(googlesheets))
  suppressMessages(require(dplyr))
  
  if (server==TRUE) {
    options(httr_oob_default = TRUE)
    gs_auth(new_user = TRUE)
  }
  
  gs <- gs_title(title)
  gs <- gs_read(gs, ws = ws, verbose=F) %>% data.frame()

  return(gs)
  
}

# Write in a Google Sheet
writeGS = function(data, title, ws, cell='A1', server=FALSE) {

  suppressMessages(require(googlesheets))
  suppressMessages(require(dplyr))

  if (server==TRUE) {
    options(httr_oob_default = TRUE)
    gs_auth(new_user = TRUE)
  }

  gs <- gs_title(title)
  gs_edit_cells(gs, ws = ws, input = data, anchor = cell, verbose=F) %>% data.frame()

}
