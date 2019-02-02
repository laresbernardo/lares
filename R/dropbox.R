####################################################################
#' Download Dropbox File by File's Name
#' 
#' This function lets the user download a file from Dropbox, specifying
#' its name and using a previously created token.
#' 
#' @param filename String. File's name
#' @param xlsx Boolean. Is it an Excel file? Can be returned as a list
#' for each tab and not as a file if needed
#' @param token_dir Character. Credential's local directory
#' @export
db_download <- function(filename, xlsx = TRUE, token_dir = NA){
  
  if (is.na(token_dir)) {
    load("~/Dropbox (Personal)/Documentos/Docs/Data/token_pers.rds")
  }
  if (token_dir == "matrix") {
    load("~/creds/token_pers.rds") 
  }
  if (is.na(token_dir) & token_dir != "matrix") {
    token <- drop_auth()
  }
  
  x <- drop_search(filename, dtoken = token)
  file <- "temp.xlsx"
  invisible(
    drop_download(x$matches[[1]]$metadata$path_lower, 
                  local_path = file, 
                  overwrite = TRUE, 
                  dtoken = token))
  
  if (xlsx) {
    results <- importxlsx(file) 
    invisible(file.remove(file))
    return(results)
  } else {
    return(message("File downloaded succesfully"))
  }
}

####################################################################
#' Download Dropbox File by File's Name
#' 
#' This function lets the user download a file from Dropbox, specifying
#' its name and using a previously created token.
#' 
#' @param filename String. File's name
#' @param dir String. Directory you wish to upload the file to
#' @param delete_file Boolean. Delete local file after uploading?
#' @param token_dir Character. Credential's local directory
#' @export
db_upload <- function (filename, dir, delete_file = FALSE, token_dir = NA) {
  
  if (is.na(token_dir)) {
    load("~/Dropbox (Personal)/Documentos/Docs/Data/token_pers.rds")
  }
  if (token_dir == "matrix") {
    load("~/creds/token_pers.rds") 
  }
  if (is.na(token_dir) & token_dir != "matrix") {
    token <- drop_auth()
  }
  
  drop_upload(filename, path = dir, dtoken = token)
  
  if (delete_file == TRUE) {
    file.remove(filename)
  }
}
