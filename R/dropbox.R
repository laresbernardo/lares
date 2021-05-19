####################################################################
#' Download Dropbox File by File's Name
#' 
#' This function lets the user download a file from Dropbox, specifying
#' its name and using a previously created token.
#' 
#' @family Tools
#' @family Credentials
#' @family Dropbox
#' @param filename String. File's name
#' @param xlsx Boolean. Is it an Excel file? Can be returned as a list
#' for each tab and not as a file if needed
#' @param newname Character. Name for new file
#' @param token_dir Character. RDS with token local directory. You may set to
#' NA if you already set your credentials (see \code{get_creds()})
#' @param token_name Character. RDS file name with your token's data
#' @export
db_download <- function(filename, 
                        xlsx = TRUE, 
                        newname = "temp.xlsx",
                        token_dir = NA, 
                        token_name = "token_pers.rds"){
  
  try_require("rdrop2")
  
  if (is.na(token_dir)) {
    load(paste0(Sys.getenv("LARES_CREDS"), "/", token_name))
  } else {
    token_file <- paste0(token_dir,"/", token_name)
    if (file.exists(token_dir)) {
      load(token_file)
    } else {
      token <- drop_auth() 
    }
  }
  
  x <- drop_search(filename, dtoken = token)
  
  invisible(
    drop_download(x$matches[[1]]$metadata$path_lower, 
                  local_path = newname, 
                  overwrite = TRUE, 
                  dtoken = token))
  
  if (xlsx) {
    results <- importxlsx(newname) 
    invisible(file.remove(newname))
    return(results)
  } else {
    message("> File downloaded succesfully!")
    return(invisible(NULL))
  }
}

####################################################################
#' Upload Dropbox File
#' 
#' This function lets the user download a file from Dropbox, specifying
#' its name and using a previously created token. 
#' 
#' @family Tools
#' @family Dropbox
#' @param filename String. File's name
#' @param dir String. Directory you wish to upload the file to
#' @param delete_file Boolean. Delete local file after uploading?
#' @param token_dir Character. Credential's local directory
#' @param token_name Character. RDS file name with your token's data
#' @export
db_upload <- function(filename, dir, delete_file = FALSE, token_dir = NA,
                      token_name = "token_pers.rds") {
  
  try_require("rdrop2")
  
  if (is.na(token_dir)) {
    load(paste0(Sys.getenv("LARES_CREDS"), "/", token_name))
  } else {
    token_file <- paste0(token_dir,"/", token_name)
    if (file.exists(token_dir)) {
      load(token_file)
    } else {
      token <- drop_auth() 
    }
  }
  
  invisible(drop_upload(filename, path = dir, dtoken = token))
  
  if (delete_file)
    file.remove(filename)
  
}
