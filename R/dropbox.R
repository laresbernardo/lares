####################################################################
#' Download/Import Dropbox File by File's Name
#' 
#' This function lets the user download a file from Dropbox, specifying
#' its name, using a previously created token or with interactive window.
#' 
#' @family Tools
#' @family Credentials
#' @family Dropbox
#' @param query Search string. This string is split (on spaces) into
#' individual words. Files will be used if they contain
#' all words in the search string.
#' @param local_path Character. Path to save file to. If \code{NULL}
#' (the default), saves file to working directory with same name.
#' If not, but a valid folder, file will be saved in this folder with
#' same basename as path. If not \code{NULL} and not a folder,
#' file will be saved to this path exactly.
#' @param xlsx Boolean. Is it an Excel file? Can be returned as a list
#' for each tab and not as a file if needed. Will delete downloaded file.
#' @param token_dir Character. RDS with token local directory. You may set to
#' NA if you already set your credentials (see \code{get_creds()})
#' @param token_name Character. RDS file name with your token's data.
#' @param quiet Boolean. Keep quiet? If not, show informative messages.
#' @return If \code{query} returns a .xlsx file and \code{xlsx=TRUE}, will
#' return a data.frame. Else, \code{local_path} string.
#' @examples
#' \dontrun{
#' # Download a specific file
#' db_download("stocksReport.Rmd", local_path = "~/Desktop/generic.Rmd")
#' # Import an Excel file from Dropbox into a data.frame
#' df <- db_download("Portfolio LC.xlsx", xlsx = FALSE)
#' }
#' @export
db_download <- function(query, 
                        local_path = NULL,
                        xlsx = TRUE,
                        token_dir = NA,
                        token_name = "token_pers.rds",
                        quiet = FALSE){
  
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
  
  x <- drop_search(query, dtoken = token)
  if (length(x$matches) == 0)
    stop("There were no files found in your Dropbox with: ", query)
  if (length(x$matches) > 1 & !quiet) {
    warning("There were multiple files found in your Dropbox with: ", query, 
            "\nUsing: ", x$matches[[1]]$metadata$name)
  }
  
  if (is.null(local_path))
    local_path <- x$matches[[1]]$metadata$name
  
  invisible(
    drop_download(x$matches[[1]]$metadata$path_lower, 
                  local_path = local_path,
                  overwrite = TRUE, 
                  dtoken = token))
  if (!quiet) message(paste("> File", x$matches[[1]]$metadata$name, "downloaded succesfully!"))
  
  if (xlsx & right(local_path, 5) == ".xlsx") {
    results <- importxlsx(local_path) 
    invisible(file.remove(local_path))
    if (!quiet) message("> File imported succesfully as an object!")
    return(results)
  } else {
    return(invisible(local_path))
  }
}

####################################################################
#' Upload Local Files to Dropbox
#' 
#' This function lets the user upload a local file to Dropbox,
#' using a previously created token or with interactive window.
#' 
#' @family Tools
#' @family Credentials
#' @family Dropbox
#' @inheritParams db_download
#' @param filename String. Local file's name to upload.
#' @param dir String. Directory you wish to upload the file to.
#' @param delete_file Boolean. Delete local file after uploading?
#' @return \code{TRUE} when successfully uploads file.
#' @export
db_upload <- function(filename, dir, delete_file = FALSE,
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
  
  invisible(drop_upload(filename, path = dir, dtoken = token))
  
  if (delete_file) file.remove(filename)
  return(invisible(TRUE))
  
}
