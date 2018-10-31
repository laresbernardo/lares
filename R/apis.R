####################################################################
#' Get API (JSON) and Transform into data.frame
#' 
#' This function lets the user bring API data as JSON format and transform it 
#' into data.frame. Designed initially for Hubspot but may work on other API
#' 
#' @param url Character. API's URL
#' @export
bring_api <- function(url) {

  require(httr)
  require(jsonlite)
  require(rlist)

  get <- httr::GET(url = url)
  message(paste0("Status: ", ifelse(get$status_code == 200, "OK", "ERROR")))
  char <- rawToChar(get$content)
  import <- data.frame(jsonlite::fromJSON(char))
  import <- jsonlite::flatten(import)
  import <- data.frame(rlist::list.cbind(lapply(import, unlist(as.character))))

  # Further transformations
  import[import == "list()"] <- NA
  import[import == "integer(0)"] <- 0
  colnames(import) <- gsub("\\.", "_", colnames(import))

  return(import)

}
