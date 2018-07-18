# Bring API data as JSON and transform it to data.frame
# Designed for Hubspot but may work on other API

bring_api <- function(url) {

  suppressMessages(require(httr))
  suppressMessages(require(jsonlite))
  suppressMessages(require(rlist))

  get <- GET(url = url)
  message(paste0("Status: ", ifelse(get$status_code == 200, "OK", "ERROR")))
  char <- rawToChar(get$content)
  import <- data.frame(fromJSON(char))
  import <- jsonlite::flatten(import)
  data.frame(rlist::list.cbind(lapply(import, unlist(as.character))))

  # Further transformations
  import[import == "list()"] <- NA
  import[import == "integer(0)"] <- 0
  colnames(import) <- gsub("\\.", "_", colnames(import))

  return(import)

}
