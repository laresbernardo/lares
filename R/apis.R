####################################################################
#' Get API (JSON) and Transform into data.frame
#'
#' This function lets the user bring API data as JSON format and
#' transform it into data.frame.
#'
#' @family Tools
#' @family API
#' @param url Character. API's URL to GET.
#' @param status Boolean. Display status message?
#' @return data.frame of \code{url} \code{GET} results or
#' \code{NULL} if no results returned by API.
#' @export
bring_api <- function(url, status = TRUE) {
  get <- GET(url = url)
  if (status) {
    message(paste("Status: ", ifelse(get$status_code == 200, "OK", "ERROR")))
  }
  char <- rawToChar(get$content)
  json <- fromJSON(char)

  if (length(json[[1]]) > 0) {
    import <- data.frame(json)
    import <- flatten(import)
    import <- data.frame(bind_rows(lapply(import, unlist(as.character))))
    import[import == "list()"] <- NA
    import[import == "integer(0)"] <- 0
    colnames(import) <- gsub("\\.", "_", colnames(import))
    import <- suppressMessages(type.convert(import, numerals = "no.loss", as.is = TRUE))
    import
  } else {
    invisible(NULL)
  }
}
