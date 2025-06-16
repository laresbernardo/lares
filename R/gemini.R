####################################################################
#' Gemini API Interaction with R
#'
#' This function lets the user interact with Google's Gemini LLM Model using
#' its API, and returns the rendered reply.
#'
#' @family API
#' @family Gemini
#' @family LLM
#' @inheritParams gpt_ask
#' @inheritParams cache_write
#' @return (Invisible) list. Content returned from API POST and processed.
#' @examples
#' \dontrun{
#' api_key <- get_credentials()$gemini$api_key
#' # Open question:
#' gemini_ask("Can you write an R function to plot a dummy histogram?", api_key)
#' # Image question
#' image <- "man/figures/automl_map.png"
#' gemini_image("Can you explain this flow with more detail?", image, api_key)
#' }
#' @export
gemini_ask <- function(ask,
                       secret_key = get_creds("gemini")$api_key,
                       url = Sys.getenv("LARES_GEMINI_API"),
                       temperature = 0.5, max_tokens = 1024,
                       quiet = FALSE, ...) {
  model_query <- "gemini-pro:generateContent"
  response <- POST(
    url = paste0(url, model_query),
    query = list(key = secret_key),
    httr::content_type_json(), encode = "json", body = list(
      contents = list(parts = list(list(text = ask))),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_tokens
      )
    )
  )
  this <- content(response)
  if ("error" %in% names(this)) {
    message(this$error$message)
  } else {
    candidates <- this$candidates
    if (!quiet) cat(unlist(lapply(candidates, function(candidate) candidate$content$parts)))
  }
  invisible(this)
}


#' @param image Character. Data to be encoded/decoded. It can be a raw vector,
#' text connection or file name.
#' @rdname gemini_ask
#' @export
gemini_image <- function(ask, image,
                         secret_key = get_creds("gemini")$api_key,
                         url = Sys.getenv("LARES_GEMINI_API"),
                         temperature = 0.5, max_tokens = 1024,
                         quiet = FALSE, ...) {
  try_require("base64enc")
  model_query <- "gemini-pro-vision:generateContent"
  response <- POST(
    url = paste0(url, model_query),
    query = list(key = secret_key),
    httr::content_type_json(), encode = "json", body = list(
      contents = list(parts = list(list(text = ask), list(inline_data = list(
        mime_type = "image/png", data = base64encode(image)
      )))),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_tokens
      )
    )
  )
  this <- content(response)
  if ("error" %in% names(this)) {
    message(this$error$message)
  } else {
    candidates <- this$candidates
    if (!quiet) cat(unlist(lapply(candidates, function(candidate) candidate$content$parts)))
  }
  invisible(this)
}
