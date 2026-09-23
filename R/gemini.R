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
#' @param model Character. Gemini model to use (e.g., "gemini-3.5-flash-lite",
#' "gemini-2.5-flash", "gemini-2.5-pro"). Defaults to the
#' \code{LARES_GEMINI_MODEL} environment variable.
#' @return (Invisible) list. Content returned from API POST and processed.
#' @examples
#' \dontrun{
#' api_key <- get_credentials()$gemini$api_key
#' # Open question:
#' gemini_ask("Can you write an R function to plot a dummy histogram?", api_key)
#' # Image question
#' image <- "man/figures/automl_map.png"
#' gemini_image("Can you explain this flow with more detail?", image, api_key)
#' # Use a specific model
#' gemini_ask("Explain R's pipe operator", model = "gemini-3.5-flash-lite")
#' }
#' @export
gemini_ask <- function(ask,
                       secret_key = get_creds("gemini")$api_key,
                       url = Sys.getenv("LARES_GEMINI_API"),
                       model = Sys.getenv("LARES_GEMINI_MODEL"),
                       temperature = 0.5, max_tokens = 1024,
                       quiet = FALSE, ...) {
  if (is.null(model) || model == "") model <- "gemini-3.5-flash-lite"
  model_query <- paste0(model, ":generateContent")
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
    if (!quiet) {
      texts <- unlist(lapply(candidates, function(cand) {
        vapply(cand$content$parts, function(p) {
          if (!is.null(p$text)) p$text else ""
        }, FUN.VALUE = character(1))
      }))
      cat(paste(texts, collapse = ""))
    }
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
                         model = Sys.getenv("LARES_GEMINI_MODEL"),
                         temperature = 0.5, max_tokens = 1024,
                         quiet = FALSE, ...) {
  try_require("base64enc")
  if (is.null(model) || model == "") model <- "gemini-3.5-flash-lite"
  model_query <- paste0(model, ":generateContent")
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
    if (!quiet) {
      texts <- unlist(lapply(candidates, function(cand) {
        vapply(cand$content$parts, function(p) {
          if (!is.null(p$text)) p$text else ""
        }, FUN.VALUE = character(1))
      }))
      cat(paste(texts, collapse = ""))
    }
  }
  invisible(this)
}
