####################################################################
#' ChatGPT API Interaction with R
#'
#' This function lets the user ask ChatGPT via its API, and returns
#' the rendered reply.
#'
#' @family API
#' @inheritParams db_download
#' @param ask Character. Redacted prompt to ask ChatGPT.
#' @param secret_key Character. Secret Key. Get yours in:
#' \href{https://platform.openai.com}{platform.openai.com}.
#' @param url Character. Base URL for OpenAI's ChatGPT API.
#' @param model Character. OpenAI model to use.
#' @return (Invisible) list. Content returned from API POST.
#' @examples
#' \dontrun{
#' api_key <- get_credentials()$openai$secret_key
#' chatgpt_ask("Can you write an R function to plot a dummy histogram?", api_key)
#' }
#' @export
chatgpt_ask <- function(ask,
                        secret_key = get_credentials()$openai$secret_key,
                        url = "https://api.openai.com/v1/chat/completions",
                        model = "gpt-3.5-turbo",
                        quiet = FALSE) {
  response <- POST(
    url = url, 
    add_headers(Authorization = paste("Bearer", secret_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = model,
      messages = list(list(
        role = "user", 
        content = ask
      ))
    )
  )
  ret <- content(response)
  if ("error" %in% names(ret)) warning(ret$error$message)
  if ("message" %in% names(ret$choices[[1]]) & !quiet)
    cat(stringr::str_trim(ret$choices[[1]]$message$content))
  return(invisible(ret))
}
