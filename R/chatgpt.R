####################################################################
#' ChatGTP API Interaction
#'
#' This function lets the user ask ChatGPT via its API, and returns
#' the rendered reply.
#'
#' @family API
#' @inheritParams db_download
#' @param ask Character. Redacted prompt to ask ChatGPT.
#' @param secret_key Character. Secret Key. Get yours in: platform.openai.com.
#' @param url Character. Base URL for OpenAI's ChatGPT API.
#' @return List. Content returned from API POST.
#' @examples
#' \donttest{
#' api_key <- lares::get_credentials()$openai$secret_key
#' chatgpt_ask("Can you write a dummy R function to plot a histogram in R?", api_key)
#' }
#' @export
chatgpt_ask <- function(ask,
                        secret_key = get_credentials()$openai$secret_key,
                        url = "https://api.openai.com/v1/chat/completions",
                        quiet = FALSE) {
  response <- POST(
    url = url, 
    add_headers(Authorization = paste("Bearer", secret_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user", 
        content = ask
      ))
    )
  )
  ret <- content(response)
  if ("message" %in% names(ret$choices[[1]]) & !quiet)
    cat(stringr::str_trim(ret$choices[[1]]$message$content))
  return(invisible(ret))
}
