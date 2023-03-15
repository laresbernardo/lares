####################################################################
#' ChatGPT API Interaction with R
#'
#' This function lets the user ask ChatGPT via its API, and returns
#' the rendered reply.
#'
#' @family API
#' @inheritParams db_download
#' @param ask Character. Redacted prompt to ask ChatGPT. If multiple asks are
#' requested, they will be concatenated with "+" into a single request.
#' @param secret_key Character. Secret Key. Get yours in:
#' \href{https://platform.openai.com}{platform.openai.com}.
#' @param url Character. Base URL for OpenAI's ChatGPT API.
#' @param model Character. OpenAI model to use.
#' @param ... Additional parameters.
#' @return (Invisible) list. Content returned from API POST and processed.
#' @examples
#' \dontrun{
#' api_key <- get_credentials()$openai$secret_key
#' # Open question:
#' gpt_ask("Can you write an R function to plot a dummy histogram?", api_key)
#' 
#' ##### The following examples return dataframes:
#' # Classify each element based on categories:
#' gpt_classify(1:10, c("odd", "even"))
#'
#' # Add all tags that apply to each element based on tags:
#' gpt_tag(
#'   c("I love chocolate", "I hate chocolate", "I like Coke"),
#'   c("food", "positive", "negative", "beverage"))
#'
#' # Extract specific information:
#' gpt_extract(
#'   c("My mail is 123@@test.com", "30 Main Street, Brooklyn, NY, USA", "+82 2-312-3456", "$1.5M"),
#'   c("email", "full state name", "country of phone number", "amount as number"))
#' 
#' # Format values
#' gpt_format(
#'   c("March 27th, 2021", "12-25-2023 3:45PM", "01.01.2000", "29 Feb 92"),
#'   format = "ISO Date getting rid of timestamps")
#'   
#' # Convert units
#' gpt_convert(c("50C", "300K"), "Fahrenheit")
#'   
#' # Create a table with data
#' gpt_table("5 random people's address in South America, email, phone, age between 18-30")
#' 
#' # Translate text to any language
#' gpt_translate(
#'   rep("I love you with all my heart", 5),
#'   language = c("spanish", "chinese", "japanese", "russian", "german"))
#' }
#' @export
gpt_ask <- function(ask,
                        secret_key = get_credentials()$openai$secret_key,
                        url = "https://api.openai.com/v1/chat/completions",
                        model = "gpt-3.5-turbo",
                        quiet = FALSE, ...) {
  if (length(ask) > 1) ask <- paste(ask, collapse = " + ")
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

#' @param x Vector. List items you wish to process
#' @param categories,tags Vector. List of possible categories/tags to consider.
#' @rdname gpt_ask
#' @export
gpt_classify <- function(x, categories, quiet = TRUE, ...) {
  prompt <- gpt_prompt_builder("category", x = x, y = categories)
  resp <- gpt_ask(prompt, quiet = quiet, ...)
  df <- gpt_markdown2df(resp)
  return(df)
}

#' @rdname gpt_ask
#' @export
gpt_tag <- function(x, tags, quiet = TRUE, ...) {
  prompt <- gpt_prompt_builder("tags", x = x, y = tags)
  resp <- gpt_ask(prompt, quiet = quiet, ...)
  df <- gpt_markdown2df(resp)
  return(df)
}

#' @param extract,format,unit Character. Length 1 or same as x to extract/format/unit
#' information from x. For example: email, country of phone number, country, amount as number,
#' currency ISO code, ISO, Fahrenheit, etc.
#' @rdname gpt_ask
#' @export
gpt_extract <- function(x, extract, quiet = TRUE, ...) {
  stopifnot(length(extract) %in% c(1, length(x)))
  prompt <- gpt_prompt_builder("extract", x = x, y = extract, cols = c("item", "extract", "value"))
  resp <- gpt_ask(prompt, quiet = quiet, ...)
  df <- gpt_markdown2df(resp)
  return(df)
}

#' @rdname gpt_ask
#' @export
gpt_format <- function(x, format, quiet = TRUE, ...) {
  stopifnot(length(format) %in% c(1, length(x)))
  prompt <- gpt_prompt_builder("format", x = x, y = format)
  resp <- gpt_ask(prompt, quiet = quiet, ...)
  df <- gpt_markdown2df(resp)
  return(df)
}

#' @rdname gpt_ask
#' @export
gpt_convert <- function(x, unit, quiet = TRUE, ...) {
  stopifnot(length(unit) %in% c(1, length(x)))
  prompt <- gpt_prompt_builder("value", x = x, y = unit)
  resp <- gpt_ask(prompt, quiet = quiet, ...)
  df <- gpt_markdown2df(resp)
  return(df)
}

#' @rdname gpt_ask
#' @export
gpt_table <- function(ask, quiet = TRUE, ...) {
  prompt <- paste("Return a markdown table for:", ask, ". If you don't know any item, replace with NA")
  resp <- gpt_ask(prompt, quiet = quiet, ...)
  df <- gpt_markdown2df(resp)
  return(df)
}

#' @param language Character. Language to translate to
#' @rdname gpt_ask
#' @export
gpt_translate <- function(x, language, quiet = TRUE, ...) {
  stopifnot(length(language) %in% c(1, length(x)))
  prompt <- gpt_prompt_builder("translate", x = x, y = language, cols = c("item", "language", "translation"))
  resp <- gpt_ask(prompt, quiet = quiet, ...)
  df <- gpt_markdown2df(resp)
  return(df)
}
 
gpt_prompt_builder <- function(type = "category", cols = c("item", type), x, y) {
  paste(
    "Return a structured markdown table with", length(cols), "columns named", v2t(cols, and = "and"),
    ". Consider the following items:", v2t(x, quotes = FALSE),
    ". For each respective item, what", type, "represent each item using:", v2t(y, quotes = FALSE),
    ". If you don't know any item, replace with NA")
}

gpt_markdown2df <- function(resp) {
  if ("message" %in% names(resp$choices[[1]])) {
    df <- resp$choices[[1]]$message$content
    # Convert markdown to data.frame
    df <- removenacols(read.table(text = df, sep = "|", header = TRUE, strip.white = TRUE, quote="\""))
    # Get rid of potential first row with all values set as --- or :---
    if (all(stringr::str_split(df[1, 1], "-")[[1]] == "")) df <- df[-1, ]
    if (substr(df[1, 1], 1, 4) == ":---") df <- df[-1, ]
    rownames(df) <- NULL
    df <- as_tibble(df)
    attr(df, "response") <- resp
    return(df)
  } else return(resp)
}
