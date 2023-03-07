####################################################################
#' ChatGPT API Interaction with R
#'
#' This function lets the user ask ChatGPT via its API, and returns
#' the rendered reply.
#'
#' @family API
#' @inheritParams db_download
#' @param ask Character. Redacted prompt to ask ChatGPT. If multiple asks are requested,
#' they will be concatenated with "+" into a single request.
#' @param secret_key Character. Secret Key. Get yours in:
#' \href{https://platform.openai.com}{platform.openai.com}.
#' @param url Character. Base URL for OpenAI's ChatGPT API.
#' @param model Character. OpenAI model to use.
#' @param ... Additional parameters.
#' @return (Invisible) list. Content returned from API POST.
#' @examples
#' \dontrun{
#' api_key <- get_credentials()$openai$secret_key
#' # Open question:
#' chatgpt_ask("Can you write an R function to plot a dummy histogram?", api_key)
#' 
#' ##### The following examples return dataframes:
#' # Classify each element based on categories:
#' chatgpt_classify(1:10, c("odd", "even"))
#'
#' # Add all tags that apply to each element based on tags:
#' chatgpt_tag(
#'   c("I love chocolate", "I hate chocolate", "I like Coke"),
#'   c("food", "positive", "negative", "beverage"))
#'
#' # Extract specific information:
#' chatgpt_extract(
#'   c("My mail is 123@@test.com", "30 Main Street, Brooklyn, NY, USA", "+82 2-312-3456", "$1.5M"),
#'   c("email", "full state name", "country of phone number", "amount as number"))
#' 
#' # Format values
#' chatgpt_format(
#'   c("March 27th, 2021", "12-25-2023 3:45PM", "01.01.2000", "29 Feb 92"),
#'   format = "ISO Date getting rid of timestamps")
#'   
#' # Convert units
#' chatgpt_convert(c("50C", "300K"), "Fahrenheit")
#'   
#' # Create a table with data
#' chatgpt_table("5 random people's address in South America, email, phone, age between 18-30")
#' 
#' # Translate text to any language
#' chatgpt_translate(
#'   rep("I love you with all my heart", 5),
#'   language = c("spanish", "chinese", "japanese", "russian", "german"))
#' }
#' @export
chatgpt_ask <- function(ask,
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
#' @rdname chatgpt_ask
#' @export
chatgpt_classify <- function(x, categories, quiet = TRUE, ...) {
  prompt <- paste("Considering all of these items:", v2t(x),
                  ". What category is each one of them using these labels:", v2t(categories),
                  .split_prompt(c("item", "category")))
  df <- resp <- chatgpt_ask(prompt, quiet = quiet, ...)
  df <- .chatgpt_create_df(resp, c("item", "category"))
  return(df)
}

#' @rdname chatgpt_ask
#' @export
chatgpt_tag <- function(x, tags, quiet = TRUE, ...) {
  prompt <- paste("Considering all of these items:", v2t(x),
                  ". Which of these tags make sense for each item:", v2t(tags),
                  .split_prompt(c("item", "selected tags")))
  df <- resp <- chatgpt_ask(prompt, quiet = quiet, ...)
  df <- .chatgpt_create_df(resp, c("item", "tags"))
  return(df)
}

#' @param extract,format,unit Character. Length 1 or same as x to extract/format/unit
#' information from x. For example: email, country of phone number, country, amount as number,
#' currency ISO code, ISO, Fahrenheit, etc.
#' @rdname chatgpt_ask
#' @export
chatgpt_extract <- function(x, extract, quiet = TRUE, ...) {
  stopifnot(length(extract) %in% c(1, length(x)))
  prompt <- paste("Considering all of these items:", v2t(x),
                  ". Extract the information of its", v2t(extract), "respectively for each item.",
                  .split_prompt(c("item", "extract", "extracted values")))
  df <- resp <- chatgpt_ask(prompt, quiet = quiet, ...)
  df <- .chatgpt_create_df(resp, cols = c("item", "extract", "value"))
  return(df)
}

#' @rdname chatgpt_ask
#' @export
chatgpt_format <- function(x, format, quiet = TRUE, ...) {
  stopifnot(length(format) %in% c(1, length(x)))
  prompt <- paste("Considering all of these items:", v2t(x),
                  ". Return the original item and formatted values for each item as/with",
                  v2t(format), "format respectively.",
                  .split_prompt(c("item", "value")))
  df <- resp <- chatgpt_ask(prompt, quiet = quiet, ...)
  df <- .chatgpt_create_df(resp, cols = c("item", "value"))
  return(df)
}

#' @rdname chatgpt_ask
#' @export
chatgpt_convert <- function(x, unit, quiet = TRUE, ...) {
  stopifnot(length(unit) %in% c(1, length(x)))
  prompt <- paste("Considering all of these values with units:", v2t(x),
                  ". Convert each item to", v2t(unit), " unit respectively.",
                  .split_prompt(c("item", "value")))
  df <- resp <- chatgpt_ask(prompt, quiet = quiet, ...)
  df <- .chatgpt_create_df(resp, cols = c("item", "value"))
  return(df)
}

#' @rdname chatgpt_ask
#' @export
chatgpt_table <- function(ask, quiet = TRUE, ...) {
  prompt <- paste("Return a markdown table for:", ask)
  df <- resp <- chatgpt_ask(prompt, quiet = quiet, ...)
  if ("message" %in% names(resp$choices[[1]])) {
    df <- markdown2df(text = resp$choices[[1]]$message$content)
  }
  attr(df, "response") <- resp
  return(df)
}

#' @param language Character. Language to translate to
#' @rdname chatgpt_ask
#' @export
chatgpt_translate <- function(x, language, quiet = TRUE, ...) {
  stopifnot(length(language) %in% c(1, length(x)))
  prompt <- paste("Considering all these texts:", v2t(x),
                  ". Translate them to", v2t(language), "respectively for each text item.",
                  "Return a mardown table with original text, translation, and language")
  df <- resp <- chatgpt_ask(prompt, quiet = quiet, ...)
  if ("message" %in% names(resp$choices[[1]])) {
    df <- markdown2df(resp$choices[[1]]$message$content) 
    colnames(df) <- c("item", "translation", "language")
  }
  return(df)
}

.chatgpt_create_df <- function(resp, cols = c("item", "value")) {
  if ("message" %in% names(resp$choices[[1]])) {
    splits <- stringr::str_split(resp$choices[[1]]$message$content, "\n")[[1]]
    splits_clean <- gsub("\n|\'", "", splits[grepl("==", splits)])
    df <- tidyr::separate(data.frame(values = splits_clean), .data$values, cols, sep = "==")
    df <- removenacols(df)
    df <- removenarows(df)
    attr(df, "response") <- resp
  }
  return(df)
}

markdown2df <- function(text) {
  try({
    df <- removenacols(read.table(text = text, sep = "|", header = TRUE, strip.white = TRUE))
    # Get rid of potential first row with all values set as ---
    if (all(stringr::str_split(df[1, 1], "-")[[1]] == "")) df <- df[-1, ]
    rownames(df) <- NULL
    return(as_tibble(df))
  })
}

.split_prompt <- function(cols = c("item", "value")) {
  paste("Do separate the information for", v2t(cols, and = "and"), "with '=='")
}
