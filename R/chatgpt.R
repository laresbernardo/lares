hist_ask <- "GPT_HIST_ASK"
hist_reply <- "GPT_HIST_REPLY"

####################################################################
#' ChatGPT API Interaction with R
#'
#' This function lets the user ask ChatGPT via its API, and returns
#' the rendered reply. There are a couple of specific verbs (functions) with a
#' preset prompt to help fetch the data in specific formats. We also
#' store the prompts and replies in current session with their respective
#' time-stamps so user can gather historical results.
#'
#' @family API
#' @inheritParams db_download
#' @param ask Character. Redacted prompt to ask ChatGPT. If multiple asks are
#' requested, they will be concatenated with "+" into a single request.
#' @param secret_key Character. Secret Key. Get yours in:
#' \href{https://platform.openai.com}{platform.openai.com}.
#' @param url Character. Base URL for OpenAI's ChatGPT API.
#' @param model Character. OpenAI model to use. This can be adjusted
#' according to the available models in the OpenAI API (such as "gpt-4").
#' @param temperature Numeric. The temperature to use for generating
#' the response. Default is 0.5. The lower the \code{temperature},
#' the more deterministic the results in the sense that the highest probable
#' next token is always picked. Increasing temperature could lead to more
#' randomness, which encourages more diverse or creative outputs. You are
#' essentially increasing the weights of the other possible tokens.
#' In terms of application, you might want to use a lower temperature value
#' for tasks like fact-based QA to encourage more factual and concise responses.
#' For poem generation or other creative tasks, it might be beneficial to
#' increase the temperature value.
#' @param max_tokens Integer. The maximum number of tokens in the response.
#' @param num_retries Integer. Number of times to retry the request in
#' case of failure. Default is 3.
#' @param pause_base Numeric. The number of seconds to wait between retries.
#' Default is 1.
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
#'   c("food", "positive", "negative", "beverage")
#' )
#'
#' # Extract specific information:
#' gpt_extract(
#'   c("My mail is 123@@test.com", "30 Main Street, Brooklyn, NY, USA", "+82 2-312-3456", "$1.5M"),
#'   c("email", "full state name", "country of phone number", "amount as number")
#' )
#'
#' # Format values
#' gpt_format(
#'   c("March 27th, 2021", "12-25-2023 3:45PM", "01.01.2000", "29 Feb 92"),
#'   format = "ISO Date getting rid of time stamps"
#' )
#'
#' # Convert temperature units
#' gpt_convert(c("50C", "300K"), "Fahrenheit")
#'
#' # Create a table with data
#' gpt_table("5 random people's address in South America, email, phone, age between 18-30")
#' gpt_table(ask = "5 largest cities, their countries, and population",
#'           colnames = c("city_name", "where", "POP"))
#'
#' # Translate text to any language
#' gpt_translate(
#'   rep("I love you with all my heart", 5),
#'   language = c("spanish", "chinese", "japanese", "russian", "german")
#' )
#'
#' # Now let's read the historical prompts and replies from current session
#' gpt_history()
#' }
#' @export
gpt_ask <- function(ask,
                    secret_key = get_credentials()$openai$secret_key,
                    url = Sys.getenv("LARES_GPT_URL"),
                    model = Sys.getenv("LARES_GPT_MODEL"),
                    num_retries = 3,
                    temperature = 0.5,
                    max_tokens = NULL,
                    pause_base = 1,
                    quiet = FALSE, ...) {
  ts <- Sys.time()
  if (length(ask) > 1) ask <- paste(ask, collapse = " + ")
  # Save historical questions
  if (cache_exists(hist_ask)) {
    cache <- cache_read(hist_ask, quiet = TRUE, ...)
    cache <- rbind(cache, data.frame(ts = ts, prompt = ask))
  } else {
    cache <- data.frame(ts = ts, prompt = ask)
  }
  cache_write(distinct(cache), hist_ask, quiet = TRUE, ...)
  # Ask ChatGPT using their API
  response <- POST(
    url = url,
    add_headers(Authorization = paste("Bearer", secret_key)),
    httr::content_type_json(),
    encode = "json",
    times = num_retries,
    pause_base = pause_base,
    body = list(
      model = model,
      temperature = temperature,
      max_tokens = max_tokens,
      messages = list(list(
        role = "user",
        content = ask
      ))
    )
  )
  ret <- content(response)
  if ("error" %in% names(ret)) warning(ret$error$message)
  if ("message" %in% names(ret$choices[[1]]) & !quiet) {
    cat(stringr::str_trim(ret$choices[[1]]$message$content))
  }
  # Save historical answers
  if (cache_exists(hist_ask)) {
    cache <- cache_read(hist_reply, quiet = TRUE, ...)
    cache <- rbind(cache, data.frame(ts = ts, reply = ret))
  } else {
    cache <- data.frame(ts = ts, prompt = ret)
  }
  cache_write(distinct(cache), hist_reply, quiet = TRUE, ...)
  return(invisible(ret))
}

#' @rdname gpt_ask
#' @export
gpt_history <- function() {
  asks <- cache_read(hist_ask, quiet = TRUE)
  if (!is.null(asks)) {
    replies <- cache_read(hist_reply, quiet = TRUE)
    hist <- as_tibble(left_join(asks, replies, by = "ts")) %>%
      select(.data$ts, .data$prompt, contains("message.content"), everything())
    return(hist)
  } else {
    warning("No historical prompts nor replies registered yet")
  }
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

#' @param colnames Vector. Force column names for your table results.
#' @rdname gpt_ask
#' @export
gpt_table <- function(ask, colnames = NULL, quiet = TRUE, ...) {
  prompt <- paste(
    "Return a markdown table for:", ask,
    ". If you don't know any item, replace with NA.",
    "For numerical values, only use a dot for decimals, no delimiters, no abbreviations")
  if (!is.null(colnames)) prompt <- paste(
    prompt,
    "Also, I fixed column names for the results to be: ", v2t(colnames))
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
  df <- chr2num(df)
  return(df)
}

gpt_prompt_builder <- function(type = "category", cols = c("item", type), x, y) {
  paste(
    "Return a structured markdown table with", length(cols), "columns named", v2t(cols, and = "and"),
    ". Consider the following items:", v2t(x, quotes = FALSE),
    ". For each respective item, what", type, "represent each item using:", v2t(y, quotes = FALSE),
    ". If you don't know any item, replace with NA"
  )
}

gpt_markdown2df <- function(resp) {
  if ("message" %in% names(resp$choices[[1]])) {
    resp <- resp$choices[[1]]$message$content
  }
  df <- try(markdown2df(resp))
  attr(df, "response") <- df
  return(df)
}

### TEST with theoretical prompt elements to build prompts in a standard way
# Instruction (starts with a verb): a specific task or instruction you want the model to perform
# Context - external information or additional context that can steer the model to better responses
# Input Data - the input or question that we are interested to find a response for
# Output Indicator - the type or format of the output.
# Note: You do not need all the four elements for a prompt and the format depends on the task at hand.
gpt_prompt_elements <- function(instruction = NULL, context = NULL, input = NULL, output = NULL) {
  elements <- list(
    instruction = if (!is.null(instruction)) paste("Instruction:", instruction) else NULL,
    context = if (!is.null(context)) paste("Context:", context) else NULL,
    input = if (!is.null(input)) paste("Input:", input) else NULL,
    output = if (!is.null(output)) paste("Output:", output) else NULL
  )
  are_null <- unlist(lapply(elements, is.null))
  if (all(are_null))
    warning("No prompt provided. Set any of the elements: ", v2t(names(elements)))
  return(
    list(
      prompt = v2t(elements[!are_null], quotes = FALSE, sep = " | "),
      elements = elements
    )
  )
}

# p <- gpt_prompt_elements(
#   instruction = "Top 10 largest cities of the world, country, region, coordinates (lat and lon), and population",
#   output = "markdown table")$prompt
# temp <- gpt_ask(p)
# df <- gpt_markdown2df(temp)
# freqs(df, Region)

# res <- gpt_table(
#   ask = "Top 10 largest cities of the world, country, region, coordinates in separate columns, and population",
#   colnames = c("city_name", "Country", "region", "latitude", "long", "total pop"))
