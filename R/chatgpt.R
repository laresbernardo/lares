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
#' @family ChatGPT
#' @family LLM
#' @inheritParams cache_write
#' @inheritParams db_download
#' @param ask Character. Redacted prompt to ask. If multiple asks are
#' requested, they will be concatenated with "+" into a single request.
#' @param secret_key Character. Secret Key. Get yours in:
#' platform.openai.com for OpenAI or makersuite.google.com/app/apikey
#' for Gemini.
#' @param url Character. Base API URL.
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
#' gpt_table(
#'   ask = "5 largest cities, their countries, and population",
#'   cols = c("city_name", "where", "POP")
#' )
#'
#' # Translate text to any language
#' gpt_translate(
#'   rep("I love you with all my heart", 5),
#'   language = c("spanish", "chinese", "japanese", "russian", "german")
#' )
#'
#' # Now let's read the historical prompts, replies, ano more from current session
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
  cache <- bind_rows(
    data.frame(ts = ts, prompt = ask),
    cache_read("GPT_HIST_ASK", quiet = TRUE, ...)
  ) %>%
    as_tibble()
  cache_write(distinct(cache), "GPT_HIST_ASK", quiet = TRUE, ...)

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
    cat(paste(stringr::str_trim(ret$choices[[1]]$message$content), "\n"))
  }

  # Save historical answers
  cache <- rbind(
    data.frame(ts = ts, reply = toJSON(ret)),
    cache_read("GPT_HIST_REPLY", quiet = TRUE, ...)
  ) %>%
    as_tibble()
  cache_write(distinct(cache), "GPT_HIST_REPLY", quiet = TRUE, ...)
  invisible(ret)
}

#' @rdname gpt_ask
#' @export
gpt_history <- function(quiet = TRUE, ...) {
  asks <- cache_read("GPT_HIST_ASK", quiet = quiet, ...)
  replies <- cache_read("GPT_HIST_REPLY", quiet = quiet, ...)
  if (!is.null(asks)) {
    if (!is.null(replies)) {
      left_join(asks, replies, by = "ts") %>%
        select(.data$ts, .data$prompt, contains("message.content"), everything())
    } else {
      asks
    }
  } else {
    message("No historical prompts nor replies registered yet")
    invisible(NULL)
  }
}

#' @param cols Vector. Force column names for your table results.
#' @rdname gpt_ask
#' @export
gpt_table <- function(x, cols = NULL, quiet = TRUE, ...) {
  p <- gpt_prompter(instruction = x, output = "table", cols = cols, ...)
  resp <- gpt_ask(p$prompt, quiet = quiet, ...)
  gpt_markdown2df(resp)
}

#' @param x Vector. List items you wish to process in your instruction
#' @param categories,tags Vector. List of possible categories/tags to consider.
#' @rdname gpt_ask
#' @export
gpt_classify <- function(x, categories, quiet = TRUE, ...) {
  p <- gpt_prompter(
    instruction = paste(
      "For each of the inputs,",
      "classify using only the options in context"
    ),
    input = x, context = categories,
    output = "table",
    cols = c("Input", "Category"),
    ...
  )
  resp <- gpt_ask(p$prompt, quiet = quiet, ...)
  gpt_markdown2df(resp)
}

#' @rdname gpt_ask
#' @export
gpt_tag <- function(x, tags, quiet = TRUE, ...) {
  p <- gpt_prompter(
    instruction = paste(
      "For each of the inputs,",
      "provide which of the context values apply as correct tags using TRUE/FALSE"
    ),
    input = x, context = tags,
    output = "table",
    ...
  )
  resp <- gpt_ask(p$prompt, quiet = quiet, ...)
  gpt_markdown2df(resp)
}

#' @param extract,format,unit Character. Length 1 or same as x to extract/format/unit
#' information from x. For example: email, country of phone number, country, amount as number,
#' currency ISO code, ISO, Fahrenheit, etc.
#' @rdname gpt_ask
#' @export
gpt_extract <- function(x, extract, quiet = TRUE, ...) {
  stopifnot(length(extract) %in% c(1, length(x)))
  p <- gpt_prompter(
    instruction = paste(
      "For each of the inputs,",
      "extract each of the information asked in context"
    ),
    input = x, context = extract,
    output = "table",
    cols = c("Input", "Element_to_extract", "Value"),
    ...
  )
  resp <- gpt_ask(p$prompt, quiet = quiet, ...)
  gpt_markdown2df(resp)
}

#' @rdname gpt_ask
#' @export
gpt_format <- function(x, format, quiet = TRUE, ...) {
  stopifnot(length(format) %in% c(1, length(x)))
  p <- gpt_prompter(
    instruction = paste(
      "For each of the inputs,",
      "standardize and format all values to the format in context"
    ),
    input = x, context = format,
    output = "table",
    cols = c("Input", "Formatted"),
    ...
  )
  resp <- gpt_ask(p$prompt, quiet = quiet, ...)
  gpt_markdown2df(resp)
}

#' @rdname gpt_ask
#' @export
gpt_convert <- function(x, unit, quiet = TRUE, ...) {
  stopifnot(length(unit) %in% c(1, length(x)))
  p <- gpt_prompter(
    instruction = paste(
      "For each of the inputs,",
      "provide new converted values using the units in context"
    ),
    input = x, context = unit,
    output = "table",
    cols = c("Input", "Original_Unit", "Total_Value", "Converted_Value", "New_Unit"),
    ...
  )
  resp <- gpt_ask(p$prompt, quiet = quiet, ...)
  gpt_markdown2df(resp)
}

#' @param language Character. Language to translate to
#' @rdname gpt_ask
#' @export
gpt_translate <- function(x, language, quiet = TRUE, ...) {
  stopifnot(length(language) %in% c(1, length(x)))
  p <- gpt_prompter(
    instruction = paste(
      "For each of the inputs,",
      "translate to the respective languages provided in context"
    ),
    input = x, context = language,
    output = "table",
    cols = c("Input", "Language", "Translation"),
    ...
  )
  resp <- gpt_ask(p$prompt, quiet = quiet, ...)
  gpt_markdown2df(resp)
}

gpt_markdown2df <- function(resp) {
  if ("message" %in% names(resp$choices[[1]])) {
    resp <- resp$choices[[1]]$message$content
  }
  df <- try(markdown2df(resp))
  attr(df, "response") <- df
  df
}

# DEPRECATED: Now using the new gpt_prompter()
# gpt_prompter_basic <- function(type = "category", cols = c("item", type), x, y) {
#   paste(
#     "Return a structured markdown table",
#     "with", length(cols), "columns named exactly", v2t(cols, and = "and"),
#     ". Consider the following items:", v2t(x, quotes = FALSE),
#     ". For each respective item, what", type, "represent each item using:", v2t(y, quotes = FALSE),
#     ". If you don't know any item, replace with NA"
#   )
# }

### Theoretical elements to build prompts in a standard and cleaner way
# TIP: Start simple and be very specific


####################################################################
#' Structured Prompt Builder for LLM (ChatGPT)
#'
#' Build standard prompts to get the right outcomes using the four theoretical
#' elements required to build standard and cleaner asks: instruction, input, context,
#' and output. Inspired by the
#' \href{https://www.promptingguide.ai/}{Prompt Engineering Guide} free guide.
#' Remember to start simple and be very specific to get exactly what you need.
#'
#' @section Elements of a Prompt:
#' \describe{
#'   \item{Instruction}{a specific task or instruction you want the model to perform.
#'   It usually starts with a verb given its an instruction.}
#'   \item{Input}{input data, elements or question that we are interested to find a response for,}
#'   \item{Context}{external information, additional context or references to steer
#'   the model to better responses.}
#'   \item{Output}{type or format of the output.}
#' }
#'
#' @family ChatGPT
#' @family LLM
#' @param instruction,input,context,output Character or vector.
#' You do not need all the four elements for a prompt and the
#' format depends on the task at hand.
#' @param quiet Boolean. Should the written prompt be printed or not?
#' @param ... Additional parameters. You can pass \code{cols} parameter to
#' explicitly set column names.
#' @return (Invisible) list with written prompt and elements provided.
#' @examples
#' # A simple formatted table with data
#' # Note: I mostly use output = "table" and enabled an auxiliary enrichment prompt
#' (p <- gpt_prompter(instruction = "Capitals of the world", output = "table"))
#'
#' # Classify
#' p <- gpt_prompter(
#'   instruction = "For each of the inputs, classify using only the options in context",
#'   input = c("Molecule", "Elephant", "Milky Way", "Cat", "Planet Earth"),
#'   context = c("Big", "Medium", "Small"),
#'   output = "table",
#'   # This cols parameter is auxiliary
#'   cols = c("Input", "Category"),
#'   quiet = FALSE
#' )
#'
#' # Tag all categories that apply
#' p <- gpt_prompter(
#'   instruction = paste(
#'     "For each of the inputs, provide which of the",
#'     "context values apply as correct tags using TRUE/FALSE"
#'   ),
#'   input = c("I love chocolate", "I hate chocolate", "I like Coke", "Who am I?", "T-REX"),
#'   context = c("food", "positive", "negative", "beverage"),
#'   output = "table",
#'   quiet = FALSE
#' )
#'
#' # Extract information from strings
#' p <- gpt_prompter(
#'   instruction = "For each of the inputs, extract each of the information asked in context",
#'   input = c("My mail is 123@@test.com", "30 Main St, NY, USA", "+82 2-312-3456", "$1.5M"),
#'   context = c("email", "full state name", "country of phone", "full non-abbreviated number"),
#'   output = "table",
#'   cols = c("Input", "Element_to_extract", "Value"),
#'   quiet = FALSE
#' )
#'
#' # Translate to several languages
#' p <- gpt_prompter(
#'   instruction = "For each of the inputs, translate to the respective languages in context",
#'   input = rep("I love you with all my heart", 5),
#'   context = c("spanish", "chinese", "japanese", "russian", "german"),
#'   output = "table",
#'   cols = c("Input", "Language", "Translation"),
#'   quiet = FALSE
#' )
#'
#' # Format date values
#' p <- gpt_prompter(
#'   instruction = paste(
#'     "For each of the inputs,",
#'     "standardize and format all values to the format in context"
#'   ),
#'   input = c("March 27th, 2021", "12-25-2023 3:45PM", "01.01.2000", "29 Feb 92"),
#'   context = "ISO Date getting rid of time stamps",
#'   output = "table",
#'   cols = c("Input", "Formatted"),
#'   quiet = FALSE
#' )
#'
#' # Convert units
#' p <- gpt_prompter(
#'   instruction = paste(
#'     "For each of the inputs,",
#'     "provide new converted values using the units in context"
#'   ),
#'   input = c("50C", "300K", "100F", "0F", "32C", "0K"),
#'   context = "Fahrenheit",
#'   output = "table",
#'   cols = c("Input", "Original_Unit", "Total_Value", "Converted_Value", "New_Unit"),
#'   quiet = FALSE
#' )
#'
#' # Read a text and answer a question related to it
#' gpt_prompter(
#'   instruction = "read",
#'   context = "Long text here",
#'   input = "Question here"
#' )$prompt
#' @export
gpt_prompter <- function(instruction = NULL,
                         input = NULL,
                         context = NULL,
                         output = NULL,
                         quiet = TRUE,
                         ...) {
  # Generic helpers for common use cases
  if ("table" %in% output) {
    output <- paste(
      "Markdown format table. If missing elements, replace with NA.",
      "For numerical columns, only use a dot for decimals, no delimiters, no abbreviations"
    )
  }
  if ("read" %in% instruction) {
    instruction <- paste(
      "You are a pro content editor who will read the context text and",
      "reply using a single concise response to the question asked in input.",
      "If the answer is not found in context, reply you cannot find the answer in the text"
    )
  }
  if ("cols" %in% names(list(...))) {
    cols <- list(...)[["cols"]]
    if (!is.null(cols)) {
      output <- paste0(
        output, "; also, it must have ", length(cols), " columns named exactly: ", v2t(cols, and = "and")
      )
    }
  }

  # Build the list with 4 elements (and additional information passed through ...)
  elements <- list(
    instruction = if (!is.null(instruction)) paste("Instruction =", paste(instruction, collapse = "; ")) else NULL,
    input = if (!is.null(input)) paste("Input =", paste(input, collapse = "; ")) else NULL,
    context = if (!is.null(context)) paste("Context =", paste(context, collapse = "; ")) else NULL,
    output = if (!is.null(output)) paste("Output =", paste(output, collapse = "; ")) else NULL,
    ...
  )
  # Get rid of already used information
  elements["cols"] <- NULL
  # Check if something's actually being prompted
  are_null <- unlist(lapply(elements, is.null))
  if (all(are_null)) warning("No prompt provided. Set any of the elements: ", v2t(names(elements)))
  if (!quiet) cat(paste(paste(elements[!are_null], collapse = "\n"), "\n"))
  invisible(
    list(
      prompt = v2t(elements[!are_null], quotes = FALSE, sep = " ### "),
      elements = list(instruction = instruction, input = input, context = context, output = output, ...)
    )
  )
}
