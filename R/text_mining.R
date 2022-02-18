####################################################################
#' Clean text
#'
#' This function lets the user clean text into getting only alphanumeric
#' characters and no accents/symbols on letters.
#'
#' @family Data Wrangling
#' @family Text Mining
#' @param text Character Vector
#' @param spaces Boolean. Keep spaces? If character input, spaces
#' will be transformed into passed argument.
#' @param lower Boolean. Transform all to lower case?
#' @param ascii Boolean. Only ASCII characters?
#' @param title Boolean. Transform to title format (upper case on first letters)
#' @return Character vector with transformed strings.
#' @examples
#' cleanText("Bernardo Lares 123")
#' cleanText("Bèrnärdo LáreS 123", lower = FALSE)
#' cleanText("Bernardo Lare$", spaces = ".", ascii = FALSE)
#' cleanText("\\@®ì÷å   %ñS  ..-X", spaces = FALSE)
#' cleanText(c("maría", "€", "núñez_a."), title = TRUE)
#' @export
#' @rdname clean_text
cleanText <- function(text, spaces = TRUE, lower = TRUE, ascii = TRUE, title = FALSE) {
  text <- as.character(text)
  if (ascii) {
    text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    text <- gsub("[^[:alnum:] ]", "", text)
  }
  if (lower) text <- tolower(text)
  if (title) text <- stringr::str_to_title(text)
  if (is.character(spaces)) text <- gsub(" ", spaces, text)
  if (isFALSE(spaces)) text <- gsub(" ", "", text)
  text <- trimws(gsub("[[:space:].]+", " ", text))
  return(text)
}


####################################################################
#' Clean title names of a data.frame/tibble object
#'
#' Resulting names are unique and consist only of the \code{_} character,
#' numbers, and ASCII letters. Capitalization preferences can be specified using
#' the \code{lower} parameter. Inspired by \code{janitor::clean_names}.
#'
#' @param df data.frame/tibble.
#' @param num Add character before only-numeric names.
#' @param ... Additional parameters passed to \code{cleanText()}.
#' @return data.frame/tibble with transformed column names.
#' @examples
#' df <- dft[1:5, 1:6] # Dummy data
#' colnames(df) <- c("ID.", "34", "x_2", "Num 123", "Nòn-äscì", "  white   Spaces  ")
#' print(df)
#' cleanNames(df)
#' cleanNames(df, lower = FALSE)
#' @export
#' @rdname clean_text
cleanNames <- function(df, num = "x", ...) {
  # Initial cleanse
  cols <- cleanText(colnames(df), ...)
  # Simple spaces turned into "_"
  cols <- trimws(gsub("[[:space:].]+", "_", cols))
  # If only numeric, add x at the begining
  onlynum <- !grepl("[[:alpha:]]", cols)
  cols[onlynum] <- paste0(num, cols[onlynum])
  # Change column names
  df <- stats::setNames(df, cols)
  # Keep tibble if original data.frame is tibble
  if ("tbl_df" %in% class(df)) df <- as_tibble(df)
  return(df)
}

####################################################################
#' Tokenize Vectors into Words
#'
#' This function transforms texts into words, calculate frequencies,
#' supress stop words in a given language.
#'
#' @family Data Wrangling
#' @family Text Mining
#' @param text Character vector. Sentences or texts you wish to tokenize.
#' @param exclude Character vector. Which words do you wish to exclude?
#' @param lang Character. Language in text (used for stop words). Example:
#' "spanish" or "english". Set to \code{NA} to ignore.
#' @param min_word_freq Integer. This will discard words that appear
#' less than <int> times. Defaults to 2. Set to \code{NA} to ignore.
#' @param min_word_len Integer. This will discard words that have
#' less than <int> characters. Defaults to 5. Set to \code{NA} to ignore.
#' @param keep_spaces Boolean. If you wish to keep spaces in each line
#' to keep unique compound words, separated with spaces, set to TRUE.
#' For example, 'one two' will be set as 'one_two' and treated as
#' a single word.
#' @param lowercase,remove_numbers,remove_punct Boolean.
#' @param remove_lettt Boolean. Repeated letters (more than 3 consecutive).
#' @param laughs Boolean. Try to unify all laughs texts.
#' @param utf Boolean. Transform all characters to UTF (no accents and crazy symbols)
#' @param df Boolean. Return a dataframe with a one-hot-encoding kind of
#' results? Each word is a column and returns if word is contained.
#' @param h2o Boolean. Return \code{H2OFrame}?
#' @param quiet Boolean. Keep quiet? If not, print messages
#' @return data.frame. Tokenized words with counters.
#' @export
textTokenizer <- function(text,
                          exclude = NULL,
                          lang = NULL,
                          min_word_freq = 5,
                          min_word_len = 2,
                          keep_spaces = FALSE,
                          lowercase = TRUE,
                          remove_numbers = TRUE,
                          remove_punct = TRUE,
                          remove_lettt = TRUE,
                          laughs = TRUE,
                          utf = TRUE,
                          df = FALSE,
                          h2o = FALSE,
                          quiet = FALSE) {
  try_require("tm")

  text <- as.character(text)
  lang <- tolower(lang)

  # text <- as.character(c("Hooooolaa 123 4 jaja ALLÁ que bueeeno toOdO!...","seguuuimos","jajaja?"))

  # When multiple words should be kept together
  if (keep_spaces) {
    if (!quiet) message(">>> Keeping spaced multi-words")
    text <- gsub(" ", "_", text) # '_' deleted later on
  }

  ## Load the data as a corpus
  docs <- VCorpus(x = VectorSource(text), readerControl = list(reader = readPlain))

  # Convert the text to lower case
  if (lowercase) {
    if (!quiet) message(">>> Transforming to lower case")
    aux <- function(x) tolower(x)
    docs <- tm_map(docs, content_transformer(aux))
  }

  # Remove numbers
  if (remove_numbers) {
    if (!quiet) message(">>> Removing numbers")
    aux <- function(x) gsub("[0-9]", " ", x)
    docs <- tm_map(docs, content_transformer(aux))
  }

  # Remove punctuations
  if (remove_punct) {
    if (!quiet) message(">>> Removing punctuation marks")
    aux <- function(x) gsub("[[:punct:] ]+", " ", x)
    docs <- tm_map(docs, content_transformer(aux))
  }

  # Eliminate extra white spaces
  if (!quiet) message(">>> Removing extra spaces")
  aux <- function(x) gsub("\\s+", " ", x)
  docs <- tm_map(docs, content_transformer(aux))

  # Remove crazy UTF-8 symbols over letters
  if (utf) {
    if (!quiet) message(">>> Transforming UTF8 to ASCII symbols")
    aux <- function(x) gsub("[^[:alnum:] ]", "", iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))
    docs <- tm_map(docs, content_transformer(aux))
  }

  # Repeated letters (more than 3 times)
  if (remove_lettt) {
    if (!quiet) message(">>> Removing repeated characters (more than 2)")
    aux <- function(x) gsub("([[:alpha:]])\\1{2,}", "\\1", x)
    docs <- tm_map(docs, content_transformer(aux))
    # Double vowels as well in spanish
    if ("spanish" %in% lang) {
      aux <- function(x) gsub("[aeiou]*([aeiou])\\1+", "\\1", x)
      docs <- tm_map(docs, content_transformer(aux))
    }
  }

  # Laughs replacements
  if (laughs) {
    if (!quiet) message(">>> Standarizing laughs")
    aux <- function(x) gsub("a*ja+j[ja]*|a*ha+h[ha]*|o?l+o+l+[ol]*", "(laugh)", x)
    docs <- tm_map(docs, content_transformer(aux))
  }

  # Remove your own stop words
  if (!is.null(exclude)) {
    if (!quiet) message(">>> Removing stopwords")
    docs <- tm_map(docs, removeWords, c("https", "http", "que", as.character(exclude)))
  }

  # Remove stopwords (common stopwords)
  if (!is.null(lang)) {
    if (!quiet) message(">>> Removing language stopwords")
    docs <- tm_map(docs, removeWords, stopwords(lang))
  }

  ## Build a term-document matrix
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  rownames(d) <- NULL

  if (!is.na(min_word_freq)) {
    if (!quiet) message(">>> Removing words with frequency lower than ", min_word_freq)
    d <- filter(d, .data$freq >= min_word_freq)
  }

  if (!is.na(min_word_len)) {
    if (!quiet) message(">>> Removing words with length lower than ", min_word_len)
    d <- filter(d, nchar(.data$freq) >= min_word_len)
  }

  if (df) {
    texts <- cleanText(unique(text))
    if (length(texts) != length(text)) {
      if (!quiet) message("Returning unique texts results...")
    }
    toksdf <- NULL
    for (i in seq_len(nrow(d))) {
      word <- as.character(d$word[i])
      vector <- grepl(word, texts)
      toksdf <- cbind(toksdf, vector)
      colnames(toksdf)[colnames(toksdf) == "vector"] <- word
      statusbar(i, nrow(d), word)
    }
    toksdf <- data.frame(texts = texts, toksdf)
    return(as_tibble(toksdf))
  } else {
    return(as_tibble(d))
  }
}


####################################################################
#' Create features out of text
#'
#' This function creates a data.frame with features based on a text vector
#'
#' @family Data Wrangling
#' @family Text Mining
#' @param text Character vector
#' @param auto Boolean. Auto create some useful parameters?
#' @param contains Character vector. Which columns do you wish to add
#' with a contains (counter) string validator?
#' @param prc Boolean. Also add percentage of each column compared with length?
#' @return data.frame with additional features based on \code{text}.
#' @examples
#' textFeats("Bernardo Lares")
#' textFeats("Bernardo Lares 123!", prc = TRUE)
#' textFeats("I'm 100% Lares...", contains = c("Lares", "lares"))
#' textFeats(c("GREAT library!!", "Have you tried this 2?", "Happy faces :D :-)"))
#' @export
textFeats <- function(text, auto = TRUE, contains = NA, prc = FALSE) {
  ret <- data.frame(text = text)


  if (auto) {
    ret <- ret %>%
      mutate(
        length = str_length(text),
        ncap = str_count(text, "[A-Z]"),
        # ncap_len = round(ncap / length, 4),
        nvoc = str_count(toupper(text), "A|E|I|O|U"),
        # nvoc_len = round(nvoc / length, 4),
        nexcl = str_count(text, fixed("!")),
        nquest = str_count(text, fixed("?")),
        nats = str_count(text, fixed("@")),
        npunct = str_count(text, "[[:punct:]]"),
        ndig = str_count(text, "[[0-9]]"),
        nword = 1 + str_count(text, "\\ "),
        nsymb = str_count(text, "&|@|#|\\$|%|\\*|\\^"),
        nsmile = str_count(text, "((?::|;|=)(?:-)?(?:\\)|D|P))")
      )
  }


  # Custom columns with contains argument
  if (!is.na(contains[1])) {
    df <- NULL
    for (i in seq_along(contains)) {
      word <- as.character(contains[i])
      vector <- str_count(text, fixed(word))
      df <- cbind(df, vector)
      colnames(df)[colnames(df) == "vector"] <- word
    }
    ret <- data.frame(ret, df)
  }

  if (prc) {
    ret <- ret %>%
      rowwise() %>%
      mutate_if(is.numeric, list(pct = ~ round(. / length, 5))) %>%
      ungroup()
  }

  return(as_tibble(ret))
}

####################################################################
#' Wordcloud Plot
#'
#' Study the distribution of a target variable vs another variable. This
#' function is quite similar to the funModeling's corrplot function.
#'
#' @family Text Mining
#' @param text Character vector
#' @param lang Character. Language in text (used for stop words)
#' @param exclude Character vector. Which word do you wish to exclude?
#' @param seed Numeric. Seed for re-producible plots
#' @param keep_spaces Boolean. If you wish to keep spaces in each line
#' to keep unique compount words, separated with spaces, set to TRUE.
#' For example, 'LA ALAMEDA' will be set as 'LA_ALAMEDA' and treated as
#' a single word.
#' @param min Integer. Words with less frequency will not be plotted
#' @param pal Character vector. Which colours do you wish to use
#' @param print Boolean. Plot results as textcloud?
#' @return wordcloud plot object
#' @export
textCloud <- function(text, lang = "english", exclude = NULL, seed = 0,
                      keep_spaces = FALSE, min = 2, pal = NA, print = TRUE) {
  try_require("wordcloud")
  on.exit(set.seed(seed))

  d <- textTokenizer(text, lang, exclude, keep_spaces)
  if (print) message(paste0(capture.output(head(d, 10)), collapse = "\n"))

  pal <- if (is.na(pal)) rev(names(lares_pal()$palette)[1:6])
  wordcloud(
    words = d$word, freq = d$freq,
    scale = c(3.5, .7),
    min.freq = min,
    max.words = 200,
    random.order = FALSE,
    rot.per = 0.2,
    colors = pal
  )
}


####################################################################
#' Sentiment Breakdown on Text
#'
#' This function searches for relevant words in a given text and adds
#' sentiments labels (joy, anticipation, surprise, positive, trust,
#' anger, sadness, fear, negative, disgust) for each of them, using NRC.
#' Then, makes a summary for all words and plot results.
#'
#' @family Text Mining
#' @param text Character vector
#' @param lang Character. Language in text (used for stop words)
#' @param exclude Character vector. Which word do you wish to exclude?
#' @param append_file Character. Add a dictionary to append. This file
#' must contain at least two columns, first with words and second with
#' the sentiment (consider sentiments on description).
#' @param append_words Dataframe. Same as append_file but appending
#' data frame with word and sentiment directly
#' @param plot Boolean. Plot results summary?
#' @param subtitle Character. Add subtitle to the plot
#' @return List. Contains data.frame with words and sentiments, summary and plot.
#' @export
sentimentBreakdown <- function(text, lang = "spanish",
                               exclude = c("maduro", "que"),
                               append_file = NA,
                               append_words = NA,
                               plot = TRUE,
                               subtitle = NA) {
  try_require("syuzhet")
  ret <- list()

  dictionary <- get_sentiment_dictionary("nrc", language = lang) %>%
    select(-.data$lang, -.data$value) %>%
    filter(!.data$word %in% exclude) %>%
    rbind(data.frame(
      word = c("(laugh)", "(laugh)"),
      sentiment = c("positive", "joy")
    ))

  if (!is.na(append_file)) {
    new_words <- read.file(normalizePath(append_file), current_wd = FALSE)[, 1:2]
    colnames(new_words) <- c("word", "sentiment")
    new_words$word <- cleanText(new_words$word)
    new_words$sentiment <- cleanText(new_words$sentiment)
    dictionary <- rbind(dictionary, new_words)
  }

  if (!is.na(append_words)) {
    if (ncol(append_words) >= 2) {
      new_words <- append_words[, 1:2]
      colnames(new_words) <- c("word", "sentiment")
      new_words$word <- cleanText(new_words$word)
      new_words$sentiment <- cleanText(new_words$sentiment)
      dictionary <- rbind(dictionary, new_words)
    } else {
      message("Be sure to have 'word' and 'sentiment' columns in append_words!")
    }
  }

  ret[["words"]] <- textTokenizer(text, lang = lang, exclude = exclude)

  ret[["result"]] <- ret$words %>%
    inner_join(dictionary, "word") %>%
    distinct(.data$word, .data$sentiment, .keep_all = TRUE)

  ret[["summary"]] <- ret$result %>%
    group_by(.data$sentiment) %>%
    summarise(freq = sum(.data$freq)) %>%
    mutate(freq = 100 * .data$freq / sum(.data$freq)) %>%
    arrange(desc(.data$freq))

  if (plot) {
    p <- ggplot(
      ret$summary, aes(
        x = reorder(.data$sentiment, .data$freq),
        y = .data$freq
      )
    ) +
      geom_col(aes(fill = .data$sentiment)) +
      theme_lares(pal = 4) +
      coord_flip() +
      guides(fill = "none") +
      labs(x = NULL, y = "Intensity", title = "Sentiment Breakdown")
    if (!is.na(subtitle)) p <- p + labs(subtitle = autoline(subtitle))
    ret[["plot"]] <- p

    # wrd <- spread(ret$result, sentiment, freq, fill = 0)
    # rownames(wrd) <- wrd$word
    # wrd$word <- NULL
    # pal <- lares_pal()$labels %>%
    #   filter(values %in% ret$result$sentiment) %>% arrange(values)
    # comparison.cloud(wrd, max.words = 100,
    #                  scale = c(3.5,.5),
    #                  title.size = NULL,
    #                  random.order = FALSE,
    #                  colors = as.character(pal$fill),
    #                  match.colors = TRUE,
    #                  title.bg.colors = "transparent")
  }
  return(ret)
}


####################################################################
#' Keyword/Topic identification using RAKE
#'
#' RAKE is a basic algorithm which tries to identify keywords in text.
#' Based on \code{udpipe} library, model models, and keywords_rake function.
#'
#' @family Text Mining
#' @param text Character vector
#' @param file Character. Name of \code{udpipe} model previously downloaded
#' for a specific language
#' @param lang Character. If file does not exist, this language will be
#' downloaded from \code{udpipe}'s models.
#' @return data.frame with topics for each \code{text} input.
#' @export
topics_rake <- function(text, file = "english-ewt-ud-2.4-190531.udpipe", lang = "english") {
  try_require("udpipe")
  if (file.exists(file)) {
    message(">>> Loading previously downloaded model...")
    model <- udpipe_load_model(file)
  } else {
    message(">>> Downloading new language model...")
    model <- udpipe_download_model(tolower(lang))
    if (model$download_failed) {
      stop(model$download_message)
    }
  }
  message(">>> Annotating text...")
  doc <- udpipe_annotate(model, cleanText(text))
  aux <- as.data.frame(doc)
  message(">>> Calculating RAKE values...")
  topics <- keywords_rake(
    x = aux, term = "lemma", group = "doc_id",
    relevant = aux$upos %in% c("NOUN", "ADJ")
  )
  return(topics)
}


####################################################################
#' Build N-grams and keep most frequent
#'
#' Build out n-grams for multiple text inputs and keep the n most frequent
#' combinations.
#'
#' @family Text Mining
#' @inheritParams remove_stopwords
#' @param ngram Integer vector. Number of continuous n items in text.
#' @param top Integer. Keep n most frequent ngrams only.
#' @param ... Additional parameters passed to \code{remove_stopwords}.
#' @return data.frame with ngrams and counters, sorted by frequency.
#' @examples
#' # You must have "tidytext" library to use this auxiliary function:
#' \dontrun{
#' women <- read.csv("https://bit.ly/3mXJOOi")
#' x <- women$description
#' ngrams(x, ngram = c(2, 3), top = 3)
#' ngrams(x, ngram = 2, top = 6, stop_words = c("a", "is", "of", "the"))
#' }
#' @export
ngrams <- function(text, ngram = c(2, 3), top = 10, stop_words = NULL, ...) {
  try_require("tidytext")
  x_counts <- lapply(ngram, function(i) {
    data.frame(msg = data.frame(text)[, 1]) %>%
      {
        if (!is.null(stop_words)[1]) {
          mutate(., msg = remove_stopwords(.data$msg, stop_words, ...))
        } else {
          .
        }
      } %>%
      unnest_tokens("comb", .data$msg, token = "ngrams", n = i) %>%
      filter(!is.na(.data$comb)) %>%
      freqs(.data$comb) %>%
      head(top) %>%
      mutate(ngram = i)
  })
  bind_rows(x_counts)
}


####################################################################
#' Remove stop-words and patterns from character vector
#'
#' Remove all stop-words and specific patterns from a character vector
#'
#' @family Text Mining
#' @param text Character vector
#' @param stop_words Character vector. Words to exclude from text. Example:
#' if you want to exclude "a", whenever that word appears it will be excluded,
#' but when the letter "a" appears in a word, it will remain.
#' @param exclude Character. Pattern to exclude using regex.
#' @param sep Character. String that separate the terms.
#' @return Character vector with removed texts.
#' @examples
#' x <- c("A brown fox jumps over a dog.", "Another brown dog.")
#' remove_stopwords(x, stop_words = c("dog", "brown", "a"), exclude = "\\.")
#' @export
remove_stopwords <- function(text, stop_words, exclude = NULL, sep = " ") {
  tok <- lapply(text, function(i) str_split(trimws(i), sep)[[1]])
  if (!is.null(exclude)[1]) {
    tok <- lapply(tok, function(i) gsub(paste(exclude, collapse = "|"), "", i))
  }
  if (!is.null(stop_words)[1]) {
    tok <- lapply(tok, function(i) i[!tolower(i) %in% unique(c("", tolower(stop_words)))])
  }
  fin <- lapply(tok, function(i) paste(i, collapse = sep))
  return(unlist(fin))
}
