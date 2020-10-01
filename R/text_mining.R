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
#' @examples 
#' cleanText("Bernardo Lares 123")
#' cleanText("Bèrnärdo LáreS 123", lower = FALSE)
#' cleanText("Bernardo Lare$", spaces = ".", ascii = FALSE)
#' cleanText("\\@®ì÷å   %ñS  ..-X", spaces = FALSE)
#' cleanText(c("maría", "€", "núñez a."), title = TRUE)
#' @export
cleanText <- function(text, spaces = TRUE, lower = TRUE, ascii = TRUE, title = FALSE) {
  text <- as.character(text)
  text <- trimws(gsub("[[:space:].]+", " ", text))
  if (ascii) {
    text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")
    text <- gsub("[^[:alnum:] ]", "", text)
  } 
  if (lower) text <- tolower(text)
  if (title) text  <- stringr::str_to_title(text)
  if (is.character(spaces)) text <- gsub(" ", spaces, text)
  if (isFALSE(spaces)) text <- gsub(" ", "", text)
  
  return(text)
}

####################################################################
#' Tokenize Vectors into Words
#' 
#' This function transforms texts into words, calculate frequencies,
#' supress stop words in a given language.
#' 
#' @family Data Wrangling
#' @family Text Mining
#' @param text Character vector
#' @param lang Character. Language in text (used for stop words)
#' @param exclude Character vector. Which word do you wish to exclude?
#' @param keep_spaces Boolean. If you wish to keep spaces in each line
#' to keep unique compount words, separated with spaces, set to TRUE. 
#' For example, 'LA ALAMEDA' will be set as 'LA_ALAMEDA' and treated as
#' a single word.
#' @param df Boolean. Return a dataframe with a one-hot-encoding kind of
#' results? Each word is a column and returns if word is contained.
#' @param min Integer. If df = TRUE, what is the minimum frequency for
#' the word to be considered.
#' @export
textTokenizer <- function(text, lang = "english", 
                          exclude = c(),
                          keep_spaces = FALSE,
                          df = FALSE,
                          min = 2) {
  
  try_require("tm")

  text <- as.character(text)
  if (keep_spaces) text <- gsub(" ", "_", text) # '_' deleted later on
  
  # text <- as.character(c("Hooooolaa 123 4 jaja ALLÁ que bueeeno toOdO!...","seguuuimos","jajaja?"))
  
  ## Load the data as a corpus
  docs <- Corpus(VectorSource(text))
  
  # Convert the text to lower case
  aux <- function(x) tolower(x)
  docs <- tm_map(docs, content_transformer(aux))
  
  # Remove numbers
  aux <- function(x) gsub("[0-9]", " ", x)
  docs <- tm_map(docs, content_transformer(aux))

  # Remove punctuations
  aux <- function(x) gsub("[[:punct:] ]+", " ", x)
  docs <- tm_map(docs, content_transformer(aux))
  
  # Remove crazy UTF-8 symbols over letters
  aux <- function(x) gsub("[^[:alnum:] ]", "", iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))
  docs <- tm_map(docs, content_transformer(aux))
  
  # Repeated letters (more than 3 times)
  aux <- function(x) gsub("([[:alpha:]])\\1{2,}", "\\1", x)
  docs <- tm_map(docs, content_transformer(aux))
  
  # Double vowels as well
  if (lang %in% c("spanish", "english")) {
    aux <- function(x) gsub("[aeiou]*([aeiou])\\1+", "\\1", x)
    docs <- tm_map(docs, content_transformer(aux))
  } 
  
  # Laughs replacements
  aux <- function(x) gsub("a*ja+j[ja]*|a*ha+h[ha]*|o?l+o+l+[ol]*", "(laugh)", x)
  docs <- tm_map(docs, content_transformer(aux))
  
  # Remove stopwords (common stopwords)
  docs <- tm_map(docs, removeWords, stopwords(lang))
  
  # Remove your own stop words
  docs <- tm_map(docs, removeWords, c("https","http","que", as.character(exclude)))
  
  # Eliminate extra white spaces
  aux <- function(x) gsub("\\s+"," ",x)
  docs <- tm_map(docs, content_transformer(aux))
  
  ## Build a term-document matrix
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  rownames(d) <- NULL
  
  if (df) {
    d <- filter(d, .data$freq >= min)
    if (min <= 1) message(paste("Filtering frequencies with less than", min))
    texts <- cleanText(unique(text))
    if (length(texts) != length(text)) message("Returning unique texts results...")
    toksdf <- c()
    for (i in 1:nrow(d)) {
      word <- as.character(d$word[i])
      vector <- grepl(word, texts)
      toksdf <- cbind(toksdf, vector)
      colnames(toksdf)[colnames(toksdf) == "vector"] <- word
      statusbar(i, nrow(d), word)
    }
    message(paste(nrow(d), "columns created succesfully!"))
    toksdf <- data.frame(texts = texts, toksdf)
    return(toksdf)
  } else {
    return(d) 
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
      mutate(length = str_length(text),
             ncap = str_count(text, "[A-Z]"),
             #ncap_len = round(ncap / length, 4),
             nvoc = str_count(toupper(text), "A|E|I|O|U"),
             #nvoc_len = round(nvoc / length, 4),
             nexcl = str_count(text, fixed("!")),
             nquest = str_count(text, fixed("?")),
             nats = str_count(text, fixed("@")),
             npunct = str_count(text, "[[:punct:]]"),
             ndig = str_count(text, "[[0-9]]"),
             nword = 1 + str_count(text, "\\ "),
             nsymb = str_count(text, "&|@|#|\\$|%|\\*|\\^"),
             nsmile = str_count(text, "((?::|;|=)(?:-)?(?:\\)|D|P))")) 
  }

  
  # Custom columns with contains argument
  if (!is.na(contains[1])) {
    df <- c()
    for (i in 1:length(contains)) {
      word <- as.character(contains[i])
      vector <- str_count(text, fixed(word))
      df <- cbind(df, vector)
      colnames(df)[colnames(df) == "vector"] <- word
    }
    ret <- data.frame(ret, df)
  }
  
  if (prc)
    ret <- ret %>% 
      rowwise() %>%
      mutate_if(is.numeric, list(pct = ~ round(./length, 5))) %>%
      ungroup()
  
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
#' @export
textCloud <- function(text, lang = "english", exclude = c(), seed = 0, 
                      keep_spaces = FALSE, min = 2, pal = NA, print = TRUE) {
  
  try_require("wordcloud")
  set.seed(seed)
  
  d <- textTokenizer(text, lang, exclude, keep_spaces)
  if (print) message(paste0(capture.output(head(d, 10)), collapse = "\n")) 
  
  pal <- if (is.na(pal)) rev(names(lares_pal()$palette)[1:6])
  wordcloud(words = d$word, freq = d$freq, 
            scale = c(3.5, .7),
            min.freq = min,
            max.words = 200, 
            random.order = FALSE, 
            rot.per = 0.2, 
            colors = pal)
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
#' @export
sentimentBreakdown <- function(text, lang = "spanish", 
                               exclude = c("maduro","que"),
                               append_file = NA, 
                               append_words = NA,
                               plot = TRUE, 
                               subtitle = NA) {
  
  try_require("syuzhet")
  ret <- list()
  
  dictionary <- get_sentiment_dictionary('nrc', language = lang) %>%
    select(-.data$lang, -.data$value) %>%
    filter(!.data$word %in% exclude) %>%
    rbind(data.frame(word = c("(laugh)","(laugh)"), 
                     sentiment = c("positive","joy")))
  
  if (!is.na(append_file)) {
    new_words <- read.file(normalizePath(append_file), current_wd = FALSE)[,1:2]
    colnames(new_words) <- c("word","sentiment")
    new_words$word <- cleanText(new_words$word)
    new_words$sentiment <- cleanText(new_words$sentiment)
    dictionary <- rbind(dictionary, new_words)
  }
  
  if (!is.na(append_words)) {
    if (ncol(append_words) >= 2) {
      new_words <- append_words[,1:2]
      colnames(new_words) <- c("word","sentiment")
      new_words$word <- cleanText(new_words$word)
      new_words$sentiment <- cleanText(new_words$sentiment)
      dictionary <- rbind(dictionary, new_words) 
    } else message("Be sure to have 'word' and 'sentiment' columns in append_words!")
  }
  
  ret[["words"]] <- textTokenizer(text, lang = lang, exclude = exclude)
  
  ret[["result"]] <- ret$words %>% 
    inner_join(dictionary, "word") %>% 
    distinct(.data$word, .data$sentiment, .keep_all = TRUE)
  
  ret[["summary"]] <- ret$result %>% 
    group_by(.data$sentiment) %>% 
    summarise(freq = sum(.data$freq)) %>% 
    mutate(freq = 100 * .data$freq/sum(.data$freq)) %>%
    arrange(desc(.data$freq))
  
  if (plot) {
    p <- ggplot(
      ret$summary, aes(x = reorder(.data$sentiment, .data$freq), 
                       y = .data$freq, fill = .data$sentiment)) +
      geom_col() + 
      theme_lares2(pal = 0) + 
      gg_fill_customs() + 
      coord_flip() + 
      guides(fill = FALSE) +
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
#' Based on udpipe library, model models, and keywords_rake function.
#' 
#' @family Text Mining
#' @param text Character vector
#' @param file Character. Name of udpipe model previously downloaded
#' for a specific language
#' @param lang Character. If file does not exist, this language will be
#' downloaded from udpipe's models
#' @export
topics_rake <- function(text, file = "english-ewt-ud-2.4-190531.udpipe", lang = "english") {
  try_require("udpipe") 
  if (file.exists(file)) {
    message(">>> Loading previously downloaded model...")
    model <- udpipe_load_model(file) 
  } else {
    message(">>> Downloading new language model...")
    model <- udpipe_download_model(tolower(lang))
    if (model$download_failed)
      stop(model$download_message)
  }
  message(">>> Annotating text...")
  doc <- udpipe_annotate(model, cleanText(text))
  aux <- as.data.frame(doc)
  message(">>> Calculating RAKE values...")
  topics <- keywords_rake(
    x = aux, term = "lemma", group = "doc_id", 
    relevant = aux$upos %in% c("NOUN", "ADJ"))
  return(topics)
}
