####################################################################
#' Clean text
#' 
#' This function lets the user clean text into getting only alphanumeric 
#' characters and no accents/symbols on letters.
#' 
#' @family Data Wrangling
#' @family Text Mining
#' @param text Character Vector
#' @param spaces Boolean. Keep spaces?
#' @param lower Boolean. Transform all to lower case?
#' @export
cleanText <- function(text, spaces = TRUE, lower = TRUE) {
  text <- as.character(text)
  output <- gsub("[^[:alnum:] ]", "", iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT"))
  if (!spaces) output <- gsub(" ", "", output)
  if (lower) output <- tolower(output)
  return(output)
}


####################################################################
#' Tokenize Vectors into Words
#' 
#' This function transforms texts into words, calculate frequencies,
#' supress stop words in a given language.
#' 
#' @family Exploratory
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
  
  # require("tm")
  options(warn = -1)
  
  text <- as.character(text)
  text <- if (keep_spaces) gsub(" ", "_", text) # '_' deleted later on
  
  ## Load the data as a corpus
  docs <- Corpus(VectorSource(text))
  ## Text transformation
  toSpace <- content_transformer(function(x , pattern) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  ## Cleaning the text
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords(lang))
  # Remove your own stop word (specify your stopwords as a character vector)
  docs <- tm_map(docs, removeWords, c("https","http",exclude))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  ## Build a term-document matrix
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  
  if (df) {
    d <- filter(d, freq >= min)
    if (min <= 1) message(paste("Filtering frequencies with less than", min))
    texts <- cleanText(unique(text))
    if (length(texts) != length(text)) message("Returning unique texts results...")
    toksdf <- c()
    for (i in 1:nrow(d)) {
      word <- as.character(d$word[i])
      vector <- grepl(word, texts)
      toksdf <- cbind(toksdf, vector)
      colnames(toksdf)[colnames(toksdf) == "vector"] <- word
      statusbar(i, nrow(d), info = word)
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
#' with a contains string validator?
#' @export
textFeats <- function(text, auto = TRUE, contains = NA) {
  
  ret <- data.frame(text = text)
  
  if (auto) {
    ret <- ret %>%
      mutate(length = str_length(text),
             ncap = str_count(text, "[A-Z]"),
             ncap_len = ncap / length,
             nexcl = str_count(text, fixed("!")),
             nquest = str_count(text, fixed("?")),
             nats = str_count(text, fixed("@")),
             npunct = str_count(text, "[[:punct:]]"),
             nword = 1 + str_count(text, "\\ "),
             nsymb = str_count(text, "&|@|#|\\$|%|\\*|\\^"),
             nsmile = str_count(text, "((?::|;|=)(?:-)?(?:\\)|D|P))")) 
  }

  
  # Custom columns with contains argument
  if (!is.na(contains)) {
    df <- c()
    for (i in 1:length(contains)) {
      word <- as.character(contains[i])
      vector <- as.integer(grepl(word, text))
      df <- cbind(df, vector)
      colnames(df)[colnames(df) == "vector"] <- word
    }
    ret <- data.frame(ret, df)
  }
  return(ret)
}


####################################################################
#' Wordcloud Plot
#' 
#' Study the distribution of a target variable vs another variable. This
#' function is quite similar to the funModeling's corrplot function.
#' 
#' @family Visualization
#' @family Exploratory
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
  
  # require("wordcloud")
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
