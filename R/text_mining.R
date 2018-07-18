####################################################################
# Simple Wordcloud
textCloud <- function(text, lang = "english", exclude = c(), seed = 0, print = T, keep_spaces = FALSE) {
  
  require("tm")
  require("SnowballC")
  require("wordcloud")
  require("RColorBrewer")
  
  set.seed(seed)
  options(warn=-1)
  
  text <- as.character(text)
  
  if (keep_spaces == TRUE) {
    text <- gsub(" ", "_", text) # '_' deleted later on
  }
  
  ## Load the data as a corpus
  docs <- Corpus(VectorSource(text))
  
  ## Text transformation
  toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
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
  docs <- tm_map(docs, removeWords, rbind("https", exclude))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  ## Build a term-document matrix
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing=TRUE)
  d <- data.frame(word = names(v), freq=v)
  
  if (print == TRUE) {
    message(paste0(capture.output(head(d, 10)), collapse = "\n")) 
  }
  
  wordcloud(words = d$word, freq = d$freq, 
            scale = c(3.5, .7),
            min.freq = 1,
            max.words = 200, 
            random.order = FALSE, 
            rot.per = 0.2, 
            colors = brewer.pal(8, "Paired"))
  
}
