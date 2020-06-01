####################################################################
#' Scrabble: Dictionaries
#' 
#' Download words from 4 different languages: English, Spanish, 
#' German, and French. Thanks to "lorenbrichter" for sharing them.
#' 
#' @family Scrabble
#' @param language Character. Any of "en","es","de","fr"
#' @examples 
#' \dontrun{
#' # For Spanish words
#' dictionary <- scrabble_dictionary("es")
#' }
#' @export
scrabble_dictionary <- function(language) {
  check_opts(language, c("en","es","de","fr"))
  message(sprintf(">>> Downloading words from '%s' dictionary. Source: %s", 
                  language, "github.com/lorenbrichter/Words"))
  url <- sprintf(
    "https://raw.githubusercontent.com/lorenbrichter/Words/master/Words/%s.txt", 
    language)
  df <- read.table(url, col.names = "words")
  df$language <- language
  return(df)
}


####################################################################
#' Scrabble: Scores
#' 
#' Get score for any word or list of words. You may set manually depending
#' on the rules and languages you are playing with. Check the examples
#' for Spanish and English values when I played Words With Friends.
#' 
#' @family Scrabble
#' @param words Character vector. Words to score
#' @param scores Dataframe. Must contain two columns: "tiles" with every
#' letter of the alphabet and "scores" for each letter's score.
#' @examples 
#' # For Spanish words
#' es_scores <- data.frame(
#'   tiles = c(tolower(LETTERS)[1:14],"ñ",tolower(LETTERS)[15:length(LETTERS)]),
#'   scores = c(1,3,2,2,1,4,3,4,1,8,10,1,3,1,8,1,3,5,1,1,1,2,4,10,10,5,10))
#' # For English words
#' en_scores <- data.frame(
#'   tiles = tolower(LETTERS),
#'   scores = c(1,4,4,2,1,4,3,3,1,10,5,2,4,2,1,4,10,1,1,1,2,5,4,8,3,10))
#' words <- c("Bernardo", "whiskey", "R is great")
#' # Score values for each language's rules
#' scrabble_score(words, es_scores)
#' scrabble_score(words, en_scores)
#' @export
scrabble_score <- function(words, scores) {
  scores <- data.frame(tiles = tolower(scores$tiles), 
                       scores = as.integer(scores$scores))
  counter <- lapply(words, function(word) {
    lapply(scores$tiles, function(letter) {
      str_count(word, letter)
    })
  })
  scores <- unlist(lapply(counter, function(x) sum(unlist(x) * scores$scores)))
  done <- data.frame(word = words, scores)
  return(done)
}

####################################################################
#' Scrabble: Highest score words finder
#' 
#' Find highest score words given a set of letters, rules, and 
#' language to win at Scrabble! You just have to find the best 
#' place to post your tiles.
#' 
#' @family Scrabble
#' @param tiles Character. The letters you wish to consider
#' @param words Dataframe. Words from \code{scrabble_dictionary()}
#' @param scores Dataframe. Must contain two columns: "tiles" with every
#' letter of the alphabet and "scores" for each letter's score.
#' @param free Integer. How many free blank tiles you have?
#' @param force_start,force_end Character. Force words to start or end with
#' a pattern of letters and position. Examples: "S" or "SO" or "__S_O"...
#' @param force_str Character vector. Force words to contain strings.
#' @param force_n Integer. Force words to be n characters long.
#' @param quiet Boolean. Do not print words as they are being searched.
#' @examples 
#' \dontrun{
#' # Dictionary for Spanish Words
#' es_words <- scrabble_dictionary("es")
#' 
#' # scores for Spanish words
#' es_scores <- data.frame(
#'   tiles = c(tolower(LETTERS)[1:14],"ñ",tolower(LETTERS)[15:length(LETTERS)]),
#'   scores = c(1,3,2,2,1,4,3,4,1,8,10,1,3,1,8,1,3,5,1,1,1,2,4,10,10,5,10))
#'   
#' scrabble_words(tiles = "holase",
#'                words = es_words$words,
#'                scores = es_scores,
#'                free = 1,
#'                force_start = "_o_a")
#' # A tibble: 96 x 2
#'   word    scores
#'   <chr>    <int>
#' 1 hozase     18
#' 2 hozas      17
#' 3 hoza       16
#' 4 hojas      15
#' 5 hoja       14
#' 6 lozas      14
#' 7 solaz      14
#' 8 xolas      14
#' 9 hoyase     13
#' 10 loza      13
#' # … with 86 more rows
#' }
#' @export
scrabble_words <- function(tiles, words, scores, 
                           free = 0, 
                           force_start = "", 
                           force_end = "", 
                           force_str = "",
                           force_n = NA, 
                           quiet = FALSE) {
  
  force_words <- function(words, string, rev = FALSE) {
    forced <- tolower(unlist(strsplit(string, "")))
    forced_which <- which(forced != "_")
    for (i in forced_which)
      words <- words[substr(words, i, i) == forced[i]]
    if (rev) words <- reverse(words)
    return(words)
  }
  
  reverse <- function(words) {
    splits <- sapply(words, function(x) strsplit(x, ""))
    reversed <- lapply(splits, rev)
    words <- as.vector(unlist(sapply(reversed, function(x) paste(x, collapse = ""))))
    return(words)
  }
  
  # Split letters
  tiles <- tolower(unlist(strsplit(tiles, "")))
  ntiles <- as.integer(length(tiles))
  # Add free letters/tiles
  if (free > 0)
    ntiles <- ntiles + free
  
  # Words can't have more letters than inputs
  words <- words[nchar(words) <= ntiles]
  # You may want to force their lengths
  if (!is.na(force_n)) words <- words[nchar(words) == force_n]
  # Words can't have different letters than inputs
  words <- words[grepl(v2t(tiles, sep = "|", quotes = FALSE), words)]
  
  # Force strings that must be contained
  if (force_str[1] != "")
    for (str in force_str)
      words <- words[grepl(tolower(str), words)] 
  # Force start/end strings
  words <- force_words(words, force_start)
  words <- force_words(reverse(words), reverse(force_end), rev = TRUE)
  
  # Let's check all applicable words
  done <- c()
  for (word in words) {
    wi <- word
    for (letter in tiles) {
      have <- str_detect(wi, letter)
      if (have) wi <- str_remove(wi, letter) 
    }
    if (nchar(wi) == free) {
      new <- scrabble_score(word, scores)
      if (!quiet) print(new)
      done <- rbind(done, new)
    }
  } 
  if (length(done) > 0) {
    done <- arrange(done, desc(scores))
    return(as_tibble(done))
  } else {
    message("No words found with set criteria!")
  } 
}

# # Dictionary for Spanish Words
# es_words <- scrabble_dictionary("es")
# 
# # scores for Spanish words
# es_scores <- data.frame(
#   tiles = c(tolower(LETTERS)[1:14],"ñ",tolower(LETTERS)[15:length(LETTERS)]),
#   scores = c(1,3,2,2,1,4,3,4,1,8,10,1,3,1,8,1,3,5,1,1,1,2,4,10,10,5,10))
# 
# # Play and win!
# scrabble_words(tiles = "hilos",
#                words = es_words$words,
#                scores = es_scores,
#                free = 0,
#                force_start = "",
#                force_end = "",
#                force_str = "i",
#                force_n = NA,
#                quiet = FALSE)
