####################################################################
#' Wordle Game Validation
#' 
#' Given and input and a word, validate each letter based on Wordle's
#' rules: correct letter in correct placement (green), correct letter in wrong
#' placement (yellow), letter is not present (red).
#'
#' @param input Character. Word to validate (5-letters)
#' @param word Character. Word actually answer (5-letters).
#' @param dictionary Character vector. List of valid words. If set to NULL
#' then will use modified \code{scrabble_dictionary()} to fetch 5 letter words.
#' Use \code{lang_dic} param to set language.
#' @param lang_dic Character. Any of: "en", "es". Only used when \code{dictionary}
#' parameter is NULL. Requires internet connection the first time. Uses cache.
#' @param method Integer. 1 for \code{scrabble_dictionary()}, 2 for reduced Wordle
#' words from github, 3 for scrapping NYTimes set of words.
#' @param print Boolean. Print validation results?
#' @return Depends on \code{cat}: NULL if TRUE or character string if FALSE.
#' @examples
#' word <- "ABBEY"
#' # Or pick a random one:
#' # word <- sample(wordle_dictionary("en"), 1)
#' wordle_check("OPENS", word)
#' wordle_check("BABES", word)
#' wordle_check("BABES", word, print = FALSE)
#' wordle_check("KEBAB", word)
#' wordle_check("ABYSS", word)
#' wordle_check("ABBEY", word)
#' @export
#' @rdname wordle
wordle_check <- function(input, word, dictionary = NULL, lang_dic = "en", method = 3, print = TRUE) {
  wordle_valid(input, dictionary, lang_dic, method)
  tiles <- toupper(unlist(strsplit(word, "")))
  in_tiles <- toupper(unlist(strsplit(input, "")))
  
  init <- rep("_", 5)
  init[which(!in_tiles %in% tiles)] <- "RED"
  init[which(in_tiles %in% tiles)] <- "YELLOW"
  init[which(in_tiles == tiles)] <- "GREEN"
  
  out <- init
  names(out) <- in_tiles
  class(out) <- c("wordle_check", class(out))
  if (print) print(out) else return(out)
}

#' @rdname wordle
#' @param x Object to print
#' @export
print.wordle_check <- function(x, print = TRUE, ...) {
  texts <- NULL
  for (i in seq_along(x))
    texts <- c(texts, formatColoured(names(x)[i], x[i], bold = TRUE, cat = FALSE))
  txt <- paste(texts, collapse = " ")
  if (print) cat(txt) else return(txt)
}

wordle_valid <- function(input, dictionary, lang_dic = "en", method = 3) {
  if (is.null(dictionary))
    dictionary <- wordle_dictionary(lang_dic, method, quiet = TRUE)
  if (!toupper(input) %in% toupper(dictionary))
    stop(input, " is not a valid input word")  
}

#' @inheritParams scrabble_dictionary
#' @export
#' @rdname wordle
wordle_dictionary <- function(lang_dic = "en", method = 3, quiet = TRUE) {
  if (is.null(lang_dic)) return(NULL)
  check_opts(lang_dic, c("en", "es"))
  cache_name <- c(lang_dic, method)
  if (cache_exists(cache_name)) {
    words <- cache_read(cache_name, quiet = quiet)
    if (!quiet) message(sprintf(
      ">>> Loaded %s '%s' words",
      formatNum(length(words), 0), lang_dic
    ))
    return(words)
  }
  if (method == 1) {
    words <- scrabble_dictionary(lang_dic, quiet)[[1]] 
  }
  if (method == 2) {
    source <- paste0(
      "https://gist.githubusercontent.com/cfreshman/",
      "a03ef2cba789d8cf00c08f767e0fad7b/raw/5d752e5f0702da315298a6bb5a771586d6ff445c/",
      "wordle-answers-alphabetical.txt")
    words <- read.delim(source, header = FALSE)$V1 
  }
  if (method == 3) {
    url <- "https://www.nytimes.com/games/wordle/main.bd4cb59c.js"
    temp <- str_split(readLines(url), '"')[[1]]
    words <- toupper(unique(temp[nchar(temp) == 5]))
    words <- grep("^[[:alnum:]]+$", words, value = TRUE)
  }
  out <- toupper(words[nchar(words) == 5])
  cache_write(out, cache_name, quiet = quiet)
  if (!quiet) message(sprintf(">>> Saved (%s words) into cache", formatNum(length(out), 0)))
  return(out)
}

#' @inheritParams wordle_check
#' @param seed Numeric. For reproducibility. Accepts more than one: will
#' run as many seeds there are.
#' @param ... Additional parameters to pass.
#' @export
#' @examples 
#' 
#' x <- wordle_simulation(input = "SAINT", word = "ABBEY", seed = 1:3)
#' # hist(sapply(x, function(x) x$iters))
#' @rdname wordle
wordle_simulation <- function(input, word, seed = NULL, quiet = FALSE, ...) {
  output <- NULL
  if (is.null(seed)) seed <- sample(1:100, 1)
  for (s in seed) {
    set.seed(s) # s = seed[1]
    seed_loop <- NULL
    i <- 1
    used_words <- input
    # First iteration with picked word
    iter <- wordle_opts(input, word, quiet = quiet, ...)
    # Second iteration onwards
    while (length(iter) > 1) {
      random_word <- sample(iter, 1)
      used_words <- c(used_words, random_word)
      iter <- wordle_opts(random_word, word, iter, quiet = quiet, ...)
      # If random word picked is the word
      if (random_word == word) break
      # If last word remaining is the one
      if (length(iter) == 1 & all(iter == word))
        iter <- wordle_opts(word, word, iter, quiet = quiet, ...)
      i <- i + 1
    }
    output[[paste0("seed_", s)]] <- list(words = used_words, iters = sum(used_words != word) + 1)
    if (!quiet) message(sprintf(">> Iterations (seed = %s): %s\n", s, i + 1))
  }
  attr(output, "input") <- input
  attr(output, "word") <- word
  class(output) <- c("wordle_simulation", class(output))
  return(invisible(output))
}

#' @rdname wordle
#' @export
print.wordle_simulation <- function(x, ...) {
  words <- lapply(x, function(x) x$words)
  iters_n <- sapply(x, function(x) x$iters)
  for_print <- list()
  split_col <- "BLUE"
  names(split_col) <- attr(x, "word")
  for (i in seq_along(x)) {
    # Namings: word + seed + iterations
    word_split_iter <- split_col
    names(word_split_iter) <- paste(attr(x, "word"), names(x)[i], "->", iters_n[i], "iterations")
    class(word_split_iter) <- "wordle_check"
    res <- NULL
    res[[1]] <- word_split_iter
    # Word by word coloring
    list <- words[[i]]
    ## Remove the word when it's guessed when more than 1 opts
    list <- list[list != attr(x, "word")]
    results <- lapply(list, function(i) wordle_check(i, attr(x, "word"), print = FALSE))
    res <- append(res, results)
    for_print <- append(for_print, res)
  }
  txts <- sapply(for_print, function(x) paste(print(x, print = FALSE)))
  cat(txts, sep = "\n")  
}

# which.max(sapply(x, function(x) x$iters))
# which.min(sapply(x, function(x) x$iters))
# wordle_simulation("SAINT", "ABBEY", 9)

# SIMULATE N SOLUTIONS BY START WORD
wordle_opts <- function(input, word, dictionary = NULL, lang_dic = "en", method = 3, quiet = FALSE, ...) {
  
  if (is.null(dictionary)) dictionary <- wordle_dictionary(lang_dic, quiet = TRUE)
  if (!word %in% dictionary) dictionary <- c(dictionary, word)
  n_words_init <- length(dictionary)
  check <- wordle_check(input, word, dictionary, lang_dic, method, print = FALSE)
  
  if (input[1] != word[1]) {
    # Prepare data.frame to filtering words that match criteria
    words_lst <- lapply(dictionary, function(x) toupper(unlist(strsplit(x, ""))))
    words_df <- bind_rows(lapply(words_lst, function(x) as.data.frame(t(x)))) %>% mutate(word = dictionary)
    
    # Letters not present
    these <- names(check[check == "RED"])
    if (length(these) > 0)
      words_df <- filter(words_df, !grepl(paste(these, collapse = "|"), .data$word))
    # Letters present but not in position
    these <- names(check[check == "YELLOW"])
    words_df <- filter(words_df, grepl(paste(these, collapse = "|"), .data$word))
    these_id <- which(check == "YELLOW")
    for (i in seq_along(these)) words_df <- words_df[words_df[, these_id[i]] != these[i],]
    # Letters present in position
    these <- names(check[check == "GREEN"])
    these_id <- which(check == "GREEN")
    for (i in seq_along(these)) words_df <- words_df[words_df[, these_id[i]] == these[i],]
    dictionary <- words_df$word 
  } else dictionary <- word
  
  input_coloured <- print(check, print = FALSE)
  if (!quiet) message(cat(input_coloured, "reduced from", formatNum(n_words_init, 0), "to", formatNum(length(dictionary), 0)))
  
  attr(dictionary, "answer") <- word
  attr(dictionary, "last_word") <- input
  
  return(dictionary)
}

# iterations <- NULL
# for (i in 1:20) iterations <- c(iterations, wordle_simulation(input = "SAINT", word = "ABBEY", seed = i))
# hist(iterations)
# wordle_simulation(input = "SAINT", word = "ABBEY", seed = which.max(iterations)) # 2 with method 3

# wordle_simulation(input = "SAINT", word = "ABBEY", seed = 15)

# word <- "TEXTS"
# wordle_simulation(word, word)

############ PLAY AREA ############

# # word <- sample(wordle_dictionary("en"), 1)
# word <- "ABBEY"
# wordle_check("OPENS", word)
# wordle_check("BABES", word)
# wordle_check("KEBAB", word)
# wordle_check("ABYSS", word)
# wordle_check("ABBEY", word)
# 
# # Cheat using scrabble_words()
# set.seed(1) # For reproducibility
# # word <- sample(wordle_dictionary("en"), 1)
# word <- "FLESH" # Secret word
# # First iteration
# wordle_check("SAINT", word)
# scrabble_words(
#   language = "en",
#   force_n = 5,
#   tiles = "S",
#   force_exclude = "AINT"
# ) %>% pull(.data$word) %>% sample(10)
# # Second iteration
# wordle_check("fords", word)
# scrabble_words(
#   language = "en",
#   force_n = 5,
#   tiles = "S",
#   force_start = "F",
#   force_exclude = "AINTORD"
# ) %>% pull(.data$word) %>% sample(10)
# # Third iteration
# wordle_check("fesse", word)
# scrabble_words(
#   language = "en",
#   force_n = 5,
#   tiles = "ES",
#   force_start = "F__S",
#   force_exclude = "AINTORD"
# )
# # Fourth iteration
# wordle_check("flesh", word)

