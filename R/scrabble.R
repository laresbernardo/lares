####################################################################
#' Scrabble: Dictionaries
#'
#' Download words from 4 different languages: English, Spanish,
#' German, and French. Words will be save into the \code{temp} directory.
#' This is an auxiliary function. You may want to use \code{scrabble_words}
#' directly if you are searching for the highest score words!
#'
#' @family Scrabble
#' @param language Character. Any of "en","es","de","fr". Set to "none"
#' if you wish to skip this step (and use \code{words} parameter in
#' \code{scrabble_words} instead).
#' @return data.frame with words and language columns.
#' @examples
#' \donttest{
#' # For Spanish words
#' dictionary <- scrabble_dictionary("es")
#' }
#' @export
scrabble_dictionary <- function(language) {
  if (length(language) != 1) {
    stop("Select only 1 language at a time...")
  }
  check_opts(language, c("en", "es", "de", "fr", "none"))
  if (language == "none") {
    return(invisible(NULL))
  }
  filename <- file.path(tempdir(), paste0(language, ".RData"))
  if (file.exists(filename)) {
    load(filename)
    message(sprintf(
      ">>> Loaded %s '%s' words",
      formatNum(nrow(words), 0), language
    ))
    return(words)
  }
  message(sprintf(
    ">>> Downloading '%s' words. Source: %s",
    language, "github.com/lorenbrichter/Words"
  ))
  url <- sprintf(
    "https://raw.githubusercontent.com/lorenbrichter/Words/master/Words/%s.txt",
    language
  )
  words <- read.table(url, col.names = "words")
  words$language <- language
  save(words, file = filename, version = 2)
  message(">>> Saved into ", filename)
  return(words)
}


####################################################################
#' Scrabble: Word Scores
#'
#' Get score for any word or list of words. You may set manually depending
#' on the rules and languages you are playing with. Check the examples
#' for Spanish and English values when I played Words With Friends.
#'
#' @family Scrabble
#' @param words Character vector. Words to score
#' @param scores Dataframe. Must contain two columns: "tiles" with every
#' letter of the alphabet and "scores" for each letter's score.
#' @return data.frame with word, scores, and length values for each \code{word}.
#' @examples
#' \donttest{
#' # For Spanish words (default)
#' es_scores <- scrabble_points("es")
#' # Custom scores for each letter
#' cu_scores <- data.frame(
#'   tiles = tolower(LETTERS),
#'   scores = c(1, 1, 1, 1, 1, 1, 1, 5, 1, 1, 5, 2, 4, 2, 1, 4, 10, 1, 1, 1, 2, 5, 4, 8, 3, 10)
#' )
#'
#' # Score values for each set of rules
#' words <- c("Bernardo", "Whiskey", "R is great")
#' scrabble_score(words, es_scores)
#' scrabble_score(words, cu_scores)
#' }
#' @export
scrabble_score <- function(words, scores) {
  scores <- data.frame(
    tiles = tolower(scores$tiles),
    scores = as.integer(scores$scores)
  )
  counter <- lapply(words, function(word) {
    lapply(scores$tiles, function(letter) {
      str_count(word, letter)
    })
  })
  scores <- unlist(lapply(counter, function(x) sum(unlist(x) * scores$scores)))
  done <- data.frame(word = words, scores) %>%
    mutate(length = nchar(word)) %>%
    arrange(desc(scores))
  return(done)
}


####################################################################
#' Scrabble: Tiles Points
#'
#' Dataframe for every letter and points given a language.
#'
#' @family Scrabble
#' @param language Character. Any of "en","es".
#' @return data.frame with tiles and scores for each letter.
#' @examples
#' scrabble_points("es")
#' scrabble_points("en")
#' # Not yet available
#' scrabble_points("fr")
#' @export
scrabble_points <- function(language) {
  if (!language %in% c("en", "es")) {
    message("We do not have the points for this language yet!")
    return(invisible(NULL))
  }
  if (language == "es") {
    scores <- data.frame(
      tiles = c(
        tolower(LETTERS)[1:14], intToUtf8(241),
        tolower(LETTERS)[15:length(LETTERS)]
      ),
      scores = c(1, 3, 2, 2, 1, 4, 3, 4, 1, 8, 10, 1, 3, 1, 8, 1, 3, 5, 1, 1, 1, 2, 4, 10, 10, 5, 10)
    )
  }
  if (language == "en") {
    scores <- data.frame(
      tiles = tolower(LETTERS),
      scores = c(1, 4, 4, 2, 1, 4, 3, 3, 1, 10, 5, 2, 4, 2, 1, 4, 10, 1, 1, 1, 2, 5, 4, 8, 3, 10)
    )
  }

  message(sprintf(">>> Auto-loaded points for '%s'", language))
  return(scores)
}


####################################################################
#' Pattern Matching for Letters considering Blanks
#'
#' Match pattern of letters considering blanks within each element of a
#' character vector, allowing counted characters between and around
#' each letter. Used as an auxiliary function for the Scrabble family
#' of functions.
#'
#' @param x Character vector
#' @param pattern Character. Character string containing a
#' semi-regular expression which uses the following logic:
#' "a_b" means any character that contains "a" followed by
#' something followed by "b", anywhere in the string.
#' @param blank Character. String to use between letters.
#' @return Boolean check for each value on \code{x}.
#' @examples
#' x <- c("aaaa", "bbbb", "baba", "aabb", "a", "ab")
#' grepl_letters(x, "ab")
#' grepl_letters(x, "_ab")
#' grepl_letters(x, "a_a")
#' grepl_letters(x, "c")
#' @export
grepl_letters <- function(x, pattern, blank = "_") {

  # When no black tile, use simple grepl function
  if (!grepl(blank, pattern)) {
    return(grepl(pattern, x, fixed = TRUE))
  }

  tiles_order <- strsplit(tolower(pattern), "")[[1]]
  ntiles <- length(tiles_order)

  words_tiles <- lapply(tolower(x), function(x) strsplit(x, "")[[1]])

  lapply(words_tiles, function(x) {
    nchars <- length(x)
    # When less tiles than strings characters, skip
    if (ntiles > nchars) {
      return(FALSE)
    }
    possible_possitions <- nchars - ntiles + 1
    combs <- data.frame(x = x)
    for (i in 1:possible_possitions) {
      vec <- rep(NA, nrow(combs))
      vec[i:(i + ntiles - 1)] <- tiles_order
      combs[, 1 + i] <- vec
    }
    combs <- replaceall(combs, "_", NA)
    for (i in 2:ncol(combs)) {
      temp <- select(combs, 1, i) %>% removenarows(all = FALSE)
      if (all(temp[1] == temp[2])) {
        return(TRUE)
      }
    }
    return(FALSE)
  }) %>% unlist()
}


####################################################################
#' Scrabble: Highest score words finder
#'
#' Find highest score words given a set of letters, rules, and
#' language to win at Scrabble! You just have to find the best
#' place to post your tiles.
#'
#' @family Scrabble
#' @param tiles Character. The letters you wish to consider.
#' @param free Integer. How many free blank tiles you have?
#' @param force_start,force_end Character. Force words to start or end with
#' a pattern of letters and position. Examples: "S" or "SO" or "__S_O"...
#' If the string contains tiles that were not specified in \code{tiles}, they
#' will automatically be included.
#' @param force_str Character vector. Force words to contain strings.
#' If the string contains tiles that were not specified in \code{tiles}, they
#' will automatically be included.
#' @param force_n,force_max Integer. Force words to be n or max n characters
#' long. Leave 0 to ignore parameter.
#' @param scores,language Character. Any of "en","es","de","fr".
#' If scores is not any of those languages, must be a data.frame that
#' contains two columns: "tiles" with every letter of the alphabet and
#' "scores" for each letter's score. If you wish
#' to overwrite or complement this dictionaries other words you can set to
#' \code{"none"} and/or use the \code{words} parameter.
#' You might also want to set this parameter globally with
#' \code{Sys.setenv("LARES_LANG" = "en")} and forget about it!
#' @param words Character vector. Use if you wish to manually add words.
#' @param quiet Boolean. Do not print words as they are being searched.
#' @return data.frame with matching words found, sorted by higher points.
#' @examples
#' \donttest{
#' # Automatic use of languages and scores
#' Sys.setenv("LARES_LANG" = "es")
#' scrabble_words(
#'   tiles = "hola",
#'   free = 2,
#'   force_start = "h",
#'   # force_end = "",
#'   force_str = "_o_a",
#'   force_n = 5,
#'   force_max = 0,
#'   quiet = TRUE
#' )
#'
#' # Words considered for a language (you can custom it too!)
#' es_words <- scrabble_dictionary("es")
#' }
#' @export
scrabble_words <- function(tiles,
                           free = 0,
                           force_start = "",
                           force_end = "",
                           force_str = "",
                           force_n = 0,
                           force_max = 0,
                           scores = Sys.getenv("LARES_LANG"),
                           language = Sys.getenv("LARES_LANG"),
                           words = NA,
                           quiet = FALSE) {
  if (is.data.frame(scores)) {
    if (!colnames(scores) %in% c("tiles", "scores")) {
      stop("Please, provide a valid scores data.frame with 'tiles' and 'scores' columns")
    }
  } else {
    scores <- scrabble_points(scores)
  }

  dictionary <- scrabble_dictionary(language)[, 1]
  if (!is.na(words)) {
    message(paste(">>> Added", formatNum(length(words), 0), "custom words"))
    dictionary <- c(words, dictionary)
  }
  words <- dictionary

  # Split letters
  tiles <- tolower(unlist(strsplit(tiles, "")))
  # Add free letters/tiles
  if (free > 0) tiles <- c(tiles, rep("_", free))
  # Add logical tiles when using force_ arguments
  tiles <- .add_letters(force_start, tiles)
  tiles <- .add_letters(force_end, tiles)
  ntiles <- as.integer(length(tiles))

  # Words can't have more letters than inputs
  words <- words[nchar(words) <= ntiles]
  # You may want to force their lengths
  if (force_n > 0) words <- words[nchar(words) == force_n]
  if (force_max > 0) words <- words[nchar(words) <= force_max]

  # Words can't have different letters than inputs (unless there are free tiles)
  if (free == 0) words <- words[grepl(v2t(tiles, sep = "|", quotes = FALSE), words)]
  # Force start/end strings
  words <- .force_words(words, force_start)
  words <- .force_words(.reverse(words), .reverse(force_end), rev = TRUE)

  # Force strings that must be contained
  if (force_str[1] != "") {
    for (str in force_str) {
      words <- words[grepl_letters(words, pattern = tolower(str), blank = "_")]
    }
  }

  if (length(word) > 0) {
    done <- scrabble_score(words, scores)
    return(as_tibble(done))
  } else {
    message("No words found with set criteria!")
  }
}

.force_words <- function(words, pattern, rev = FALSE, invert = FALSE) {
  forced <- tolower(unlist(strsplit(pattern, "")))
  forced_which <- which(forced != "_")
  for (i in forced_which) {
    these <- substr(words, i, i) == forced[i]
    if (invert) these <- !these
    words <- words[these]
  }
  if (rev) words <- .reverse(words)
  return(words)
}

.reverse <- function(words) {
  splits <- sapply(words, function(x) strsplit(x, ""))
  reversed <- lapply(splits, rev)
  words <- as.vector(unlist(lapply(reversed, function(x) paste(x, collapse = ""))))
  return(words)
}

.add_letters <- function(str, tiles) {
  if (str != "") {
    str_tiles <- tolower(unlist(strsplit(str, "")))
    which <- !str_tiles %in% c(tiles, "_")
    if (any(which)) {
      new <- str_tiles[which]
      tiles <- c(tiles, new)
      message(sprintf(
        ">>> %s %s not in your tiles; included!",
        v2t(new, and = "and"),
        ifelse(length(new) > 1, "were", "was")
      ))
    }
  }
  return(tiles)
}

# library(lares)
# library(dplyr)
# library(stringr)
# Sys.setenv("LARES_LANG" = "es")
#
# words <- scrabble_dictionary("es")$words
#
# # Play and win!
# scrabble_words(tiles = "abcdef",
#                free = 0,
#                force_start = "be",
#                force_end = "do",
#                force_str = "n",
#                force_n = 0,
#                force_max = 0,
#                quiet = FALSE)
#
# x <- scrabble_score(words, scores)
