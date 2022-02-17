####################################################################
#' Scrabble: Dictionaries
#'
#' Download words from 4 different languages: English, Spanish,
#' German, and French. Words will be save into the \code{temp} directory.
#' This is an auxiliary function. You may want to use \code{scrabble_words}
#' directly if you are searching for the highest score words!
#' 
#' @family Scrabble
#' @param lang_dic Character. Any of "en","es","de","fr". Set to NULL
#' if you wish to skip this step (and use \code{words} parameter in
#' \code{scrabble_words} instead).
#' @param quiet Boolean. Keep quiet? If not, print informative messages.
#' @return data.frame with words and language columns.
#' @examples
#' \donttest{
#' # For Spanish words
#' dictionary <- scrabble_dictionary("es")
#' }
#' @export
#' @rdname scrabble
scrabble_dictionary <- function(lang_dic, quiet = FALSE) {
  if (is.null(lang_dic)) return(invisible(NULL))
  if (length(lang_dic) != 1) {
    stop("Select only 1 language at a time...")
  }
  check_opts(lang_dic, c("en", "es", "de", "fr"))
  if (cache_exists(lang_dic)) {
    words <- cache_read(lang_dic, quiet = quiet)
    if (!quiet) message(sprintf(
      ">>> Loaded %s '%s' words",
      formatNum(nrow(words), 0), lang_dic
    ))
    return(words)
  }
  message(sprintf(
    ">>> Downloading '%s' words. Source: %s",
    lang_dic, "github.com/lorenbrichter/Words"
  ))
  url <- sprintf(
    "https://raw.githubusercontent.com/lorenbrichter/Words/master/Words/%s.txt",
    lang_dic
  )
  words <- read.table(url, col.names = "words")
  words$language <- lang_dic
  cache_write(words, lang_dic, quiet = quiet)
  if (!quiet) message(sprintf(">>> Saved (%s words) into cache", formatNum(nrow(words), 0)))
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
#' @param scores.df Dataframe. Must contain two columns: "tiles" with every
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
#' @rdname scrabble
scrabble_score <- function(words, scores.df) {
  scores <- data.frame(
    tiles = tolower(scores.df$tiles),
    scores = as.integer(scores.df$scores)
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
#' @param lang Character. Any of "en","es". Set to NULL
#' if you wish to skip this step (and use \code{words} parameter in
#' \code{scrabble_words()} instead).
#' @return data.frame with tiles and scores for each alphabet letter.
#' @examples
#' scrabble_points("es")
#' scrabble_points("en")
#' # Not yet available
#' scrabble_points("fr")
#' @export
#' @rdname scrabble
scrabble_points <- function(lang) {
  if (is.null(lang)) {
    message(">>> Skipping points schema...")
    return(invisible(NULL))
  }
  if (!lang %in% c("en", "es")) {
    message("We do not have the points for this language yet!")
    return(invisible(NULL))
  }
  if (lang == "es") {
    scores <- data.frame(
      tiles = c(
        tolower(LETTERS)[1:14], intToUtf8(241),
        tolower(LETTERS)[15:length(LETTERS)]
      ),
      scores = c(1, 3, 2, 2, 1, 4, 3, 4, 1, 8, 10, 1, 3, 1, 8, 1, 3, 5, 1, 1, 1, 2, 4, 10, 10, 5, 10)
    )
  }
  if (lang == "en") {
    scores <- data.frame(
      tiles = tolower(LETTERS),
      scores = c(1, 4, 4, 2, 1, 4, 3, 3, 1, 10, 5, 2, 4, 2, 1, 4, 10, 1, 1, 1, 2, 5, 4, 8, 3, 10)
    )
  }

  message(sprintf(">>> Loaded points for '%s'", lang))
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
#' @param force_exclude Character vector. Exclude words containing these tiles.
#' Not very useful for Scrabble but relevant for Wordle.
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
#'   force_n = 4,
#'   force_str = "_o_a"
#' )
#' 
#' wordle <- c("board", "tempo", "shoes", "hoard")
#' scrabble_words(
#'   language = NULL,
#'   words = wordle,
#'   force_n = 5,
#'   force_str = "O_R"
#' )
#'
#' # Words considered for a language (you can custom it too!)
#' es_words <- scrabble_dictionary("es")
#' }
#' @export
#' @rdname scrabble
scrabble_words <- function(tiles = "",
                           free = 0,
                           force_start = "",
                           force_end = "",
                           force_str = "",
                           force_exclude = "",
                           force_n = 0,
                           force_max = 0,
                           language = Sys.getenv("LARES_LANG"),
                           scores = language,
                           words = NA,
                           quiet = FALSE) {
  
  ### POINTS
  
  tiles <- paste(tiles, collapse = "")
  if (is.data.frame(scores)) {
    if (!all(colnames(scores) %in% c("tiles", "scores"))) {
      stop("Please, provide a valid scores data.frame with 'tiles' and 'scores' columns")
    }
  } else {
    scores <- scrabble_points(scores)
  }

  ### TILES
  
  message(">>> Setting up tiles...")
  # Split letters
  tiles <- tolower(unlist(strsplit(tiles, "")))
  # Add free letters/tiles
  if (free > 0) tiles <- c(tiles, rep("_", free))
  # Add logical tiles when using force_ arguments
  tiles <- .add_letters(force_start, tiles)
  tiles <- .add_letters(force_end, tiles)
  tiles <- .add_letters(force_str, tiles)
  # Force N letters (complete free)
  if (force_n > 0 & length(tiles) < force_n)
    tiles <- c(tiles, rep("_", times = (force_n - length(tiles))))
  ntiles <- as.integer(length(tiles))
  # Exclude these tiles
  force_not <- unique(tolower(unlist(strsplit(force_exclude, ""))))
  
  message("Tiles: ", v2t(tiles))
  
  ### WORDS
  
  # Consolidate dictionary
  dictionary <- scrabble_dictionary(language)[, 1]
  nwords <- length(dictionary)
  if (!is.na(words[1])) {
    dictionary <- unique(tolower(c(words, dictionary)))
    unique_new <- length(dictionary) - nwords
    message(sprintf(">>> Added %s custom words %s", formatNum(length(words), 0), ifelse(
      unique_new != length(words), sprintf("(%s new)", unique_new), "")
    ))
  }
  words <- dictionary
  # Words can't have more letters than inputs
  words <- words[nchar(words) <= ntiles]
  .temp_print(length(words))
  # Exclude specific tiles (Wordle)
  if (length(force_not) > 0) words <- words[!grepl(paste(force_not, collapse = "|"), words)]
  .temp_print(length(words))
  # You may want to force their lengths
  if (force_n > 0) words <- words[nchar(words) == force_n]
  .temp_print(length(words))
  if (force_max > 0) words <- words[nchar(words) <= force_max]
  .temp_print(length(words))
  # Words can't have different letters than inputs
  words <- words[.all_tiles_present(words, tiles, free = 0)]
  .temp_print(length(words))
  # Force start/end strings
  words <- .force_words(words, force_start)
  .temp_print(length(words))
  words <- .force_words(.reverse(words), .reverse(force_end), rev = TRUE)
  .temp_print(length(words))
  # Force strings that must be contained
  if (force_str[1] != "") {
    for (str in force_str) {
      words <- words[grepl_letters(words, pattern = tolower(str), blank = "_")]
      .temp_print(length(words))
    }
  }

  if (length(words) > 0) {
    done <- scrabble_score(words, scores)
    if (sum(done$scores) == 0) done$scores <- NULL
    return(as_tibble(done))
  } else {
    message("No words found with set criteria")
  }
}

.temp_print <- function(x) {
  if (FALSE) print(x)
}

# Tile used, tile that must be skipped on next iterations
.all_tiles_present <- function(words, tiles, free = 0) {
  free <- free + sum(tiles == "_")
  for (x in tiles) words <- sub(x, "", words)
  nchar(words) <= free
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
        "%s %s not in your tiles: now included",
        v2t(new, and = "and"),
        ifelse(length(new) > 1, "were", "was")
      ))
    }
  }
  return(tiles)
}

# devtools::load_all()
# library(dplyr)
# library(stringr)
# Sys.setenv("LARES_LANG" = "es")
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
# # Fix this case (not using the tiles provided)
# scrabble_words(tiles = "bernardo",
#                free = 0,
#                quiet = FALSE)
