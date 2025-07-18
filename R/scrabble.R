####################################################################
#' Scrabble: Dictionaries
#'
#' Download words from 4 different languages: English, Spanish,
#' German, and French. Words will be save into the \code{temp} directory.
#' This is an auxiliary function. You may want to use \code{scrabble_words}
#' directly if you are searching for the highest score words!
#'
#' @family Games
#' @param lang_dic Character. Any of "en","es","de","fr". Set to NULL
#' if you wish to skip this step (and use \code{words} parameter in
#' \code{scrabble_words} instead).
#' @return data.frame with words and language columns.
#' @export
#' @rdname scrabble
scrabble_dictionary <- function(lang_dic, quiet = FALSE) {
  if (is.null(lang_dic)) {
    invisible(NULL)
  } else {
    if (length(lang_dic) != 1) {
      stop("Select only 1 language at a time...")
    }
    check_opts(lang_dic, c("en", "es", "de", "fr"))
    if (cache_exists(lang_dic)) {
      words <- cache_read(lang_dic, quiet = quiet)
      if (!quiet) {
        message(sprintf(
          ">>> Loaded %s '%s' words",
          formatNum(nrow(words), 0), lang_dic
        ))
      }
      words
    } else {
      if (!haveInternet()) {
        message("No internet connetion...")
        invisible(NULL)
      } else {
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
        words
      }
    }
  }
}


####################################################################
#' Scrabble: Word Scores
#'
#' Get score for any word or list of words. You may set manually depending
#' on the rules and languages you are playing with. Check the examples
#' for Spanish and English values when I played Words With Friends.
#'
#' @param words Character vector. Words to score
#' @param scores.df Dataframe. Must contain two columns: "tiles" with every
#' letter of the alphabet and "scores" for each letter's score.
#' @return data.frame with word, scores, and length values for each \code{word}.
#' @examples
#' \donttest{
#' if (haveInternet()) {
#'   # For Spanish words (default)
#'   es_scores <- scrabble_points("es")
#'   # Custom scores for each letter
#'   cu_scores <- data.frame(
#'     tiles = tolower(LETTERS),
#'     scores = c(
#'       1, 1, 1, 1, 1, 1, 1, 5, 1, 1, 5, 2, 4, 2, 1,
#'       4, 10, 1, 1, 1, 2, 5, 4, 8, 3, 10
#'     )
#'   )
#'
#'   # Score values for each set of rules
#'   words <- c("Bernardo", "Whiskey", "R is great")
#'   scrabble_score(words, es_scores)
#'   scrabble_score(words, cu_scores)
#' }
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
  data.frame(word = words, scores) %>%
    mutate(length = nchar(word)) %>%
    arrange(desc(scores))
}


####################################################################
#' Scrabble: Tiles Points
#'
#' Dataframe for every letter and points given a language.
#'
#' @param lang Character. Any of "en","es" or "chars". Set to NULL
#' if you wish to skip this step (and use \code{words} parameter in
#' \code{scrabble_words()} instead). The "chars" parameter will
#' score the number of characters a word has.
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
    invisible(NULL)
  } else {
    if (!lang %in% c("en", "es", "chars", "unique")) {
      message("There are no points structure for this language/system yet")
      invisible(NULL)
    } else {
      if (lang == "es") {
        scores <- data.frame(
          tiles = c(
            tolower(LETTERS)[1:14], intToUtf8(241),
            tolower(LETTERS)[15:length(LETTERS)]
          ),
          scores = c(
            1, 3, 2, 2, 1, 4, 3, 4, 1, 8, 10, 1, 3, 1,
            8, 1, 3, 5, 1, 1, 1, 2, 4, 10, 10, 5, 10
          )
        )
      }
      if (lang == "en") {
        scores <- data.frame(
          tiles = tolower(LETTERS),
          scores = c(
            1, 4, 4, 2, 1, 4, 3, 3, 1, 10, 5, 2, 4,
            2, 1, 4, 10, 1, 1, 1, 2, 5, 4, 8, 3, 10
          )
        )
      }
      if (lang %in% c("chars", "unique")) {
        scores <- data.frame(
          tiles = tolower(LETTERS)
        ) %>% mutate(scores = 1)
      }
      message(sprintf(">>> Points system: '%s'", lang))
      scores
    }
  }
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
  # If no blank tile, use simple grepl
  if (!grepl(blank, pattern, fixed = TRUE)) {
    grepl(pattern, x, fixed = TRUE)
  } else {
    tiles <- strsplit(tolower(pattern), "")[[1]]
    n_tiles <- length(tiles)
    sapply(x, function(word) {
      chars <- strsplit(tolower(word), "")[[1]]
      n_chars <- length(chars)
      if (n_tiles > n_chars) {
        FALSE
      } else {
        possible_positions <- n_chars - n_tiles + 1
        any(sapply(0:(possible_positions - 1), function(offset) {
          slice <- chars[(1 + offset):(offset + n_tiles)]
          all(tiles == blank | tiles == slice)
        }))
      }
    })
  }
}

####################################################################
#' Scrabble: Highest score words finder
#'
#' Find highest score words given a set of letters, rules, and
#' language to win at Scrabble! You just have to find the best
#' place to post your tiles.
#'
#' @param tiles Character. The letters you wish to consider.
#' @param free Integer. How many free blank tiles you have?
#' @param force_start,force_end Character. Force words to start or end with
#' a pattern of letters and position. Examples: "S" or "SO" or "__S_O"...
#' If the string contains tiles that were not specified in \code{tiles}, they
#' will automatically be included.
#' @param force_str Character vector. Force words to contain strings.
#' If the string contains tiles that were not specified in \code{tiles}, they
#' will automatically be included.
#' @param force_exclude,exclude_here Character vector. Exclude words containing
#' these tiles (and positions). Not very relevant on Scrabble but for Wordle.
#' @param force_n,force_max Integer. Force words to be n or max n characters
#' long. Leave 0 to ignore parameter.
#' @param pattern Character string. Custom regex patterns you'd like to match.
#' @param repeated Boolean. By default, no replacement allowed. When activated,
#' a single tile can be repeated and won't be "used and discarded".
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
#' @param print Boolean. Print how many words are left by step.
#' @return data.frame with matching words found, sorted by higher points.
#' @examples
#' \donttest{
#' if (haveInternet()) {
#'   # Automatic use of languages and scores
#'   Sys.setenv("LARES_LANG" = "es")
#'   scrabble_words(
#'     tiles = "hola",
#'     free = 2,
#'     force_start = "h",
#'     force_n = 4,
#'     force_str = "_o_a",
#'     exclude_here = "__z|j"
#'   )
#'
#'   wordle <- c("board", "tempo", "shoes", "hoard")
#'   scrabble_words(
#'     language = NULL,
#'     words = wordle,
#'     force_n = 5,
#'     force_str = "O_R"
#'   )
#'
#'   # Words considered for a language (you can custom it too!)
#'   es_words <- scrabble_dictionary("es")
#' }
#' }
#' @export
#' @rdname scrabble
scrabble_words <- function(tiles = "",
                           free = 0,
                           force_start = "",
                           force_end = "",
                           force_str = "",
                           force_exclude = "",
                           exclude_here = "",
                           force_n = 0,
                           force_max = 0,
                           pattern = "",
                           repeated = FALSE,
                           language = Sys.getenv("LARES_LANG"),
                           scores = language,
                           words = NULL,
                           quiet = FALSE,
                           print = TRUE) {
  ### POINTS

  tiles <- paste(tiles, collapse = "")
  if (is.data.frame(scores)) {
    if (!all(colnames(scores) %in% c("tiles", "scores"))) {
      stop("Please, provide a valid scores data.frame with 'tiles' and 'scores' columns")
    }
  } else {
    scores.df <- scrabble_points(scores)
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
  if (force_n > 0 && length(tiles) < force_n) {
    tiles <- c(tiles, rep("_", times = (force_n - length(tiles))))
  }
  ntiles <- as.integer(length(tiles))
  # Exclude these tiles
  force_not <- unique(tolower(unlist(strsplit(force_exclude, ""))))

  message("Tiles: ", v2t(toupper(tiles)))

  ### WORDS

  # Consolidate dictionary
  dictionary <- scrabble_dictionary(language)[, 1]
  nwords <- length(dictionary)
  if (!is.null(words)) {
    dictionary <- unique(tolower(c(words, dictionary)))
    unique_new <- length(dictionary) - nwords
    message(sprintf(">>> Added %s custom words %s", formatNum(length(words), 0), ifelse(
      unique_new != length(words), sprintf("(%s new)", unique_new), ""
    )))
  }
  words <- tolower(dictionary)
  # Words can't have more letters than inputs by default
  if (!repeated) words <- words[nchar(words) <= ntiles]
  .temp_print(length(words))
  # Exclude specific tiles (Wordle)
  if (length(force_not) > 0) words <- words[!grepl(paste(force_not, collapse = "|"), words)]
  .temp_print(length(words))
  # You may want to force their lengths
  if (force_n > 0) words <- words[nchar(words) == force_n]
  .temp_print(length(words))
  if (force_max > 0 && !repeated) words <- words[nchar(words) <= force_max]
  .temp_print(length(words))
  # Words can't have different letters than inputs
  words <- words[.all_tiles_present(words, tiles, free = 0, repeated = repeated)]
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
  # Force custom patterns that must be contained
  if (pattern[1] != "") {
    for (str in pattern) {
      words <- words[grep(tolower(str), words, perl = TRUE)]
      .temp_print(length(words))
    }
  }
  # Exclude letters from positions (Wordle)
  if (exclude_here[1] != "") {
    for (eh in exclude_here) {
      pos_tiles <- str_split_merge(tolower(eh))
      for (i in seq_along(pos_tiles)) {
        these <- str_split(pos_tiles, "\\|")[i][[1]]
        if (!any(these %in% letters)) next
        located <- stringr::str_locate_all(words, pos_tiles[i])
        these <- !unlist(lapply(located, function(x) sum(x[, 1] == i) > 0))
        words <- words[these]
        .temp_print(length(words))
      }
    }
  }

  .temp_print(length(words), last = TRUE)
  if (length(words) > 0) {
    if ("unique" %in% scores) {
      this <- lapply(words, function(x) sum(!!str_count(x, letters)))
      done <- data.frame(word = words, scores = unlist(this)) %>%
        mutate(length = str_length(.data$word)) %>%
        arrange(desc(.data$scores), desc(.data$length))
    } else {
      done <- scrabble_score(words, scores.df)
    }
    if (sum(done$scores) == 0) done$scores <- NULL
    as_tibble(done)
  } else {
    message("No words found with set criteria")
    NULL
  }
}

.temp_print <- function(x, print = TRUE, last = FALSE) {
  if (print) if (!last) formatColoured(paste(x, "> ")) else formatColoured(paste(x, "\n"))
}

# Tile used, tile that must be skipped on next iterations (when repeated is TRUE)
.all_tiles_present <- function(words, tiles, free = 0, repeated = FALSE) {
  free <- free + sum(tiles == "_")
  fx <- ifelse(repeated, gsub, sub)
  for (x in tiles) words <- fx(x, "", words)
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
  words
}

.reverse <- function(words) {
  original_class <- class(words)
  words <- as.character(words)
  splits <- lapply(words, function(x) strsplit(x, ""))
  reversed <- lapply(splits, function(x) rev(x[[1]]))
  words <- unlist(lapply(reversed, function(x) paste(x, collapse = "")))
  class(words) <- original_class
  words
}

.add_letters <- function(str, tiles) {
  if (str[1] != "") {
    str_tiles <- tolower(unlist(strsplit(str, "")))
    # Get rid of non alpha-numeric values
    str_tiles <- str_tiles[grepl("[[:alnum:]]", str_tiles)]
    which <- !str_tiles %in% c(tiles, "_")
    if (any(which)) {
      new <- str_tiles[which]
      tiles <- c(tiles, new)
      message(sprintf(
        "%s %s not in your tiles: now included",
        v2t(toupper(new), and = "and"),
        ifelse(length(new) > 1, "were", "was")
      ))
    }
  }
  tiles
}

# x <- "AB|CDE|F|GHIJK"
str_split_merge <- function(x, sep = "|") {
  temp <- stringr::str_split(x, "")[[1]]
  seps <- which(temp == "|")
  for (i in seq_along(temp)[-length(temp)]) {
    if (!i %in% c(seps, seps - 1)) temp[i] <- paste0(temp[i], ",")
  }
  stringr::str_split(paste(temp, collapse = ""), ",")[[1]]
}

# devtools::load_all()
# library(dplyr)
# library(stringr)
# Sys.setenv("LARES_LANG" = "en")
# words <- scrabble_dictionary("en")$words
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

# scrabble_words(
#   language = "en", # SOARE
#   tiles = "O",
#   force_start = "S",
#   force_end = "",
#   force_n = 5,
#   force_exclude = "ARE",
#   exclude_here = "_O__S"
# ) %>% pull(word)
