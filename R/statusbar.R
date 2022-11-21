####################################################################
#' Progressive Status Bar (Loading)
#'
#' This function lets the user view a progressbar for a 'for' loop.
#'
#' @family Tools
#' @inheritParams h2o_automl
#' @param run Iterator. for loop or an integer with the current loop number.
#' Start with 1 preferibly
#' @param max.run Number. Maximum number of loops
#' @param label String. With additionaly information to be printed
#' at the end of the line. The default is \code{run}.
#' @param msg Character. Finish message.
#' @param type Character. Loading type style: equal, domino, sword, filled.
#' @param start_time POSIXct. Start time to consider. If NA, then
#' when first iteration starts will be set as start time. Useful
#' for when first iteration is showed as done but started a few
#' seconds/minutes ago.
#' @param multiples Integer. Only print when multiples of N (to avoid)
#' wasting resources on fast and lots of iterations.
#' @return No return value, called for side effects.
#' @examples
#' for (i in 1:9) {
#'   statusbar(i, 9, multiples = 2)
#'   Sys.sleep(0.3)
#' }
#' @export
statusbar <- function(run = 1, max.run = 100, label = run, msg = "",
                      type = Sys.getenv("LARES_STATUSBAR"),
                      start_time = NA, multiples = 1, alarm = FALSE) {
  if (run == 1 && is.na(start_time)) tic("startclock")
  if (!is.na(start_time)) tic("startclock", start = start_time)

  if (multiples > 1) {
    if (!run %% multiples == 0 && run != max.run) {
      return()
    }
  }
  if (length(run) > 1 && !is.numeric(run)) {
    stop("Parameter 'run' must be a numerical value!")
  }
  if (length(max.run) == 0 && !is.numeric(run)) {
    stop("Parameter 'max.run' needs to be greater than 0!")
  }

  width.set <- getOption("width") * 0.5
  width.labs <- getOption("width") - width.set - 15

  if (length(max.run) > 1) {
    percent <- which(run == max.run) / length(max.run)
  } else {
    percent <- run / max.run
  }

  if (type == "domino") syms <- list(first = "|", middle = "/", last = "_")
  if (type == "equal") syms <- list(first = " ", middle = "=", last = "=")
  if (type == "sword") syms <- list(first = " ", middle = ">", last = ":")
  if (type == "filled") syms <- list(first = " ", middle = "\u2588", last = "\u2588")

  percent.step <- trunc(percent * width.set, 5)
  part_done <- paste0(rep(syms$last, percent.step), collapse = "")
  part_middle <- if (percent == 1) syms$last else syms$middle
  part_left <- paste0(rep(syms$first, width.set - percent.step), collapse = "")
  perc <- signif(percent * 100, 3)
  if (percent == 1 && msg != "") label <- msg
  parts <- ifelse(label != "", paste("|", label), label)
  parts <- stringr::str_pad(parts, width = width.labs, pad = " ", side = "right")

  now <- toc("startclock", quiet = TRUE, type = "clock")$time
  progress <- glued("{now} [{part_done}{part_middle}{part_left}] {perc}% {parts}")
  if (percent == 1) progress <- paste(progress, "\n")
  cat("\r", progress)
  flush.console()

  if (alarm) {
    try_require("beepr", stop = FALSE)
    try(beep())
  }
}
