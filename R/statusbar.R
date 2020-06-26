####################################################################
#' Progressive Status Bar (Loading)
#' 
#' This function lets the user view a progressbar for a 'for' loop. 
#' 
#' @family Tools
#' @param run Iterator. for loop or an integer with the current loop number.
#' Start with 1 preferibly
#' @param max.run Number. Maximum number of loops
#' @param label String. With additionaly information to be printed 
#' at the end of the line. The default is \code{run}.
#' @param msg Character. Finish message
#' @param type Character. Loading type style: equal, domino
#' @param start_time POSIXct. Start time to consider. If NA, then
#' when first iteration starts will be set as start time. Useful
#' for when first iteration is showed as done but started a few 
#' seconds/minutes ago.
#' @examples
# for (i in 1:10) {
#   statusbar(i, 10)
#   Sys.sleep(0.2)
# }
#' @export
statusbar <- function(run = 1, max.run = 100, label = run, 
                      msg = "DONE", type = "equal",
                      start_time = NA){
  
  if (length(run) > 1 & !is.numeric(run)) 
    stop("run must be a numerical value!")
  if (length(max.run) == 0 & !is.numeric(run)) 
    stop("max.run needs to be greater than 0!")
  
  percent.max <- getOption("width") * 0.5
  
  # formatTimeSmart <- function(x) {
  #   if (x < 60) {
  #     suffix <- "s"
  #     value <- x
  #   } else if (x < 60 * 60) {
  #     suffix <- "m"
  #     value <- x / 60
  #   } else {
  #     suffix <- "h"
  #     value <- x / (60 * 60)
  #   }
  #   return(paste0(sprintf('%.1f', value), suffix))
  # }
  
  if (run == 1 & is.na(start_time))
    options("startclock" = Sys.time())
  if (!is.na(start_time))
    options("startclock" = start_time)
  
  if (length(max.run) > 1) {
    percent <- which(run == max.run) / length(max.run)
  } else percent <- run / max.run
  
  if (type == "domino") {
    first <- "|"
    middle <- "/"
    last <- "_"
  }
  if (type == "equal") {
    first <- " "
    middle <- "="
    last <- "="
  }
  
  percent.step <- trunc(percent * percent.max, 5)
  space <- paste(rep(" ", 20), collapse = " ")
  progress <- paste0(
    "[", paste0(rep(last, percent.step), collapse = ""), 
    ifelse(percent.step != percent.max, middle, last),
    paste0(rep(first, percent.max - percent.step), collapse = ""),"] ", 
    round(percent * 100, 0), "% | ",
    paste(ifelse(run != max.run, paste(label, space), paste(
      msg,paste(rep(" ", 18), collapse = ""),"\n"))))
  
  now <- format(.POSIXct(difftime(
    Sys.time(), getOption("startclock"), units = "secs"), tz = "GMT"), "%H:%M:%S")
  flush.console()
  cat("\r", paste(now, progress))
  if (run == max.run) options("startclock" = NULL)
}
