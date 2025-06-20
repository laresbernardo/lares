####################################################################
#' Prevent Computer from Sleeping by Simulating Mouse Activity
#'
#' Keeps the computer awake by moving the mouse cursor and clicking
#' periodically with a human-like behavior. It automatically detects
#' the operating system (Windows or macOS) and runs the appropriate
#' underlying logic.
#'
#' The cursor initially moves to the upper-left corner (0,0), then
#' performs small random movements and clicks periodically to avoid
#' system sleep. The function exits if the user moves the mouse away
#' from the corner or when the specified off-time is reached.
#'
#' Requires \code{cliclick} installed on macOS (install via Homebrew:
#' \code{brew install cliclick}), and the \code{KeyboardSimulator}
#' package on Windows.
#'
#' @family Tools
#' @param sec_range Numeric vector of length 2. Range (in seconds) of
#'   random intervals between mouse movements and clicks. Default is \code{c(20, 60)}.
#' @param off_time Numeric. Decimal hour (24h format) to stop the function
#'   automatically, e.g. 18.5 means 18:30 (6:30 PM). Default is \code{18.5}.
#' @param quiet Logical. If \code{TRUE}, suppresses progress messages.
#'   Default is \code{FALSE}.
#' @return Invisibly returns \code{NULL} when the function exits.
#' @examples
#' \dontrun{
#' # Keep the computer awake until 8:00 PM, with mouse actions every 30 to 60 seconds
#' dont_sleep(sec_range = c(30, 60), off_time = 20)
#' }
#' @export
dont_sleep <- function(sec_range = c(20, 60), off_time = 18.5, quiet = FALSE) {
  os <- Sys.info()[["sysname"]]
  if (os == "Darwin") {
    stay_awake_mac(sec_range, off_time, quiet)
  } else if (os == "Windows") {
    stay_awake_windows(sec_range, off_time, quiet)
  } else {
    stop("Unsupported OS: ", os)
  }
}

stay_awake_core <- function(
    get_mouse_position,
    move_mouse,
    click_mouse,
    move_to_origin,
    sec_range = c(20, 60),
    off_time = 18.5,
    quiet = FALSE,
    label = "stay_awake") {
  move_to_origin()
  message("Mouse moved to (0,0). Starting loop...")

  start_time <- Sys.time()
  lares::tic(paste(label, start_time))

  repeat {
    now <- format(Sys.time(), format = "%Y-%M-%d %H:%M:%S")
    pos <- get_mouse_position()
    if (length(pos) != 2 || any(pos != c(0, 0))) {
      # Final update to "Started Xs ago" replacing previous line
      lap <- lares::toc(paste(label, start_time), quiet = TRUE)
      now_fmt <- format(start_time, format = "%Y-%M-%d %H:%M:%S")
      if (!quiet) cat(sprintf("\rStarted %s ago (%s)    \n", lap$time, now_fmt))

      # Then print mouse moved message
      message("Mouse moved by user. Exiting at ", now)
      break
    }

    lap <- lares::toc(paste(label, start_time), quiet = TRUE)
    now_fmt <- format(start_time, format = "%Y-%M-%d %H:%M:%S")
    if (!quiet) cat(sprintf("\rStarted %s ago (%s)    ", lap$time, now_fmt))

    # Random movement
    dx <- sample(-20:20, 1)
    dy <- sample(-20:20, 1)
    move_mouse(dx, dy)
    move_to_origin()
    click_mouse()

    # Off-time check
    now_posix <- as.POSIXlt(Sys.time())
    if ((now_posix$hour + now_posix$min / 60 + now_posix$sec / 3600) >= off_time) {
      message("\nOff-time triggered at ", now_posix)
      break
    }

    Sys.sleep(runif(1, sec_range[1], sec_range[2]))
  }

  invisible(NULL)
}

# macOS wrapper
stay_awake_mac <- function(sec_range = c(20, 60), off_time = 18.5, quiet = FALSE) {
  if (Sys.info()[["sysname"]] != "Darwin") stop("This function is for macOS only.")
  if (system("which cliclick", intern = TRUE) == "") stop("Install cliclick first: brew install cliclick")

  get_mouse_position <- function() {
    out <- system2("cliclick", args = "p:", stdout = TRUE)
    as.numeric(unlist(regmatches(out, gregexpr("[0-9]+", out))))
  }
  move_mouse <- function(dx, dy) {
    dx_str <- if (dx >= 0) paste0("+", dx) else as.character(dx)
    dy_str <- if (dy >= 0) paste0("+", dy) else as.character(dy)
    system2("cliclick", args = sprintf("m:%s,%s", dx_str, dy_str))
  }
  move_to_origin <- function() system2("cliclick", args = "m:0,0")
  click_mouse <- function() system2("cliclick", args = "c:0,0")

  stay_awake_core(get_mouse_position, move_mouse, click_mouse, move_to_origin,
    sec_range, off_time, quiet,
    label = "stay_awake_mac"
  )
}

# Windows wrapper
stay_awake_windows <- function(sec_range = c(20, 60), off_time = 18.5, quiet = FALSE) {
  if (Sys.info()[["sysname"]] != "Windows") stop("This function is for Windows only.")
  try_require("KeyboardSimulator")

  get_mouse_position <- function() mouse.get_cursor()
  move_mouse <- function(dx, dy) mouse.move(dx, dy, duration = 0)
  move_to_origin <- function() mouse.move(0, 0, duration = 0)
  click_mouse <- function() mouse.click(button = "left")

  stay_awake_core(get_mouse_position, move_mouse, click_mouse, move_to_origin,
    sec_range, off_time, quiet,
    label = "stay_awake_windows"
  )
}
