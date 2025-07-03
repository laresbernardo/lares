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
#' random intervals between mouse movements and clicks. Default is \code{c(20, 60)}.
#' @param off_time Numeric. Decimal hour (24h format) to stop the function
#' automatically, e.g. 18.5 means 18:30 (6:30 PM). Default is \code{18.5}.
#' @param quiet Logical. If \code{TRUE}, suppresses progress messages.
#' Default is \code{FALSE}.
#' @return Invisibly returns \code{NULL} when the function exits.
#' @examples
#' \dontrun{
#' # Keep the computer awake until 8:00 PM, with mouse actions every 30 to 60 seconds
#' dont_sleep(sec_range = c(30, 60), off_time = 20)
#' # Summary of amount of daily time running function
#' dont_sleep_time()
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

#' @rdname dont_sleep
#' @export
dont_sleep_time <- function(quiet = FALSE) {
  msg <- NULL
  if (length(ls(.dont_sleep_tracker)) == 0) {
    if (!quiet) message("No time tracked yet in this session.")
  } else {
    if (!quiet) cat("Time spent running dont_sleep() per day in current session:\n")
    for (day in ls(.dont_sleep_tracker)) {
      seconds <- get(day, envir = .dont_sleep_tracker)
      hours <- floor(seconds / 3600)
      minutes <- floor((seconds %% 3600) / 60)
      secs <- round(seconds %% 60)
      msgi <- sprintf("  %s | %02d:%02d:%02d\n", day, hours, minutes, secs)
      if (!quiet) cat(msgi)
      msg <- c(msg, trimws(msgi))
    }
    msg <- data.frame(msg = msg) %>%
      mutate(
        date = as.Date(gsub(" \\| .*", "", .data$msg)),
        sleep_time = lubridate::hms(gsub(".* \\| ", "", .data$msg), roll = TRUE)
      ) %>%
      select(-1)
  }
  invisible(msg)
}

.dont_sleep_tracker <- new.env(parent = emptyenv())

.track_dont_sleep <- function(start_time) {
  end_time <- Sys.time()
  start_date <- as.Date(start_time)
  end_date <- as.Date(end_time)
  if (start_date == end_date) {
    # Same day, assign normally
    elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
    if (!exists(as.character(start_date), envir = .dont_sleep_tracker)) {
      assign(as.character(start_date), elapsed, envir = .dont_sleep_tracker)
    } else {
      previous <- get(as.character(start_date), envir = .dont_sleep_tracker)
      assign(as.character(start_date), previous + elapsed, envir = .dont_sleep_tracker)
    }
  } else {
    # Crosses midnight, split time by day
    midnight <- as.POSIXct(paste0(start_date + 1, " 00:00:00"), tz = attr(start_time, "tzone"))
    first_day_secs <- as.numeric(difftime(midnight, start_time, units = "secs"))
    second_day_secs <- as.numeric(difftime(end_time, midnight, units = "secs"))
    # Assign first day
    if (!exists(as.character(start_date), envir = .dont_sleep_tracker)) {
      assign(as.character(start_date), first_day_secs, envir = .dont_sleep_tracker)
    } else {
      previous <- get(as.character(start_date), envir = .dont_sleep_tracker)
      assign(as.character(start_date), previous + first_day_secs, envir = .dont_sleep_tracker)
    }
    # Assign second day
    if (!exists(as.character(end_date), envir = .dont_sleep_tracker)) {
      assign(as.character(end_date), second_day_secs, envir = .dont_sleep_tracker)
    } else {
      previous <- get(as.character(end_date), envir = .dont_sleep_tracker)
      assign(as.character(end_date), previous + second_day_secs, envir = .dont_sleep_tracker)
    }
  }
}

.stay_awake_core <- function(
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
  tic(paste(label, start_time))
  timeout <- moved <- FALSE

  # Ensure time is tracked even if user stops manually
  on.exit(
    {
      .track_dont_sleep(start_time)
      if (!quiet) {
        lap <- toc(paste(label, start_time), quiet = TRUE)
        now_fmt <- format(start_time, format = "%Y-%M-%d %H:%M:%S")
        cat(sprintf("\rStarted %s ago (%s)    \n", lap$time, now_fmt))
        if (moved) message("Mouse moved by user. Exiting at ", now)
        if (timeout) message("Off-time triggered at ", format(now_posix, "%Y-%m-%d %H:%M:%S"))
      }
    },
    add = TRUE
  )

  repeat {
    now <- format(Sys.time(), format = "%Y-%M-%d %H:%M:%S")
    pos <- get_mouse_position()
    if (length(pos) != 2 || any(pos != c(0, 0))) {
      moved <- TRUE
      break
    }

    lap <- toc(paste(label, start_time), quiet = TRUE)
    now_fmt <- format(start_time, format = "%Y-%M-%d %H:%M:%S")
    if (!quiet) cat(sprintf("\rStarted %s ago (%s)    ", lap$time, now_fmt))
    .track_dont_sleep(start_time)

    dx <- sample(-20:20, 1)
    dy <- sample(-20:20, 1)
    move_mouse(dx, dy)
    move_to_origin()
    click_mouse()

    now_posix <- as.POSIXlt(Sys.time())
    if ((now_posix$hour + now_posix$min / 60 + now_posix$sec / 3600) >= off_time) {
      timeout <- TRUE
      break
    }

    if (length(sec_range) == 1) sec_range <- rep(sec_range, 2)
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

  .stay_awake_core(get_mouse_position, move_mouse, click_mouse, move_to_origin,
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

  .stay_awake_core(get_mouse_position, move_mouse, click_mouse, move_to_origin,
    sec_range, off_time, quiet,
    label = "stay_awake_windows"
  )
}
