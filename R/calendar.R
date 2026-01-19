####################################################################
#' Split ICS File
#'
#' This function splits a large ICS calendar file into smaller files.
#'
#' @family Tools
#' @param file Character. Path to the ICS file to split.
#' @param n_splits Integer. Number of files to split into.
#' @param prefix Character. Prefix for the output filenames.
#' @param dir Character. Directory to save the output files.
#' Defaults to same directory as input file.
#' @return Invisible list. The paths of the created files.
#' @export
cal_split <- function(file, n_splits = 5, prefix = "calendar_split_", dir = NULL) {
  
  if (!file.exists(file)) stop("File not found: ", file)
  if (is.null(dir)) dir <- dirname(file)
  lines <- readLines(file, warn = FALSE)

  # 1. Extract the header (lines before the first VEVENT)
  header_end <- grep("BEGIN:VEVENT", lines)[1] - 1
  if (is.na(header_end)) {
    stop("No 'BEGIN:VEVENT' found in the file. Is this a valid ICS file?")
  }
  calendar_header <- lines[1:header_end]

  # 2. Extract the footer (usually "END:VCALENDAR")
  calendar_footer <- "END:VCALENDAR"

  # 3. Identify start and end positions for every event block
  event_starts <- grep("BEGIN:VEVENT", lines)
  event_ends <- grep("END:VEVENT", lines)

  if (length(event_starts) != length(event_ends)) {
    stop("Mismatch between 'BEGIN:VEVENT' and 'END:VEVENT' counts.")
  }

  if (length(event_starts) == 0) {
    stop("No events found to split.")
  }

  # Create a list where each element is a full VEVENT block
  events <- lapply(seq_along(event_starts), function(i) {
    lines[event_starts[i]:event_ends[i]]
  })

  # 4. Calculate how many events go into each of the n_splits files
  num_events <- length(events)
  events_per_file <- ceiling(num_events / n_splits)

  message(sprintf(
    "Splitting %s events into %s files (approx %s events/file)...",
    formatNum(num_events, 0), n_splits, events_per_file
  ))

  # 5. Split and save the files
  output_files <- c()
  for (i in 1:n_splits) {
    # Determine range of events for this specific file
    start_idx <- ((i - 1) * events_per_file) + 1
    end_idx <- min(i * events_per_file, num_events)

    if (start_idx <= num_events) {
      # Combine Header + Subset of Events + Footer
      current_content <- c(
        calendar_header,
        unlist(events[start_idx:end_idx]),
        calendar_footer
      )

      # Write to a new file
      output_filename <- paste0(prefix, i, ".ics")
      output_path <- file.path(dir, output_filename)
      writeLines(current_content, output_path)
      output_files <- c(output_files, output_path)
    }
  }

  # Return the list of created files
  invisible(output_files)
}
