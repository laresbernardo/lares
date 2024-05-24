####################################################################
#' Compare Variables with their Distributions
#'
#' Compare the distribution of a target variable vs another variable. This
#' function automatically splits into quantiles for numerical variables.
#' Custom and tidyverse friendly.
#'
#' @family Exploratory
#' @family Visualization
#' @param data Dataframe
#' @param ... Variables. Main (target variable) and secondary (values
#' variable) to group by (if needed).
#' @param type Integer. 1 for both plots, 2 for counter plot only, 3 for
#' percentages plot only.
#' @param ref Boolean. Show a reference line if levels = 2? Quite useful
#' when data is unbalanced (not 50/50) because a reference line is drawn.
#' @param note Character. Caption for the plot.
#' @param top Integer. Filter and plot the most n frequent for categorical values.
#' @param breaks Integer. Number of splits for numerical values.
#' @param na.rm Boolean. Ignore \code{NA}s if needed.
#' @param force Character. Force class on the values data. Choose between 'none',
#' 'character', 'numeric', 'date'
#' @param trim Integer. Trim labels until the nth character for categorical values
#' (applies for both, target and values)
#' @param clean Boolean. Use \code{cleanText()} for categorical values (applies
#' for both, target and values)
#' @param abc Boolean. Do you wish to sort by alphabetical order?
#' @param custom_colours Boolean. Use custom colours function?
#' @param plot Boolean. Return a plot? Otherwise, a table with results
#' @param chords Boolean. Use a chords plot?
#' @param save Boolean. Save the output plot in our working directory
#' @param subdir Character. Into which subdirectory do you wish to save the plot to?
#' @return Plot when \code{plot=TRUE} with two plots in one: counter distribution
#' grouped by cuts, and proportions distribution grouped by same cuts. data.frame when
#' \code{plot=FALSE} with counting, percentages, and cumulative percentages results.
#' When \code{type} argument is used, single plots will be returned.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dft) # Titanic dataset
#'
#' # Relation for categorical/categorical values
#' distr(dft, Survived, Sex)
#'
#' # Relation for categorical/numeric values
#' dft %>%
#'   distr(Survived, Fare, plot = FALSE) %>%
#'   head(10)
#' # Sort values
#' dft %>% distr(Survived, Fare, abc = TRUE)
#' # Less splits/breaks
#' dft %>% distr(Survived, Fare, abc = TRUE, breaks = 5)
#'
#' # Distribution of numerical only
#' dft[dft$Fare < 20, ] %>% distr(Fare)
#'
#' # Distribution of numerical/numerical
#' dft %>% distr(Fare, Age)
#'
#' # Select only one of the two default plots of distr()
#' dft %>% distr(Survived, Age, type = 2)
#' dft %>% distr(Survived, Age, type = 3)
#' @export
distr <- function(data, ...,
                  type = 1,
                  ref = TRUE,
                  note = NA,
                  top = 10,
                  breaks = 10,
                  na.rm = FALSE,
                  force = "none",
                  trim = 0,
                  clean = FALSE,
                  abc = FALSE,
                  custom_colours = FALSE,
                  plot = TRUE,
                  chords = FALSE,
                  save = FALSE,
                  subdir = NA) {
  # # To handle scientific notation inputs correctly
  # on.exit(options("scipen" = 999))

  data <- data.frame(data)

  vars <- enquos(...)
  var1 <- vars[[1]]
  if (length(vars) > 1) {
    var2 <- vars[[2]]
  } else {
    var2 <- NULL
  }

  # When we only have one variable, use freqs()
  if (length(vars) == 1) {
    value <- select(data, !!var1)
    variable_name <- colnames(value)
    value <- value[, 1]
    value <- .force_class(value, force)
    value <- .fxtrim(value, trim)
    value <- .fxclean(value, clean)

    df <- data.frame(value = value, dummy = 0)
    df <- .fxna_rm(df, na.rm)

    is.Date <- function(x) inherits(x, "Date")
    is.POSIXct <- function(x) inherits(x, "POSIXct")
    is.POSIXlt <- function(x) inherits(x, "POSIXlt")
    if (is.numeric(value) || is.Date(value) || is.POSIXct(value) || is.POSIXlt(value)) {
      # Continuous and date values
      if (is.numeric(value)) {
        p <- ggplot(df, aes(x = .data$value))
      } else {
        p <- ggplot(df, aes(x = date(.data$value)))
      }
      p <- p +
        geom_density(fill = "deepskyblue", alpha = 0.7, adjust = 1 / 3) +
        labs(
          y = NULL, x = NULL, fill = "Density",
          title = "Density Distribution",
          subtitle = paste("Variable:", variable_name),
          caption = paste("Obs:", formatNum(nrow(df), 0))
        ) +
        theme_lares()
      if (top != 10) {
        p <- p + xlim(0, top)
      }
    } else {
      # Discrete values
      p <- freqs(df, value, plot = TRUE, variable_name = variable_name, abc = abc, top = top)
    }
    # Return table with results?
    if (!plot) {
      output <- df %>% freqs(value, top = top)
      return(output)
    }
    return(p)
  }

  # Check if secondary variable exists and fix if possible
  var <- gsub('"', "", as_label(var2))
  if (!var %in% colnames(data)) {
    msg <- paste("Not a valid input:", var, "was transformed or does not exist.")
    maybes <- colnames(data)[grepl(var, colnames(data))]
    if (length(maybes) > 0 && maybes[1] %in% colnames(data)) {
      message(paste0(
        "Maybe you meant one of: ", vector2text(maybes), ". ",
        "Automatically using '", maybes[1], "'"
      ))
      var2 <- quos(maybes[1])
      warning(msg)
    } else {
      stop(msg)
    }
  }

  targets <- select(data, !!var1)
  targets_name <- colnames(targets)
  targets <- targets[, 1]
  value <- select(data, !!var2)
  variable_name <- colnames(value)
  # Transformations
  value <- value[, 1] # do.call("c", value)
  value <- .force_class(value, force)
  value <- .fxtrim(value, trim)
  value <- .fxclean(value, clean)

  if (length(targets) != length(value)) {
    message("The targets and value vectors should be the same length.")
    stop(message(paste(
      "Currently, targets has", length(targets),
      "rows and value has", length(value)
    )))
  }

  # For num-num distributions or too many unique target variables
  if (length(unique(targets)) >= 8) {
    if (is.numeric(targets) && is.numeric(value)) {
      subtitle <- paste0(
        "Variables: ", variable_name, " vs. ", targets_name,
        ". Obs: ", formatNum(length(value), 0)
      )
      df <- data.frame(x = targets, y = value)
      df <- .fxna_rm(df, na.rm = TRUE)
      p <- df %>%
        ggplot(aes(x = .data$x, y = .data$y)) +
        stat_density_2d(aes(fill = after_stat(.data$level)), geom = "polygon") +
        labs(
          title = "2D Density Distribution",
          x = targets_name, y = variable_name,
          subtitle = subtitle
        ) +
        scale_x_comma() +
        scale_y_comma() +
        theme_lares()
      return(p)
    }
    message("You should try a 'target' variable with max 8 different values.")
    message("Automatically trying a chords plot...")
    chords <- TRUE
  }

  # Chords plot
  if (chords) {
    df <- data.frame(value = value, targets = targets)
    output <- freqs(df, targets, value)
    if (!na.rm) {
      output <- output %>% replaceall(NA, "NA")
    }
    title <- "Frequency Chords Diagram"
    subtitle <- paste("Variables:", targets_name, "to", variable_name)
    if (!plot) {
      return(output)
    }
    return(plot_chord(
      output$targets, output$value, output$n,
      mg = 13, title = title, subtitle = subtitle
    ))
  }

  # Only n numeric values, really numeric?
  if (is.numeric(value) && length(unique(value)) <= 8) {
    value <- .force_class(value, class = "char")
  }

  # Turn numeric variables into quantiles
  if (is.numeric(value)) {
    breaks <- ifelse(top != 10, top, breaks)
    value <- quants(value, breaks, return = "labels")
    cuts <- length(unique(value[!is.na(value)]))
    if (cuts != breaks) {
      message(paste(
        "When dividing", variable_name, "into", breaks, "quantiles,",
        cuts, "cuts/groups are possible."
      ))
    }
    top <- top + 1
  }

  # Finally, we have our data.frame
  df <- data.frame(targets = targets, value = value)
  df <- .fxna_rm(df, na.rm)

  # Captions for plots
  subtitle <- paste0(
    "Variables: ", targets_name, " vs. ", variable_name,
    ". Obs: ", formatNum(nrow(df), 0)
  )

  freqs <- df %>%
    group_by(.data$targets, .data$value) %>%
    count() %>%
    ungroup() %>%
    arrange(desc(.data$n)) %>%
    group_by(.data$value) %>%
    mutate(
      p = round(100 * .data$n / sum(.data$n), 2),
      pcum = cumsum(.data$p)
    ) %>%
    ungroup() %>%
    filter(!is.na(.data$value)) %>%
    mutate(
      row = row_number(),
      order = suppressWarnings(ifelse(
        grepl("\\(|\\)", .data$value),
        as.numeric(as.character(substr(gsub(",.*", "", .data$value), 2, 100))),
        .data$row
      ))
    )
  if (length(unique(value)) > top && !is.numeric(value)) {
    message(paste("Filtering the", top, "most frequent values. Use 'top' to overrule."))
    which <- freqs(df, .data$value) %>% slice(1:top)
    freqs <- freqs %>%
      mutate(value = ifelse(.data$value %in% which$value, as.character(.data$value), "OTHERS")) %>%
      group_by(.data$value, .data$targets) %>%
      select(-.data$row, -.data$order) %>%
      summarise(n = sum(.data$n)) %>%
      mutate(p = round(100 * n / sum(.data$n), 2)) %>%
      ungroup() %>%
      arrange(desc(.data$n)) %>%
      mutate(
        row = row_number(),
        order = row_number()
      )
  }

  # Sort values alphabetically or ascending if numeric
  if (abc) freqs <- mutate(freqs, order = rank(.data$value))

  # Counter plot
  if (type %in% c(1, 2)) {
    vadj <- ifelse(type == 1, -0.15, 0.5)
    hadj <- ifelse(type == 1, 0.5, -0.15)
    count <- ggplot(freqs, aes(
      x = reorder(as.character(.data$value), .data$order), y = .data$n,
      fill = as.character(.data$targets),
      label = formatNum(.data$n, 0), ymax = max(.data$n) * 1.1
    )) +
      geom_col(position = "dodge", colour = "transparent") +
      geom_text(
        colour = "black",
        check_overlap = TRUE,
        position = position_dodge(0.9),
        size = 3, vjust = vadj, hjust = hadj
      ) +
      labs(x = NULL, y = "Counter [#]", fill = targets_name, caption = note) +
      theme(legend.position = "top") +
      guides(colour = "none") +
      theme(axis.title.y = element_text(size = rel(0.8), angle = 90)) +
      scale_y_comma(expand = c(0, 0)) +
      theme_lares(pal = 1)
    # Give an angle to labels when more than...
    if (length(unique(value)) >= 7) {
      count <- count + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
    # Custom colours if wanted...
    if (custom_colours) {
      count <- count + suppressWarnings(gg_fill_customs())
    }
  }

  # Proportions (%) plot
  if (type %in% c(1, 3)) {
    prop <- freqs %>%
      group_by(.data$value) %>%
      mutate(size = sum(.data$n) / sum(freqs$n)) %>%
      mutate(ptag = ifelse(p < 3, "", as.character(round(.data$p, 1)))) %>%
      ggplot(aes(
        x = reorder(.data$value, -.data$order),
        y = .data$p / 100, label = .data$ptag,
        fill = as.character(.data$targets)
      )) +
      geom_col(position = "fill", colour = "transparent") +
      geom_text(aes(size = .data$size, colour = as.character(.data$targets)),
        position = position_stack(vjust = 0.5)
      ) +
      scale_size(range = c(2.2, 3)) +
      coord_flip() +
      labs(x = "Proportions [%]", y = NULL, fill = targets_name, caption = note) +
      theme(legend.position = "top") +
      guides(colour = "none", size = "none") +
      scale_y_percent(expand = c(0, 0)) +
      theme(axis.title.y = element_text(size = rel(0.8), angle = 90)) +
      theme_lares(pal = 1)

    # Show a reference line if levels = 2; quite useful when data is unbalanced (not 50/50)
    if (length(unique(targets)) == 2 && ref) {
      distr <- df %>%
        freqs(.data$targets) %>%
        arrange(as.character(.data$targets))
      h <- signif(100 - distr$p[1], 3)
      prop <- prop +
        geom_hline(
          yintercept = h / 100, colour = "purple",
          linetype = "dotted", alpha = 0.8
        ) +
        geom_label(aes(0, h / 100, label = h, vjust = -0.05),
          size = 2.5, fill = "white", alpha = 0.8
        )
    }
    # Custom colours if wanted...
    if (custom_colours) {
      prop <- prop + suppressMessages(gg_fill_customs())
    }
  }

  # Export file name and folder
  if (save) {
    file_name <- paste0(
      "viz_distr_",
      cleanText(targets_name), ".vs.",
      cleanText(variable_name),
      case_when(type == 2 ~ "_c", type == 3 ~ "_p", TRUE ~ ""), ".png"
    )
    if (!is.na(subdir)) {
      # dir.create(file.path(getwd(), subdir), recursive = TRUE)
      file_name <- paste(subdir, file_name, sep = "/")
    }
  }

  # Plot the results and save if needed
  if (type == 1) {
    count <- count + labs(
      title = "Distribution and Proportions",
      subtitle = subtitle, caption = ""
    ) +
      theme(plot.margin = margin(10, 15, -15, 15))
    prop <- prop + guides(fill = "none") + labs(caption = note) +
      theme(plot.margin = margin(-5, 15, -15, 15))
    p <- (count / prop) + plot_layout(ncol = 1, nrow = 2)
    if (save) p <- p + ggsave(file_name, width = 10, height = 7)
  }
  if (type == 2) {
    count <- count + coord_flip() +
      labs(title = "Distribution Plot", subtitle = subtitle, caption = "")
    if (save) count <- count + ggsave(file_name, width = 8, height = 6)
    p <- count
  }
  if (type == 3) {
    prop <- prop + labs(title = "Proportions Plot", subtitle = subtitle, caption = "")
    if (save) prop <- prop + ggsave(file_name, width = 8, height = 6)
    p <- prop
  }

  if (!plot) {
    return(select(freqs, -.data$order, -.data$row))
  } else {
    return(p)
  }
}

.force_class <- function(value, class = "none") {
  if (class != "none") {
    if (grepl("char|fact", class) && is.numeric(value)) {
      value <- as.character(value)
    }
    if (grepl("num|int", class) && !is.numeric(value)) {
      value <- as.numeric(value)
    }
    if (grepl("dat|day|time", class)) {
      value <- gsub(" .*", "", as.character(value))
      value <- lubridate::date(value)
    }
  }
  return(value)
}

.fxtrim <- function(value, trim, targets = NA) {
  if (trim > 0) {
    if (!is.numeric(value)) {
      value <- substr(value, 1, trim)
    }
    if (!is.numeric(targets) && !is.na(targets)) {
      targets <- substr(targets, 1, trim)
    }
    message(paste("Chopping everything to", trim, "characters..."))
  }
  return(value)
}

.fxclean <- function(value, clean = FALSE, targets = NA) {
  if (clean) {
    if (!is.numeric(value)) {
      value <- cleanText(value, spaces = FALSE)
    }
    if (!is.numeric(targets) && !is.na(targets)) {
      targets <- cleanText(targets, spaces = FALSE)
    }
  }
  return(value)
}

.fxna_rm <- function(df, na.rm = FALSE) {
  if (na.rm) {
    df <- df[complete.cases(df), ]
  }
  return(df)
}

# options(lifecycle_repeat_warnings = TRUE)
# library(dplyr)
# data("starwars")
# foo <- function(x, ...) {
#   temp <- enquos(...)
#   group_by(x, !!!temp[[1]]) %>% tally()
# }
# foo(starwars, sex, gender)

# Unquoting language objects with `!!!` is deprecated as of rlang 0.4.0.
# Please use `!!` instead.
#
# # Bad:
# dplyr::select(data, !!!enquo(x))
#
# # Good:
# dplyr::select(data, !!enquo(x))    # Unquote single quosure
# dplyr::select(data, !!!enquos(x))  # Splice list of quosures
