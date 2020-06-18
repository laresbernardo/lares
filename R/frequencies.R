####################################################################
#' Frequencies Calculations and Plot
#' 
#' This function lets the user group, count, calculate percentages 
#' and cumulatives. It also plots results if needed. Tidyverse friendly.
#' 
#' @family Frequency
#' @family Exploratory
#' @family Visualization
#' @param df Data.frame
#' @param ... Variables. Variables you wish to process. Order matters.
#' If no variables are passed, the whole data.frame will be considered
#' @param wt Variable, numeric. Weights.
#' @param rel Boolean. Relative percentages (or absolute)?
#' @param results Boolean. Return results in a dataframe?
#' @param variable_name Character. Overwrite the main variable's name
#' @param plot Boolean. Do you want to see a plot? Three variables tops.
#' @param rm.na Boolean. Remove NA values in the plot? (not filtered for 
#' numerical output; use na.omit() or filter() if needed)
#' @param title Character. Overwrite plot's title with.
#' @param subtitle Character. Overwrite plot's subtitle with.
#' @param top Integer. Filter and plot the most n frequent for 
#' categorical values. Set to NA to return all values
#' @param abc Boolean. Do you wish to sort by alphabetical order?
#' @param save Boolean. Save the output plot in our working directory
#' @param subdir Character. Into which subdirectory do you wish to 
#' save the plot to?
#' @examples 
#' options("lares.font"=NA) # Temporal
#' data(dft) # Titanic dataset
#' 
#' # How many survived?
#' dft %>% freqs(Survived)
#' 
#' # How many survived per Class?
#' dft %>% freqs(Pclass, Survived, abc = TRUE)
#' 
#' # How many survived per Class with relative percentages?
#' dft %>% freqs(Pclass, Survived, abc = TRUE, rel = TRUE)
#' 
#' # Using a weighted feature
#' dft %>% freqs(Pclass, Survived, wt = Fare/100)
#' 
#' # Let's check the results with plots:
#' 
#' #' # How many survived and see plot?
#' dft %>% freqs(Survived, plot = TRUE)
#' 
#' # How many survived per class?
#' dft %>% freqs(Survived, Pclass, plot = TRUE)
#' 
#' # Per class, how many survived?
#' dft %>% freqs(Pclass, Survived, plot = TRUE)
#' 
#' # Per sex and class, how many survived?
#' dft %>% freqs(Sex, Pclass, Survived, plot = TRUE)
#' 
#' # Frequency of tickets + Survived
#' dft %>% freqs(Survived, Ticket, plot = TRUE)
#' 
#' # Frequency of tickets: top 10 only and order them alphabetically
#' dft %>% freqs(Ticket, plot = TRUE, top = 10, abc = TRUE)
#' @export
freqs <- function(df, ..., wt = NULL,
                  rel = FALSE,
                  results = TRUE, 
                  variable_name = NA, 
                  plot = FALSE, rm.na = FALSE,
                  title = NA, subtitle = NA,
                  top = 20, abc = FALSE,
                  save = FALSE, subdir = NA) {
  
  vars <- quos(...)
  weight <- enquo(wt)
  
  # Probably an error but might be usefull for the user instead
  if (length(vars) == 0) {
    output <- freqs_df(df, plot = !plot, save = save, subdir = subdir)
    return(output)
  }
  
  output <- df %>% 
    group_by(!!!vars) %>% 
    tally(wt = !!weight) %>% 
    arrange(desc(.data$n)) %>%
    {if (!rel) ungroup(.) else .} %>%
    mutate(p = round(100*.data$n/sum(.data$n), 2))
  
  # Sort values alphabetically or ascending if numeric
  if (abc) {
    message("Sorting variable(s) alphabetically")
    output <- output %>% 
      arrange(!!!vars, desc(n)) %>% 
      mutate(order = row_number())
  } else {
    output <- output %>% 
      arrange(desc(.data$n)) %>%
      mutate(order = row_number())
  }
  
  # Add cumulative percentage (given the abc logic)
  output <- mutate(output, pcum = cumsum(.data$p))
  
  # No plot
  if (!plot && !save) {
    if (results) 
      return(output) 
    else 
      invisible(return())  
  }
  
  if (ncol(output) - 4 >= 4) {
    stop(
      paste("Sorry, but trying to plot more than 3 features is as complex as it sounds.",
            "You should try another method to understand your analysis!"))
  }
  obs_total <- sum(output$n)
  obs <- sprintf("Total Obs.: %s", formatNum(obs_total, 0))
  weight_text <- ifelse((as_label(weight) != "NULL"), 
                        sprintf("(weighted by %s)", as_label(weight)), "")
  
  # Use only the most n frequent values/combinations only
  values <- unique(output[,length(vars)])
  if (nrow(values) > top) {
    if (!is.na(top)) {
      output <- output %>% arrange(desc(.data$n)) %>% slice(1:top)
      message(
        sprintf("Slicing the top %s (out of %s) values; use 'top' parameter to overrule.", 
                top, nrow(values)))
      note <- sprintf("[%s most frequent]", top)
      obs <- sprintf("Obs.: %s (out of %s)", formatNum(sum(output$n), 0), formatNum(obs_total, 0))
    }
  } else {note <- ""}
  
  if (abc) note <- gsub("most frequent", "A-Z sorted values", note)
  
  reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
  }
  scale_x_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
  }
  
  plot <- ungroup(output)
  
  if (rm.na)  plot <- plot[complete.cases(plot), ]
  
  # Create some dynamic aesthetics
  plot$labels <- paste0(formatNum(plot$n, decimals = 0), " (", signif(plot$p, 4), "%)")
  plot$label_colours <- ifelse(plot$p > mean(range(plot$p)) * 0.9, "m", "f")
  lim <- 0.35
  plot$label_hjust <- ifelse(
    plot$n < min(plot$n) + diff(range(plot$n)) * lim, -0.1, 1.05)
  plot$label_colours <- ifelse(
    plot$label_colours == "m" & plot$label_hjust < lim, "f", plot$label_colours)
  variable <- colnames(plot)[1]
  
  # When one feature
  if (ncol(output) - 3 == 2) { 
    type <- 1
    colnames(plot)[1] <- "names"
    p <- ggplot(plot, aes(x = reorder(.data$names, -.data$order), 
                          y = .data$n, label = .data$labels, fill = .data$p))
  }
  # When two features
  else if (ncol(output) - 3 == 3) { 
    type <- 2
    facet_name <- colnames(plot)[2]
    colnames(plot)[1] <- "facet"
    colnames(plot)[2] <- "names"
    plot$facet <- suppressWarnings(replacefactor(plot$facet, NA, "NA"))
    p <- plot %>%
      ggplot(aes(x = reorder_within(.data$names, -.data$order, .data$facet), 
                 y = .data$n, label = .data$labels, fill = .data$p)) +
      scale_x_reordered()
  }
  # When three features
  else if (ncol(output) - 3 == 4) { 
    type <- 3
    facet_name1 <- colnames(plot)[2]
    facet_name2 <- colnames(plot)[3]
    colnames(plot)[1] <- "facet2"
    colnames(plot)[2] <- "facet1"
    colnames(plot)[3] <- "names"
    plot$facet2 <- suppressWarnings(replacefactor(plot$facet2, NA, "NA"))
    plot$facet1 <- suppressWarnings(replacefactor(plot$facet1, NA, "NA"))
    p <- plot %>%
      ggplot(aes(x = reorder_within(.data$names, .data$n, .data$facet2), 
                 y = .data$n, label = .data$labels, fill = .data$p)) +
      scale_x_reordered()
  }
  
  # Plot base
  p <- p + geom_col(alpha = 0.95, width = 0.8, colour = "transparent") +
    geom_text(aes(hjust = .data$label_hjust, colour = .data$label_colours), size = 2.8) + 
    coord_flip() + guides(colour = FALSE) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = ifelse(is.na(title), paste("Frequencies and Percentages"), title),
         subtitle = ifelse(is.na(subtitle), 
                           paste("Variable:", ifelse(!is.na(variable_name), variable_name, variable), note, weight_text), 
                           subtitle), caption = obs) +
    scale_fill_gradient(low = "lightskyblue2", high = "navy") +
    theme_lares2(legend = "none", grid = "Xx") + gg_text_customs() + 
    scale_y_continuous(position = "right", expand = c(0, 0), labels = comma,
                       limits = c(0, 1.03 * max(output$n)))
  
  # When two features
  if (type == 2) { 
    p <- p + 
      facet_grid(as.character(.data$facet) ~ ., scales = "free", space = "free") + 
      labs(subtitle = ifelse(is.na(subtitle), 
                             paste("Variables:", facet_name, "grouped by", variable, "\n", note, weight_text), 
                             subtitle),
           caption = obs)
  }
  
  # When three or more features
  else if (type >= 3) { 
    if (length(unique(facet_name2)) > 3) {
      p <- freqs_plot(df, ..., top = top, rm.na = rm.na, title = title, subtitle = subtitle)
      return(p)
    }
    p <- p + facet_grid(as.character(.data$facet2) ~ as.character(.data$facet1), scales = "free") + 
      labs(title = ifelse(is.na(title), "Frequencies and Percentages:", title),
           subtitle = ifelse(is.na(subtitle), 
                             paste("Variables:", facet_name2, "grouped by", facet_name1, "[x] and", 
                                   variable, "[y]", "\n", note, weight_text), 
                             subtitle),
           caption = obs)
  }
  
  # Export file name and folder for plot
  if (save) export_plot(p, "viz_freqs", vars, subdir = subdir)
  return(p)  
}


####################################################################
#' Plot for All Frequencies on Dataframe
#' 
#' This function lets the user analize data by visualizing the
#' frequency of each value of each column from a whole data frame.
#' 
#' @family Frequency
#' @family Exploratory
#' @family Visualization
#' @param df Data.frame
#' @param max Numeric. Top variance threshold. Range: (0-1].
#' These variables will be excluded
#' @param min Numeric. Minimum variance threshold. Range: [0-1). 
#' These values will be grouped into a high frequency (HF) value
#' @param novar Boolean. Remove no variance columns?
#' @param plot Boolean. Do you want to see a plot? Three variables tops
#' @param top Integer. Plot most relevant (less categories) variables
#' @param quiet Boolean. Keep quiet? (or show variables exclusions)
#' @param save Boolean. Save the output plot in our working directory
#' @param subdir Character. Into which subdirectory do you wish to 
#' save the plot to?
#' @examples 
#' options("lares.font" = NA) # Temporal
#' data(dft) # Titanic dataset
#' freqs_df(dft)
#' @export
freqs_df <- function(df, 
                     max = 0.9, min = 0.0, novar = TRUE,
                     plot = TRUE, top = 30,
                     quiet = FALSE,
                     save = FALSE, subdir = NA) {
  
  if (is.vector(df)) 
    return(freqs(data.frame(values = df), .data$values))
  
  df <- df[!unlist(lapply(df, is.list))]
  names <- lapply(df, function(x) length(unique(x)))
  unique <- as.data.frame(names)
  colnames(unique) <- names(names)
  which <- rownames(t(-sort(unique)))
  
  # Too much variance
  no <- names(unique)[unique > nrow(df) * max]
  if (length(no) > 0) {
    if (!quiet) message(paste(length(no), "variables with more than", max, 
                              "variance exluded:", vector2text(no)))
    which <- which[!which %in% no] 
  }
  
  # No variance at all
  if (novar) {
    no <- zerovar(df)
    if (length(no) > 0) {
      if (!quiet) message(paste(length(no), "variables with no variance exluded:", 
                                vector2text(no)))
      which <- which[!which %in% no] 
    } 
  }
  
  # Too many columns
  if (length(which) > top) {
    no <- which[(top + 1):length(which)]
    if (!quiet) message(paste("Using the", top, "variables with less distinct categories.",
                              length(no), "variables excluded:", vector2text(no)))
    which <- which[1:top]
  }
  
  if (length(which) > 0) {
    for (i in 1:length(which)) {
      if (i == 1) out <- c()
      iter <- which[i]
      counter <- table(df[iter], useNA = "ifany")
      res <- data.frame(value = names(counter), 
                        count = as.vector(counter), 
                        col = iter) %>%
        arrange(desc(.data$count))
      out <- rbind(out, res)
    } 
    out <- out %>% 
      mutate(p = round(100 * .data$count / nrow(df), 2)) %>%
      mutate(value = ifelse(.data$p > min * 100, as.character(.data$value), "(HF)")) %>%
      group_by(.data$col, .data$value) %>% 
      summarise(p = sum(.data$p), count = sum(.data$count)) %>% 
      arrange(desc(.data$count)) %>% 
      ungroup()
  } else { 
    warning("No relevant information to display regarding your data.frame!") 
    return(invisible(NULL))
  }
  
  if (plot) {
    out <-  out %>%
      mutate(value = ifelse(is.na(.data$value), "NA", as.character(.data$value))) %>%
      mutate(col = factor(.data$col, levels = which)) %>%
      mutate(label = ifelse(.data$p > 8, as.character(.data$value), "")) %>%
      group_by(.data$col) %>% 
      mutate(alpha = log(.data$count)) %>%
      mutate(alpha = as.numeric(ifelse(.data$value == "NA", 0, .data$alpha))) %>%
      mutate(alpha = as.numeric(ifelse(is.infinite(.data$alpha), 0, .data$alpha)))
    
    p <- ggplot(out, aes(x = .data$col, y = .data$count, fill = .data$col, 
                         label = .data$label, colour = .data$col)) + 
      geom_col(aes(alpha = .data$alpha), position = "fill", 
               colour = "transparent", width = 0.95, size = 0.1) + 
      geom_text(position = position_fill(vjust = .5), size = 3) +
      coord_flip() + labs(x = NULL, y = NULL, title = "Global Values Frequencies") +
      scale_y_percent(expand = c(0, 0)) +
      guides(fill = FALSE, colour = FALSE, alpha = FALSE) +
      theme_lares2(pal = 1) + 
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed",
                 color = "black", size = 0.5, alpha = 0.3)
    if (save) export_plot(p, "viz_freqs_df", subdir = subdir)
    return(p)
  } else {
    out <- select(out, .data$col, .data$value, .data$count, .data$p) %>%
      rename(n = .data$count, variable = .data$col) %>%
      group_by(.data$variable) %>%
      mutate(pcum = round(cumsum(.data$p), 2))
    return(out)
  }
}


####################################################################
#' Combinated Frequencies Plot for Categoral Features
#' 
#' Plot frequencies of multiple categories within a data.frame in 
#' a new fancy way. Tidyverse friendly, based on lares::freqs(),
#' no limits on amount of features to evaluate.
#' 
#' @family Frequency
#' @family Exploratory
#' @family Visualization
#' @param df Data.frame
#' @param ... Variables. Variables you wish to process. Order matters.
#' If no variables are passed, the whole data.frame will be considered
#' @param top Integer. Filter and plot the most n frequent for  values.
#' @param rm.na Boolean. Remove NA values in the plot? (not filtered for 
#' numerical output; use na.omit() or filter() if needed)
#' @param abc Boolean. Do you wish to sort by alphabetical order?
#' @param title Character. Overwrite plot's title with.
#' @param subtitle Character. Overwrite plot's subtitle with.
#' @examples 
#' options("lares.font" = NA) # Temporal
#' data(dft) # Titanic dataset
#' 
#' df <- freqs_plot(dft, Pclass, Survived)
#' head(df$data)
#' plot(df)
#' 
#' freqs_plot(dft, Pclass, Survived, Sex, Embarked)
#' 
#' freqs_plot(dft, Pclass, Survived, Sex, Embarked, top = 15)
#' @export
freqs_plot <- function(df, ..., top = 10, rm.na = FALSE, abc = FALSE,
                       title = NA, subtitle = NA) {
  
  vars <- quos(...)
  
  if (length(vars) == 0)
    return(freqs(df))
  
  aux <- df %>% 
    freqs(!!!vars, rm.na = rm.na) %>% 
    mutate_if(is.factor, as.character) %>%
    mutate_at(1:length(vars), as.character) %>%
    mutate(order = ifelse(.data$order > top, "...", .data$order))
  if ("..." %in% aux$order)
    message(paste(
      "Showing", top, "most frequent values. Tail of",
      nrow(aux) - top,
      "other values grouped into one"))
  for (i in 1:(ncol(aux) - 4))
    aux[,i][aux$order == "...",] <- "Tail"  
  aux <- aux %>%
    group_by(!!!vars, .data$order) %>%
    summarise_all(sum) %>%
    ungroup()
  vars_names <- colnames(aux)[1:(ncol(aux) - 4)]
  
  labels <- aux %>% 
    mutate_all(as.character) %>% 
    tidyr::pivot_longer(colnames(aux)[1:(ncol(aux) - 4)]) %>%
    mutate(label = sprintf("%s: %s", .data$name, .data$value)) %>%
    mutate(n = as.integer(.data$n), 
           pcum = as.numeric(.data$pcum),
           p = as.numeric(.data$p)) %>%
    ungroup() %>% arrange(desc(.data$pcum))
  
  if (is.na(title))
    title <- "Absolute Frequencies"
  if (is.na(subtitle))
    subtitle <- paste("Grouped by", vector2text(vars_names, quotes = FALSE))
  mg <- 5
  
  p1 <- aux %>% 
    mutate(aux = ifelse(.data$order == "...", "grey", "black")) %>%
    mutate(order = paste0(.data$order, "\n", signif(as.numeric(.data$p), 2), "%")) %>%
    ggplot(aes(x = reorder(.data$order, .data$pcum), y = .data$n, label = .data$p)) +
    geom_col(aes(fill = .data$aux)) +
    scale_fill_manual(values = c("black", "grey55")) +
    labs(x = NULL, y = NULL, title = title, subtitle = subtitle) + 
    scale_y_comma() +
    guides(fill = FALSE) +
    theme_lares2() +
    theme(plot.margin = margin(mg, mg, 0, mg))
  
  p2 <- labels %>%
    {if (abc) mutate(., n = -rank(.data$label)) else .} %>%
    mutate(label = ifelse(.data$order == "...", " Mixed Tail", .data$label)) %>%
    ggplot(aes(x = reorder(.data$order, .data$pcum), 
               y = reorder(.data$label, .data$n), 
               group = .data$pcum)) +
    geom_point(size = 4) +
    labs(x = NULL, y = NULL, 
         caption = paste("Total observations:", formatNum(nrow(df), 0))) + 
    guides(colour = FALSE) +
    theme_lares2(which = "XY") +
    theme(axis.text.x = element_blank(),
          plot.margin = margin(0, mg, mg, mg))
  if (length(vars) > 1)
    p2 <- p2 + geom_path()
  
  p <- (p1 / p2)
  attr(p, "data") <- aux
  attr(p, "labels") <- labels
  return(p)
}


####################################################################
#' Frequencies on Lists and Plot
#' 
#' Visualize frequency of elements on a list or list vector, which
#' combinations and elements are the most frequent. Your variable can be
#' a character vector with comma separated values or a list vector.
#' 
#' @family Frequency
#' @family Exploratory
#' @family Visualization
#' @param df Data.frame
#' @param var Variable. Variables you wish to process. 
#' @param wt Variable, numeric. Select a numeric column to use 
#' in the colour scale, used as sum, mean... of those values for each
#' of the combinations. 
#' @param fx Character. Set operation: mean, sum
#' @param rm.na Boolean. Remove NA value from \code{wt}?
#' @param min_elements Integer. Exclude combinations with less than n elements
#' @param limit Integer. Show top n combinations and elements. The rest
#' will be grouped into a single element.
#' @param unique Boolean. a,b = b,a?
#' @param abc Boolean. Do you wish to sort by alphabetical order?
#' @param title Character. Added to the plot.
#' @param plot Boolean. Plot viz? Will be generated anyways in the output object
#' @examples 
#' options("lares.font"=NA) # Temporal
#' df <- dplyr::starwars
#' print(head(df))
#' 
#' # Characters per movies combinations in a list column
#' print(head(df$films, 2))
#' df %>% freqs_list(films)
#' 
#' # Skin colours in a comma-separated column
#' print(head(df$skin_color))
#' x <- freqs_list(starwars, skin_color, min_elements = 2, plot = FALSE)
#' # Inside "x" we'll have:
#' lapply(x, names)
#' x$plot
#' 
#' # Using the 'wt' argument to add a continuous value dimension
#' freqs_list(df, films, wt = height, abc = TRUE, limit = 8, min_elements = 2,
#'            title = "Star Wars:\nCharacter's\nHeights per Films")
#' @export
freqs_list <- function(df, var, 
                       wt = NA, fx = "mean",
                       rm.na = FALSE,
                       min_elements = 1,
                       limit = 10, 
                       unique = TRUE, 
                       abc = FALSE, 
                       title = "",
                       plot = TRUE) {
  
  var_str <- deparse(substitute(var))
  check_opts(var_str, colnames(df))
  
  # Change column names for easier manipulation
  colnames(df)[colnames(df) == var_str] <- "which"
  
  wt_str <- deparse(substitute(wt))
  if (wt_str != "NA") {
    if (wt_str %in% colnames(df)) {
      message(paste(">>> Colour weight:", fx, wt_str))
      colnames(df)[colnames(df) == wt_str] <- "wt" 
    }
  } else df$wt <- 0
  if (!is.numeric(df$wt)) stop("Input ", wt_str, " must be a numeric column.")
  
  # If list then convert
  if (is.list(df$which))
    df$which <- unlist(lapply(df$which, function(x) paste(x, collapse = ","))) 
  
  # check if it's a valid column
  if(!any(grepl(",", df$which)))
    stop(sprintf(
      "Your input (%s) is not valid: does NOT contain commas or is no a list.", var_str))
  
  # Order values so a,b = b,a
  if (unique) {
    values <- strsplit(unlist(df$which), ",")
    values <- lapply(values, sort)
    values <- unlist(lapply(values, function(x) paste(x, collapse=","))) 
  } else values <- df$which
  
  # One hot encoding
  vals <- data.frame(var = values, wt = df$wt) %>% 
    filter(.data$var != "") %>%
    {if (rm.na) filter(., !is.na(.data$wt)) else .}%>%
    group_by(.data$var) %>%
    summarise(n = n(),
              wt = ifelse(
                fx == "mean", mean(.data$wt, na.rm = TRUE), 
                ifelse(fx == "sum", sum(.data$wt, na.rm = TRUE), 1)),
              .groups = "drop") %>%
    arrange(desc(.data$n)) %>%
    mutate(p = 100*.data$n/sum(.data$n), order = row_number()) %>%
    data.frame() %>%
    ohe_commas("var") %>%
    # Remove combinations with less than "min_elements" elements
    mutate(combs = rowSums(.[unlist(lapply(., is.logical))], na.rm = TRUE)) %>%
    filter(.data$combs >= min_elements) %>% select(-.data$combs)
  
  elements <- vals %>%
    tidyr::gather(.) %>% 
    mutate(n = rep(vals$n, ncol(vals))) %>%
    mutate(wt = rep(vals$wt, ncol(vals))) %>%
    filter(.data$value == TRUE) %>% 
    group_by(.data$key) %>%
    summarise(n = sum(.data$n),
              wt = ifelse(
                fx == "mean", mean(.data$wt, na.rm = TRUE), 
                ifelse(fx == "sum", sum(.data$wt, na.rm = TRUE), 1)),
              .groups = "drop") %>%
    {if (abc) arrange(., .data$key) else arrange(., desc(.data$n))} %>%
    mutate(p = 100 * .data$n/nrow(df), order = row_number()) %>%
    mutate(key = as.factor(.data$key)) %>%
    mutate(label = sprintf("%s (%s)", .data$key, formatNum(.data$p, 0, pos = "%"))) %>%
    mutate(label = ifelse(.data$order > limit, "...", .data$label)) %>%
    mutate(label = as.factor(.data$label))
  
  tgthr <- vals %>% 
    select(contains("var_")) %>%
    tidyr::gather("var") %>%
    mutate(var = factor(.data$var, levels = rev(elements$key))) %>%
    group_by(.data$var) %>% 
    mutate(id = row_number(), order = row_number()) %>%
    mutate(label = sprintf("#%s", id)) %>%
    filter(.data$value == TRUE) %>%
    mutate(label = ifelse(.data$id > limit, "...", .data$label)) %>%
    mutate(var = ifelse(
      as.character(.data$var) %in% as.character(elements$key[elements$label != "..."]),
      as.character(.data$var), "...")) %>% 
    mutate(var = factor(.data$var, levels = rev(c(as.character(elements$key), "..."))))
  
  # Amount of data considered
  caption <- sprintf("Observations: %s | Elements: %s", sum(vals$n), sum(elements$n))
  if (min_elements > 1) caption <- v2t(c(caption, sprintf(
      "Excluding combinations with less than %s elements", min_elements)),
      quotes = FALSE, sep = "\n")
  
  # Scatter plot: combinations
  p1 <- tgthr %>%
    ggplot(aes(x = reorder(.data$label, .data$order), 
               y = var, group = .data$label)) +
    geom_point(size = 4) + geom_path() +
    theme_lares2(mg = -1, which = "XY") +
    labs(x = NULL, y = NULL, caption = caption) +
    theme(axis.text.y = element_blank(),
          plot.margin = margin(0,0,0,0)) +
    scale_x_discrete(position = "top")
  
  # Bar plot: elements
  p2 <- elements %>%
    mutate(label = gsub("var_","", .data$label)) %>%
    ggplot(aes(x = reorder(.data$label, -.data$order), 
               y = -.data$n, 
               fill = .data$wt)) +
    coord_flip() + geom_col() +
    labs(y = NULL, x = NULL) +
    theme_lares2(mg = -1) +
    scale_y_continuous(position = "right", 
                       labels = function(x) formatNum(abs(x), 0)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    guides(fill = FALSE)
  
  # Bar plot: combinations
  p3 <- vals %>%
    mutate(var = ifelse(row_number() > limit, "...", .data$var)) %>%
    ggplot(aes(x = reorder(.data$var, .data$order), 
               y = .data$n, fill = .data$wt)) +
    geom_col() + 
    theme_lares2(mg = -1) + 
    scale_y_continuous(labels = function(x) formatNum(abs(x), 0)) +
    labs(x = NULL, y = "Combination Frequency",
         fill = if (wt_str != "NA") str_to_title(paste(fx, wt_str, sep = "\n")) else NULL) +
    theme(axis.text.x  = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    scale_fill_continuous(guide = guide_colourbar(direction = "horizontal"))
  if (wt_str == "NA") p3 <- p3 + guides(fill = FALSE)
  
  # Join all plots
  layout <- "
  AAABBBBB
  CCCBBBBB
  DDDEEEEE
  DDDEEEEE
  DDDEEEEE"
  p <- noPlot(title, 4) + p3 + guide_area() + 
    p2 + p1 + plot_layout(design = layout) +
    plot_layout(guides = 'collect')
  if (plot) plot(p)

  # Prepare dataframe outputs
  ohe <- ohe_commas(data.frame(vals = values), "vals")
  colnames(ohe) <- gsub("vals_", "", colnames(ohe))
  elements <- mutate(elements, key = gsub("var_", "", .data$key)) %>%
    rename("element" = .data$key) %>% select(-.data$label)
  vals <- mutate(vals, var = gsub("var_", "", .data$var)) %>%
    rename("combination" = .data$var)
  colnames(vals) <- gsub("var_", "", colnames(vals))
  
  results <- list(plot = p, 
                  ohe = ohe, 
                  elements = elements,
                  combinations = vals)
  
  return(invisible(results))
}
