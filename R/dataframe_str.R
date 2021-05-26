####################################################################
#' Dataset columns and rows structure
#' 
#' This function lets the user to check quickly the structure of a
#' dataset (data.frame). It returns multiple counters for useful metrics,
#' a plot, and a list of column names for each of the column metrics.
#' 
#' @family Exploratory
#' @param df Dataframe
#' @param return Character. Return "skimr" for skim report, "numbers" for
#' stats and numbers, "names" for a list with the column names of each of 
#' the class types, "plot" for a nice plot with "numbers" output, "distr"
#' for an overall summary plot showing categorical, numeric, and missing 
#' values by using \code{plot_df}
#' distributions
#' @param subtitle Character. Add subtitle to plot
#' @param quiet Boolean. Keep quiet or show other options available?
#' @return Depending on \code{return} input and based on your \code{df} structure:
#' \itemize{
#'   \item \code{list} with the names of the columns classified by class
#'   \item \code{data.frame} with numbers: total values, row, columns,
#'   complete rows
#'   \item \code{plot} with visualizations
#' }
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dft) # Titanic dataset
#' df_str(dft, "names")
#' df_str(dft, "numbers", quiet = TRUE)
#' df_str(dft, "plot", quiet = TRUE)
#' @export
df_str <- function(df, 
                   return = "plot", 
                   subtitle = NA, 
                   quiet = FALSE){
  
  if (!quiet) {
    rets <- c("skimr","numbers","names","distr","plot")
    message(paste("Other available 'return' options:", vector2text(rets[rets != return]))) 
  }
  
  df <- data.frame(df)
  
  if (return == "skimr") {
    try_require("skimr")
    return(skim(df))
  }
  
  if (return == "distr") {
    p <- plot_df(df)
    return(p)
  }
  
  names <- list(
    cols = colnames(df),
    nums = colnames(df)[unlist(lapply(df, is.numeric))],
    char = colnames(df)[unlist(lapply(df, is.character))],
    factor = colnames(df)[unlist(lapply(df, is.factor))],
    logic = colnames(df)[unlist(lapply(df, is.logical))])
  names[["time"]] <- names$cols[!colnames(df) %in% c(
    names$nums, names$char, names$factor, names$logic)]
  names[["allnas"]] <- names$cols[unlist(lapply(df, function(x) all(is.na(x))))] 
  
  if (return == "names") 
    return(names)
  
  numbers <- data.frame(
    "Total Values" = nrow(df) * ncol(df),
    "Total Rows" = nrow(df),
    "Total Columns" = ncol(df),
    "Numeric Columns" = length(names$nums),
    "Character Columns" = length(names$char),
    "Factor Columns" = length(names$factor),
    "Logical Columns" = length(names$logic),
    "Time/Date Columns" = length(names$time),
    "All Missing Columns" = length(names$allnas),
    "Missing Values" = sum(is.na(df)),
    "Complete Rows" = sum(complete.cases(df)),
    "Memory Usage" = as.numeric(object.size(df)))
  
  intro2 <- data.frame(counter = t(numbers)) %>%
    mutate(metric = row.names(.),
           type = ifelse(grepl("Column", colnames(numbers)), "Columns",
                         ifelse(grepl("Rows", colnames(numbers)), "Rows", "Values")),
           p = ifelse(.data$type == "Columns", 100*.data$counter/numbers$Total.Columns,
                      ifelse(.data$type == "Rows", 100*.data$counter/numbers$Total.Rows, 
                             100*.data$counter/numbers$Total.Values)),
           p = round(.data$p, 2),
           type = factor(.data$type, levels = c("Values", "Columns", "Rows"))) %>%
    select(.data$metric, .data$counter, .data$type, .data$p)
  
  if (return == "numbers") return(select(intro2, -.data$type))
  
  if (return == "plot") {
    p <- intro2 %>%
      filter(!.data$metric %in% "Memory.Usage") %>%
      mutate(x = ifelse(.data$p < 75, -0.15, 1.15)) %>%
      ggplot(aes(x = reorder(.data$metric, as.integer(.data$counter)), 
                 y = .data$p, fill = .data$type,
                 label = formatNum(.data$counter, 0))) + 
      geom_col() + coord_flip() + ylim(0, 100) +
      theme_minimal() + guides(fill = FALSE) +
      labs(title = "Dataset overall structure", 
           x = "", y = "% of total", fill = "", 
           caption = paste("Memory Usage:", formatNum(numbers$Memory.Usage/(1024*1024)),"Mb")) +
      facet_grid(type ~., scales = "free", space = "free") + 
      geom_text(aes(hjust = .data$x), size = 3) +
      theme_lares(pal = 1)
    if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)
    return(p)
  }
}


####################################################################
#' Plot All Numerical Features (Boxplots)
#' 
#' This function filters numerical columns and plots boxplots.
#' 
#' @family Exploratory
#' @param df Dataframe
#' @return Plot. Result of \code{df} numerical features.
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dft) # Titanic dataset
#' plot_nums(dft)
#' @export
plot_nums <- function(df) {
  which <- df %>% select_if(is.numeric)
  if (length(which) > 0) {
    p <- gather(which) %>%
      filter(!is.na(.data$value)) %>%
      ggplot(aes(x = .data$key, y = .data$value)) +
      geom_jitter(alpha = 0.2, size = 0.8) +
      geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 1) +
      facet_wrap(.data$key~., scales = "free") + 
      labs(title = "Numerical Features Boxplots", x = NULL, y = NULL) +
      theme_lares() +
      theme(axis.text.y = element_blank(), 
            axis.text.x = element_text(vjust = 2, size = 8),
            panel.spacing.y = unit(-.5, "lines"),
            strip.text = element_text(size = 10, vjust = -1.3)) +
      coord_flip()
    return(p)
  } else {
    message("No numerical variables found!")
  }
}


####################################################################
#' Plot All Categorical Features (Frequencies)
#' 
#' This function filters categorical columns and plots the frequency
#' for each value on every feature.
#' 
#' @family Exploratory
#' @param df Dataframe
#' @return Plot. Result of \code{df} categorical features.
#' @export
plot_cats <- function(df) {
  plot <- df %>% select_if(Negate(is.numeric)) 
  if (length(plot) > 0) {
    p <- plot %>% freqs(plot = TRUE) + 
      labs(title = "Categorical Features Frequencies") 
    return(p)
  } else {
    message("No categorical variables found!")
  }
}


####################################################################
#' Plot Summary of Numerical and Categorical Features
#' 
#' This function plots all columns frequencies and boxplots, for 
#' categorical and numerical respectively.
#' 
#' @family Exploratory
#' @param df Dataframe
#' @return Plot. Result of \code{df} categorical and numerical features.
#' @export
plot_df <- function(df) {
  
  plots <- list()
  
  cats <- plot_cats(df)
  if (length(cats) != 0) plots[["cats"]] <- cats +
    theme(plot.title = element_text(size = 12))
  
  nums <- plot_nums(df)
  if (length(nums) != 0) plots[["nums"]] <- nums  + 
    theme(plot.title = element_text(size = 12))
  
  mis <- missingness(df, plot = TRUE, summary = FALSE) 
  if (length(mis) != 0) plots[["miss"]] <- mis + 
    theme(plot.title = element_text(size = 12)) + guides(fill = FALSE)
  
  if (length(plots) == 3) heights <- c(4/12, 1/2, 3/12) 
  if (length(plots) == 2) heights <- c(0.5, 0.5) 
  if (length(plots) == 1) heights <- NULL 
  
  margin <- theme(plot.margin = unit(c(0.1,0.5,0.1,0.5), "cm"))
  plots <- lapply(plots, "+", margin)
  p <- wrap_plots(plots, heights = heights)
  return(p)
}
