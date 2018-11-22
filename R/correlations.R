####################################################################
#' Correlation table
#'
#' This function correlates a whole dataframe, filtering automatically
#' all numerical values.
#'
#' @param df Dataframe. It doesn't matter if it's got non-numerical
#' columns: they will be filtered!
#' @param method Character. Any of: c("pearson", "kendall", "spearman")
#' @param dummy Boolean. Should One Hot Encoding be applied to categorical columns? 
#' @param dates Boolean. Do you want the function to create more features
#' out of the date/time columns?
#' @param redundant Boolean. Should we keep redundat columns? i.e. It the
#' column only has two different values, should we keep both new columns?
#' @param logs Boolean. Automatically calculate log(values) for numerical
#' variables (not binaries)
#' @param plot Boolean. Do you wish to see a plot?
#' @param top Integer. Select top N most relevant variables? Filtered 
#' and sorted by mean of each variable's correlations
#' @export
corr <- function(df, method = "pearson", dummy = TRUE, dates = TRUE, 
                 redundant = TRUE, logs = FALSE, plot = FALSE, top = NA) {
  
  options(warn=-1)
  
  # One hot encoding for categorical features
  if (dummy == TRUE) {
    df <- ohe(df, summary = FALSE, redundant = redundant, dates = dates)
  }
  
  # Select only numerical features and create log+1 for each one
  d <- numericalonly(df, logs = logs)
  
  # Correlations
  rs <- cor(d, use = "pairwise.complete.obs", method = method)
  rs[is.na(rs)] <- 0
  cor <- round(data.frame(rs), 4)
  row.names(cor) <- colnames(cor)
  
  # Top N
  if (!is.na(top)) {
    message(paste("Returning the top", top, "variables only..."))
    imp <- cor %>% 
      summarise_all(funs(mean(.))) %>% t() %>% 
      data.frame(variable=row.names(.), mean=abs(.)) %>%
      arrange(desc(abs(mean)))
    which <- as.vector(imp$variable[1:top])
    cor <- cor %>% select(one_of(which)) %>% 
      filter(row.names(.) %in% which) 
  }
  
  # Plot
  if (plot == TRUE) {
    corr_plot(cor, logs = FALSE)
  }
  
  return(cor)
  
}


####################################################################
#' Correlation between variable and dataframe
#'
#' This function correlates a whole dataframe with a single feature.
#'
#' @param df Dataframe.
#' @param var Character. Name of the variable to correlate
#' @param method Character. Any of: c("pearson", "kendall", "spearman")
#' @param plot Boolean. Do you wish to plot the result?
#' @param logs Boolean. Automatically calculate log(values) for numerical
#' variables (not binaries)
#' @param top Integer. If you want to plot the top correlations, define how many
#' @param zeroes Do you wish to keep zeroes in correlations too?
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
corr_var <- function(df, var, 
                     method = "pearson", 
                     plot = TRUE, 
                     logs = TRUE, 
                     top = NA, 
                     zeroes = FALSE,
                     save = FALSE, 
                     subdir = NA,
                     file_name = "viz_corrvar.png") {
  
  # Calculate correlations
  rs <- corr(df, method = method, logs = logs)
  
  # Check if main variable exists
  if (!var %in% colnames(rs)) {
    message("Your input `",var, "` is not a valid variable because that column doesn't exist!")
    maybes <- colnames(rs)[grepl(var, colnames(rs))]
    if (length(maybes) > 0) {
      message(paste("Maybe you meant one of these:", vector2text(maybes)))
    }
    stop()
  }
  
  d <- data.frame(variables = colnames(rs), corr = rs[, c(var)])
  d <- d[(d$corr < 1 & !is.na(d$corr)),]
  d <- d[order(-abs(d$corr)), ]
  
  original_n <- nrow(d)
  
  if (zeroes == FALSE) {
    d <- d[d$corr != 0, ]
  }
  
  # Limit automatically when more than 30 observations
  if (is.na(top) & nrow(d) > 30) {
    top <- 30
    message(paste("Automatically reduced results to the top", top, "variables.",
                  "Use the 'top' parameter to override this limit."))
  }
  
  if (!is.na(top)) {
    d <- d[1:as.integer(top), ]
  }
  
  d <- d[complete.cases(d), ]
  
  if (plot == TRUE) {
    plot <- d %>% 
      mutate(pos = ifelse(corr > 0, TRUE, FALSE)) %>%
      ggplot(aes(x = reorder(variables, abs(corr)), 
                 y = abs(corr), fill = pos, label = 100 * corr)) +
      geom_hline(aes(yintercept=0), alpha = 0.5) +
      geom_col(width = 0.1) + coord_flip() + theme_minimal() +
      geom_label(hjust = 0.5, size = 2.6, inherit.aes = TRUE, colour = "white") +
      scale_fill_discrete(name = "", breaks = c("TRUE","FALSE")) +
      guides(fill = FALSE) +
      labs(title=paste("Correlation of", var, "vs other variables"), 
           x = "", y = "Correlation") +
      scale_y_continuous(labels = scales::percent)
    if (!is.na(top) & top < original_n) { 
      plot <- plot + 
        labs(subtitle = paste("Plotting top", top, "out of", 
                              original_n, "variables (original+dummy)"))
    }
    print(plot)
  }
  
  if (!is.na(subdir)) {
    options(warn=-1)
    dir.create(file.path(getwd(), subdir), recursive = T)
    file_name <- paste(subdir, file_name, sep="/")
  }
  if (save == TRUE) {
    plot <- plot + ggsave(file_name, width = 6, height = 6)
  }
  
  return(d)
  
}



####################################################################
#' Correlation plot
#'
#' This function correlates a whole dataframe with a single feature.
#'
#' @param df Dataframe.
#' @param method Character. Any of: c("pearson", "kendall", "spearman")
#' @param order Character. The ordering method of the correlation matrix.
#' Any of: c("original", "AOE", "FPC", "hclust", "alphabet")
#' @param type Character. The visualization method of correlation matrix to be used. 
#' Any of c("circle", "square", "ellipse", "number", "pie", "shade" and "color")
#' @param logs Boolean. Automatically calculate log(values) for numerical
#' variables (not binaries) and plot them
#' @export
corr_plot <- function(df, method = "pearson", order = "FPC", type = "square", logs = TRUE) {
  
  c <- corr(df, method, logs = logs)
  plot <- corrplot(as.matrix(c),
                   order = order,
                   method = type, 
                   type = "lower",
                   diag = FALSE)
  return(plot)
}
