####################################################################
#' Correlation table
#'
#' This function correlates a whole dataframe, filtering automatically
#' all numerical values.
#'
#' @param df Dataframe. It doesn't matter if it's got non-numerical
#' columns: they will be filtered!
#' @param method Character. Any of: c("pearson", "kendall", "spearman")
#' @param logs Boolean. Automatically calculate log(values) for numerical
#' variables (not binaries)
#' @param plot Boolean. Do you wish to see a plot?
#' @param top Integer. Select top N most relevant variables? Filtered 
#' and sorted by mean of each variable's correlations
#' @export
corr <- function(df, method = "pearson", logs = TRUE, plot = FALSE, top = NA) {
  
  library(dplyr)
  
  d <- lares::numericalonly(df, logs = logs)
  
  # Correlations
  rs <- cor(d, use = "pairwise.complete.obs", method = method)
  rs[is.na(rs)] <- 0
  cor <- round(data.frame(rs), 4)
  row.names(cor) <- colnames(cor)
  
  # Top N
  if (!is.na(top)) {
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
    lares::corr_plot(cor, logs = FALSE)
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
#' @export
corr_var <- function(df, var, method = "pearson", plot = TRUE, 
                     logs = TRUE, top = NA, zeroes = FALSE) {
  
  require(ggplot2)
  require(scales)
  
  rs <- lares::corr(df, method = method, logs = logs)
  d <- data.frame(variables = colnames(rs), corr = rs[, c(var)])
  d <- d[(d$corr < 1 & !is.na(d$corr)),]
  d <- d[order(-abs(d$corr)), ]
  
  original_n <- nrow(d)
  
  if (zeroes == FALSE) {
    d <- d[d$corr != 0, ]
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
    if (!is.na(top) & top < nrow(d)) { plot <- plot + labs(subtitle = paste(
      "Plotting top", top, "out of", original_n, "numeric/binary variables"))
    }
    print(plot)
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
  
  require(corrplot)
  
  c <- lares::corr(df, method, logs = logs)
  
  return(
    corrplot::corrplot(as.matrix(c),
                       order = order,
                       method = type, 
                       type = "lower",
                       diag = FALSE)
  )
}
