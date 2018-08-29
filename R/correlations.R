####################################################################
#' Correlation table
#'
#' This function correlates a whole dataframe, filtering automatically
#' all numerical values.
#'
#' @param df Dataframe. It doesn't matter if it's got non-numerical
#' columns: they will be filtered!
#' @param method Character. Any of: c("pearson", "kendall", "spearman")
#' @export
corr <- function(df, method = "pearson", plot = FALSE) {
  
  library(dplyr)
  
  # Which character columns may be used as numeric?
  transformable <- apply(df, 2, function(x) length(unique(x)))
  which <- names(transformable[transformable==2])
  dfn <- df[,colnames(df) %in% which]
  
  # Join everything
  non_numeric <- mutate_all(dfn, function(x) as.integer(as.factor(x))-1)
  numeric <- select_if(df, is.numeric)
  d <- cbind(numeric, non_numeric[!colnames(non_numeric) %in% colnames(numeric)])
  
  # Correlations
  rs <- cor(d, use = "pairwise.complete.obs", method = method)
  rs[is.na(rs)] <- 0
  cor <- round(data.frame(rs), 4)
  
  if (plot == TRUE) {
    lares::corr_plot(cor)
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
#' @param zeroes Do you wish to keep zeroes in correlations too?
#' @export
corr_var <- function(df, var, method = "pearson", plot = TRUE, zeroes = FALSE) {
  
  require(ggplot2)
  
  rs <- corr(df, method = method)
  d <- data.frame(variables = colnames(rs), corr = rs[, c(var)])
  d <- d[(d$corr < 1 & !is.na(d$corr)),]
  d <- d[order(-abs(d$corr)), ]
  
  if (zeroes == FALSE) {
    d <- d[d$corr != 0, ]
  }
  
  if (plot == TRUE) {
    plot <- d %>% 
      mutate(pos = ifelse(corr > 0, TRUE, FALSE)) %>%
      ggplot(aes(reorder(variables, corr), 100*corr, 
                 fill = pos, label = 100*corr)) +
      geom_hline(aes(yintercept=0), alpha = 0.5) +
      geom_col(width = 0.1) + coord_flip() + theme_minimal() +
      geom_label(hjust = 0.5, size = 2.6, inherit.aes = TRUE, colour = "white") +
      scale_fill_discrete(name = "", breaks = c("TRUE","FALSE")) +
      guides(fill = FALSE) +
      labs(title=paste("Correlation of", var, "vs other variables"), 
           x = "", y = "Correlation")
    
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
#' @export
corr_plot <- function(df, method = "pearson", order = "FPC", type = "square") {
  
  require(corrplot)
  
  c <- lares::corr(df, method)
  
  return(
    corrplot(as.matrix(c),
             order = order,
             method = type, 
             type = "lower",
             diag = FALSE)
  )
}
