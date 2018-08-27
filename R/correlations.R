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
corr <- function(df, method = "pearson") {
  d <- select_if(df, is.numeric)
  rs <- cor(d, use = "pairwise.complete.obs", method = method)
  rs <- signif(rs, 4)
  rs <- as.data.frame(rs)
  return(rs)
}


####################################################################
#' Correlation between variable and dataframe
#'
#' This function correlates a whole dataframe with a single feature.
#'
#' @param df Dataframe.
#' @param var Character. Name of the variable to correlate
#' @param method Character. Any of: c("pearson", "kendall", "spearman")
#' @export
corr_var <- function(df, var, method = "pearson", plot = TRUE) {
  
  require(ggplot2)
  
  rs <- corr(df, method = method)
  d <- data.frame(variables = colnames(rs), corr = rs[, c(var)])
  d <- d[d$corr < 1,]
  
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
