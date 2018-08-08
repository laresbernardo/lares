####################################################################
#' Calculate missingness percentage on a data.frame
#' 
#' This function lets the user calculate the percentage of NAs or
#' missingness in a data.frame.
#' 
#' @param df Dataframe. Dataframe to study
#' @param plot Boolean. Do you wish to print results in a simple histogram?
#' @param bins Integer. Number of breaks on plotted histogram
#' @export
missingness <- function(df, plot = FALSE, bins = 25) {
  
  require(tidyr)
  require(dplyr)
  
  m <- df %>%
    summarize_all(.funs = ~ sum(is.na(.)) / length(.)) %>%
    gather() %>%
    arrange(desc(value)) %>%
    filter(value > 0) 
  colnames(m) <- c("variable", "missingness")
  
  if (plot == TRUE) {
    hist(m$missingness, 
         breaks = bins,
         xlab = "Missing values (%)", 
         main = "Distribution of Missing Values by Percentages", 
         col = "lightgreen")
  }
  
  return(m)
  
}


####################################################################
#' Full NAs Analysis on a data.frame
#' 
#' This function lets the user analyze NAs in a data.frame using 
#' VIM and funModeling libraries
#' 
#' @param df Dataframe. Dataframe to study
#' @param print Boolean. Do you wish to print results?
#' @export
nas <- function(df, print = FALSE) {
  
  require(dplyr)
  require(VIM)
  require(funModeling)
  
  nas <- df_status(df, print = print) %>% 
    filter(q_na > 0) %>% 
    arrange(desc(q_na))
  
  subset <- subset(df, select=c(nas$variable))
  
  VIM::aggr(subset, 
            col=c('navyblue','red'), 
            numbers=TRUE, 
            sortVars=TRUE,
            labels=names(data), 
            cex.axis=.7, 
            gap=2,
            ylab=c("Histogram of missing data","Pattern"))
}
