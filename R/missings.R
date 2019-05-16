####################################################################
#' Calculate missingness percentage on a data.frame
#' 
#' This function lets the user calculate the percentage of NAs or
#' missingness in a data.frame.
#' 
#' @param df Dataframe. Dataframe to study
#' @param plot Boolean. Do you wish to plot results?
#' @export
missingness <- function(df, plot = FALSE) {
  
  m <- df %>%
    summarize_all(.funs = ~ sum(is.na(.))) %>%
    gather() %>%
    arrange(desc(value)) %>%
    filter(value > 0) %>%
    mutate(missing = round(100*value/nrow(df),2))
  colnames(m) <- c("variable", "missing","missingness")
  
  if (plot) {
    p <- gg_bars(m$variable, m$missing, p = m$missingness,
                 title = "Missing Values Frequencies",
                 axis = "# Missings (%)", obs = FALSE)
    return(p)
  }
  return(m)
}
