####################################################################
#' Calculate and Visualize Missingness
#' 
#' This function lets the user calculate the percentage of NAs or
#' missingness in a data.frame. It also plots the results if needed.
#' 
#' @family Exploratory
#' @param df Dataframe. Dataframe to study
#' @param plot Boolean. Do you wish to plot results?
#' @param full Boolean. Return all variables (or only with missings)?
#' @param subtitle Character. Subtitle to show in plot
#' @param summary Boolean. Show numerical summary text?
#' @export
missingness <- function(df, plot = FALSE, full = FALSE, 
                        subtitle = NA, summary = TRUE) {
  
  if (sum(is.na(df)) == 0) {
    message("No missing values found!")
    invisible(return(NULL))
  }
  
  m <- df %>%
    summarize_all(.funs = ~ sum(is.na(.))) %>%
    gather() %>%
    arrange(desc(value)) %>%
    filter(value > 0) %>%
    mutate(missing = round(100*value/nrow(df),2))
  colnames(m) <- c("variable", "missing","missingness")
  
  if (plot) {
    obs <- nrow(df) * ncol(df)
    miss <- sum(m$missing)
    missp <- 100 * miss/obs
    note <- sprintf("Total values: %s | Total missings: %s (%s)", 
                    formatNum(obs, 0), 
                    formatNum(miss, 0), 
                    formatNum(missp, 1, pos = "%"))
    
    
    p <- is.na(df) %>% data.frame() %>% 
      # tidyr::pivot_longer(cols = everything()) %>% # The world is not yet prepared!
      tidyr::gather() %>%
      {if (!full) 
        filter(., key %in% m$variable) else .} %>%
      mutate(type = ifelse(key %in% m$variable, "with", "without")) %>%
      group_by(key) %>%
      mutate(row_num = row_number()) %>%
      mutate(perc = round(100*sum(value)/nrow(df),2)) %>%
      mutate(label = ifelse(type == "with", paste0(key, " | ", perc,"%"), key)) %>%
      arrange(value) %>% 
      ggplot(aes(x = reorder(label, perc), y = row_num, fill = value)) + 
      geom_raster() + 
      coord_flip() +
      {if (full) 
        facet_grid(type ~ ., space = "free", scales = "free")} +
      {if (summary) 
        scale_y_comma(note, expand = c(0, 0)) else 
          scale_y_comma(NULL, expand = c(0, 0))} +
      scale_fill_grey(name = NULL, labels = c("Present", "Missing"), expand = c(0, 0)) +
      labs(title = "Missing values", x = "", subtitle = if (!is.na(subtitle)) subtitle) +
      theme_lares2(legend = "top") +
      theme(axis.text.y  = element_text(size = 8))
    
    return(p)
  }
  return(m)
}


####################################################################
#' Impute Missing Values (using MICE)
#' 
#' This function uses the MICE methodology to impute missing values.
#' 
#' @family Data Wrangling
#' @family Machine Learning
#' @param df Dataframe. Dataframe to transform
#' @param m Integer. Number of multiple imputations
#' @param iters Integer. Number of iterations
#' @param seed Integer. Set a seed for reproducibility
#' @param quiet Boolean. Keep quiet? (or print replacements)
#' @export
impute <- function(df, m = 5, iters = 5, seed = 0, quiet = FALSE){
  try_require("mice")
  set.seed(seed)
  aux <- mice(df, seed = seed, m = m, maxit = iters, printFlag = !quiet)
  ret <- complete(aux)
  return(ret)
}
