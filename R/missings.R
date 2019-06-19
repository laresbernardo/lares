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
#' @export
missingness <- function(df, plot = FALSE, full = FALSE, subtitle = NA) {
  
  if (sum(is.na(df)) == 0) {
    message("No missing values found!")
    return()
  }
  
  m <- df %>%
    summarize_all(.funs = ~ sum(is.na(.))) %>%
    gather() %>%
    arrange(desc(value)) %>%
    filter(value > 0) %>%
    mutate(missing = round(100*value/nrow(df),2))
  colnames(m) <- c("variable", "missing","missingness")
  
  if (plot) {
    
    # # Old Plot
    # p <- gg_bars(m$variable, m$missing, p = m$missingness,
    #              title = "Missing Values Frequencies",
    #              axis = "# Missings (%)", obs = FALSE)
    
    obs <- nrow(df)*ncol(df)
    miss <- sum(m$missing)
    missp <- 100*miss/obs
    
    note <- paste0("Total values: ", formatNum(obs, 0),
                   " | Total missings: ", formatNum(miss, 0), 
                   " (",formatNum(missp, 1),"%)")
    
    p <- is.na(df) %>% data.frame() %>% tidyr::gather() %>%
      {if (!full) filter(., key %in% m$variable) else .} %>%
      mutate(type = ifelse(key %in% m$variable, "with", "without")) %>%
      group_by(key) %>%
      mutate(row_num = row_number()) %>%
      mutate(perc = round(100*sum(value)/nrow(df),2)) %>%
      mutate(label = ifelse(type == "with", paste0(key, " | ", perc,"%"), key)) %>%
      arrange(value) %>% 
      ggplot(aes(x = reorder(label, perc), y = row_num, fill = value)) + 
      geom_raster() + 
      coord_flip() +
      {if (full) facet_grid(type ~ ., space = "free", scales = "free")} +
      scale_y_continuous(note, expand = c(0, 0), labels = scales::comma) +
      scale_fill_grey(name = "", labels = c("Present", "Missing")) +
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
  set.seed(seed)
  mids <- mice(df, seed = seed, m = m, maxit = iters, printFlag = !quiet)
  full <- complete(mids)
  return(full)
}
