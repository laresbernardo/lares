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
missingness <- function(df, plot = FALSE, full = TRUE, subtitle = NA) {
  
  if (sum(is.na(df)) == 0) {
    message("No missing values found!")
    return()
  }
  
  m <- df %>%
    summarize_all(.funs = ~ sum(is.na(.))) %>%
    tidyr::gather() %>%
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
    
    note <- paste0("Total observations: ", formatNum(obs, 0),
                   " | Total missings: ", formatNum(miss, 0), 
                   " (",formatNum(missp, 1),"%)")
    
    p <- is.na(df) %>% data.frame() %>% tidyr::gather() %>%
      mutate(type = ifelse(key %in% m$variable, "with", "without")) %>%
      group_by(key) %>%
      mutate(row_num = row_number()) %>%
      mutate(perc = round(100*sum(value)/nrow(df),2)) %>%
      mutate(label = ifelse(type == "with", paste0(key, " | ", perc,"%"), key)) %>%
      {if (!full) filter(., label == "with") else .} %>%
      arrange(value) %>% 
      ggplot(aes(x = reorder(label, perc), y = row_num, fill = value)) + 
      geom_raster() + 
      coord_flip() +
      facet_grid(type ~ ., space = "free", scales = "free") +
      scale_y_continuous(note, expand = c(0, 0)) +
      scale_fill_grey(name = "", labels = c("Present", "Missing")) +
      labs(title = "Missing values", x = "", subtitle = if (!is.na(subtitle)) subtitle) +
      theme_lares2(legend = "top") +
      theme(axis.text.y  = element_text(size = 8))
    return(p)
  }
  return(m)
}
