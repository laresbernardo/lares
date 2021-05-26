####################################################################
#' Calculate and Visualize Missingness
#' 
#' This function lets the user calculate the percentage of NAs or
#' missingness in a data.frame. It also plots the results if needed.
#' 
#' @family Exploratory
#' @family Missing Values
#' @param df Dataframe. Dataframe to study
#' @param plot Boolean. Do you wish to plot results?
#' @param full Boolean. Return all variables (or only with missings)?
#' @param subtitle Character. Subtitle to show in plot
#' @param summary Boolean. Show numerical summary text?
#' @return data.frame with each variable, number of missing values and percentage.
#' If \code{plot=TRUE}, a plot with the same information reflected.
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' 
#' # Dummy data
#' df <- data.frame(A = c(1:5), 
#'                  B = c(NA, NA, 1, 1, 1), 
#'                  C = rep(NA, 5), 
#'                  D = c(NA, LETTERS[1:4]))
#' 
#' # Missing values summary
#' missingness(df)
#' 
#' # Visual results
#' 
#' missingness(df, plot = TRUE)
#' 
#' # Show all variables (including those with no missing values)
#' missingness(df, plot = TRUE, full = TRUE)
#' @export
missingness <- function(df, plot = FALSE, full = FALSE, 
                        subtitle = NA, summary = TRUE) {
  
  if (sum(is.na(df)) == 0) {
    if (summary) message("No missing values found!")
    invisible(return(NULL))
  }
  
  m <- df %>%
    ungroup() %>%
    summarize_all(.funs = ~ sum(is.na(.))) %>%
    gather() %>%
    arrange(desc(.data$value)) %>%
    filter(.data$value > 0) %>%
    mutate(missing = round(100*.data$value/nrow(df),2))
  colnames(m) <- c("variable", "missing","missingness")
  
  if (plot) {
    obs <- nrow(df) * ncol(df)
    miss <- sum(m$missing)
    missp <- 100 * miss/obs
    note <- sprintf("%s cols x %s rows | Values: %s | Missing: %s (%s)", 
                    formatNum(ncol(df), 0), 
                    formatNum(nrow(df), 0), 
                    formatNum(obs, 0), 
                    formatNum(miss, 0), 
                    formatNum(missp, 1, pos = "%"))
    
    
    p <- is.na(df) %>% data.frame() %>% 
      tidyr::gather() %>%
      {if (!full) 
        filter(., .data$key %in% m$variable) else .} %>%
      mutate(type = ifelse(.data$key %in% m$variable, "with", "without")) %>%
      group_by(.data$key) %>%
      mutate(row_num = row_number()) %>%
      mutate(perc = round(100*sum(.data$value)/nrow(df),2)) %>%
      mutate(label = ifelse(.data$type == "with", paste0(.data$key, " | ", .data$perc,"%"), .data$key)) %>%
      arrange(.data$value) %>% 
      ggplot(aes(x = reorder(.data$label, .data$perc), y = .data$row_num, fill = .data$value)) + 
      geom_raster() + 
      coord_flip() +
      {if (full) 
        facet_grid(.data$type ~ ., space = "free", scales = "free")} +
      {if (summary) 
        scale_y_comma(note, expand = c(0, 0)) else 
          scale_y_comma(NULL, expand = c(0, 0))} +
      scale_fill_grey(name = NULL, labels = c("Present", "Missing"), expand = c(0, 0)) +
      labs(title = "Missing values", x = "", subtitle = if (!is.na(subtitle)) subtitle) +
      theme_lares(legend = "top") +
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
#' @family Missing Values
#' @param df Dataframe. Dataframe to transform.
#' @param m Integer. Number of multiple imputations.
#' @param iters Integer. Number of iterations.
#' @param seed Integer. Set a seed for reproducibility.
#' @param quiet Boolean. Keep quiet? (or print replacements).
#' @return data.frame with imputed values.
#' @export
impute <- function(df, m = 5, iters = 5, seed = 0, quiet = FALSE){
  try_require("mice")
  on.exit(set.seed(seed))
  aux <- mice(df, seed = seed, m = m, maxit = iters, printFlag = !quiet)
  ret <- complete(aux)
  return(ret)
}
