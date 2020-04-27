####################################################################
#' Sum Calculations and Plot
#' 
#' This function lets the user group, sum, calculate percentages 
#' and cumulatives. It also plots results if needed. Tidyverse friendly.
#' 
#' @family Exploratory
#' @family Visualization
#' @param df Data.frame
#' @param ... Variables. Variables you wish to process. Order matters.
#' If no variables are passed, the whole data.frame will be considered
#' @param fun Function (sum by default)
#' @param which Character vector. Select variables to sum. If not set,
#' all numeric variables will be returned
#' @param top Integer. Filter and plot the n largest n values by group
#' @param plot Boolean. Do you want to see a plot? Three variables tops.
#' @export
summer <- function(df, ..., 
                   fun = sum,
                   which = NA, top = NA,
                   plot = FALSE) {
  
  vars <- quos(...)
  
  if (length(vars) > 0) {
    grouped <- TRUE
    aux <- freqs(df, !!!vars)
    df <- df %>% group_by(!!!vars)
    groups <- colnames(aux)[1:(which(colnames(aux) == "n") - 1)]
    message('Grouped by: ', vector2text(groups))
  } else grouped <- FALSE
  
  # SUM ALL NUMERIC VALUES
  output <- df %>% summarise_if(is.numeric, fun, na.rm = TRUE)
  if (grouped) output <- output %>% left_join(aux, groups)
  
  # Select specific columns
  if (!is.na(which)[1]) {
    cols <- colnames(output)[colnames(output) %in% which]
    message('Numerical columns: ', vector2text(cols))
  } else {
    cols <- colnames(output)[!colnames(output) %in% c("pcum","order","p")]
    which <- cols[!cols %in% c("n", groups)]
  }
  if (grouped) cols <- c("n", cols)
  output <- select(output, !!!vars, cols)
  
  # PLOT
  if (plot & grouped) {
    #aux <- tidyr::pivot_longer(output, which) %>% 
    aux <- gather(output, which) %>% 
      mutate(label = paste(!!!vars)) %>%
      arrange(desc(value))
    if (!is.na(top)) {
      message(paste("Slicing the top", top, "for each numerical variable"))
      aux <- aux %>% group_by(name) %>% slice(1:top)
    }

    reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
      new_x <- paste(x, within, sep = sep)
      stats::reorder(new_x, by, FUN = fun)
    }
    scale_x_reordered <- function(..., sep = "___") {
      reg <- paste0(sep, ".+$")
      ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
    }
    
    plot <- ggplot(aux, 
                   aes(x = reorder_within(label, value, name), y = value)) +
      geom_col() + coord_flip() +
      facet_wrap(. ~ name, scales = "free", ncol = 1) +
      theme_lares2(pal = 2, grid = "Xx") + scale_x_reordered() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = NULL, title = "Numerical Columns Summed")
    if (grouped) plot <- plot + 
      labs(subtitle = paste("Grouped by", vector2text(groups, quotes = FALSE)))
    return(plot)
  }
  return(ungroup(output))
}

# summer(dft)
# summer(dft, Survived, Pclass, plot = TRUE, top = 3)
# summer(dft, Survived, Pclass, which = "Fare", plot = TRUE)
# summer(dft, Pclass, Survived, which = c("Fare","Age"), plot = TRUE)
