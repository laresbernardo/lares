####################################################################
#' Frequencies Calculations and Plot
#' 
#' This function lets the user group, count, calculate 
#' percentages and cumulatives. It also plots if needed. 
#' Perfect for inside with dplyr's pipes.
#' 
#' @param vector Data.frame
#' @param ... Variables. Variables you wish to process. Order matters.
#' @param plot Boolean. Do you want to see a plot? Three variables tops.
#' @param rm.na Boolean. Remove NA values in the plot? (not filtered for 
#' numerical output; use dplyr::filter(!is.na(...)) if needed)
#' @export
freqs <- function(vector, ..., plot = FALSE, rm.na = FALSE) {
  
  require(dplyr)
  
  output <- vector %>%
    group_by_(.dots = lazyeval::lazy_dots(...)) %>%
    tally() %>% arrange(desc(n)) %>%
    mutate(p = round(100*n/sum(n),2), pcum = cumsum(p))
  
  if (plot == TRUE) {
    
    if (ncol(output) - 3 <= 3) { 
      
      require(ggplot2)
      require(scales)
      options(warn=-1)
      
      plot <- ungroup(output)
      
      if (rm.na == TRUE) {
        plot <- plot[complete.cases(plot), ]
      }
      
      # Create some dynamic aesthetics
      plot$labels <- paste0(lares::formatNum(plot$n, decimals = 0),
                            " (", signif(plot$p, 4), "%)")
      plot$label_colours <- ifelse(plot$p > mean(range(plot$p)) * 0.9, "m", "f")
      lim <- 0.35
      plot$label_hjust <- ifelse(
        plot$n < min(plot$n) + diff(range(plot$n)) * lim, -0.1, 1.05)
      plot$label_colours <- ifelse(
        plot$label_colours == "m" & plot$label_hjust < lim, "f", plot$label_colours)
      variable <- colnames(plot)[1]
      colnames(plot)[1] <- "names"
      
      # When two features
      if (ncol(output) - 3 == 2) { 
        facet_name <- colnames(plot)[2]
        colnames(plot)[1] <- "facet"
        colnames(plot)[2] <- "names"
        plot$facet[is.na(plot$facet)] <- "NA"
      }
      # When three features
      if (ncol(output) - 3 == 3) { 
        facet_name1 <- colnames(plot)[2]
        facet_name2 <- colnames(plot)[3]
        colnames(plot)[1] <- "facet2"
        colnames(plot)[2] <- "facet1"
        colnames(plot)[3] <- "names"
        plot$facet2[is.na(plot$facet2)] <- "NA"
        plot$facet1[is.na(plot$facet1)] <- "NA"
      }
      
      # Plot base
      p <- ggplot(plot, aes(x = reorder(as.character(names), n),
                            y = n, label = labels, fill = p)) +
        geom_col(alpha=0.9, width = 0.8) +
        geom_text(aes(
          hjust = label_hjust,
          colour = label_colours), size = 2.6) + lares::gg_text_customs() +
        coord_flip() + theme_minimal() + guides(colour = FALSE) +
        labs(x = "", y = "Counter", fill = "[%]",
             title = paste("Frequencies and Percentages:", variable)) +
        scale_fill_gradient(low = "lightskyblue2", high = "navy")
      
      # When two features
      if (ncol(output) - 3 == 2) { 
        p <- p + facet_grid(as.character(facet) ~ .) + 
          labs(subtitle = paste("Inside the facet grids:", facet_name)) +
          theme_light()
      }
      # When three features
      if (ncol(output) - 3 == 3) { 
        if (length(unique(facet_name2)) > 3) {
          stop("Please, try with a (third) variable with 3 or less cateogries!")
        }
        p <- p + facet_grid(as.character(facet2) ~ as.character(facet1)) + 
          labs(title = paste("Frequencies and Percentages:", facet_name1, "and", variable),
               subtitle = paste("Inside the facet grids:", facet_name2)) +
          theme_light()
      }
      print(p)
    } else {
      # When more than two features
      message("Sorry, but trying to plot more than 3 features is as complex as it sounds...")
    }
  }
  
  return(output)
  
}
