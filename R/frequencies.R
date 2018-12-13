####################################################################
#' Frequencies Calculations and Plot
#' 
#' This function lets the user group, count, calculate percentages 
#' and cumulatives. It also plots results if needed. Tidyverse friendly.
#' 
#' @param vector Data.frame
#' @param ... Variables. Variables you wish to process. Order matters.
#' @param results Boolean. Return results in a dataframe?
#' @param variable_name Character. Overwrite the main variable's name
#' @param plot Boolean. Do you want to see a plot? Three variables tops.
#' @param rm.na Boolean. Remove NA values in the plot? (not filtered for 
#' numerical output; use na.omit() or filter() if needed)
#' @param top Integer. Filter and plot the most n frequent for 
#' categorical values. Set to NA to return all values
#' @param abc Boolean. Do you wish to sort by alphabetical order? If set
#' to FALSE then frequency will sort the values
#' @param save Boolean. Save the output plot in our working directory
#' @param subdir Character. Into which subdirectory do you wish to 
#' save the plot to?
#' @export
freqs <- function(vector, ..., results = TRUE, 
                  variable_name = NA, 
                  plot = FALSE, rm.na = FALSE, 
                  top = 40, abc = FALSE, 
                  save = FALSE, subdir = NA) {
  
  vars <- quos(...)
  
  output <- vector %>%
    group_by(!!!vars) %>% 
    tally() %>% arrange(desc(n)) %>%
    mutate(p = round(100*n/sum(n),2), pcum = cumsum(p))
  
  if (plot == TRUE | save == TRUE) {
    
    # Use only the most n frequent values/combinations only
    values <- output[,1]
    if(nrow(values) > top) {
      if (!is.na(top)) {
        message(paste0("Filtering the top ", top, " (out of ", formatNum(nrow(values), 0),
                       ") frequent values. Use the 'top' parameter if you want to overrule."))
        output <- output[1:top, ]
      }
    }
    
    # Sort values alphabetically or ascending if numeric
    if (abc == TRUE) {
      output <- output %>% mutate(order = rank(as.character(!!!vars)))
      message("Sorting variable(s) alphabetically")
    } else {
      output <- output %>% mutate(order = rank(-n))
    }
    
    if (nrow(output) >= 20 & length(vars) >= 2) {
      message("Recommendation: use the `lares::distr` function instead")
    }
    
    if (ncol(output) - 3 <= 4) { 
      
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
      if (ncol(output) - 3 == 3) { 
        facet_name <- colnames(plot)[2]
        colnames(plot)[1] <- "facet"
        colnames(plot)[2] <- "names"
        plot$facet[is.na(plot$facet)] <- "NA"
      }
      # When three features
      if (ncol(output) - 3 == 4) { 
        facet_name1 <- colnames(plot)[2]
        facet_name2 <- colnames(plot)[3]
        colnames(plot)[1] <- "facet2"
        colnames(plot)[2] <- "facet1"
        colnames(plot)[3] <- "names"
        plot$facet2[is.na(plot$facet2)] <- "NA"
        plot$facet1[is.na(plot$facet1)] <- "NA"
      }
      
      # Plot base
      p <- ggplot(plot, aes(x = reorder(as.character(names), -order),
                            y = n, label = labels, fill = p)) +
        geom_col(alpha=0.9, width = 0.8) +
        geom_text(aes(
          hjust = label_hjust,
          colour = label_colours), size = 2.6) + lares::gg_text_customs() +
        coord_flip() + theme_minimal() + guides(colour = FALSE) +
        labs(x = "", y = "Counter", fill = "[%]",
             title = paste("Frequencies and Percentages"),
             subtitle = paste(
               "Variable:", ifelse(!is.na(variable_name), variable_name, variable))) +
        scale_fill_gradient(low = "lightskyblue2", high = "navy")
      
      # When two features
      if (ncol(output) - 3 == 3) { 
        p <- p + facet_grid(as.character(facet) ~ .) + 
          labs(subtitle = paste("Inside the facet grids:", facet_name)) +
          theme_light()
      }
      # When three features
      if (ncol(output) - 3 == 4) { 
        if (length(unique(facet_name2)) > 3) {
          stop("Please, try with a (third) variable with 3 or less cateogries!")
        }
        p <- p + facet_grid(as.character(facet2) ~ as.character(facet1)) + 
          labs(title = paste("Frequencies and Percentages:", facet_name1, "and", variable),
               subtitle = paste("Inside the facet grids:", facet_name2)) +
          theme_light()
      }
      plot(p)
    } else {
      # When more than two features
      message("Sorry, but trying to plot more than 3 features is as complex as it sounds...")
    } 
    
    # Export file name and folder for plot
    if (save == TRUE) {
      export_plot(p, "viz_freqs", vars, subdir = subdir)
    }
    
    output <- output %>% select(-order)
    
  }
  
  if (results == TRUE) {
    return(output)
  }
}
