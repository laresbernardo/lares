####################################################################
#' Frequencies Calculations and Plot
#' 
#' This function lets the user group, count, calculate percentages 
#' and cumulatives. It also plots results if needed. Tidyverse friendly.
#' 
#' @param vector Data.frame
#' @param ... Variables. Variables you wish to process. Order matters.
#' @param wt Variable, numeric. Weights.
#' @param results Boolean. Return results in a dataframe?
#' @param variable_name Character. Overwrite the main variable's name
#' @param plot Boolean. Do you want to see a plot? Three variables tops.
#' @param rm.na Boolean. Remove NA values in the plot? (not filtered for 
#' numerical output; use na.omit() or filter() if needed)
#' @param title Character. Overwrite plot's title with.
#' @param subtitle Character. Overwrite plot's subtitle with.
#' @param top Integer. Filter and plot the most n frequent for 
#' categorical values. Set to NA to return all values
#' @param abc Boolean. Do you wish to sort by alphabetical order? If set
#' to FALSE then frequency will sort the values
#' @param save Boolean. Save the output plot in our working directory
#' @param subdir Character. Into which subdirectory do you wish to 
#' save the plot to?
#' @export
freqs <- function(vector, ..., wt = NULL,
                  results = TRUE, 
                  variable_name = NA, 
                  plot = FALSE, rm.na = FALSE,
                  title = NA, subtitle = NA,
                  top = 20, abc = FALSE,
                  save = FALSE, subdir = NA) {
  
  vars <- quos(...)
  weight <- enquo(wt)
  
  output <- vector %>%
  {if (as.character(weight)[2] == "NULL") group_by(., !!!vars) %>% tally() else 
    count(., !!!vars, wt = !!weight)} %>%
    arrange(desc(n)) %>%
    mutate(p = round(100*n/sum(n),2), pcum = cumsum(p))
  
  if (plot == TRUE | save == TRUE) {
    
    if (ncol(output) - 3 >= 4) {
      # When more than two features
      message(paste(
        "Sorry, but trying to plot more than 3 features is as complex as it sounds.",
        "You should try another method to understand your analysis!"))
    } else {
      obs_total <- sum(output$n)
      obs <- paste("Total Obs.:", formatNum(obs_total, 0))
      weight_text <- ifelse((as.character(weight)[2] != "NULL"), 
                            paste0("(weighted by ", as.character(weight)[2], ")"), "")
      
      # Use only the most n frequent values/combinations only
      values <- unique(output[,(ncol(output)-3)],)
      if(nrow(values) > top) {
        if (!is.na(top)) {
          output <- output %>% slice(1:top)
          message(paste0("Slicing the top ", top, 
                         " (out of ", nrow(values),
                         ") frequencies; use 'top' parameter to overrule."))
          note <- paste0("(", top, " most frequent)")
          obs <- paste0("Obs.: ", formatNum(sum(output$n), 0), " (out of ", formatNum(obs_total, 0), ")")
        }
      } else { note <- "" }
      
      # Sort values alphabetically or ascending if numeric
      if (abc == TRUE) {
        message("Sorting variable(s) alphabetically")
        output <- output %>% arrange(!!!vars, desc(n)) %>% 
          mutate(order = row_number())
      } else {
        output <- output %>% arrange(desc(n)) %>%
          mutate(order = row_number())
      }
      
      if (ncol(output) - 3 <= 4) { 
        
        options(warn=-1)
        
        reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
          new_x <- paste(x, within, sep = sep)
          stats::reorder(new_x, by, FUN = fun)
        }
        scale_x_reordered <- function(..., sep = "___") {
          reg <- paste0(sep, ".+$")
          ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
        }
        
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
        
        # When one feature
        if (ncol(output) - 3 == 2) { 
          type <- 1
          colnames(plot)[1] <- "names"
          p <- ggplot(plot, aes(x = reorder(names, -order), y = n,label = labels, fill = p))
        }
        # When two features
        if (ncol(output) - 3 == 3) { 
          type <- 2
          facet_name <- colnames(plot)[2]
          colnames(plot)[1] <- "facet"
          colnames(plot)[2] <- "names"
          plot$facet[is.na(plot$facet)] <- "NA"
          p <- plot %>%
            ggplot(aes(x = reorder_within(names, -order, facet), 
                       y = n, label = labels, fill = p)) +
            scale_x_reordered()
        }
        # When three features
        if (ncol(output) - 3 == 4) { 
          type <- 3
          facet_name1 <- colnames(plot)[2]
          facet_name2 <- colnames(plot)[3]
          colnames(plot)[1] <- "facet2"
          colnames(plot)[2] <- "facet1"
          colnames(plot)[3] <- "names"
          plot$facet2[is.na(plot$facet2)] <- "NA"
          plot$facet1[is.na(plot$facet1)] <- "NA"
          p <- plot %>%
            ggplot(aes(x = reorder_within(names, n, facet2), 
                       y = n, label = labels, fill = p)) +
            scale_x_reordered()
        }
        
        # Plot base
        p <- p + geom_col(alpha=0.9, width = 0.8) +
          geom_text(aes(hjust = label_hjust, colour = label_colours), size = 2.6) + 
          coord_flip() + theme_minimal() + guides(colour = FALSE) +
          labs(x = "", y = "Counter", fill = "[%]",
               title = ifelse(is.na(title), paste("Frequencies and Percentages"), title),
               subtitle = ifelse(is.na(subtitle), 
                                 paste("Variable:", ifelse(!is.na(variable_name), variable_name, variable), weight_text, note), 
                                 subtitle), caption = obs) +
          scale_y_continuous(labels = comma) +
          scale_fill_gradient(low = "lightskyblue2", high = "navy") +
          gg_text_customs() + theme_lares2() +
          theme(legend.position="none")
        
        # When two features
        if (type == 2) { 
          p <- p + 
            facet_grid(as.character(facet) ~ ., scales = "free", space = "free") + 
            labs(subtitle = ifelse(is.na(subtitle), 
                                   paste("Variables:", facet_name, "grouped by", variable, "\n", weight_text, note), 
                                   subtitle),
                 caption = obs)
        }
        
        # When three features
        if (type == 3) { 
          if (length(unique(facet_name2)) > 3) {
            stop("Please, try with a (third) variable with 3 or less cateogries!")
          }
          p <- p + facet_grid(as.character(facet2) ~ as.character(facet1), scales = "free") + 
            labs(title = ifelse(is.na(title), "Frequencies and Percentages:", title),
                 subtitle = ifelse(is.na(subtitle), 
                                   paste("Variables:", facet_name2, "grouped by", facet_name1, "[x] and", 
                                         variable, "[y]", "\n", weight_text, note), 
                                   subtitle),
                 caption = obs)
        }
        return(p)
      }
      
      # Export file name and folder for plot
      if (save == TRUE) {
        export_plot(p, "viz_freqs", vars, subdir = subdir)
      }
      output <- output %>% select(-order)
    }
    return(p)
  }
  
  if (results == TRUE) {
    return(output)
  }
}
