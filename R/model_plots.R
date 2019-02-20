####################################################################
#' Density plot for discrete and continuous values
#' 
#' This function plots discrete and continuous values results
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param thresh Integer. Threshold for selecting binary or regression 
#' models: this number is the threshold of unique values we should 
#' have in 'tag' (more than: regression; less than: classification)
#' @param model_name Character. Model's name
#' @param subtitle Character. Subtitle to show in plot
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_density <- function(tag, 
                          score, 
                          thresh = 5,
                          model_name = NA, 
                          subtitle = NA, 
                          save = FALSE, 
                          subdir = NA,
                          file_name = "viz_distribution.png") {
  
  # require(ggplot2)
  # require(gridExtra)
  # require(scales)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  if (length(unique(tag)) <= thresh) {
    
    out <- data.frame(tag = as.character(tag),
                      score = as.numeric(score))
    
    if (max(out$score) <= 1) {
      out$score <- score * 100
    }
    
    p1 <- ggplot(out) + theme_minimal() +
      geom_density(aes(x = score, group = tag, fill = as.character(tag)), 
                   alpha = 0.6, adjust = 0.25) + 
      guides(fill = guide_legend(title="Tag")) + 
      xlim(0, 100) + 
      labs(title = "Classification Model Results",
           y = "Density by tag", x = "Score")
    
    p2 <- ggplot(out) + theme_minimal() + 
      geom_density(aes(x = score), 
                   alpha = 0.9, adjust = 0.25, fill = "deepskyblue") + 
      labs(x = "", y = "Density")
    
    p3 <- ggplot(out) + theme_minimal() + 
      geom_line(aes(x = score, y = (1 - ..y..), color = as.character(tag)), 
                stat = 'ecdf', size = 1) +
      geom_line(aes(x = score, y = (1 - ..y..)), 
                stat = 'ecdf', size = 0.5, colour = "black", linetype="dotted") +
      ylab('Cumulative') + xlab('') + guides(color=FALSE)
    
    if(!is.na(subtitle)) {
      p1 <- p1 + labs(subtitle = subtitle)
    }
    
    if(!is.na(model_name)) {
      p1 <- p1 + labs(caption = model_name)
    }
    
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep="/")
    }
    
    if(save == TRUE) {
      png(file_name, height = 1800, width = 2100, res = 300)
      grid.arrange(
        p1, p2, p3, 
        ncol = 2, nrow = 2, heights = 2:1,
        layout_matrix = rbind(c(1,1), c(2,3)))
      dev.off()
    }
    
    return(
      grid.arrange(
        p1, p2, p3, 
        ncol = 2, nrow = 2, heights = 2:1,
        layout_matrix = rbind(c(1,1), c(2,3))))
    
  } else {
    
    df <- data.frame(
      rbind(cbind(values = tag, type = "Real"), 
            cbind(values = score, type = "Model")))
    df$values <- as.numeric(as.character(df$values))
    
    p <- ggplot(df) + theme_minimal() +
      geom_density(aes(x = values, fill = as.character(type)), 
                   alpha = 0.6, adjust = 0.25) + 
      labs(y = "Density", x = "Continuous values") +
      scale_x_continuous(labels = comma) +
      guides(fill = guide_legend(override.aes = list(size=1))) +
      theme(legend.title=element_blank(),
            legend.position = "top")
    
    if(!is.na(model_name)) {
      p <- p + labs(caption = model_name)
    }
    
    if(!is.na(subtitle)) {
      p <- p + labs(subtitle = subtitle)
    }  
    
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep="/")
    }
    
    if (save == TRUE) {
      p <- p + ggsave(file_name, width = 6, height = 6)
    }
    return(p)
  }
}


####################################################################
#' Variables Importances Plot
#' 
#' This function plots Variable Importances
#' 
#' @param var Vector. Variable or column's names
#' @param imp Vector. Importance of said variables. Must have same length as var
#' @param colours If possitive and negative contribution is known
#' @param limit Integer. Limit how many variavles you wish to plot
#' @param model_name Character. Model's name
#' @param subtitle Character. Subtitle to show in plot
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_importance <- function(var, 
                             imp, 
                             colours = NA, 
                             limit = 15, 
                             model_name = NA, 
                             subtitle = NA,
                             save = FALSE, 
                             subdir = NA, 
                             file_name = "viz_importance.png") {
  
  # require(ggplot2)
  # require(gridExtra)
  options(warn=-1)
  
  if (length(var) != length(imp)) {
    message("The variables and importance values vectors should be the same length.")
    stop(message(paste("Currently, there are",length(var),"variables and",length(imp),"importance values!")))
  }
  
  if (is.na(colours)) {
    colours <- "deepskyblue" 
  }
  
  out <- data.frame(var = var, imp = imp, Type = colours)
  
  if (length(var) < limit) {
    limit <- length(var)
  }
  
  output <- out[1:limit,]
  
  p <- ggplot(output, 
              aes(x = reorder(var, imp), y = imp * 100, 
                  label = round(100 * imp, 1))) + 
    geom_col(aes(fill = Type), width = 0.1) +
    geom_point(aes(colour = Type), size = 6) + 
    coord_flip() + xlab('') + theme_minimal() +
    ylab('Importance') + 
    geom_text(hjust = 0.5, size = 2, inherit.aes = TRUE, colour = "white") +
    labs(title = paste0("Variables Importances. (", limit, " / ", length(var), " plotted)"))
  
  if (length(unique(output$Type)) == 1) {
    p <- p + geom_col(fill = colours, width = 0.2) +
      geom_point(colour = colours, size = 6) + 
      guides(fill = FALSE, colour = FALSE) + 
      geom_text(hjust = 0.5, size = 2, inherit.aes = TRUE, colour = "white")
  }
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }  
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir), recursive = T)
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
  
}


####################################################################
#' ROC Curve Plot
#' 
#' This function plots ROC Curves with AUC values with 95\% confidence range
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param model_name Character. Model's name
#' @param subtitle Character. Subtitle to show in plot
#' @param interval Numeric. Interval for breaks in plot
#' @param plotly Boolean. Use plotly for plot's output for an interactive plot
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_roc <- function(tag, 
                      score, 
                      model_name = NA, 
                      subtitle = NA, 
                      interval = 0.2, 
                      plotly = FALSE,
                      save = FALSE, 
                      subdir = NA, 
                      file_name = "viz_roc.png") {
  
  # require(pROC)
  # require(ggplot2)
  # require(plotly)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  roc <- pROC::roc(tag, score, ci=T)
  coords <- data.frame(
    x = rev(roc$specificities),
    y = rev(roc$sensitivities))
  ci <- data.frame(roc$ci, row.names = c("min","AUC","max"))
  
  
  p <- ggplot(coords, aes(x = x, y = y)) +
    geom_line(colour = "deepskyblue", size = 1) +
    geom_point(colour = "blue3", size = 0.9, alpha = 0.4) +
    geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), alpha = 0.2, linetype = "dotted") + 
    scale_x_reverse(name = "1 - Specificity [False Positive Rate]", limits = c(1,0), 
                    breaks = seq(0, 1, interval), expand = c(0.001,0.001)) + 
    scale_y_continuous(name = "Sensitivity [True Positive Rate]", limits = c(0,1), 
                       breaks = seq(0, 1, interval), expand = c(0.001, 0.001)) +
    theme_minimal() + 
    theme(axis.ticks = element_line(color = "grey80")) +
    coord_equal() + 
    ggtitle("ROC Curve: AUC") +
    annotate("text", x = 0.25, y = 0.10, size = 4.2, 
             label = paste("AUC =", round(100*ci[c("AUC"),],2))) +
    annotate("text", x = 0.25, y = 0.05, size = 2.8, 
             label = paste0("95% CI: ", round(100*ci[c("min"),],2),"-", round(100*ci[c("max"),],2)))
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }  
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if (plotly == TRUE) {
    p <- ggplotly(p)
  }
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir), recursive = T)
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
  
}


####################################################################
#' Cuts by quantiles for score plot
#' 
#' This function cuts by quantiles any score or prediction
#' 
#' @param score Vector. Predicted value or model's result
#' @param splits Integer. Numer of separations to plot
#' @param model_name Character. Model's name
#' @param subtitle Character. Subtitle to show in plot
#' @param table Boolean. Do you wish to return a table with results?
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_cuts <- function(score, 
                       splits = 10, 
                       model_name = NA, 
                       subtitle = NA, 
                       table = FALSE,
                       save = FALSE, 
                       subdir = NA, 
                       file_name = "viz_ncuts.png") {
  
  # require(ggplot2)
  options(warn=-1)
  
  if (splits > 25) {
    stop("You should try with less splits!")
  }
  
  deciles <- quantile(score, 
                      probs = seq((1/splits), 1, length = splits), 
                      names = TRUE)
  deciles <- data.frame(range = row.names(as.data.frame(deciles)),
                        cuts = as.vector(signif(deciles, 6)))
  rownames(deciles) <- NULL
  
  p <- ggplot(deciles, 
              aes(x = reorder(range, cuts), y = cuts * 100, 
                  label = round(100 * cuts, 1))) + 
    geom_col(fill="deepskyblue") + 
    xlab('') + theme_minimal() + ylab('Score') + 
    geom_text(vjust = 1.5, size = 3, inherit.aes = TRUE, colour = "white", check_overlap = TRUE) +
    labs(title = paste("Cuts by score: using", splits, "equal-sized buckets"))
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  } 
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir), recursive = T)
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  if(table == TRUE){
    return(deciles)
  } else {
    return(p) 
  }
}


####################################################################
#' Cuts by quantiles on absolut and percentual errors plot
#' 
#' This function cuts by quantiles on absolut and percentual errors
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param splits Integer. Numer of separations to plot
#' @param title Character. Title to show in plot
#' @param model_name Character. Model's name
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_cuts_error <- function(tag, 
                             score, 
                             splits = 10, 
                             title = NA, 
                             model_name = NA, 
                             save = FALSE, 
                             subdir = NA, 
                             file_name = "viz_ncuts_error.png") {
  
  if (splits > 25) {
    stop("You should try with less splits!")
  }
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  df <- data.frame(tag = tag, score = score) %>%
    mutate(real_error = tag - score,
           abs_error = abs(real_error),
           p_error = 100 * real_error/tag) %>%
    filter(abs(p_error) <= 150)
  
  # Useful function
  quants <- function(values, splits = 10, just = 0.3) {
    cuts <- quantile(values, 
                     probs = seq((1/splits), 1, length = splits), 
                     names = TRUE)
    cuts <- data.frame(deciles = names(cuts), cut = cuts)
    thresh <- max(cuts$cut) / 2
    cuts$gg_pos <- ifelse(cuts$cut > thresh, 1 + just, -just)
    cuts$colour <- ifelse(cuts$gg_pos < 0, "f", "m")
    row.names(cuts) <- NULL
    return(cuts)
  }
  
  # First: absolute errors
  deciles_abs <- quants(df$abs_error, splits = splits, just = 0.3)
  p_abs <- ggplot(deciles_abs, aes(x = reorder(deciles, cut), y = cut, label = signif(cut, 3))) +
    geom_col(fill="deepskyblue") + 
    xlab('') + theme_minimal() + ylab('Absolute Error') + 
    geom_text(aes(vjust = gg_pos, colour = colour), size = 2.7, inherit.aes = TRUE, check_overlap = TRUE) +
    labs(subtitle = paste("Cuts and distribution by absolute error")) +
    scale_y_continuous(labels = comma) + guides(colour=F) +
    gg_text_customs()
  
  # Second: percentual errors
  deciles_perabs <- quants(abs(df$p_error), splits = splits, just = 0.3)
  p_per <- ggplot(deciles_perabs, aes(x = reorder(deciles, cut), y = cut, label = signif(cut, 3))) +
    geom_col(fill="deepskyblue") + 
    xlab('') + theme_minimal() + ylab('Percetage Error') + 
    geom_text(aes(vjust = gg_pos, colour = colour), size = 2.7, inherit.aes = TRUE, check_overlap = TRUE) +
    labs(subtitle = paste("Cuts and distribution by absolute percentage error")) +
    scale_y_continuous(labels = comma) + guides(colour=F) +
    gg_text_customs()
  
  # Third: errors distribution
  pd_error <- ggplot(df) + 
    geom_density(aes(x=p_error), fill="deepskyblue", alpha = 0.7) +
    xlab('') + ylab('Error Density') + theme_minimal() +
    scale_x_continuous(labels=function(x) paste0(x,"%")) +
    geom_vline(xintercept = 0, alpha = 0.5, colour = "navy", linetype = "dotted")
  
  if(!is.na(title)) {
    p_abs <- p_abs + labs(title = title)
  } 
  
  if(!is.na(model_name)) {
    pd_error <- pd_error + labs(caption = model_name)
  }
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir), recursive = T)
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  if(save == TRUE) {
    png(file_name, height = 1800, width = 1800, res = 300)
    grid.arrange(
      p_abs, p_per, pd_error,
      heights = c(1.8,1.8,1),
      ncol = 1, nrow = 3)
    dev.off()
  }
  
  return(
    grid.arrange(
      p_abs, p_per, pd_error,
      heights = c(2,2,1),
      ncol = 1, nrow = 3)
  )
  
}


####################################################################
#' Split and compare quantiles plot
#' 
#' This function lets us split and compare quantiles on a given prediction to
#' compare different categorical values vs scores grouped by equal sized buckets.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param splits Integer. Numer of separations to plot
#' @param subtitle Character. Subitle to show in plot
#' @param model_name Character. Model's name
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_splits <- function(tag, 
                         score, 
                         splits = 5, 
                         subtitle = NA, 
                         model_name = NA,
                         save = FALSE, 
                         subdir = NA, 
                         file_name = "viz_splits.png") {
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  if (splits > 10) {
    stop("You should try with less splits!")
  }
  
  df <- data.frame(tag, score)
  npersplit <- round(nrow(df)/splits)
  
  # For continuous tag values
  if (length(unique(tag)) > 6) {
    names <- df %>% 
      mutate(tag = as.numeric(tag), 
             quantile = ntile(tag, splits)) %>% group_by(quantile) %>%
      summarise(n = n(), 
                max_score = round(max(tag), 1), 
                min_score = round(min(tag), 1)) %>%
      mutate(quantile_tag = paste0(quantile," (",min_score,"-",max_score,")"))
    df <- df %>% 
      mutate(quantile = ntile(tag, splits)) %>%
      left_join(names, by = c("quantile")) %>% mutate(tag = quantile_tag) %>% 
      select(-quantile, -n, -max_score, -min_score)
    
  } else {
    # For categorical tag values
    names <- df %>% 
      mutate(quantile = ntile(score, splits)) %>% group_by(quantile) %>%
      summarise(n = n(), 
                max_score = signif(max(score), 2), 
                min_score = signif(min(score), 2)) %>%
      mutate(quantile_tag = paste0(quantile," (",min_score,"-",max_score,")")) 
  }
  
  p <- df %>% 
    mutate(quantile = ntile(score, splits)) %>% 
    group_by(quantile, tag) %>% tally() %>%
    ungroup() %>% group_by(tag) %>% 
    arrange(desc(quantile)) %>%
    mutate(p = round(100*n/sum(n),2),
           cum = cumsum(100*n/sum(n))) %>%
    left_join(names, by = c("quantile")) %>%
    ggplot(aes(x = as.character(tag), y = p, label = as.character(p),
               fill = reorder(as.character(quantile_tag),quantile))) +
    geom_col(position = "stack") + theme_minimal() +
    geom_text(size = 3, position = position_stack(vjust = 0.5), check_overlap = TRUE) +
    xlab("Tag") + ylab("Total Percentage by Tag") +
    guides(fill = guide_legend(title=paste0("~",npersplit," p/split"))) +
    labs(title = "Tag vs Score Splits Comparison") +
    scale_fill_brewer(palette = "Spectral")
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }  
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir), recursive = T)
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
  
}


####################################################################
#' AUC and LogLoss Plots
#' 
#' This function can plot AUC and LogLoss Plots from a h2o_automl results object
#' 
#' @param results Object. Results object from h2o_automl function
#' @param subtitle Character. Subitle to show in plot
#' @param model_name Character. Model's name
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_metrics <- function(results, 
                          subtitle = NA, 
                          model_name = NA, 
                          save = FALSE, 
                          subdir = NA,
                          file_name = "viz_metrics.png") {
  
  plots_data <- data.frame(
    trees = results$model@model$scoring_history$number_of_trees,
    train_ll = results$model@model$scoring_history$training_logloss,
    test_ll = results$model@model$scoring_history$validation_logloss,
    train_auc = results$model@model$scoring_history$training_auc,
    test_auc = results$model@model$scoring_history$validation_auc)
  ll <- ggplot(plots_data) + theme_minimal() +
    geom_hline(yintercept = 0.69315, alpha = 0.5, linetype = 'dotted') + 
    geom_line(aes(x=trees, y=train_ll, colour="Train"), size=0.5) +
    geom_line(aes(x=trees, y=test_ll, colour="Test"), size=1) +
    labs(title = "Logarithmic Loss vs Number of Trees",
         colour = "Dataset", x = "# of trees", y = "LogLoss") +
    scale_colour_brewer(palette = "Set1") + 
    geom_text(aes(x=trees, y=train_ll, colour="Train", 
                  label=round(train_ll,2)),
              check_overlap = TRUE, nudge_y=0.03, size=3) +
    geom_text(aes(x=trees, y=test_ll, colour="Test", 
                  label=round(test_ll,2)),
              check_overlap = TRUE, nudge_y=0.03, size=3) + 
    theme(strip.text.x = element_blank(),
          strip.background = element_rect(colour="white", fill="white"),
          legend.position=c(0.1, 0.05))
  au <- ggplot(plots_data) + theme_minimal() +
    geom_line(aes(x=trees, y=train_auc*100, colour="Train"), size=0.5) +
    geom_line(aes(x=trees, y=test_auc*100, colour="Test"), size=1) +
    geom_hline(yintercept = 50, alpha = 0.5, linetype = 'dotted', colour="black") + 
    labs(title = "Area Under the Curve vs Number of Trees",
         colour = "Dataset", x = "# of trees", y = "AUC") +
    scale_colour_brewer(palette = "Set1") + guides(colour=FALSE) +
    geom_text(aes(x=trees, y=train_auc*100, colour="Train", 
                  label=round(train_auc*100,2)),
              check_overlap = TRUE, nudge_y=3, size=3) +
    geom_text(aes(x=trees, y=test_auc*100, colour="Test", 
                  label=round(test_auc*100,2)),
              check_overlap = TRUE, nudge_y=3, size=3)
  
  if(!is.na(subtitle)) {
    ll <- ll + labs(subtitle = subtitle)
  }  
  
  if(!is.na(model_name)) {
    au <- au + labs(caption = model_name)
  }
  
  if (save == TRUE) {
    
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep="/")
    }
    
    png(file_name, height = 1800, width = 2100, res = 300)
    grid.arrange(ll, au, ncol = 1, nrow = 2)
    dev.off()
  }
  
  return(grid.arrange(ll, au, ncol = 1, nrow = 2))
  
}


####################################################################
#' Linear Regression Results Plot
#' 
#' This function plots a Linear Regression Result
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param regression Boolean. Plot with geom_smooth's lm regression?
#' @param subtitle Character. Subitle to show in plot
#' @param model_name Character. Model's name
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_lineal <- function(tag, 
                         score, 
                         regression = FALSE,
                         subtitle = NA, 
                         model_name = NA, 
                         save = FALSE, 
                         subdir = NA,
                         file_name = "viz_lineal.png") {

  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  results <- data.frame(tag = tag, score = score, dist = 0)
  results <- results[complete.cases(results), ]
  
  for (i in 1:nrow(results)) { 
    results$dist[i] <- dist2d(c(results$tag[i],results$score[i]), c(0,0), c(1,1)) 
  }
  
  fit <- lm(results$score ~ results$tag)
  labels <- paste(
    paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 4)),
    #paste("Pval =", signif(summary(fit)$coef[2,4], 3)), 
    paste("RMSE =", signif(rmse(results$tag, results$score), 4)), 
    paste("MAE =", signif(mae(results$tag, results$score), 4)), 
    sep="\n")
  
  p <- ggplot(results, aes(x = tag, y = score, colour = dist)) +
    geom_point() + theme_minimal() + 
    labs(title = "Regression Model Results",
         x = "Real value", y = "Predicted value",
         colour = "Deviation") +
    annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -0.05, label = labels, size = 2.8) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    scale_colour_continuous(labels = comma) +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1)) +
    guides(colour = guide_colorbar(barwidth = 0.9, barheight = 4.5))
  
  if (regression == TRUE) {
    p <- p + geom_smooth("lm", alpha = 0.8) 
  }
  
  # Draw reference line for correlation
  intercept <- summary(fit)$coefficients[1]
  slope <- summary(fit)$coefficients[2]
  p <- p + geom_abline(slope = slope, intercept = intercept, 
                       alpha = 0.5, colour = "orange", size=0.6)
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }  
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
  
}


####################################################################
#' MPLOTS Score Full Report Plots
#' 
#' This function plots a whole dashboard with a model's results. It will automatically
#' detect if it's a categorical or regression's model by checking how many different
#' unique values the independent variable (tag) has.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param splits Integer. Numer of separations to plot
#' @param thresh Integer. Threshold for selecting binary or regression 
#' models: this number is the threshold of unique values we should 
#' have in 'tag' (more than: regression; less than: classification)
#' @param subtitle Character. Subitle to show in plot
#' @param model_name Character. Model's name
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_full <- function(tag, 
                       score, 
                       splits = 8, 
                       thresh = 5,
                       subtitle = NA, 
                       model_name = NA, 
                       save = FALSE, 
                       subdir = NA,
                       file_name = "viz_full.png") {
  
  # require(ggplot2)
  # require(gridExtra)
  # require(dplyr)
  
  options(warn=-1)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", length(tag), "rows and score has", length(score))))
  }
  
  # Categorical Models
  if (length(unique(tag)) <= thresh) {
    p1 <- mplot_density(tag = tag, score = score, 
                        subtitle = subtitle, model_name = model_name)
    p2 <- mplot_splits(tag = tag, score = score, splits = splits)
    p3 <- mplot_roc(tag = tag, score = score)
    p4 <- mplot_cuts(score = score) 
    
    if(save == TRUE) {
      
      if (!is.na(subdir)) {
        dir.create(file.path(getwd(), subdir), recursive = T)
        file_name <- paste(subdir, file_name, sep="/")
      }
      
      png(file_name, height = 2000, width = 3200, res = 300)
      grid.arrange(
        p1, p2, p3, p4,
        widths = c(1.3,1),
        layout_matrix = rbind(c(1,2), c(1,2), c(1,3), c(4,3)))
      dev.off()
    }
    
    return(
      grid.arrange(
        p1, p2, p3, p4,
        widths = c(1.3,1),
        layout_matrix = rbind(c(1,2), c(1,2), c(1,3), c(4,3)))
    ) 
    
  } else {
    
    # Numerical models
    p1 <- mplot_lineal(tag = tag, score = score, subtitle = subtitle, model_name = model_name)
    p2 <- mplot_density(tag = tag, score = score)
    p3 <- mplot_cuts_error(tag = tag, score = score, splits = splits)
    
    if(save == TRUE) {
      
      if (!is.na(subdir)) {
        dir.create(file.path(getwd(), subdir), recursive = T)
        file_name <- paste(subdir, file_name, sep="/")
      }
      
      png(file_name, height = 2000, width = 3200, res = 300)
      grid.arrange(
        p1, p2, p3,
        heights = c(0.6, 0.4),
        widths = c(0.45, 0.55),
        layout_matrix = rbind(c(1,3), c(2,3)))
      dev.off()
    }
    
    return(grid.arrange(
      p1, p2, p3,
      heights = c(0.6, 0.4),
      widths = c(0.45, 0.55),
      layout_matrix = rbind(c(1,3), c(2,3)))
    )
  }
}
