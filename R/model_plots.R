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
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  if (length(unique(tag)) <= thresh) {
    
    out <- data.frame(tag = as.character(tag), score = score)
    
    if (is.numeric(out$score)) {
      if (max(out$score) <= 1) {
        out$score <- score * 100 
      }
    }
    
    p1 <- ggplot(out) + 
      geom_density(aes(x = as.numeric(score), 
                       group = tag, fill = as.character(tag)), 
                   alpha = 0.6, adjust = 0.25) + 
      guides(fill = guide_legend(title="Tag")) + 
      labs(title = "Classification Model Results",
           y = "Density by tag", x = "Score", fill = "") + 
      theme_lares2(pal = 1) +
      theme(legend.position = "top",
            legend.justification = c(0, 0),
            legend.title = element_blank())
    
    # if (is.numeric(score)) {
    #   p1 <- p1 + xlim(0, 100) 
    # }
    
    p2 <- ggplot(out) + 
      geom_density(aes(x = score), alpha = 0.9, adjust = 0.25, fill = "deepskyblue") + 
      labs(x = "", y = "Density") + theme_lares2()
    
    p3 <- ggplot(out) + 
      geom_line(aes(x = as.numeric(score), y = (1 - ..y..), color = as.character(tag)), 
                stat = 'ecdf', size = 1) +
      geom_line(aes(x = as.numeric(score), y = (1 - ..y..)), 
                stat = 'ecdf', size = 0.5, colour = "black", linetype="dotted") +
      ylab('Cumulative') + xlab('') + guides(color=FALSE) + theme_lares2(pal = 2)
    
    p1 <- p1 + theme(plot.margin = margin(10, 5, 0, 5))
    p2 <- p2 + theme(plot.margin = margin(0, 0, 5, 5))
    p3 <- p3 + theme(plot.margin = margin(0, 5, 5, 0))
    
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
    
    p <- arrangeGrob(p1, p2, p3, 
                     ncol = 2, nrow = 2, heights = 2:1,
                     layout_matrix = rbind(c(1,1), c(2,3)))
    
  } else {
    
    df <- data.frame(
      rbind(cbind(values = tag, type = "Real"), 
            cbind(values = score, type = "Model")))
    df$values <- as.numeric(as.character(df$values))
    
    p <- ggplot(df) + 
      geom_density(aes(x = values, fill = as.character(type)), 
                   alpha = 0.6, adjust = 0.25) + 
      labs(y = "Density", x = "Continuous values", fill="") +
      guides(colour = FALSE) +
      theme_lares2(pal = 1, legend = "top")
    
    if(!is.na(model_name)) {
      p <- p + labs(caption = model_name)
    }
    
    if(!is.na(subtitle)) {
      p <- p + labs(subtitle = subtitle)
    }  
    
  }
  
  if (save == TRUE) {
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep="/")
    }
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
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
    geom_col(aes(fill = Type), width = 0.08) +
    geom_point(aes(colour = Type), size = 6.2) + 
    coord_flip() + xlab('') + 
    ylab('Importance') + 
    geom_text(hjust = 0.5, size = 2.1, inherit.aes = TRUE, colour = "white") +
    labs(title = paste0("Most Relevant Variables (top ", limit, " of ", length(var), ")")) +
    theme_lares2()
  
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
  
  if (save == TRUE) {
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep="/")
    }
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
  
}


####################################################################
#' ROC Curve Plot
#' 
#' This function plots ROC Curves with AUC values with 95\% confidence 
#' range. It also works for multi-categorical models.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param multis Data.frame. Containing columns with each category score 
#' (only used when more than 2 categories coexist)
#' @param sample Integer. Number of samples to use for rendering plot.
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
                      multis = NA,
                      sample = 1000,
                      model_name = NA, 
                      subtitle = NA, 
                      interval = 0.2, 
                      plotly = FALSE,
                      save = FALSE, 
                      subdir = NA, 
                      file_name = "viz_roc.png") {
  
  # require(ggplot2)
  # require(plotly)
  
  if (is.na(multis)) {
    rocs <- ROC(tag, score)
    ci <- rocs$ci
  } else {
    rocs <- ROC(tag, score, multis)
    ci <- rocs$ci["mean"]
  }
  coords <- rocs$roc
  
  if (sample < min(table(coords$label))) {
    coords <- coords %>% group_by(label) %>% sample_n(sample)
    message("ROC Curve Plot rendered with sampled data...")
  }
  
  scale <- function(x) sprintf("%.1f", x)
  p <- ggplot(coords, aes(x = fpr, y = tpr, group=label)) +
    geom_line(colour = "deepskyblue", size = 0.8) +
    geom_point(aes(colour=label), size = 0.7, alpha = 0.8) +
    geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), alpha = 0.2, linetype = "dotted") + 
    scale_x_reverse(name = "1 - Specificity [False Positive Rate]", limits = c(1,0), 
                    breaks = seq(0, 1, interval), expand = c(0.001,0.001),
                    labels=scale) + 
    scale_y_continuous(name = "Sensitivity [True Positive Rate]", limits = c(0,1), 
                       breaks = seq(0, 1, interval), expand = c(0.001, 0.001),
                       labels=scale) +
    coord_equal() +
    theme(axis.ticks = element_line(color = "grey80")) +
    labs(title = "ROC Curve: AUC", colour = "") +
    guides(colour = guide_legend(ncol = 3)) +
    annotate("text", x = 0.25, y = 0.10, size = 4.2, 
             label = paste("AUC =", round(100*ci[c(2),],2))) +
    annotate("text", x = 0.25, y = 0.05, size = 2.8, 
             label = paste0("95% CI: ", round(100*ci[c(1),],2),"-", 
                            round(100*ci[c(3),],2))) +
    theme_lares2(bg_colour = "white", pal = 2, legend = "bottom")
  
  if (is.na(multis)) {
    p <- p + guides(colour = FALSE)
  }
  
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
  
  p <- deciles %>%
    #mutate(label_colours = ifelse(cuts*100 < 50, "1", "m")) %>%
    ggplot(aes(x = reorder(range, cuts), y = cuts * 100)) + 
    geom_col(fill="deepskyblue") + 
    xlab('Cumulative volume') + ylab('Score') + 
    geom_text(aes(label = round(100 * cuts, 1),
                  vjust = ifelse(cuts*100 < 50, -0.3, 1.3)), 
              size = 3, colour = "black", inherit.aes = TRUE, check_overlap = TRUE) +
    guides(colour = FALSE) +
    labs(title = paste0("Score cuts (", splits, " equal-sized buckets)")) +
    theme_lares2()
  
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
    xlab('') + ylab('Absolute Error') + 
    geom_text(aes(vjust = gg_pos, colour = colour), size = 2.7, inherit.aes = TRUE, check_overlap = TRUE) +
    labs(subtitle = paste("Cuts and distribution by absolute error")) +
    scale_y_continuous(labels = comma) + guides(colour=F) +
    gg_text_customs() + theme_lares2(bg_colour = "white")
  
  # Second: percentual errors
  deciles_perabs <- quants(abs(df$p_error), splits = splits, just = 0.3)
  p_per <- ggplot(deciles_perabs, aes(x = reorder(deciles, cut), y = cut, label = signif(cut, 3))) +
    geom_col(fill="deepskyblue") + 
    xlab('') + ylab('Percetage Error') + 
    geom_text(aes(vjust = gg_pos, colour = colour), size = 2.7, inherit.aes = TRUE, check_overlap = TRUE) +
    labs(subtitle = paste("Cuts and distribution by absolute percentage error")) +
    scale_y_continuous(labels = comma) + guides(colour=F) +
    gg_text_customs() + theme_lares2(bg_colour = "white")
  
  # Third: errors distribution
  pd_error <- ggplot(df) + 
    geom_density(aes(x=p_error), fill="deepskyblue", alpha = 0.7) +
    xlab('') + ylab('Error Density') + 
    scale_x_continuous(labels=function(x) paste0(x,"%")) +
    geom_vline(xintercept = 0, alpha = 0.5, colour = "navy", linetype = "dotted") + 
    theme_lares2(bg_colour = "white")
  
  if(!is.na(title)) {
    p_abs <- p_abs + labs(title = title)
  } 
  
  if(!is.na(model_name)) {
    pd_error <- pd_error + labs(caption = model_name)
  }
  
  p <- arrangeGrob(
    p_abs, p_per, pd_error,
    heights = c(1.8,1.8,1),
    ncol = 1, nrow = 3)
  
  if(save == TRUE) {
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep="/")
    }
    png(file_name, height = 1800, width = 1800, res = 300)
    plot(p)
    dev.off()
  }
  
  return(p)
  
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
      mutate(quantile_tag = paste0(quantile," (",
                                   round(100*min_score,1),"-",
                                   round(100*max_score,1),")")) 
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
    geom_col(position = "stack") + 
    geom_text(size = 3, position = position_stack(vjust = 0.5), check_overlap = TRUE) +
    xlab("Tag") + ylab("Total Percentage by Tag") +
    guides(fill = guide_legend(title=paste0("~",npersplit," p/split"))) +
    labs(title = "Tag vs Score Splits Comparison") +
    scale_fill_brewer(palette = "Spectral") +
    theme_lares2()
  
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
  ll <- ggplot(plots_data) + 
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
    theme_lares2(pal = 1) +
    theme(strip.text.x = element_blank(),
          strip.background = element_rect(colour="white", fill="white"),
          legend.position=c(0.1, 0.05))
  au <- ggplot(plots_data) + 
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
              check_overlap = TRUE, nudge_y=3, size=3) +
    theme_lares2(pal = 1)
  
  if(!is.na(subtitle)) {
    ll <- ll + labs(subtitle = subtitle)
  }  
  
  if(!is.na(model_name)) {
    au <- au + labs(caption = model_name)
  }
  
  p <- arrangeGrob(ll, au, ncol = 1, nrow = 2)
  
  if (save == TRUE) {
    
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep="/")
    }
    
    png(file_name, height = 1800, width = 2100, res = 300)
    plot(p)
    dev.off()
  }
  
  return(plot(p))
  
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
    geom_point() + theme_lares2() +
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
#' @param score Vector. Predicted value or model's result. Must be numeric
#' for categorical binomial models and continuous regression models; must
#' be categorical for multi-categorical models (also need multis param).
#' @param multis Data.frame. Containing columns with each category score 
#' (only used when more than 2 categories coexist)
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
                       multis = NA,
                       splits = 8, 
                       thresh = 6,
                       subtitle = NA, 
                       model_name = NA, 
                       save = FALSE, 
                       subdir = NA,
                       file_name = "viz_full.png") {
  
  options(warn=-1)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", length(tag), "rows and score has", length(score))))
  }
  
  
  # Categorical Binomial Models
  if (length(unique(tag)) == 2 & is.numeric(score)) {
    
    p1 <- mplot_density(tag = tag, score = score, subtitle = subtitle, model_name = model_name)
    p2 <- mplot_splits(tag = tag, score = score, splits = splits) +
      theme(plot.margin = margin(10, 8, 5, 0))
    p3 <- mplot_roc(tag = tag, score = score) +
      theme(plot.margin = margin(0, 8, 5, 0))
    p4 <- mplot_cuts(score = score) +
      theme(plot.margin = margin(-3, 0, 5, 8))
    
    p <- arrangeGrob(
      p1, p2, p3, p4,
      widths = c(1.3,1),
      layout_matrix = rbind(c(1,2), c(1,2), 
                            c(1,3), c(4,3)))
  }
  
  # Multi-Categorical Models
  if (length(unique(tag)) > 2 & length(unique(tag)) <= thresh) {
    m <- model_metrics(tag, score, multis)
    p <- arrangeGrob(
      m$plots$conf_matrix + 
        labs(title = "Confusion Matrix", 
             caption = if (!is.na(model_name)) model_name), 
      m$plots$ROC, 
      ncol = 2, nrow = 1)
  }  
  
  # Regression Continuous Models
  if (is.numeric(tag) & is.numeric(score) & length(unique(tag)) > thresh) {
    
    p1 <- mplot_lineal(tag = tag, score = score, subtitle = subtitle, model_name = model_name) +
      theme_lares2(bg_colour = "white")
    p2 <- mplot_density(tag = tag, score = score)
    p3 <- mplot_cuts_error(tag = tag, score = score, splits = splits)
    
    p <- arrangeGrob(
      p1, p2, p3,
      heights = c(0.6, 0.4),
      widths = c(0.45, 0.55),
      layout_matrix = rbind(c(1,3), c(2,3)))
  }
  
  if (save == TRUE) {
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep="/")
    }
    png(file_name, height = 2000, width = 3200, res = 300)
    plot(p)
    dev.off()
  }
  
  plot(p)
  
}


####################################################################
#' Confussion Matrix Plot
#' 
#' This function plots a confussion matrix.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param thresh Numeric. Value which splits the results for the 
#' confusion matrix.
#' @param subtitle Character. Subtitle to show in plot
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_conf <- function (tag, score, thresh = 0.5,
                        subtitle = NA, save = FALSE, subdir = NA, 
                        file_name = "viz_conf_mat.png") {
  
  df <- data.frame(tag, score)
  
  # About tags
  labels <- df %>% group_by(tag) %>% tally() %>% arrange(desc(n)) %>% .$tag
  df <- df %>% mutate(tag = factor(tag, levels = unique(tag)))
  
  # About scores
  if (is.numeric(df$score) & length(unique(tag)) == 2) {
    means <- df %>% group_by(tag) %>% summarise(mean = mean(score))
    target <- means$tag[means$mean == max(means$mean)]
    other <- means$tag[means$mean == min(means$mean)]
    df <- df %>% mutate(pred = ifelse(
      score >= thresh, as.character(target), as.character(other)))
  } else {
    df <- df %>% mutate(pred = score)
  }
  
  # Frequencies
  plot_cf <- df %>% freqs(tag, pred) %>% 
    mutate(label = paste0(formatNum(n, 0),"\n", p,"%"))
  levels <- plot_cf %>% group_by(pred) %>% summarise(n = sum(n)) %>% 
    arrange(desc(n)) %>% mutate(pred = factor(pred, levels = labels)) %>% .$pred
  trues <- plot_cf %>% filter(tag == pred) %>% .$n %>% sum(.)
  total <- sum(plot_cf$n)
  metrics <- paste0("ACC ", round(100*(trues / total), 2), "%")
  
  p <- ggplot(plot_cf, aes(
    y = factor(tag, levels = rev(labels)), 
    x = factor(pred, levels = labels), 
    fill= n, size=n, 
    label = label)) +
    geom_tile() + theme_lares2() +
    geom_text(colour="white") + 
    scale_size(range = c(3.1, 4.5)) + coord_equal() + 
    guides(fill=FALSE, size=FALSE, colour=FALSE) +
    labs(x="Predicted values", y="Real values",
         title = ifelse(length(labels) == 2,
                        paste("Confusion Matrix", 
                              ifelse(thresh!=0.5, paste("with Threshold =", thresh), "")),
                        paste0("Confusion Matrix (", length(labels), " categories)")),
         subtitle = metrics) +
    theme(axis.text.x = element_text(angle=30, hjust=0)) +
    scale_x_discrete(position = "top") +
    theme(axis.text.x.bottom = element_blank(), 
          axis.ticks.x.bottom = element_blank(),
          axis.text.y.right = element_blank(),
          axis.ticks.y.right = element_blank()) +
    theme_lares2()
  
  if (!is.na(subtitle)) {
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
#' Cumulative Gain Plot
#' 
#' The cumulative gains plot, often named ‘gains plot’, helps us 
#' answer the question: When we apply the model and select the best 
#' X deciles, what % of the actual target class observations can we 
#' expect to target?
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param target Value. Which is your target positive value? If 
#' set to 'auto', the target with largest mean(score) will be 
#' selected. Change the value to overwrite.
#' @param splits Integer. Numer of quantiles to split the data
#' @param highlight Character or Integer. Which split should be used
#' for the automatic conclussion in the plot? Set to "auto" for
#' best value, "none" to turn off or the number of split.
#' @param caption Character. Caption to show in plot
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @param quiet Boolean. Do not show message for auto target?
#' @export
mplot_gain <- function(tag, score, target = "auto", splits = 10, highlight = "auto", 
                       caption = NA, save = FALSE, subdir = NA, 
                       file_name = "viz_gain.png", quiet = FALSE) {
  
  gains <- gain_lift(tag, score, target, splits, quiet = quiet) 
  
  p <- gains %>%
    mutate(percentile = as.numeric(percentile)) %>%
    ggplot(aes(x = percentile)) + theme_lares2(pal=2) +
    geom_line(aes(y = optimal, linetype = "Optimal"), colour = "black", alpha = 0.6) +
    geom_line(aes(y = random, linetype = "Random"), colour = "black", alpha = 0.6) +
    geom_line(aes(y = gain, linetype = "Model"), colour = "darkorange", size = 1.2) +
    geom_label(aes(y = gain, label = ifelse(gain == 100, NA, round(gain))), alpha = 0.9) +
    scale_y_continuous(breaks = seq(0, 100, 10)) + guides(colour=FALSE) +
    scale_x_continuous(minor_breaks = NULL, 
                       breaks = seq(0, splits, 1)) +
    labs(title = "Cumulative Gains Plot", linetype = "",
         y = "Cumulative gains [%]", 
         x = paste0("Percentiles [",splits,"]")) +
    theme(legend.position = c(0.88, 0.2))
  
  if (highlight == "auto") {
    highlight <- as.integer(gains$percentile[gains$lift == max(gains$lift)])
  }
  if (highlight %in% gains$percentile & highlight != "none") {
    highlight <- as.integer(highlight)
    note <- paste0("If we select the top ", 
                   round(highlight*100/splits),"% observations with highest scores,\n",
                   round(gains$gain[gains$percentile == highlight]),"% of all target class will be picked ",
                   "(", round(gains$lift[gains$percentile == highlight]), "% better than random)")
    p <- p + labs(subtitle = note)
  } else {
    message("That highlight value is not a percentile. Try any integer from 1 to ", splits)
  }
  
  if (!is.na(caption)) {
    p <- p + labs(caption = caption)
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
#' Cumulative Response Plot
#' 
#' The response gains plot helps us answer the question: When we 
#' apply the model and select up until ntile X, what is the expected 
#' % of target class observations in the selection?
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param target Value. Which is your target positive value? If 
#' set to 'auto', the target with largest mean(score) will be 
#' selected. Change the value to overwrite.
#' @param splits Integer. Numer of quantiles to split the data
#' @param highlight Character or Integer. Which split should be used
#' for the automatic conclussion in the plot? Set to "auto" for
#' best value, "none" to turn off or the number of split.
#' @param caption Character. Caption to show in plot
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @param quiet Boolean. Do not show message for auto target?
#' @export
mplot_response <- function(tag, score, target = "auto", splits = 10, highlight = "auto", 
                           caption = NA, save = FALSE, subdir = NA, 
                           file_name = "viz_response.png", quiet = FALSE) {
  
  gains <- gain_lift(tag, score, target, splits, quiet = quiet) %>% 
    mutate(percentile = as.numeric(percentile),
           cum_response = 100 * cumsum(target)/cumsum(total))
  rand <- 100 * sum(gains$target)/sum(gains$total)
  gains <- gains %>% mutate(cum_response_lift = 100 * cum_response/rand - 100)
  
  p <- gains %>%
    ggplot(aes(x = percentile)) + theme_lares2(pal=2) +
    geom_hline(yintercept = rand, colour = "black", linetype = "dashed") +
    geom_line(aes(y = cum_response), size = 1.2) +
    geom_label(aes(y = cum_response, label = round(cum_response)), alpha = 0.9) +
    geom_text(label = paste0("Random: ", round(rand, 1), "%"), 
              y = rand, x = 1, vjust = 1.2, hjust = 0, alpha = 0.2) +
    scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 10)) + 
    scale_x_continuous(minor_breaks = NULL, 
                       breaks = seq(0, splits, 1)) +
    labs(title = "Cumulative Response Plot", linetype = "",
         y = "Cumulative response [%]", 
         x = paste0("Percentiles [",splits,"]")) +
    theme(legend.position = c(0.88, 0.2)) 
  
  if (highlight == "auto") {
    highlight <- as.integer(gains$percentile[gains$lift == max(gains$lift)])
  }
  if (highlight %in% gains$percentile & highlight != "none") {
    highlight <- as.integer(highlight)
    note <- paste0("If we select the top ", 
                   round(highlight*100/splits),"% observations with highest scores,\n",
                   round(gains$cum_response[gains$percentile == highlight]),"% belong to the target class ",
                   "(", round(gains$cum_response_lift[gains$percentile == highlight]), "% better than random)")
    p <- p + labs(subtitle = note)
  } else {
    message("That highlight value is not a percentile. Try any integer from 1 to ", splits)
  }
  
  if (!is.na(caption)) {
    p <- p + labs(caption = caption)
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
