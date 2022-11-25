####################################################################
#' Density plot for discrete and continuous values
#'
#' This function plots discrete and continuous values results
#'
#' @family ML Visualization
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
#' @return Plot with distribution and performance results.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dfr) # Results for AutoML Predictions
#' lapply(dfr[c(1, 3)], head)
#'
#' # Plot for binomial results
#' mplot_density(dfr$class2$tag, dfr$class2$scores, subtitle = "Titanic Survived Model")
#'
#' # Plot for regression results
#' mplot_density(dfr$regr$tag, dfr$regr$score, model_name = "Titanic Fare Model")
#' @export
mplot_density <- function(tag,
                          score,
                          thresh = 6,
                          model_name = NA,
                          subtitle = NA,
                          save = FALSE,
                          subdir = NA,
                          file_name = "viz_distribution.png") {
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", length(tag), "rows and score has", length(score))))
  }

  if (length(unique(tag)) <= thresh) {
    out <- data.frame(tag = as.character(tag), score = score)

    if (is.numeric(out$score)) {
      if (max(out$score) <= 1) {
        out$score <- score * 100
      }
    }

    p1 <- ggplot(out) +
      geom_density(
        aes(
          x = as.numeric(.data$score),
          group = .data$tag,
          fill = as.character(.data$tag)
        ),
        alpha = 0.6, adjust = 0.25, size = 0
      ) +
      labs(
        title = "Classification Model Results",
        y = "Density by tag", x = "Score", fill = NULL
      ) +
      theme_lares(pal = 1, legend = "bottom") +
      theme(
        legend.title = element_blank(),
        legend.key.size = unit(.2, "cm")
      )

    p2 <- ggplot(out) +
      geom_density(aes(x = .data$score),
        size = 0, alpha = 0.9, adjust = 0.25, fill = "deepskyblue"
      ) +
      labs(x = NULL, y = "Density") +
      theme_lares()

    p3 <- ggplot(out, aes(x = as.numeric(.data$score), colour = as.character(.data$tag))) +
      stat_ecdf(size = 1) +
      ylab("Cumulative") +
      labs(x = NULL) +
      guides(colour = "none") +
      theme_lares(pal = 2)

    p1 <- p1 + theme(plot.margin = margin(10, 5, 5, 5))
    p2 <- p2 + theme(plot.margin = margin(0, 0, 5, 5))
    p3 <- p3 + theme(plot.margin = margin(0, 5, 5, 0))

    if (!is.na(subtitle)) p1 <- p1 + labs(subtitle = subtitle)
    if (!is.na(model_name)) p1 <- p1 + labs(caption = model_name)

    if (!is.na(subdir)) {
      # dir.create(file.path(getwd(), subdir), recursive = TRUE)
      file_name <- paste(subdir, file_name, sep = "/")
    }

    p <- (p1 / (p2 | p3)) + plot_layout(heights = c(1.2, 1))
  } else {
    df <- data.frame(
      rbind(
        cbind(values = tag, type = "Real"),
        cbind(values = score, type = "Model")
      )
    )
    df$values <- as.numeric(as.character(df$values))

    p <- ggplot(df) +
      geom_density(aes(x = .data$values, fill = as.character(.data$type)),
        alpha = 0.6, adjust = 0.25
      ) +
      labs(y = "Density", x = "Continuous values", fill = NULL) +
      guides(colour = "none") +
      theme_lares(pal = 1, legend = "top")

    if (!is.na(model_name)) p <- p + labs(caption = model_name)
    if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)
  }

  if (save) export_plot(p, file_name, subdir = subdir, width = 6, height = 6)

  return(p)
}


####################################################################
#' Variables Importances Plot
#'
#' This function plots Variable Importances
#'
#' @family ML Visualization
#' @inheritParams mplot_density
#' @param var Vector. Variable or column's names
#' @param imp Vector. Importance of said variables. Must have same length as var
#' @param colours If positive and negative contribution is known
#' @param limit Integer. Limit how many variables you wish to plot
#' @return Plot with ranked importance variables results.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' df <- data.frame(
#'   variable = LETTERS[1:6],
#'   importance = c(4, 6, 6.7, 3, 4.8, 6.2) / 100,
#'   positive = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
#' )
#' head(df)
#'
#' mplot_importance(
#'   var = df$variable,
#'   imp = df$importance,
#'   model_name = "Random values model"
#' )
#'
#' # Add a colour for categories
#' mplot_importance(
#'   var = df$variable,
#'   imp = df$importance,
#'   colours = df$positive,
#'   limit = 4
#' )
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
  if (length(var) != length(imp)) {
    message("The variables and importance values vectors should be the same length.")
    stop(message(paste("Currently, there are", length(var), "variables and", length(imp), "importance values!")))
  }

  if (is.na(colours[1])) {
    colours <- "deepskyblue"
  }

  out <- data.frame(var = var, imp = 100 * imp, Type = colours)

  if (length(var) < limit) limit <- length(var)

  output <- out[1:limit, ]

  p <- ggplot(
    output,
    aes(
      x = reorder(.data$var, .data$imp), y = .data$imp,
      label = formatNum(.data$imp, 1)
    )
  ) +
    geom_col(aes(fill = .data$Type), width = 0.08, colour = "transparent") +
    geom_point(aes(colour = .data$Type), size = 6.2) +
    coord_flip() +
    geom_text(hjust = 0.5, size = 2.1, inherit.aes = TRUE, colour = "white") +
    labs(
      title = paste0("Most Relevant Variables (top ", limit, " of ", length(var), ")"),
      x = NULL, y = NULL
    ) +
    scale_y_continuous(
      position = "right", expand = c(0, 0),
      limits = c(0, 1.03 * max(output$imp))
    ) +
    theme_lares()

  if (length(unique(output$Type)) == 1) {
    p <- p + geom_col(fill = colours, width = 0.2, colour = "transparent") +
      geom_point(colour = colours, size = 6) +
      guides(fill = "none", colour = "none") +
      geom_text(hjust = 0.5, size = 2, inherit.aes = TRUE, colour = "white")
  }

  if (!is.na(model_name)) p <- p + labs(caption = model_name)
  if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)

  if (save) export_plot(p, file_name, subdir = subdir, width = 6, height = 6)

  return(p)
}


####################################################################
#' ROC Curve Plot
#'
#' This function plots ROC Curves with AUC values with 95\% confidence
#' range. It also works for multi-categorical models.
#'
#' @family ML Visualization
#' @param tag Vector. Real known label.
#' @param score Vector. Predicted value or model's result.
#' @param multis Data.frame. Containing columns with each category probability
#' or score (only used when more than 2 categories coexist).
#' @param sample Integer. Number of samples to use for rendering plot.
#' @param model_name Character. Model's name
#' @param subtitle Character. Subtitle to show in plot
#' @param interval Numeric. Interval for breaks in plot
#' @param squared Boolean. Keep proportions?
#' @param plotly Boolean. Use plotly for plot's output for an interactive plot
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @return Plot with ROC curve and AUC performance results.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dfr) # Results for AutoML Predictions
#' lapply(dfr[c(1, 2)], head)
#'
#' # ROC Curve for Binomial Model
#' mplot_roc(dfr$class2$tag, dfr$class2$scores,
#'   model_name = "Titanic Survived Model"
#' )
#'
#' # ROC Curves for Multi-Categorical Model
#' mplot_roc(dfr$class3$tag, dfr$class3$score,
#'   multis = subset(dfr$class3, select = -c(tag, score)),
#'   squared = FALSE,
#'   model_name = "Titanic Class Model"
#' )
#' @export
mplot_roc <- function(tag,
                      score,
                      multis = NA,
                      sample = 1000,
                      model_name = NA,
                      subtitle = NA,
                      interval = 0.2,
                      squared = TRUE,
                      plotly = FALSE,
                      save = FALSE,
                      subdir = NA,
                      file_name = "viz_roc.png") {
  if (is.na(multis)[1]) {
    rocs <- ROC(tag, score)
    ci <- rocs$ci
  } else {
    rocs <- ROC(tag, score, multis)
    ci <- rocs$ci["mean"]
  }
  coords <- rocs$roc

  if (sample < min(table(coords$label))) {
    coords <- coords %>%
      group_by(.data$label) %>%
      sample_n(sample)
    message("ROC Curve Plot rendered with sampled data...")
  }

  scale <- function(x) sprintf("%.1f", x)
  p <- ggplot(coords, aes(x = .data$fpr, y = .data$tpr, group = .data$label)) +
    geom_line(colour = "deepskyblue", size = 0.8) +
    geom_point(aes(colour = .data$label), size = 0.7, alpha = 0.8) +
    geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), alpha = 0.2, linetype = "dotted") +
    scale_x_reverse(
      name = "1 - Specificity [False Positive Rate]", limits = c(1, 0),
      breaks = seq(0, 1, interval), expand = c(0.001, 0.001),
      labels = scale
    ) +
    scale_y_continuous(
      name = "Sensitivity [True Positive Rate]", limits = c(0, 1),
      breaks = seq(0, 1, interval), expand = c(0.001, 0.001),
      labels = scale
    ) +
    theme(axis.ticks = element_line(color = "grey80")) +
    labs(title = "ROC Curve: AUC", colour = NULL) +
    guides(colour = guide_legend(ncol = 3)) +
    annotate("text",
      x = 0.25, y = 0.10, size = 4.2,
      label = paste("AUC =", round(100 * ci[2, ], 2))
    ) +
    annotate("text",
      x = 0.25, y = 0.05, size = 2.8,
      label = paste0(
        "95% CI: ", round(100 * ci[1, ], 2), "-",
        round(100 * ci[3, ], 2)
      )
    ) +
    theme_lares(plot_colour = "white", pal = 2, legend = "bottom")

  if (squared) p <- p + coord_equal()
  if (is.na(multis)[1]) p <- p + guides(colour = "none")
  if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)
  if (!is.na(model_name)) p <- p + labs(caption = model_name)

  if (save) export_plot(p, file_name, subdir = subdir, width = 6, height = 6)

  if (plotly) {
    try_require("plotly")
    p <- ggplotly(p)
  }

  return(p)
}


####################################################################
#' Cuts by quantiles for score plot
#'
#' This function cuts by quantiles any score or prediction.
#'
#' @family ML Visualization
#' @inheritParams mplot_roc
#' @param splits Integer. Numer of separations to plot
#' @param table Boolean. Do you wish to return a table with results?
#' @return Plot with performance results by cuts.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dfr) # Results for AutoML Predictions
#' head(dfr$class2)
#'
#' # Data
#' mplot_cuts(dfr$class2$scores, splits = 5, table = TRUE)
#'
#' # Plot
#' mplot_cuts(dfr$class2$scores, model_name = "Titanic Survived Model")
#' @export
mplot_cuts <- function(score,
                       splits = 10,
                       model_name = NA,
                       subtitle = NA,
                       table = FALSE,
                       save = FALSE,
                       subdir = NA,
                       file_name = "viz_ncuts.png") {
  if (splits > 25) stop("You should try with less splits!")

  deciles <- quantile(score,
    probs = seq((1 / splits), 1, length = splits),
    names = TRUE
  )
  deciles <- data.frame(
    range = row.names(as.data.frame(deciles)),
    cuts = as.vector(signif(deciles, 6))
  )
  rownames(deciles) <- NULL

  p <- deciles %>%
    # mutate(label_colours = ifelse(cuts*100 < 50, "1", "m")) %>%
    ggplot(aes(x = reorder(.data$range, .data$cuts), y = .data$cuts * 100)) +
    geom_col(fill = "deepskyblue", colour = "transparent") +
    xlab("Cumulative volume") +
    ylab("Score") +
    geom_text(
      aes(
        label = round(100 * .data$cuts, 1),
        vjust = ifelse(.data$cuts * 100 < 50, -0.3, 1.3)
      ),
      size = 3, colour = "black", inherit.aes = TRUE, check_overlap = TRUE
    ) +
    guides(colour = "none") +
    labs(title = sprintf("Score cuts (%s quantiles)", splits)) +
    theme_lares()

  if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)
  if (!is.na(model_name)) p <- p + labs(caption = model_name)

  if (save) export_plot(p, file_name, subdir = subdir, width = 6, height = 6)

  if (table) {
    return(deciles)
  } else {
    return(p)
  }
}


####################################################################
#' Cuts by quantiles on absolute and percentual errors plot
#'
#' This function cuts by quantiles on absolute and percentual errors
#'
#' @family ML Visualization
#' @inheritParams mplot_roc
#' @param splits Integer. Number of separations to plot
#' @param title Character. Title to show in plot
#' @return Plot with error results by cuts.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dfr) # Results for AutoML Predictions
#' head(dfr$regr)
#' mplot_cuts_error(dfr$regr$tag, dfr$regr$score,
#'   model_name = "Titanic Fare Model"
#' )
#' @export
mplot_cuts_error <- function(tag,
                             score,
                             splits = 10,
                             title = NA,
                             model_name = NA,
                             save = FALSE,
                             subdir = NA,
                             file_name = "viz_ncuts_error.png") {
  if (splits > 25) stop("You should try with less splits!")

  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", length(tag), "rows and score has", length(score))))
  }

  df <- data.frame(tag = tag, score = score) %>%
    mutate(
      real_error = .data$tag - .data$score,
      abs_error = abs(.data$real_error),
      p_error = 100 * .data$real_error / .data$tag
    ) %>%
    filter(abs(.data$p_error) <= 150)

  # Useful function
  quantsfx <- function(values, splits = 10, just = 0.3) {
    cuts <- quantile(values,
      probs = seq((1 / splits), 1, length = splits),
      names = TRUE
    )
    cuts <- data.frame(deciles = names(cuts), cut = cuts)
    thresh <- max(cuts$cut) / 2
    cuts$gg_pos <- ifelse(cuts$cut > thresh, 1 + just, -just)
    cuts$colour <- ifelse(cuts$gg_pos < 0, "f", "m")
    row.names(cuts) <- NULL
    return(cuts)
  }

  # First: absolute errors
  deciles_abs <- quantsfx(df$abs_error, splits = splits, just = 0.3)
  p_abs <- ggplot(deciles_abs, aes(
    x = reorder(.data$deciles, .data$cut),
    y = .data$cut, label = signif(.data$cut, 3)
  )) +
    geom_col(fill = "deepskyblue", colour = "transparent") +
    labs(x = NULL, y = "Absolute [#]") +
    geom_text(aes(vjust = .data$gg_pos, colour = .data$colour),
      size = 2.7, inherit.aes = TRUE, check_overlap = TRUE
    ) +
    labs(subtitle = paste("Cuts and distribution by absolute error")) +
    scale_y_comma() +
    guides(colour = "none") +
    theme_lares(pal = 4, plot_colour = "white")

  # Second: percentual errors
  deciles_perabs <- quantsfx(abs(df$p_error), splits = splits, just = 0.3)
  p_per <- ggplot(
    deciles_perabs,
    aes(
      x = reorder(.data$deciles, .data$cut),
      y = .data$cut, label = signif(.data$cut, 3)
    )
  ) +
    geom_col(fill = "deepskyblue", colour = "transparent") +
    labs(x = NULL, y = "Absolute [%]") +
    geom_text(aes(vjust = .data$gg_pos, colour = .data$colour),
      size = 2.7, inherit.aes = TRUE, check_overlap = TRUE
    ) +
    labs(subtitle = paste("Cuts and distribution by absolute percentage error")) +
    scale_y_comma() +
    guides(colour = "none") +
    theme_lares(pal = 4, plot_colour = "white")

  # Third: errors distribution
  pd_error <- ggplot(df) +
    geom_density(aes(x = .data$p_error),
      fill = "deepskyblue", alpha = 0.7
    ) +
    labs(x = NULL, y = "Density [%]") +
    geom_vline(xintercept = 0, alpha = 0.5, colour = "navy", linetype = "dotted") +
    theme_lares(plot_colour = "white")

  if (!is.na(title)) p_abs <- p_abs + labs(title = title)
  if (!is.na(model_name)) pd_error <- pd_error + labs(caption = model_name)

  p <- (p_abs + p_per + pd_error) +
    plot_layout(ncol = 1, heights = c(1.8, 1.8, 1))

  if (save) {
    if (!is.na(subdir)) {
      # dir.create(file.path(getwd(), subdir), recursive = TRUE)
      file_name <- paste(subdir, file_name, sep = "/")
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
#' @family ML Visualization
#' @inheritParams mplot_roc
#' @param splits Integer. Number of separations to plot
#' @return Plot with distribution and performance results by splits.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dfr) # Results for AutoML Predictions
#' lapply(dfr, head)
#'
#' # For categorical (binary) values
#' mplot_splits(dfr$class2$tag, dfr$class2$scores,
#'   splits = 4,
#'   model_name = "Titanic Survived Model"
#' )
#'
#' # For categorical (+2) values
#' mplot_splits(dfr$class3$tag, dfr$class2$scores,
#'   model_name = "Titanic Class Model"
#' )
#'
#' # For continuous values
#' mplot_splits(dfr$regr$tag, dfr$regr$score,
#'   splits = 4,
#'   model_name = "Titanic Fare Model"
#' )
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
    stop(message(paste("Currently, tag has", length(tag), "rows and score has", length(score))))
  }

  if (splits > 10) stop("You should try with less splits!")

  df <- data.frame(tag, score)
  npersplit <- round(nrow(df) / splits)

  # For continuous tag values
  if (length(unique(tag)) > 6) {
    names <- df %>%
      mutate(
        tag = as.numeric(.data$tag),
        quantile = ntile(.data$tag, splits)
      ) %>%
      group_by(.data$quantile) %>%
      summarise(
        n = n(),
        max_score = round(max(.data$tag), 1),
        min_score = round(min(.data$tag), 1)
      ) %>%
      mutate(quantile_tag = paste0(quantile, " (", .data$min_score, "-", .data$max_score, ")"))
    df <- df %>%
      mutate(quantile = ntile(.data$tag, splits)) %>%
      left_join(names, by = "quantile") %>%
      mutate(tag = .data$quantile_tag) %>%
      select(-.data$quantile, -.data$n, -.data$max_score, -.data$min_score)
  } else {
    # For categorical tag values
    names <- df %>%
      mutate(quantile = ntile(.data$score, splits)) %>%
      group_by(.data$quantile) %>%
      summarise(
        n = n(),
        max_score = signif(max(.data$score), 2),
        min_score = signif(min(.data$score), 2)
      ) %>%
      mutate(quantile_tag = paste0(
        .data$quantile, " (",
        round(100 * .data$min_score, 1), "-",
        round(100 * .data$max_score, 1), ")"
      ))
  }

  p <- df %>%
    mutate(quantile = ntile(.data$score, splits)) %>%
    group_by(.data$quantile, .data$tag) %>%
    tally() %>%
    ungroup() %>%
    group_by(.data$tag) %>%
    arrange(desc(.data$quantile)) %>%
    mutate(
      p = round(100 * .data$n / sum(.data$n), 2),
      cum = cumsum(100 * .data$n / sum(.data$n))
    ) %>%
    left_join(names, by = "quantile") %>%
    ggplot(aes(
      x = as.character(.data$tag), y = .data$p, label = as.character(.data$p),
      fill = reorder(as.character(.data$quantile_tag), .data$quantile)
    )) +
    geom_col(position = "stack", colour = "transparent") +
    geom_text(size = 3, position = position_stack(vjust = 0.5), check_overlap = TRUE) +
    xlab("Tag") +
    ylab("Total Percentage by Tag") +
    guides(fill = guide_legend(title = paste0("~", npersplit, " p/split"))) +
    labs(title = "Split Groups") +
    scale_fill_brewer(palette = "Spectral") +
    theme_lares()

  if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)
  if (!is.na(model_name)) p <- p + labs(caption = model_name)

  if (save) export_plot(p, file_name, subdir = subdir, width = 6, height = 6)

  return(p)
}


####################################################################
#' Model Metrics and Performance Plots
#'
#' This function generates plots of the metrics of a predictive model.
#' This is an auxiliary function used in \code{model_metrics()} when
#' the parameter \code{plot} is set to \code{TRUE}.
#'
#' @family ML Visualization
#' @inheritParams mplot_roc
#' @param results Object. Results object from h2o_automl function
#' @return Plot with \code{results} performance.
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
    test_auc = results$model@model$scoring_history$validation_auc
  )
  ll <- ggplot(plots_data) +
    geom_hline(yintercept = 0.69315, alpha = 0.5, linetype = "dotted") +
    geom_line(aes(x = .data$trees, y = .data$train_ll, colour = "Train"), size = 0.5) +
    geom_line(aes(x = .data$trees, y = .data$test_ll, colour = "Test"), size = 1) +
    labs(
      title = "Logarithmic Loss vs Number of Trees",
      colour = "Dataset", x = "# of trees", y = "LogLoss"
    ) +
    scale_colour_brewer(palette = "Set1") +
    geom_text(
      aes(
        x = .data$trees, y = .data$train_ll, colour = "Train",
        label = round(.data$train_ll, 2)
      ),
      check_overlap = TRUE, nudge_y = 0.03, size = 3
    ) +
    geom_text(
      aes(
        x = .data$trees, y = .data$test_ll, colour = "Test",
        label = round(.data$test_ll, 2)
      ),
      check_overlap = TRUE, nudge_y = 0.03, size = 3
    ) +
    theme_lares(pal = 1) +
    theme(
      strip.text.x = element_blank(),
      strip.background = element_rect(colour = "white", fill = "white"),
      legend.position = c(0.1, 0.05)
    )
  au <- ggplot(plots_data) +
    geom_line(aes(x = .data$trees, y = .data$train_auc * 100, colour = "Train"), size = 0.5) +
    geom_line(aes(x = .data$trees, y = .data$test_auc * 100, colour = "Test"), size = 1) +
    geom_hline(yintercept = 50, alpha = 0.5, linetype = "dotted", colour = "black") +
    labs(
      title = "Area Under the Curve vs Number of Trees",
      colour = "Dataset", x = "# of trees", y = "AUC"
    ) +
    scale_colour_brewer(palette = "Set1") +
    guides(colour = "none") +
    geom_text(
      aes(
        x = .data$trees, y = .data$train_auc * 100, colour = "Train",
        label = round(.data$train_auc * 100, 2)
      ),
      check_overlap = TRUE, nudge_y = 3, size = 3
    ) +
    geom_text(
      aes(
        x = .data$trees, y = .data$test_auc * 100, colour = "Test",
        label = round(.data$test_auc * 100, 2)
      ),
      check_overlap = TRUE, nudge_y = 3, size = 3
    ) +
    theme_lares(pal = 1)

  if (!is.na(subtitle)) ll <- ll + labs(subtitle = subtitle)
  if (!is.na(model_name)) au <- au + labs(caption = model_name)

  p <- (ll / au) + plot_layout(ncol = 1)

  if (save) {
    if (!is.na(subdir)) {
      # dir.create(file.path(getwd(), subdir), recursive = TRUE)
      file_name <- paste(subdir, file_name, sep = "/")
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
#' @family ML Visualization
#' @inheritParams mplot_roc
#' @return Plot with linear distribution and performance results.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dfr) # Results for AutoML Predictions
#' lapply(dfr, head)
#' mplot_lineal(dfr$regr$tag, dfr$regr$score, model_name = "Titanic Fare Model")
#' @export
mplot_lineal <- function(tag,
                         score,
                         subtitle = NA,
                         model_name = NA,
                         save = FALSE,
                         subdir = NA,
                         file_name = "viz_lineal.png") {
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", length(tag), "rows and score has", length(score))))
  }

  results <- data.frame(tag = tag, score = score, dist = 0)
  results <- results[complete.cases(results), ]

  for (i in seq_len(nrow(results))) {
    results$dist[i] <- dist2d(c(results$tag[i], results$score[i]), c(0, 0), c(1, 1))
  }

  fit <- lm(results$score ~ results$tag)
  labels <- paste(
    paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 4)),
    # paste("Pval =", signif(summary(fit)$coef[2,4], 3)),
    paste("RMSE =", signif(rmse(results$tag, results$score), 4)),
    paste("MAE =", signif(mae(results$tag, results$score), 4)),
    sep = "\n"
  )

  p <- ggplot(results, aes(x = .data$tag, y = .data$score, colour = .data$dist)) +
    geom_point() +
    labs(
      title = "Regression Model Results",
      x = "Real value", y = "Predicted value",
      colour = "Deviation"
    ) +
    annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -0.05, label = labels, size = 2.8) +
    scale_x_comma() +
    scale_y_comma() +
    scale_colour_continuous(labels = function(x) formatNum(x, decimals = NULL, signif = 3)) +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1)) +
    guides(colour = guide_colorbar(barwidth = 0.9, barheight = 4.5)) +
    theme_lares()

  # Draw reference lines for correlation
  intercept <- summary(fit)$coefficients[1]
  slope <- summary(fit)$coefficients[2]
  p <- p +
    geom_abline(
      slope = 1,
      intercept = 0,
      alpha = 0.3,
      colour = "grey70",
      size = 0.6
    ) +
    geom_abline(
      slope = slope,
      intercept = intercept,
      alpha = 0.5,
      colour = "orange",
      size = 0.6
    )

  if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)
  if (!is.na(model_name)) p <- p + labs(caption = model_name)
  if (save) export_plot(p, file_name, subdir = subdir, width = 6, height = 6)

  return(p)
}


####################################################################
#' MPLOTS Score Full Report Plots
#'
#' This function plots a whole dashboard with a model's results. It will automatically
#' detect if it's a categorical or regression's model by checking how many different
#' unique values the independent variable (tag) has.
#'
#' @family ML Visualization
#' @inheritParams mplot_roc
#' @param splits Integer. Number of separations to plot
#' @param thresh Integer. Threshold for selecting binary or regression
#' models: this number is the threshold of unique values we should
#' have in 'tag' (more than: regression; less than: classification)
#' @param plot Boolean. Plot results? If not, plot grid object returned
#' @return Multiple plots gathered into one, showing \code{tag} vs
#' \code{score} performance results.
#' @examples
#' \donttest{
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dfr) # Results for AutoML Predictions
#' lapply(dfr, head)
#'
#' # Dasboard for Binomial Model
#' mplot_full(dfr$class2$tag, dfr$class2$scores,
#'   model_name = "Titanic Survived Model"
#' )
#'
#' # Dasboard for Multi-Categorical Model
#' mplot_full(dfr$class3$tag, dfr$class3$score,
#'   multis = subset(dfr$class3, select = -c(tag, score)),
#'   model_name = "Titanic Class Model"
#' )
#'
#' # Dasboard for Regression Model
#' mplot_full(dfr$regr$tag, dfr$regr$score,
#'   model_name = "Titanic Fare Model"
#' )
#' }
#' @export
mplot_full <- function(tag,
                       score,
                       multis = NA,
                       splits = 8,
                       thresh = 6,
                       subtitle = NA,
                       model_name = NA,
                       plot = TRUE,
                       save = FALSE,
                       subdir = NA,
                       file_name = "viz_full.png") {
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", length(tag), "rows and score has", length(score))))
  }


  # Categorical Binomial Models
  if (length(unique(tag)) == 2 && is.numeric(score)) {
    p1 <- mplot_density(tag = tag, score = score, subtitle = subtitle, model_name = model_name)
    p2 <- mplot_splits(tag = tag, score = score, splits = splits) +
      theme(plot.margin = margin(10, 8, 5, 0))
    p3 <- mplot_roc(tag = tag, score = score, squared = FALSE) +
      theme(plot.margin = margin(0, 8, 8, 0))
    p4 <- mplot_cuts(score = score) +
      theme(plot.margin = margin(-3, 0, 5, 8))

    p <- (wrap_plots(p1) + p2 + p4 + p3) +
      plot_layout(
        ncol = 2, nrow = 2,
        heights = c(1.4, 1),
        widths = c(1.5, 1)
      )
  }

  # Multi-Categorical Models
  if (length(unique(tag)) > 2 && length(unique(tag)) <= thresh) {
    m <- model_metrics(tag, score, multis, thresh = thresh)
    p1 <- m$plots$conf_matrix +
      labs(
        title = "Confusion Matrix",
        caption = if (!is.na(model_name)) model_name
      )
    p2 <- m$plots$ROC
    p <- (p1 + p2) + plot_layout(ncol = 2, nrow = 1)
  }

  # Regression Continuous Models
  if (is.numeric(tag) && is.numeric(score) && length(unique(tag)) > thresh) {
    p1 <- mplot_lineal(tag = tag, score = score, subtitle = subtitle, model_name = model_name) +
      theme_lares(plot_colour = "white")
    p2 <- mplot_density(tag = tag, score = score)
    p3 <- mplot_cuts_error(tag = tag, score = score, splits = splits)
    p <- ((p1 / p2) | p3) + plot_layout(widths = c(1, 1.4))
  }

  if (save) export_plot(p, file_name, subdir = subdir, width = 15, height = 10)

  if (plot) plot(p) else return(p)
}


####################################################################
#' Confussion Matrix Plot
#'
#' This function plots a confussion matrix.
#'
#' You may use \code{conf_mat()} to get calculate values.
#'
#' @family ML Visualization
#' @inheritParams mplot_roc
#' @inheritParams conf_mat
#' @param abc Boolean. Arrange columns and rows alphabetically?
#' @param squared Boolean. Force plot to be squared?
#' @param top Integer. Plot only the most n frequent variables.
#' Set to \code{NA} to plot all.
#' @return Plot with confusion matrix results.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dfr) # Results for AutoML Predictions
#' lapply(dfr, head)
#'
#' # Plot for Binomial Model
#' mplot_conf(dfr$class2$tag, dfr$class2$scores,
#'   model_name = "Titanic Survived Model"
#' )
#'
#' # Plot for Multi-Categorical Model
#' mplot_conf(dfr$class3$tag, dfr$class3$score,
#'   model_name = "Titanic Class Model"
#' )
#' @export
mplot_conf <- function(tag, score, thresh = 0.5, abc = TRUE,
                       squared = FALSE, diagonal = TRUE, top = 20,
                       subtitle = NA, model_name = NULL,
                       save = FALSE, subdir = NA,
                       file_name = "viz_conf_mat.png") {
  df <- data.frame(tag, score)

  # About tags
  # Keep only most frequent tags?
  if (!is.na(top)) {
    if (length(unique(df$tag)) > top) {
      tops <- freqs(df, tag) %>%
        pull(1) %>%
        head(top)
      df <- df %>% mutate(
        tag = ifelse(.data$tag %in% tops, as.character(.data$tag), "Others"),
        score = ifelse(.data$score %in% tops, as.character(.data$score), "Others")
      )
    }
  }
  values <- df %>%
    group_by(.data$tag) %>%
    tally() %>%
    {
      if (abc) arrange(., .data$tag) else arrange(., desc(.data$n))
    } %>%
    mutate(label = sprintf("%s \n(%s)", .data$tag, formatNum(.data$n, 0)))
  labels <- values$tag
  df <- mutate(df, tag = factor(.data$tag, levels = labels))

  if (!diagonal) df <- filter(df, .data$tag != .data$score)

  # About scores
  if (is.numeric(df$score) && length(unique(tag)) == 2) {
    means <- df %>%
      group_by(.data$tag) %>%
      summarise(mean = mean(.data$score))
    target <- means$tag[means$mean == max(means$mean)]
    other <- means$tag[means$mean == min(means$mean)]
    df <- df %>% mutate(pred = ifelse(
      .data$score >= thresh, as.character(target), as.character(other)
    ))
  } else {
    df <- df %>% mutate(pred = .data$score)
  }

  # Frequencies
  plot_cf <- df %>%
    freqs(.data$tag, .data$pred) %>%
    ungroup() %>%
    group_by(.data$tag) %>%
    mutate(aux = 100 * .data$n / sum(.data$n)) %>%
    mutate(label = paste0(
      formatNum(.data$n, 0), "\n",
      # formatNum(.data$p, 1), "%T\n",
      "(", formatNum(.data$aux, 1), "%)"
    ))
  trues <- sum(plot_cf$n[as.character(plot_cf$tag) == as.character(plot_cf$pred)])
  total <- sum(plot_cf$n)
  acc <- formatNum(100 * (trues / total), 2, pos = "%")
  obs <- formatNum(nrow(df), 0)
  metrics <- sprintf("%s observations | AAC %s", obs, acc)

  p <- ggplot(plot_cf, aes(
    y = as.numeric(factor(.data$tag, levels = rev(labels))),
    x = as.numeric(factor(.data$pred, levels = labels)),
    fill = .data$n, size = .data$aux, label = .data$label
  )) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "orange") +
    geom_text(lineheight = .8) +
    scale_size(range = c(2.9, 3.4)) +
    guides(fill = "none", size = "none", colour = "none") +
    labs(
      x = "Predicted values", y = "Real values",
      title = paste("Confusion Matrix", ifelse(
        thresh != 0.5, paste("with Threshold =", thresh), ""
      ), ifelse(
        diagonal == FALSE, paste("without diagonal values"), ""
      )),
      subtitle = metrics, caption = model_name
    ) +
    theme_lares() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 0),
      axis.title.x = element_text(hjust = 0.5),
      axis.text.x.bottom = element_blank(),
      axis.ticks.x.bottom = element_blank(),
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank()
    ) +
    theme(axis.text.x = element_text(angle = 30, hjust = 0)) +
    scale_x_continuous(
      breaks = seq_along(labels),
      labels = labels,
      position = "bottom",
      sec.axis = sec_axis(~.,
        breaks = seq_along(labels),
        labels = labels
      )
    ) +
    scale_y_continuous(
      breaks = seq_along(labels),
      labels = labels,
      position = "right",
      sec.axis = sec_axis(~.,
        breaks = seq_along(labels),
        labels = rev(values$label)
      )
    )

  if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)
  if (squared) p <- p + coord_equal()

  if (save) export_plot(p, file_name, subdir = subdir, width = 6, height = 6)

  return(p)
}


####################################################################
#' Cumulative Gain Plot
#'
#' The cumulative gains plot, often named ‘gains plot’, helps us
#' answer the question: When we apply the model and select the best
#' X deciles, what % of the actual target class observations can we
#' expect to target? The cumulative gains chart shows the percentage
#' of the overall number of cases in a given category "gained" by
#' targeting a percentage of the total number of cases.
#'
#' @family ML Visualization
#' @inheritParams mplot_roc
#' @param target Value. Which is your target positive value? If
#' set to 'auto', the target with largest mean(score) will be
#' selected. Change the value to overwrite. Only works for binary classes
#' @param splits Integer. Numer of quantiles to split the data
#' @param highlight Character or Integer. Which split should be used
#' for the automatic conclussion in the plot? Set to "auto" for
#' best value, "none" to turn off or the number of split.
#' @param caption Character. Caption to show in plot
#' @param quiet Boolean. Do not show message for auto target?
#' @return Plot with gain and performance results by cuts.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dfr) # Results for AutoML Predictions
#' lapply(dfr, head)
#'
#' # Plot for Binomial Model
#' mplot_gain(dfr$class2$tag, dfr$class2$scores,
#'   caption = "Titanic Survived Model",
#'   target = "FALSE"
#' )
#' mplot_gain(dfr$class2$tag, dfr$class2$scores,
#'   caption = "Titanic Survived Model",
#'   target = "TRUE"
#' )
#'
#' # Plot for Multi-Categorical Model
#' mplot_gain(dfr$class3$tag, dfr$class3$score,
#'   multis = subset(dfr$class3, select = -c(tag, score)),
#'   caption = "Titanic Class Model"
#' )
#' @export
mplot_gain <- function(tag, score, multis = NA, target = "auto",
                       splits = 10, highlight = "auto",
                       caption = NA, save = FALSE, subdir = NA,
                       file_name = "viz_gain.png", quiet = FALSE) {
  if (is.na(multis)[1]) {
    gains <- gain_lift(tag, score, target, splits, quiet = quiet)
    aux <- data.frame(x = c(0, gains$percentile), y = c(0, gains$optimal))
    df <- mutate(gains, percentile = as.numeric(.data$percentile))
    p <- ggplot(df, aes(x = .data$percentile)) +
      # Range area
      geom_polygon(data = aux, aes(.data$x, .data$y), alpha = 0.1) +
      # Model line
      geom_line(aes(y = .data$gain), colour = "darkorange", size = 1.2) +
      geom_label(data = df[df$gain != 100, ], aes(
        y = .data$gain,
        label = round(.data$gain)
      ), alpha = 0.9) +
      guides(colour = "none") +
      scale_y_continuous(breaks = seq(0, 100, 10)) +
      scale_x_continuous(minor_breaks = NULL, breaks = seq(0, splits, 1)) +
      labs(
        title = paste("Cumulative Gains for", gains$value[1]),
        linetype = NULL,
        y = "Cumulative gains [%]",
        x = paste0("Percentiles [", splits, "]")
      ) +
      theme_lares(pal = 2) +
      theme(legend.position = c(0.88, 0.2))

    if (highlight == "auto") {
      highlight <- gains %>%
        arrange(desc(.data$lift)) %>%
        slice(1) %>%
        .$percentile
    }
    if (highlight %in% gains$percentile && highlight != "none") {
      highlight <- as.integer(highlight)
      note <- paste0(
        "If we select the top ",
        round(highlight * 100 / splits), "% cases with highest probabilities,\n",
        round(gains$gain[gains$percentile == highlight]), "% of all target class will be picked ",
        "(", round(gains$lift[gains$percentile == highlight]), "% better than random)"
      )
      p <- p + labs(subtitle = note)
    } else {
      message("That highlight value is not a percentile. Try any integer from 1 to ", splits)
    }
  } else {
    df <- data.frame(tag = tag, score = score, multis)
    out <- aux <- NULL
    for (i in 1:(ncol(df) - 2)) {
      g <- gain_lift(df$tag, df[, 2 + i], splits = splits, quiet = quiet) %>%
        mutate(label = colnames(df)[2 + i])
      x <- data.frame(
        x = as.factor(c(0, g$percentile)),
        y = c(0, g$optimal),
        label = as.character(g$label[1])
      )
      out <- rbind(out, g)
      aux <- rbind(aux, x)
    }
    p <- out %>%
      mutate(percentile = factor(.data$percentile, levels = unique(out$percentile))) %>%
      ggplot(aes(x = .data$percentile, group = .data$label)) +
      # # Random line
      # geom_line(aes(y = random, linetype = "Random"), colour = "black") +
      # # Optimal line
      # geom_line(aes(y = optimal, colour = label, linetype = "Optimal"), size = 0.4) +
      # Possible area
      geom_polygon(data = aux, aes(x = .data$x, y = .data$y, group = .data$label), alpha = 0.1) +
      # Model line
      geom_line(aes(y = .data$gain, colour = .data$label), size = 1) +
      geom_label(aes(y = .data$gain, label = round(.data$gain)), alpha = 0.8) +
      guides(colour = "none") +
      theme_lares(pal = 2) +
      labs(
        title = "Cumulative Gains for Multiple Labels",
        subtitle = paste(
          "If we select the top nth percentile with highest probabilities,",
          "\nhow much of that specific target class will be picked?"
        ),
        x = paste0("Percentiles [", splits, "]"),
        y = "Cumulative Gains [%]",
        linetype = "Reference", colour = "Label"
      ) +
      scale_y_continuous(minor_breaks = seq(0, 100, 10), breaks = seq(0, 100, 20)) +
      facet_grid(label ~ .)
  }

  aux <- freqs(data.frame(tag = tag), .data$tag)
  text <- paste(
    "Assuming rate of",
    vector2text(round(aux$p), sep = "/", quotes = FALSE), "for",
    vector2text(aux$tag, sep = "/", quotes = FALSE)
  )
  if (!is.na(caption)) caption <- paste(text, caption, sep = "\n") else caption <- text
  p <- p + labs(caption = caption)

  if (save) export_plot(p, file_name, subdir = subdir, width = 6, height = 6)

  return(p)
}


####################################################################
#' Cumulative Response Plot
#'
#' The response gains plot helps us answer the question: When we
#' apply the model and select up until ntile X, what is the expected
#' % of target class observations in the selection?
#'
#' @family ML Visualization
#' @inheritParams mplot_roc
#' @inheritParams mplot_gain
#' @return Plot with cumulative response and performance results by cuts.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dfr) # Results for AutoML Predictions
#' lapply(dfr, head)
#'
#' # Plot for Binomial Model
#' mplot_response(dfr$class2$tag, dfr$class2$scores,
#'   caption = "Titanic Survived Model",
#'   target = "TRUE"
#' )
#' mplot_response(dfr$class2$tag, dfr$class2$scores,
#'   caption = "Titanic Survived Model",
#'   target = "FALSE"
#' )
#'
#' # Plot for Multi-Categorical Model
#' mplot_response(dfr$class3$tag, dfr$class3$score,
#'   multis = subset(dfr$class3, select = -c(tag, score)),
#'   caption = "Titanic Class Model"
#' )
#' @export
mplot_response <- function(tag, score, multis = NA, target = "auto",
                           splits = 10, highlight = "auto",
                           caption = NA, save = FALSE, subdir = NA,
                           file_name = "viz_response.png", quiet = FALSE) {
  if (is.na(multis)[1]) {
    gains <- gain_lift(tag, score, target, splits, quiet = quiet) %>%
      mutate(
        percentile = as.numeric(.data$percentile),
        cum_response = 100 * cumsum(.data$target) / cumsum(.data$total)
      )
    target <- gains$target[1]
    rand <- 100 * sum(gains$target) / sum(gains$total)
    gains <- gains %>% mutate(cum_response_lift = 100 * .data$cum_response / rand - 100)

    p <- gains %>%
      ggplot(aes(x = .data$percentile)) +
      theme_lares(pal = 2) +
      geom_hline(yintercept = rand, colour = "black", linetype = "dashed") +
      geom_line(aes(y = .data$cum_response, colour = "x"), size = 1.2) +
      geom_label(aes(y = .data$cum_response, label = round(.data$cum_response)), alpha = 0.9) +
      geom_text(
        label = paste0("Random: ", round(rand, 1), "%"),
        y = rand, x = 1, vjust = 1.2, hjust = 0, alpha = 0.2
      ) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
      scale_x_continuous(
        minor_breaks = NULL,
        breaks = seq(0, splits, 1)
      ) +
      labs(
        title = paste("Cumulative Response for", gains$value[1]),
        linetype = NULL,
        y = "Cumulative response [%]",
        x = paste0("Percentiles [", splits, "]")
      ) +
      theme(legend.position = c(0.88, 0.2)) +
      guides(colour = "none")

    if (highlight == "auto") {
      highlight <- gains %>%
        arrange(desc(.data$lift)) %>%
        slice(1) %>%
        .$percentile
    }
    if (highlight %in% gains$percentile && highlight != "none") {
      highlight <- as.integer(highlight)
      note <- paste0(
        "If we select the top ",
        round(highlight * 100 / splits), "% cases with highest probabilities,\n",
        round(gains$cum_response[gains$percentile == highlight]), "% belong to the target class ",
        "(", round(gains$cum_response_lift[gains$percentile == highlight]), "% better than random)"
      )
      p <- p + labs(subtitle = note)
    } else {
      message("That highlight value is not a percentile. Try any integer from 1 to ", splits)
    }
  } else {
    df <- data.frame(tag = tag, score = score, multis)
    for (i in 1:(ncol(df) - 2)) {
      if (i == 1) out <- NULL
      g <- gain_lift(df$tag, df[, 2 + i], target, splits, quiet = quiet) %>%
        mutate(label = colnames(df)[2 + i])
      out <- rbind(out, g)
    }
    p <- out %>%
      mutate(percentile = factor(.data$percentile, levels = unique(out$percentile))) %>%
      group_by(.data$label) %>%
      mutate(
        rand = 100 * sum(.data$target) / sum(.data$total),
        cum_response = 100 * cumsum(.data$target) / cumsum(.data$total)
      ) %>%
      ggplot(aes(group = .data$label, x = .data$percentile)) +
      geom_hline(aes(yintercept = .data$rand), colour = "black", linetype = "dashed") +
      geom_line(aes(y = .data$cum_response, colour = .data$label), size = 1.2) +
      geom_label(aes(y = .data$cum_response, label = round(.data$cum_response)), alpha = 0.9) +
      geom_text(aes(label = paste0("Random: ", round(.data$rand, 1), "%"), y = .data$rand),
        x = 1, vjust = 1.2, hjust = 0, alpha = 0.2
      ) +
      labs(
        title = "Cumulative Response Plot", linetype = NULL,
        y = "Cumulative response [%]",
        x = paste0("Percentiles [", splits, "]")
      ) +
      theme(legend.position = c(0.88, 0.2)) +
      theme_lares(pal = 2) +
      labs(
        title = "Cumulative Response for Multiple Labels",
        subtitle = paste(
          "If we select the top nth percentile with highest probabilities,",
          "\nhow many observations belong to that specific target class?"
        ),
        x = paste0("Percentiles [", max(as.numeric(out$percentile)), "]"),
        y = "Cumulative Response [%]",
        linetype = "Reference", colour = "Label"
      ) +
      facet_grid(.data$label ~ .) +
      guides(colour = "none") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))
  }

  aux <- freqs(data.frame(tag = tag), tag)
  text <- paste(
    "Assuming rate of",
    vector2text(round(aux$p), sep = "/", quotes = FALSE), "for",
    vector2text(aux$tag, sep = "/", quotes = FALSE)
  )
  if (!is.na(caption)) caption <- paste(text, caption, sep = "\n") else caption <- text
  p <- p + labs(caption = caption)

  if (save) export_plot(p, file_name, subdir = subdir, width = 6, height = 6)

  return(p)
}


####################################################################
#' Top Hit Ratios for Multi-Classification Models
#'
#' Calculate and plot a multi-class model's predictions accuracy
#' based on top N predictions and distribution of probabilities.
#'
#' @family ML Visualization
#' @inheritParams mplot_roc
#' @return Plot with performance results over most frequent categories.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dfr) # Results for AutoML Predictions
#' mplot_topcats(dfr$class3$tag, dfr$class3$score,
#'   multis = subset(dfr$class3, select = -c(tag, score)),
#'   model_name = "Titanic Class Model"
#' )
#' @export
mplot_topcats <- function(tag, score, multis, model_name = NA) {
  df <- data.frame(tag, score, multis)
  cats <- unique(tag)

  DF <- df %>%
    mutate(label = row_number()) %>%
    tidyr::gather("name", "value", -.data$tag, -.data$score, -.data$label) %>%
    group_by(.data$label) %>%
    arrange(.data$label, desc(.data$value)) %>%
    mutate(
      rank = row_number(),
      cumprob = cumsum(.data$value)
    )

  p1 <- DF %>%
    mutate(
      correct_Top1 = .data$tag %in% .data$name[1],
      correct_Top2 = .data$tag %in% .data$name[1:2],
      correct_Top3 = .data$tag %in% .data$name[1:3],
      correct_Top4 = .data$tag %in% .data$name[1:4],
      correct_Top5 = .data$tag %in% .data$name[1:5]
    ) %>%
    ungroup() %>%
    select(.data$label, .data$tag, .data$score, starts_with("correct_top")) %>%
    distinct() %>%
    select(starts_with("correct_top")) %>%
    tidyr::gather() %>%
    group_by(.data$key) %>%
    summarize(total = n(), value = sum(.data$value), .groups = "drop") %>%
    mutate(correct = .data$value / max(.data$total)) %>%
    mutate(key = gsub("correct_", "", .data$key)) %>%
    head(length(cats)) %>%
    ggplot(aes(
      x = .data$key, y = .data$correct,
      label = formatNum(100 * .data$correct, 0, pos = "%")
    )) +
    geom_col() +
    geom_label() +
    scale_y_percent(limits = c(0, 1)) +
    labs(
      title = "Hit Ratios (Accuracy)",
      x = "Top N Predicted Categories",
      y = "Hit Ratio [%]"
    ) +
    theme_lares()
  if (!is.na(model_name)) {
    p1 <- p1 + labs(caption = model_name)
  }

  p2 <- DF %>%
    filter(.data$rank <= 5) %>%
    ggplot(aes(x = as.character(.data$rank), y = .data$value)) +
    # geom_jitter(alpha = 0.1) +
    geom_boxplot() + # outlier.shape = NA, alpha = 0.8
    labs(
      caption = paste(
        "Observations:", formatNum(nrow(df), 0), "|",
        "Unique labels:", formatNum(length(unique(DF$score)), 0)
      ),
      title = "How certain is the model?",
      x = "Top N Predicted Category",
      y = "Prediction (Certainty) [%]"
    ) +
    scale_y_percent(limits = c(0, 1)) +
    theme_lares()

  p <- p1 + p2
  return(p)
}
