####################################################################
#' SHAP values for H2O Models
#' 
#' SHAP (SHapley Additive exPlanations) by Lundberg and Lee (2016) is a 
#' method to explain individual predictions. SHAP is based on the game 
#' theoretically optimal Shapley Values. Calculate SHAP values for
#' h2o models in which each row is an observation and each column a feature.
#' Use `plot` method to visualize features importance and distributions.
#'
#' @param model \code{h2o_automl} object or \code{h2o} model.
#' @param test String or Dataframe. Leave "auto" to use \code{h2o_automl}'s
#' test dataset or pass a valid dataframe.
#' @param ... Additional argument for \code{predict_contributions.H2OModel}
#' @examples 
#' \dontrun{
#' shap_df <- h2o_shap(h2o_automl_object)
#' plot(shap_df)
#' }
#' @export
h2o_shap <- function(model, test = "auto", ...) {
  
  # When h2o_automl object and test = "auto"
  if (!is.null(attr(model, "type"))) {
    if (!is.data.frame(test)) {
      if (test == "auto") {
        test <- model$datasets$test
        scores <- model$scores_test
        model <- model$model 
        auto <- TRUE
      } 
    }
  } else auto <- FALSE
  
  # Calculate SHAP values
  test <- quiet(as.h2o(test))
  shap <- quiet(predict_contributions.H2OModel(model, test, ...))
  
  class(shap) <- c(class(shap), "h2o_shap")
  if (auto) attr(shap, "scores") <- scores
  attr(shap, "test") <- as_tibble(test)
  return(shap)
}

####################################################################
#' @rdname print
#' @param x \code{h2o_shap} object
#' @param relevant Boolean. Keep only relevant non-trivial (>0) features
#' @param top Integer. Plot only top n values (as in imoprtance)
#' @param quiet Boolean. Print messages?
#' @param ... Additional parameters
#' @export
plot.h2o_shap <- function(x, relevant = TRUE, top = 15, quiet = FALSE, ...) {
  
  # if (!inherits(x, 'h2o_shap'))
  #   stop('Object must be class h2o_shap')
  try_require("ggbeeswarm")
  scores <- attr(x, "scores")[,2]
  
  df <- x %>%
    as.data.frame %>%
    select(-.data$BiasTerm) %>%
    gather("feature", "shap_value") %>%
    mutate(score = rep(scores, length(unique(.data$feature)))) %>%
    group_by(.data$feature) %>%
    mutate(shap_importance = mean(abs(.data$shap_value)),
           shap_mean = mean(.data$shap_value),
           varies = length(unique(.data$shap_value)) != 1) %>%
    filter(.data$varies == relevant)
  features <- distinct(df, .data$feature, .data$shap_importance) %>%
    arrange(desc(.data$shap_importance)) %>% pull(.data$feature)
  df$feature <- factor(df$feature, levels = features)
  
  if (length(features) > top) {
    if (!quiet) message(sprintf(
      "Plotting top %s important features only. Use `top` argument to overwrite.", top))
    df <- dplyr::filter(df, .data$feature %in% features[1:top])
    subtitle <- sprintf("%s most important variables (of %s)", top, length(features))
  }
  
  # SHAP contribution plot
  p1 <- ggplot(df, aes(
    colour = .data$score,
    x = .data$shap_value, 
    y = reorder(.data$feature, .data$shap_importance))) +
    geom_vline(xintercept = 0, alpha = 0.5, colour = "orange") +
    geom_quasirandom(
      groupOnX = FALSE, varwidth = TRUE, size = 0.9, 
      alpha = 0.5, width = 0.2) +
    scale_colour_gradient(low = "red", high = "blue") +
    labs(x = "SHAP values", y = NULL, 
         title = "SHAP Distribution") +
    theme_lares(legend = "none")
  
  # SHAP importance plot
  p2 <- df %>% 
    select(.data$feature, .data$shap_importance) %>%
    distinct() %>% 
    ggplot(aes(x = reorder(.data$feature, .data$shap_importance),
               y = .data$shap_importance)) +
    geom_col(fill = 'black', alpha = 0.8) +
    coord_flip() +
    labs(x = NULL, y = "mean(|SHAP value|)", 
         title = "Feature Importance") +
    theme_lares()
  if (length(features) > top) {
    p2 <- p2 + labs(caption = sprintf("%s most important features (of %s)", top, length(features)))
  }
  
  # Combine plots
  p <- p1 + p2
  return(p)
  
}
