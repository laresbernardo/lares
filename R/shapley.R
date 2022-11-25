####################################################################
#' SHAP values for H2O Models
#'
#' SHAP (SHapley Additive exPlanations) by Lundberg and Lee (2016) is a
#' method to explain individual predictions. SHAP is based on the game
#' theoretically optimal Shapley Values. Calculate SHAP values for
#' h2o models in which each row is an observation and each column a feature.
#' Use \code{plot} method to visualize features importance and distributions.
#'
#' @family SHAP
#' @param model \code{h2o_automl} object or \code{h2o} model.
#' @param test String or Dataframe. Leave "auto" to use \code{h2o_automl}'s
#' test dataset or pass a valid dataframe.
#' @param scores Numeric vector. If test != "auto", you must provide predicted values
#' @param y Character. If test != "auto", you must provide y variable's name
#' @param ... Additional argument for \code{predict_contributions.H2OModel}
#' @return H2OFrame with shap values for every observation and feature.
#' @examples
#' \dontrun{
#' # Train a h2o_automl model
#' model <- h2o_automl(dft, Survived,
#'   max_models = 1, target = TRUE,
#'   ignore = c("Ticket", "Cabin", "PassengerId"),
#'   quiet = TRUE
#' )
#'
#' # Calculate SHAP values
#' SHAP_values <- h2o_shap(model)
#' # Equivalent to:
#' # SHAP_values <- h2o_shap(
#' #  model = model$model,
#' #  test = model$datasets$test,
#' #  scores = model$scores_test$scores)
#'
#' # Check SHAP results
#' head(SHAP_values)
#'
#' # You must have "ggbeeswarm" library to use this auxiliary function:
#' # Plot SHAP values (feature importance)
#' plot(SHAP_values)
#'
#' # Plot some of the variables (categorical)
#' shap_var(SHAP_values, Pclass)
#'
#' # Plot some of the variables (numerical)
#' shap_var(SHAP_values, Fare)
#' }
#' @export
h2o_shap <- function(model, test = "auto", scores = "auto", y = "y", ...) {
  # When h2o_automl object and test = "auto"
  if (!is.null(attr(model, "type"))) {
    if (!is.data.frame(test)) {
      if (test == "auto") {
        test <- model$datasets$test
        scores <- model$scores_test[, 2]
        algos <- c("DRF", "GBM", "XGBOOST")
        if (!toupper(model$algorithm) %in% algos) {
          warning(paste(
            "You've passed a", model$algorithm, "model.",
            "Accepted algorithms:", v2t(algos)
          ))
        }
        auto <- TRUE
        y <- model$y
        model <- model$model
      }
    }
  } else {
    auto <- FALSE
  }

  # Calculate SHAP values
  test <- .quiet_h2o(as.h2o(test))
  shap <- .quiet_h2o(predict_contributions.H2OModel(model, test, ...))

  class(shap) <- c(class(shap), "h2o_shap")
  attr(shap, "test") <- as_tibble(test)
  attr(shap, "scores") <- scores
  if (auto) attr(shap, "y") <- y
  return(shap)
}

####################################################################
#' @rdname h2o_shap
#' @aliases h2o_shap
#' @param x h2o_shap object
#' @param relevant Boolean. Keep only relevant non-trivial (>0) features
#' @param top Integer. Plot only top n values (as in importance)
#' @param quiet Boolean. Print messages?
#' @export
plot.h2o_shap <- function(x, relevant = TRUE, top = 15, quiet = FALSE, ...) {
  if (!inherits(x, "h2o_shap")) {
    stop("Pass a valid h2o_shap object to proceed!")
  }

  try_require("ggbeeswarm", stop = FALSE)
  if ("ggbeeswarm" %in% (.packages())) {
    fun <- function() {
      geom_quasirandom(
        groupOnX = FALSE, varwidth = TRUE, size = 0.9,
        alpha = 0.5, width = 0.2
      )
    }
  } else {
    fun <- function() geom_jitter(height = 0.2, alpha = 0.5)
  }

  scores <- attr(x, "scores")

  df <- x %>%
    as.data.frame() %>%
    select(-.data$BiasTerm) %>%
    gather("feature", "shap_value") %>%
    mutate(score = rep(scores, length(unique(.data$feature)))) %>%
    group_by(.data$feature) %>%
    mutate(
      shap_importance = mean(abs(.data$shap_value)),
      shap_mean = mean(.data$shap_value),
      varies = length(unique(.data$shap_value)) != 1
    ) %>%
    filter(.data$varies == relevant)
  features <- distinct(df, .data$feature, .data$shap_importance) %>%
    arrange(desc(.data$shap_importance)) %>%
    pull(.data$feature)
  df$feature <- factor(df$feature, levels = features)

  if (length(features) > top) {
    df <- dplyr::filter(df, .data$feature %in% features[1:top])
    subtitle <- sprintf("%s most important variables (of %s)", top, length(features))
  }

  # SHAP contribution plot
  pal <- names(lares_pal()$palette)
  p1 <- ggplot(df, aes(
    colour = .data$score,
    x = .data$shap_value,
    y = reorder(.data$feature, .data$shap_importance)
  )) +
    geom_vline(xintercept = 0, alpha = 0.5, colour = "black") +
    fun() +
    scale_colour_gradient(low = pal[3], high = pal[1]) +
    labs(
      x = "SHAP values", y = NULL,
      title = "SHAP Distribution"
    ) +
    theme_lares(legend = "none")

  # SHAP importance plot
  p2 <- df %>%
    select(.data$feature, .data$shap_importance) %>%
    distinct() %>%
    ggplot(aes(
      x = reorder(.data$feature, .data$shap_importance),
      y = .data$shap_importance
    )) +
    geom_col(fill = "black", alpha = 0.8) +
    coord_flip() +
    labs(
      x = NULL, y = "mean(|SHAP value|)",
      title = "Feature Importance"
    ) +
    theme_lares()
  if (length(features) > top) {
    p2 <- p2 + labs(caption = sprintf("%s most important features (of %s)", top, length(features)))
  }

  # Combine plots
  p <- p1 + p2
  return(p)
}


####################################################################
#' SHAP-based dependence plots for categorical/numerical features (PDP)
#'
#' Having a \code{h2o_shap} object, plot a dependence plot for any
#' categorical or numerical feature.
#'
#' @family SHAP
#' @param x \code{h2o_shap} object
#' @param var Variable name
#' @param keep_outliers Boolean. Outliers detected with z-score and 3sd
#' may be suppress or kept in your plot. Keep them?
#' @return ggplot2 objct with shap values plotted
#' @inherit h2o_shap examples
#' @export
shap_var <- function(x, var, keep_outliers = FALSE) {
  if (!inherits(x, "h2o_shap")) {
    stop("Pass a valid h2o_shap object to proceed!")
  }

  try_require("ggbeeswarm", stop = FALSE)
  if ("ggbeeswarm" %in% (.packages())) {
    fun <- function() {
      geom_quasirandom(
        groupOnX = TRUE, varwidth = TRUE, size = 1,
        alpha = 0.6, width = 0.4
      )
    }
  } else {
    fun <- function() geom_jitter(height = 0.2, alpha = 0.5)
  }

  test <- attr(x, "test")
  scores <- attr(x, "scores")
  y <- attr(x, "y")
  var <- enquo(var)
  name <- as_label(var)

  # Gather everything up
  shap_df2 <- x %>%
    as.data.frame() %>%
    select(starts_with(name)) %>%
    mutate(
      model_result = attr(x, "scores"),
      real_value = pull(attr(x, "test")[name], 1)
    ) %>%
    tidyr::gather(starts_with(name), key = "feature", value = "shap")

  type <- if (is.numeric(pull(shap_df2, .data$real_value)[1])) "numerical" else "categorical"
  title <- glued("SHAP values for {type} feature: {as_label(var)}")

  # Outliers
  if (type == "numerical") {
    shap_df2 <- mutate(shap_df2, outlier = outlier_zscore(.data$real_value))
  } else {
    shap_df2$outlier <- FALSE
  }
  outs <- freqs(shap_df2, .data$outlier)
  outs_msg <- NULL
  if (!keep_outliers) {
    if (nrow(outs) > 1) {
      outs_msg <- glued("{outs$n[2]} outlier data points excluded (out of {nrow(shap_df2)})")
      shap_df2 <- filter(shap_df2, .data$outlier == FALSE)
    }
  }

  pal <- names(lares_pal()$palette)
  p <- shap_df2 %>%
    ggplot(aes(x = .data$real_value, y = .data$shap, colour = .data$model_result)) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    fun() +
    geom_smooth(method = "loess", formula = "y ~ x", colour = "black", size = 0.6) +
    geom_smooth(method = "lm", formula = "y ~ x", colour = "black", size = 0.3) +
    scale_colour_gradient(low = pal[3], high = pal[1]) +
    labs(
      x = name, y = paste("SHAP values for", name),
      colour = "Prediction",
      title = title, caption = outs_msg,
      subtitle = paste("Predicted variable:", y)
    ) +
    theme_lares()

  return(p)
}

# # Train a model
# model <- h2o_automl(dft, Survived, max_models = 1,
#                     target = TRUE,
#                     ignore = c("Ticket", "Cabin", "PassengerId"))
#
# # Calculate SHAP values
# SHAP_values <- h2o_shap(model)
#
# # Plot some of the variables (categorical and numerical)
# shap_var(SHAP_values, Pclass)
# shap_var(SHAP_values, Fare)
# shap_var(SHAP_values, Fare, keep_outliers = TRUE)
# shap_var(SHAP_values, Age)
