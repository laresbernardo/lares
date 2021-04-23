####################################################################
#' Ranked Predictive Power of Cross-Features (x2y)
#' 
#' The relative reduction in error when we go from a baseline model
#' (average for continuous and most frequent for categorical features) to
#' a predictive model, can measure the strength of the relationship between
#' two features. In other words, \code{x2y} measures the ability of \code{x}
#' to predict \code{y}. We use CART (Classification And Regression Trees) models
#' to be able to 1) compare numerical and non-numerical features, 2) detect
#' non-linear relationships, and 3) because they are easy/quick to train.
#' 
#' This \code{x2y} metric is based on Rama Ramakrishnan's 
#' \href{https://bit.ly/3sOVbei}{post}: An Alternative to the Correlation
#' Coefficient That Works For Numeric and Categorical Variables. This analysis
#' complements our \code{lares::corr_cross()} output.
#' 
#' @param df data.frame
#' @param target Character. If you are only interested in the \code{x2y}
#' values between particular variable(s) in \code{df}, set
#' name(s) of the variable(s) you are interested in. Keep \code{NULL}
#' to calculate for every variable (column).
#' @param confidence Boolean. Calculate 95\% confidence intervals estimated
#' with N \code{bootstraps}.
#' @param bootstraps Integer. If \code{confidence=TRUE}, how many bootstraps?
#' The more iterations we run the more precise the confidence internal will be.
#' @param symmetric Boolean. \code{x2y} metric is not symmetric with respect to
#' \code{x} and \code{y}. The extent to which \code{x} can predict \code{y} can
#' be different from the extent to which \code{y} can predict \code{x}. Set
#' \code{symmetric=TRUE} if you wish to average both numbers.
#' @param plot Boolean. Return a plot? If not, only a data.frame with calculated
#' results will be returned.
#' @param top Integer. Show/plot only top N predictive cross-features. Set
#' to \code{NULL} to return all.
#' @param quiet Boolean. Keep quiet? If not, show progress bar.
#' @param ... Additional parameters passed to \code{x2y_metric()}
#' @examples
#' data(dft) # Titanic dataset
#' x2y(dft, target = c("Survived","Age"), top = 10, quiet = TRUE)
#' x2y(dft, target = "Fare", confidence = TRUE, bootstraps = 10)
#' 
#' # Plot (symmetric) results
#' symm <- x2y(dft, target = "Fare", symmetric = TRUE)
#' plot(symm)
#' 
#' # Symmetry: x2y vs y2x
#' set.seed(42)
#' x <- seq(-1, 1, 0.01)
#' y <- sqrt(1 - x^2) + rnorm(length(x), mean = 0, sd = 0.05)
#' 
#' # Knowing x reduces the uncertainty about the value of y a lot more than
#' # knowing y reduces the uncertainty about the value of x
#' x2y_plot(x, y)
#' x2y_plot(y, x)
#' @export
x2y <- function(df, target = NULL, symmetric = FALSE,
                plot = FALSE, top = 20, quiet = "auto", ...) {
  
  pairs <- combn(ncol(df), 2)
  pairs <- cbind(pairs, pairs[2:1, ])
  
  if (!is.null(target)) {
    check_opts(target, colnames(df))
    cols <- which(colnames(df) %in% target)
    with_cols <- apply(pairs, 2, function(x) any(cols %in% x))
    pairs <- pairs[,with_cols]
  }
  
  n <- dim(pairs)[2]
  
  confidence <- isTRUE(list(...)[["confidence"]])
  bootstraps <- list(...)[["bootstraps"]]
  bootstraps <- ifelse(confidence & !is.null(bootstraps), bootstraps, 0)
  results <- data.frame(x = names(df)[pairs[1,]], y = names(df)[pairs[2,]])
  x2ys <- data.frame()
  
  # Show progress bar when quiet = "auto" and lots of data
  if (!is.logical(quiet)) 
    quiet <- n*nrow(df) < 5e4 & bootstraps < 50
  
  for (i in 1:n) {
    x <- pull(df, pairs[1, i])
    y <- pull(df, pairs[2, i])
    r <- x2y_metric(x, y, ...)
    x2ys <- rbind(x2ys, r)
    if (!quiet) statusbar(i, n)
  }
  
  results <- cbind(results, x2ys) %>%
    arrange(desc(.data$x2y), desc(.data$obs_p)) %>%
    as_tibble()
  
  if (symmetric) results <- results %>%
    mutate(xy = apply(results[,1:2], 1, function(x)
      paste(sort(x), collapse = "<>"))) %>%
    group_by(.data$xy) %>%
    summarise_if(is.numeric, function(x) mean(x, na.rm = TRUE)) %>%
    arrange(desc(.data$x2y), desc(.data$obs_p)) %>%
    tidyr::separate(.data$xy, c("x","y"), sep = "<>")
  
  if (!is.null(top)) results <- head(results, 10)
  if (confidence) attr(results, "bootstraps") <- attr(r, "bootstraps")
  attr(results, "symmetric") <- symmetric
  class(results) <- c("x2y", class(results))
  if (plot) return(plot(results))
  return(results)
}


#' @rdname x2y
#' @param x,y Vectors. Categorical or numerical vectors of same length.
#' @export
x2y_metric <- function(x, y, confidence = FALSE, bootstraps = 20) {
  
  results <- list()
  missing <-  is.na(x) | is.na(y)
  results$obs_p <- round(100 * (1 - sum(missing) / length(x)), 2)
  x <- x[!missing]
  y <- y[!missing]
  results$x2y <- x2y_vals(x, y)
  
  if (confidence) {
    results$lower_ci <- NA
    results$upper_ci <- NA
    if (!is.na(results$x2y) & results$x2y > 0) {
      n <- length(x)
      draws <- replicate(bootstraps, simple_boot(x, y))
      errors <- draws - results$x2y
      results$lower_ci <- results$x2y - round(
        quantile(errors, probs = 0.975, na.rm = TRUE), 2)
      results$upper_ci <- results$x2y - round(
        quantile(errors, probs = 0.025, na.rm = TRUE), 2)
    }
    attr(results, "bootstraps") <- bootstraps
  }
  class(results) <- c("x2y_metric", class(results))
  return(results)
}

#' @rdname x2y
#' @export
x2y_plot <- function(x, y, ...) {
  pred <- data.frame(x2y_preds(x, y)) %>% mutate(id = rownames(.))
  pred <- data.frame(id = as.character(1:length(x))) %>% left_join(pred, "id")
  df <- data.frame(x, y, pred = pred[,2]) %>% removenarows(all = FALSE)
  p <- ggplot(df, aes(x = .data$x)) +
    geom_point(aes(y = .data$y), size = 0.5) +
    geom_line(aes(y = .data$pred)) +
    scale_color_brewer(name = NULL) +
    labs(title = "x's predictive power over y",
         subtitle = sprintf("x2y: %s", x2y_metric(x, y)$x2y)) +
    theme_lares()
  return(p)
}

#' @rdname x2y
#' @aliases x2y
#' @export
plot.x2y <- function(x, ...) {
  if (!inherits(x, 'x2y')) stop('Object must be class x2y')
  if (nrow(x) > 0) {
    x$var <- paste(x$x,ifelse(isTRUE(attr(x, "symmetric")), "><", ">>"), x$y)
    p <- ggplot(x, aes(y = reorder(.data$var, .data$x2y), x = .data$x2y/100)) +
      geom_col(colour = "transparent") +
      geom_text(aes(label = sub('^(-)?0[.]', '\\1.', signif(.data$x2y, 3)),
                    hjust = ifelse(.data$x2y > max(.data$x2y)/5, 1.1, -0.1)),
                size = 3) +
      scale_x_percent(expand = c(0, 0), position = "top") +
      labs(title = "Ranked Predictive Power of Cross-Features (x2y)",
           caption = ifelse(
             isTRUE(attr(x, "symmetric")),
             "Symmetric results: mean(x2y, y2x)",
             "Non-symmetric results: x2y != y2x"),
           x = NULL, y = NULL) +
      theme_lares()
    return(p)
  }
}

x2y_preds <- function(x, y) {
  # If no variance
  if (length(unique(x)) == 1 | length(unique(y)) == 1)
    return(NA)
  if (is.numeric(y)) {
    # If y is continuous
    preds <- predict(rpart(y ~ x, method = "anova"), type = 'vector')
  } else {
    # If y is categorical
    preds <- predict(rpart(y ~ x, method = "class"), type = 'class')
  }
  class(preds) <- c("x2y_preds", class(preds))
  return(preds)
}

x2y_vals <- function(x, y) {
  if (length(unique(x)) == 1 | length(unique(y)) == 1) return(NA)
  preds <- x2y_preds(x, y)
  if (is.numeric(y)) {
    mae_reduction(preds, y)
  } else {
    misclass_reduction(preds, y)
  }
}

mae_reduction <- function(y_hat, y_actual) {
  model_error <- mean(abs(y_hat - y_actual))
  baseline <- mean(y_actual, na.rm = TRUE)
  baseline_error <-  mean(abs(baseline - y_actual))
  result <- 1 - model_error/baseline_error
  result <- max(0.0, min(result, 1.0))
  round(100*result, 2)
}

misclass_reduction <- function(y_hat, y_actual) {
  tab <- table(y_hat, y_actual)
  model_error <- 1 - sum(diag(tab))/sum(tab)
  majority_class <- names(which.max(table(y_actual)))
  baseline.preds <- rep(majority_class, length(y_actual))
  baseline_error <- mean(baseline.preds != y_actual)
  result <- 1 - model_error/baseline_error
  result <- max(0.0, min(result, 1.0))
  round(100*result, 2)
}

simple_boot <- function(x, y) {
  ids <- sample(length(x), replace = TRUE)
  x2y_vals(x[ids], y[ids])
}
