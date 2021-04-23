####################################################################
#' Ranked Predictive Power Cross-Features (x2y metric)
#' 
#' The porcentual reduction in error when we go from a baseline model to
#' a predictive model measures the strength of the relationship between 
#' features x and y. This metric, x2y, measures the ability of x to 
#' predict y. Based on Rama Ramakrishnan's post: An Alternative to the
#' Correlation Coefficient That Works For Numeric and Categorical Variables.
#' 
#' @param df data.frame
#' @param target Character. If you are only interested in the \code{x2y}
#' values between particular variable(s) in \code{df}, set
#' name(s) of the variable(s) you are interested in. Keep \code{NULL}
#' to calculate for every variable (column).
#' @param confidence Boolean. Calculate 95% confidence intervals estimated
#' with n \code{bootstraps}.
#' @param bootstraps Integer. If \code{confidence=TRUE}, how many bootstraps?
#' The more iterations we run the more precise the \code{confidence}internal.
#' @param plot Boolean. Return a plot? If not, only a data.frame with calculated
#' results will be returned.
#' @param top Integer. Show/plot only top N predictive cross-features. Set
#' to \code{NULL} to return all.
#' @param quiet Boolean. Keep quiet? If not, show progress bar.
#' @param ... Additional parameters passed to \code{x2y_calc()}
#' @examples
#' \dontrun{
#' x2y(dft, c("Survived","Age"))
#' x2y(dft, "Fare", confidence = TRUE, bootstraps = 10)
#' }
#' @export
x2y <- function(df, target = NULL, plot = FALSE, top = 20, quiet = "auto", ...) {
  
  try_require("rpart")
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
    quiet <- n*nrow(df) < 1e4 & bootstraps < 50
  
  for (i in 1:n) {
    x <- pull(df, pairs[1, i])
    y <- pull(df, pairs[2, i])
    r <- x2y_calc(x, y, ...)
    x2ys <- rbind(x2ys, r)
    if (!quiet) statusbar(i, n)
  }
  
  results <- cbind(results, x2ys) %>%
    arrange(desc(.data$x2y), desc(.data$obs_p)) %>%
    as_tibble()
  
  if (!is.null(top)) head(results, top)
  if (confidence) attr(results, "bootstraps") <- attr(r, "bootstraps")
  class(results) <- c("x2y", class(results))
  if (plot) return(plot(results))
  return(results)
}

#' @rdname x2y
#' @param x,y Vectors. Categorical or numerical vectors of same length.
#' @export
x2y_calc <- function(x, y, confidence = FALSE, bootstraps = 10) {
  
  results <- list()
  missing <-  is.na(x) | is.na(y)
  results$obs_p <- round(100 * (1 - sum(missing) / length(x)), 2)
  x <- x[!missing]
  y <- y[!missing]
  results$x2y <- x2y_inner(x, y)
  
  if (confidence == TRUE) {
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
  return(results)
}

#' @rdname x2y
#' @aliases x2y
#' @export
plot.x2y <- function(x, ...) {
  if (!inherits(x, 'x2y')) stop('Object must be class x2y')
  mutate(x, var = sprintf("%s > %s", .data$x, .data$y)) %>%
    ggplot(aes(y = reorder(.data$var, .data$x2y), x = .data$x2y/100)) +
    geom_col(colour = "transparent") +
    geom_text(aes(label = sub('^(-)?0[.]', '\\1.', signif(.data$x2y, 3)),
                  hjust = ifelse(.data$x2y > max(.data$x2y)/5, 1.1, -0.1)), 
              size = 3) +
    scale_x_percent(expand = c(0, 0), position = "top") +
    labs(title = "Ranked Predictive Power Cross-Features (x2y)", 
         x = NULL, y = NULL) +
    theme_lares()
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

x2y_inner <- function(x, y) {
  # If no variance
  if (length(unique(x)) == 1 | length(unique(y)) == 1) return(NA)
  # If y is continuous
  if (is.numeric(y)) {
    preds <- predict(rpart(y ~ x, method = "anova"), type = 'vector')
    mae_reduction(preds, y)
  } else {
    # If y is categorical
    preds <- predict(rpart(y ~ x, method = "class"), type = 'class')
    misclass_reduction(preds, y)
  }
}

simple_boot <- function(x, y) {
  ids <- sample(length(x), replace = TRUE)
  x2y_inner(x[ids], y[ids])
}
