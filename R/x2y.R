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
#' @param df data.frame. Note that variables with no variance will be ignored.
#' @param target Character vector. If you are only interested in the \code{x2y}
#' values between particular variable(s) in \code{df}, set
#' name(s) of the variable(s) you are interested in. Keep \code{NULL}
#' to calculate for every variable (column). Check \code{target_x} and
#' \code{target_y} parameters as well.
#' @param confidence Boolean. Calculate 95\% confidence intervals estimated
#' with N \code{bootstraps}.
#' @param bootstraps Integer. If \code{confidence=TRUE}, how many bootstraps?
#' The more iterations we run the more precise the confidence internal will be.
#' @param symmetric Boolean. \code{x2y} metric is not symmetric with respect to
#' \code{x} and \code{y}. The extent to which \code{x} can predict \code{y} can
#' be different from the extent to which \code{y} can predict \code{x}. Set
#' \code{symmetric=TRUE} if you wish to average both numbers.
#' @param target_x,target_y Boolean. Force target features to be part of
#' \code{x} OR \code{y}?
#' @param plot Boolean. Return a plot? If not, only a data.frame with calculated
#' results will be returned.
#' @param top Integer. Show/plot only top N predictive cross-features. Set
#' to \code{NULL} to return all.
#' @param quiet Boolean. Keep quiet? If not, show progress bar.
#' @param ohse Boolean. Use \code{lares::ohse()} to pre-process the data?
#' @param corr Boolean. Add correlation and pvalue data to compare with? For
#' more custom studies, use \code{lares::corr_cross()} directly.
#' @param ... Additional parameters passed to \code{x2y_metric()}
#' @return Depending on \code{plot} input, a plot or a data.frame with x2y results.
#' @examples
#' \donttest{
#' data(dft) # Titanic dataset
#' x2y_results <- x2y(dft, quiet = TRUE, max_cat = 10, top = NULL)
#' head(x2y_results, 10)
#' plot(x2y_results, type = 2)
#'
#' # Confidence intervals with 10 bootstrap iterations
#' x2y(dft,
#'   target = c("Survived", "Age"),
#'   confidence = TRUE, bootstraps = 10, top = 8
#' )
#'
#' # Compare with mean absolute correlations
#' x2y(dft, "Fare", corr = TRUE, top = 6, target_x = TRUE)
#'
#' # Plot (symmetric) results
#' symm <- x2y(dft, target = "Survived", symmetric = TRUE)
#' plot(symm, type = 1)
#'
#' # Symmetry: x2y vs y2x
#' on.exit(set.seed(42))
#' x <- seq(-1, 1, 0.01)
#' y <- sqrt(1 - x^2) + rnorm(length(x), mean = 0, sd = 0.05)
#'
#' # Knowing x reduces the uncertainty about the value of y a lot more than
#' # knowing y reduces the uncertainty about the value of x. Note correlation.
#' plot(x2y_preds(x, y), corr = TRUE)
#' plot(x2y_preds(y, x), corr = TRUE)
#' }
#' @export
x2y <- function(df, target = NULL, symmetric = FALSE,
                target_x = FALSE, target_y = FALSE,
                plot = FALSE, top = 20, quiet = "auto",
                ohse = FALSE, corr = FALSE, ...) {
  if (target_x & target_y) {
    stop("Set to TRUE only target_x OR target_y, not both.")
  }
  if ((target_x || target_y) & symmetric) {
    target_x <- target_y <- FALSE
    warning("When symmetric, target_x NOR target_y will affect results.")
  }

  df <- select(df, -zerovar(df))
  if (ohse) df <- lares::ohse(df, ...)
  pairs <- combn(ncol(df), 2)
  pairs <- cbind(pairs, pairs[2:1, ])

  if (!is.null(target)) {
    check_opts(target, colnames(df))
    cols <- which(colnames(df) %in% target)
    with_cols <- apply(pairs, 2, function(x) any(cols %in% x))
    pairs <- pairs[, with_cols]
  }

  n <- dim(pairs)[2]

  confidence <- isTRUE(list(...)[["confidence"]])
  bootstraps <- list(...)[["bootstraps"]]
  bootstraps <- ifelse(confidence & !is.null(bootstraps), bootstraps, 0)
  results <- data.frame(x = names(df)[pairs[1, ]], y = names(df)[pairs[2, ]])
  x2ys <- data.frame()

  # Show progress bar when quiet = "auto" and lots of data
  if (!is.logical(quiet)) {
    quiet <- n * nrow(df) < 5e4 & bootstraps < 50
  }

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

  if (symmetric) {
    results <- results %>%
      mutate(xy = apply(results[, 1:2], 1, function(x) {
        paste(sort(x), collapse = "<>")
      })) %>%
      group_by(.data$xy) %>%
      summarise_if(is.numeric, function(x) mean(x, na.rm = TRUE)) %>%
      arrange(desc(.data$x2y), desc(.data$obs_p)) %>%
      tidyr::separate(.data$xy, c("x", "y"), sep = "<>")
  }

  if (corr) results <- .x2y_addcorr(results, df)

  if (!is.null(target)) {
    if (target_x) results <- filter(results, .data$x %in% target)
    if (target_y) results <- filter(results, .data$y %in% target)
  }

  if (!is.null(top)) results <- head(results, top)
  if (confidence) attr(results, "bootstraps") <- attr(r, "bootstraps")
  attr(results, "symmetric") <- symmetric
  class(results) <- c("x2y", class(results))
  if (plot) {
    return(plot(results))
  }
  return(results)
}

.x2y_addcorr <- function(x2y, df) {
  corr_df <- corr_cross(df, plot = FALSE, quiet = TRUE) %>%
    group_by(.data$group1, .data$group2) %>%
    summarise(
      mean_abs_corr = mean(abs(.data$corr), na.rm = TRUE),
      mean_pvalue = mean(.data$pvalue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(abs(.data$mean_abs_corr)))
  results <- x2y %>%
    left_join(corr_df, by = c("x" = "group1", "y" = "group2")) %>%
    left_join(corr_df, by = c("x" = "group2", "y" = "group1")) %>%
    tidyr::unite("mean_abs_corr", contains("mean_abs_corr"), na.rm = TRUE, remove = TRUE) %>%
    tidyr::unite("mean_pvalue", contains("mean_pvalue"), na.rm = TRUE, remove = TRUE) %>%
    mutate(
      mean_abs_corr = as.numeric(eval(.data$mean_abs_corr)),
      mean_pvalue = as.numeric(eval(.data$mean_pvalue))
    )
  return(results)
}


#' @rdname x2y
#' @param x,y Vectors. Categorical or numerical vectors of same length.
#' @param max_cat Integer. Maximum number of unique \code{x} or \code{y} values
#' when categorical. Will select then most frequent values and the rest will
#' be passed as \code{""}.
#' @export
x2y_metric <- function(x, y, confidence = FALSE, bootstraps = 20, max_cat = 20) {
  results <- list()
  missing <- is.na(x) | is.na(y)
  results$obs_p <- round(100 * (1 - sum(missing) / length(x)), 2)
  x <- x[!missing]
  y <- y[!missing]
  results$x2y <- .x2y_vals(x, y, max_cat)

  if (confidence) {
    results$lower_ci <- NA
    results$upper_ci <- NA
    if (!is.na(results$x2y) & results$x2y > 0) {
      n <- length(x)
      draws <- replicate(bootstraps, .simple_boot(x, y))
      errors <- draws - results$x2y
      results$lower_ci <- results$x2y - round(
        quantile(errors, probs = 0.975, na.rm = TRUE), 2
      )
      results$upper_ci <- results$x2y - round(
        quantile(errors, probs = 0.025, na.rm = TRUE), 2
      )
    }
    attr(results, "bootstraps") <- bootstraps
  }
  class(results) <- c("x2y_metric", class(results))
  return(results)
}

#' @rdname x2y
#' @export
plot.x2y_preds <- function(x, corr = FALSE, ...) {
  if (!inherits(x, "x2y_preds")) stop("Object must be class x2y_preds")
  p <- ggplot(x, aes(x = .data$x)) +
    geom_point(aes(y = .data$y), size = 0.5) +
    geom_line(aes(y = .data$p),
      colour = names(lares_pal()[[2]])[2],
      size = 0.8, alpha = 0.7
    ) +
    scale_color_brewer(name = NULL) +
    labs(
      title = "x's predictive power over y",
      subtitle = sprintf("x2y: %s", x2y_metric(x$x, x$y)$x2y)
    ) +
    theme_lares()
  if (corr & is.numeric(x$x) & is.numeric(x$y)) {
    p <- p + labs(caption = paste("Correlation:", signif(cor(x$x, x$y), 1))) +
      geom_smooth(aes(y = .data$y), method = "lm", formula = "y ~ x", size = 0.5)
  }
  return(p)
}

#' @rdname x2y
#' @param type Integer. Plot type: \code{1} for tile plot,
#' \code{2} for ranked bar plot.
#' @aliases x2y
#' @export
plot.x2y <- function(x, type = 1, ...) {
  if (!inherits(x, "x2y")) stop("Object must be class x2y")
  check_opts(type, 1:2)
  if (nrow(x) > 0) {
    x$var <- paste(x$x, ifelse(isTRUE(attr(x, "symmetric")), "><", ">>"), x$y)
    x$x2y <- signif(x$x2y, 3)
    if (type == 1) {
      p <- x %>%
        filter(.data$x2y > 0) %>%
        ggplot(aes(y = reorder(.data$var, .data$x2y), x = .data$x2y / 100)) +
        geom_col(colour = "transparent") +
        geom_text(aes(
          label = sub("^(-)?0[.]", "\\1.", signif(.data$x2y, 3)),
          hjust = ifelse(.data$x2y > max(.data$x2y) / 5, 1.1, -0.1)
        ),
        size = 3
        ) +
        scale_x_percent(expand = c(0, 0), position = "top") +
        labs(
          title = "Ranked Predictive Power of Cross-Features (x2y)",
          subtitle = ifelse(
            isTRUE(attr(x, "symmetric")),
            "Symmetric results: mean(x2y, y2x)",
            "Non-symmetric results: x2y != y2x"
          ),
          x = NULL, y = NULL
        ) +
        theme_lares()
    }
    if (type == 2) {
      p <- x %>%
        filter(.data$x2y > 0) %>%
        group_by(.data$x) %>%
        mutate(ximp = max(.data$x2y, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(.data$y) %>%
        mutate(yimp = max(.data$x2y, na.rm = TRUE)) %>%
        ungroup() %>%
        ggplot(aes(
          x = reorder(.data$x, -.data$ximp),
          y = reorder(.data$y, -.data$yimp),
          fill = .data$x2y, label = .data$x2y
        )) +
        geom_tile() +
        geom_text(size = 3.2) +
        scale_fill_gradient(low = "white", high = names(lares::lares_pal()[[2]])[1]) +
        labs(
          title = "Cross-features Predictive Power (x2y)",
          x = NULL, y = NULL,
          subtitle = ifelse(
            isTRUE(attr(x, "symmetric")),
            "Symmetric results: mean(x2y, y2x)",
            "Non-symmetric results: x2y != y2x"
          )
        ) +
        theme_lares(grid = "") +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
        coord_equal()
    }
    return(p)
  }
}

#' @rdname x2y
#' @export
x2y_preds <- function(x, y, max_cat = 10) {
  # If no variance
  if (length(unique(x)) == 1 | length(unique(y)) == 1) {
    return(NA)
  }
  # If x is categorical
  x <- .reduce_cats(x, max_cat)
  # If y is continuous
  if (is.numeric(y)) {
    preds <- predict(rpart(y ~ x, method = "anova"), type = "vector")
  } else {
    # If y is categorical
    y <- .reduce_cats(y, max_cat)
    preds <- predict(rpart(y ~ x, method = "class"), type = "class")
  }
  preds <- as_tibble(data.frame(x = x, y = y)) %>%
    removenarows(all = FALSE) %>%
    mutate(p = preds)
  attr(preds, "max_cat") <- max_cat
  class(preds) <- c("x2y_preds", class(preds))
  return(preds)
}

.x2y_vals <- function(x, y, ...) {
  if (length(unique(x)) == 1 | length(unique(y)) == 1) {
    return(NA)
  }
  preds <- x2y_preds(x, y, ...)$p
  if (is.numeric(y)) {
    .mae_reduction(preds, y)
  } else {
    .misclass_reduction(preds, y)
  }
}

.mae_reduction <- function(y_hat, y_actual) {
  model_error <- mean(abs(y_hat - y_actual))
  baseline <- mean(y_actual, na.rm = TRUE)
  baseline_error <- mean(abs(baseline - y_actual))
  result <- 1 - model_error / baseline_error
  result <- max(0.0, min(result, 1.0))
  round(100 * result, 2)
}

.misclass_reduction <- function(y_hat, y_actual) {
  tab <- table(y_hat, y_actual)
  model_error <- 1 - sum(diag(tab)) / sum(tab)
  majority_class <- names(which.max(table(y_actual)))
  baseline.preds <- rep(majority_class, length(y_actual))
  baseline_error <- mean(baseline.preds != y_actual)
  result <- 1 - model_error / baseline_error
  result <- max(0.0, min(result, 1.0))
  round(100 * result, 2)
}

.simple_boot <- function(x, y, ...) {
  ids <- sample(length(x), replace = TRUE)
  .x2y_vals(x[ids], y[ids], ...)
}

.reduce_cats <- function(x, max_cat) {
  if (!is.numeric(x) & length(unique(x)) > max_cat) {
    x <- as.character(x)
    top <- head(names(sort(table(x), decreasing = TRUE)), max_cat)
    x[!x %in% top] <- ""
  }
  return(x)
}
