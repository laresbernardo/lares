####################################################################
#' Most Relevant Features Using Lasso Regression
#'
#' Use Lasso regression to identify the most relevant variables that
#' can predict/identify another variable. You might want to compare
#' with \code{corr_var()} and/or \code{x2y()} results to compliment
#' the analysis No need to standardize, center or scale your data.
#' Tidyverse friendly.
#'
#' @family Machine Learning
#' @family Exploratory
#' @inheritParams get_mp3
#' @param df Dataframe. Any dataframe is valid as \code{ohse} will be applied to
#' process categorical values, and values will be standardize automatically.
#' @param variable Variable. Dependent variable or response.
#' @param ignore Character vector. Variables to exclude from study.
#' @param nlambdas Integer. Number of lambdas to be used in a search.
#' @param nfolds Integer. Number of folds for K-fold cross-validation (>= 2).
#' @param top Integer. Plot top n results only.
#' @param seed Numeric.
#' @param ... Additional parameters passed to \code{ohse()}.
#' @return List. Contains lasso model coefficients, performance metrics, the
#' actual model fitted and a plot.
#' @examples
#' \dontrun{
#' # CRAN
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dft) # Titanic dataset
#'
#' m <- lasso_vars(dft, Survived, ignore = c("Cabin"))
#' print(m$coef)
#' print(m$metrics)
#' plot(m$plot)
#' }
#' @export
lasso_vars <- function(df, variable,
                       ignore = NULL,
                       nlambdas = 100,
                       nfolds = 10,
                       top = 20,
                       quiet = FALSE,
                       seed = 123, ...) {
  tic("lasso_vars")
  try_require("h2o")
  quiet(h2o.init(nthreads = -1, port = 54321))
  h2o.no_progress()
  on.exit(set.seed(seed))
  var <- enquo(variable)
  df <- select(df, !!var, everything())
  if (!is.null(ignore)) ignore <- c(ignore, as_label(var))

  # Run one-hot-smart-encoding
  temp <- data.frame(as.vector(df[, 1]), ohse(df[, -1], ignore = ignore, quiet = quiet, ...))
  colnames(temp)[1] <- "main"

  temp <- temp %>%
    filter(!is.na(.data$main)) %>%
    mutate(main = as.vector(base::scale(as.numeric(.data$main), scale = FALSE, center = TRUE)))

  if (!quiet) message(">>> Searching for optimal lambda with CV...")
  lasso_logistic <- h2o.glm(
    alpha = 1,
    seed = seed,
    y = "main",
    nfolds = nfolds,
    training_frame = as.h2o(temp),
    lambda_search = TRUE,
    nlambdas = nlambdas,
    standardize = TRUE
  )
  if (!quiet) message("Found best lambda: ", signif(lasso_logistic@model$lambda_best, 5))
  if (!quiet) message(">>> Fetching most relevant variables...")
  t_lasso_model_val <- h2o.glm(
    y = "main",
    training_frame = as.h2o(temp),
    alpha = 1,
    lambda = lasso_logistic@model$lambda_best
  )
  t_pred <- h2o.predict(t_lasso_model_val, as.h2o(temp))

  # R-squared, values closer to 1 are the best (rsq)
  rsq <- model_metrics(tag = temp$main, score = as.vector(t_pred[, 1]), thresh = 1)
  rsq$metrics$bestlambda <- lasso_logistic@model$lambda_best

  t_lasso_model_coeff <- lasso_logistic@model$coefficients_table %>%
    arrange(desc(abs(.data$standardized_coefficients))) %>%
    # To avoid numbers like -0.000000000000000364
    mutate(standardized_coefficients = as.numeric(ifelse(
      .data$names == "Intercept", 0, .data$standardized_coefficients
    )))

  t_lasso_model_coeff <- mutate(
    t_lasso_model_coeff,
    abs = abs(.data$standardized_coefficients),
    prc = .data$abs / sum(.data$abs),
    coef = ifelse(.data$coefficients > 0, "positive", "negative")
  ) %>%
    filter(.data$prc > 0)
  if (!quiet) message(">>> Generating plots for ", as_label(var), "...")
  if (nrow(t_lasso_model_coeff) > top && !quiet) {
    message(paste("- Plotting only the", top, "most relevant features..."))
  }

  p <- t_lasso_model_coeff %>%
    head(top) %>%
    ggplot(aes(
      x = reorder(.data$names, .data$prc),
      y = abs(.data$standardized_coefficients)
    )) +
    geom_col(aes(fill = .data$coef)) +
    coord_flip() +
    labs(
      x = NULL, y = "Absolute Standarized Coefficient",
      title = "Most Relevant Features (Lasso Regression)",
      subtitle = sprintf(
        "Response variable: %s | RSQ = %s",
        as_label(var), round(rsq$metrics$rsq, 4)
      ),
      fill = "Coeff > 0"
    ) +
    theme_lares(legend = "none", pal = 4) +
    scale_y_percent(expand = c(0, 0), position = "right")

  toc("lasso_vars", quiet = quiet)

  list(
    coef = as_tibble(t_lasso_model_coeff),
    metrics = as_tibble(rsq$metrics),
    model = invisible(t_lasso_model_val),
    plot = p
  )
}
