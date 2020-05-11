####################################################################
#' Find Most Relevant Features Using Lasso Regression
#' 
#' Use Lasso regression to identify the most relevant variables that 
#' can predict/identify another variable. You might want to compare 
#' with corr_var() results for further understanding. No need to
#' standardize your data.
#' 
#' @family Machine Learning
#' @param df Dataframe
#' @param variable Variable.
#' @param ignore Character vector. Variables to exclude from study
#' @param nlambdas Integer. Number of lambdas to be used in a search
#' @param seed Numeric
#' @param ... ohse parameters
#' @examples 
#' \dontrun{
#' data(dft)
#' m <- lasso_vars(dft, Survived, ignore = c("Ticket", "Cabin"))
#' }
#' @export
lasso_vars <- function(df, variable, ignore = NA, nlambdas = 100, seed = 123, ...) {
  
  tic("lasso_vars")
  quiet(h2o.init(nthreads = -1, port = 54321, min_mem_size = "8g"))
  h2o.no_progress()
  set.seed(seed)
  var <- enquo(variable)
  df <- select(df, !!var, everything())
  
  if (!is.na(ignore)[1])
    ignore <- c(ignore, as.character(var)[2])
  else 
    ignore <- as.character(var)[2]
  
  # Run one-hot-smart-encoding
  temp <- data.frame(var = as.vector(df[,1]), ohse(df, ignore = ignore, ...)) %>%
    dplyr::filter(!is.na(var)) %>%
    mutate(var = as.vector(base::scale(as.numeric(.data$var), scale = FALSE, center = TRUE)))
  
  message(">>> Searching for optimal lambda with CV...")
  lasso_logistic <- h2o.glm(alpha = 1,
                            seed = seed,
                            y = "var",
                            nfolds = 10,
                            training_frame = as.h2o(temp),
                            lambda_search = TRUE,
                            nlambdas = nlambdas,
                            standardize = TRUE)
  message("Found best lambda: ", signif(lasso_logistic@model$lambda_best, 5))
  message(">>> Fetching most relevant variables...")
  t_lasso_model_val <- h2o.glm(y = "var", 
                               training_frame = as.h2o(temp), 
                               alpha = 1, 
                               lambda = lasso_logistic@model$lambda_best)
  t_pred <- h2o.predict(t_lasso_model_val, as.h2o(temp))
  
  # R-squared, values closer to 1 are the best (rsq)
  rsq <- model_metrics(tag = temp$var, score = as.vector(t_pred[,1]), thresh = 1)
  rsq$metrics$bestlambda <- lasso_logistic@model$lambda_best
  
  t_lasso_model_coeff <- lasso_logistic@model$coefficients_table %>%
    arrange(desc(abs(.data$standardized_coefficients)))
  
  message(">>> Ploting results for ", as.character(var)[2], "...")
  p <- t_lasso_model_coeff %>%
    filter(.data$names != "Intercept") %>%
    mutate(abs = abs(.data$standardized_coefficients),
           prc = .data$abs/sum(.data$abs)) %>%
    filter(.data$prc > 0) %>%
    ggplot(aes(x = reorder(.data$names, .data$prc),
               y = abs(.data$standardized_coefficients),
               fill = .data$coefficients > 0)) + 
    coord_flip() + geom_col() +
    labs(x = NULL, y = "Absolute Standarized Coefficient",
         title = "Most Relevant Features (Lasso Regression)",
         subtitle = paste("RSQ =", round(rsq$metrics$rsq, 4)),
         fill = "Coeff > 0") +
    theme_lares2(pal = 1, legend = "top") +
    scale_y_percent(expand = c(0, 0))
  
  toc("lasso_vars")
  
  return(list(coef = as_tibble(t_lasso_model_coeff), 
              metrics = as_tibble(rsq$metrics),
              plot = p))
}
