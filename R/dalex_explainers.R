####################################################################
#' DALEX Explainer for H2O
#' 
#' DALEX helper function to create an \code{explainer} object using
#' a \code{h2o} trained model.
#' 
#' @family Interpretability
#' @param df Dataframe. Must contain all columns and predictions
#' @param model Model object (H2O)
#' @param y Character or Variable name. Variable's column name.
#' @param ignore Character vector. Which columns should be ignored?
#' @return List; explainer. Containing the model, data, y, predict_function,
#' y_hat, residuals, class, label, model_info, residual_function, and weights.
#' @aliases dalex_explainer
#' @examples
#' # You must have \code{DALEX} library to use this auxiliary function:
#' \dontrun{
#' data(dft) # Titanic dataset
#' 
#' # TRAIN A SIMPLE MODEL
#' dfm <- h2o_automl(dft, y = "Survived",
#'                   ignore = c("Ticket", "PassengerId", "Cabin"),
#'                   max_models = 1)
#' 
#' # EXPLAINER
#' explainer <- h2o_explainer(df = dfm$datasets$test, model = dfm$model, y = "Survived")
#' explainer$data <- na.omit(explainer$data)
#' 
#' # CATEGORICAL EXAMPLE
#' class <- dalex_variable(explainer, vars = c("Pclass", "Sex"))
#' class$plot
#' 
#' # NUMERICAL EXAMPLE
#' num <- dalex_variable(explainer, vars = c("Fare", "Age"))
#' num$plot
#' 
#' # LOCAL EXAMPLE
#' local <- dalex_local(explainer, row = 1)
#' # OR YOU COULD MANUALLY INPUT THE OBSERVATION
#' local <- dalex_local(explainer, observation = explainer$data[1,])
#' local$plot
#' 
#' # xai2shiny's UI (needs to be installed from ModelOriented/xai2shiny)
#' xai2shiny(explainer, run = TRUE)
#' }
#' @export
h2o_explainer <- function(df, model, y = "tag", ignore = NA) {
  
  try_require("DALEX")
  
  if (!any(grepl("H2O", class(model))))
    stop("This function currently works with h2o models only!")
  
  df <- data.frame(df) %>% 
    # No need to use prediction results
    select(-c(which(colnames(.) == "train_test"):ncol(.))) %>%
    # Exclude variables with no variance
    select(-one_of(zerovar(.)))
  
  y <- gsub('"', "", as_label(enquo(y)))
  
  if (!y %in% colnames(df))
    stop(paste("The y value", y, "is not in your data.frame"))
  
  if (!is.na(ignore[1]))
    df <- df[,!(colnames(df) %in% ignore)]
  
  x_valid <- select(df, -one_of(y))
  y_valid <- df[y][,1]
  
  h2o_predict_fx <- function(model, newdata) {
    try_require("h2o")
    h2o.no_progress()
    results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
    return(results[[3L]])
  }
  
  explainer <- explain.default(
    model = model,
    data = x_valid,
    y = y_valid,
    predict_function = h2o_predict_fx,
    label = model@model_id)
  explainer$model_info$package <- "h2o"
  
  return(explainer)
  
}


####################################################################
#' DALEX Local
#' 
#' DALEX function for local interpretations
#' 
#' @family Interpretability
#' @param explainer Object. Result from h2o_explainer function
#' @param observation Data.frame. If you want to use an observation
#' that was not in the original explainer function, add here. Else, use row
#' @param row Dataframe. Row number from the data.frame used in explainer.
#' @param type Character. The type of variable attributions. 
#' Either shap, oscillations, break_down or break_down_interactions.
#' @return List. Containing observation, breakdown results, and breakdown plot.
#' @export
dalex_local <- function(explainer, observation = NA, row = 1, type = "break_down") {
  
  try_require("DALEX")
  tic("dalex_local")
  
  subtitle <- paste0("Observation #", v2t(row, quotes = FALSE))
  if (!is.data.frame(observation)) {
    observation <- explainer$data[row,] 
  } else {
    if (!all(colnames(observation) %in% colnames(explainer$data)))
      stop(paste("All columns must be present in your observation:",
                 v2t(colnames(explainer$data))))
  }
  
  # BREAKDOWN
  breakdown <- predict_parts(explainer, new_observation = observation, type = type)
  
  p <- plot(breakdown) + 
    theme_lares(legend = "none") + 
    labs(subtitle = NULL, caption = subtitle)
  
  return <- list(observation = observation, breakdown = breakdown, plot = p)
  toc("dalex_local")
  return(return)
  
}


####################################################################
#' DALEX Residuals
#' 
#' DALEX function for residuals
#' 
#' @family Interpretability
#' @param explainer Object. Result from h2o_explainer function
#' @return Plot. Based of \code{explainer} residual results.
#' @export
dalex_residuals <- function(explainer) {
  
  try_require("DALEX")
  
  resids <- model_performance(explainer)
  
  p1 <- plot(resids) + theme_lares(legend = "none")
  p2 <- plot(resids, geom = "boxplot") + theme_lares(legend = "none")
  
  p <- p1 + p2 + plot_layout(nrow = 2)
  
  return(p)
  
}


####################################################################
#' DALEX Partial Dependency Plots (PDP)
#' 
#' DALEX auxiliary function for creating Partial Dependency Plots and
#' study variable's responses vs independent vector.
#' 
#' @family Interpretability
#' @param explainer Object. Result from \code{h2o_explainer} function.
#' @param vars Character vector. Which features do you wish to study?
#' @param force_class Character. If you wish to force a class on your 
#' vars, which one do you need?
#' @return List. Containing PDP results, plot and \code{vars} input.
#' @examples
#' # You must have \code{DALEX} library to use this auxiliary function:
#' \dontrun{
#' # Having an "explainer" object created with \code{h2o_explainer}:
#' # For numerical variables
#' dalex_variable(explainer, vars = c("Age", "Fare"))
#' # For categorical variables
#' dalex_variable(explainer, vars = c("Pclass","Sex"))
#' }
#' @export
dalex_variable <- function(explainer, vars, force_class = NA) {
  
  try_require("DALEX")
  tic("dalex_variable")
  
  all_vars <- colnames(explainer$data)
  if (!all(vars %in% all_vars))
    stop("Select any variable(s) from the following: ", v2t(all_vars))
  
  if (!is.na(force_class)) {
    classes <- c('factor','numeric')
    if (force_class %in% classes) {
      class(explainer$data[[vars]]) <- force_class
      message(paste("Changeed class to", force_class))
    } else {
      if (!is.na(force_class))
        stop(paste("Try using force_class:", vector2text(classes)))
    } 
  }
  
  aux <- model_profile(explainer, variables = vars)
  p <- plot(aux) + theme_lares() + labs(y = "Average Prediction")
  pdp <- list(pdp = aux, plot = p, vars = vars)
  
  toc("dalex_variable")
  return(pdp)
  
}
