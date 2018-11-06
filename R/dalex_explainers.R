####################################################################
#' DALEX Explainer
#' 
#' DALEX library function to create an explainer object
#' 
#' @param df Dataframe
#' @param model Model. Use a H2O Model by default. If another, use pred parameter
#' @param model_name Character. Name of your model
#' @param pred Function. Custom function for calculation predictions
#' @export
dalex_explainer <- function(df, model, model_name = "lares::h2o_automl", pred = NA) {
  
  x_valid <- as.data.frame(df) %>% select(-tag)
  y_valid <- as.integer(df$tag)
  
  # Prediction function
  if (is.na(pred)) {
    pred <- function(model, newdata)  {
      h2o.no_progress()
      results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
      return(results[[3L]])
    }
  }
  
  explainer <- explain(
    model = model,
    data = x_valid,
    y = y_valid,
    predict_function = pred,
    label = model_name
  )
  
  return(explainer)
  
}


####################################################################
#' DALEX Local
#' 
#' DALEX library function for local interpretations
#' 
#' @param explainer Object. Result from dalex_explainer function
#' @param row Dataframe. An observation which you want to study
#' @param plot Boolean. Do you wish to see the results plot?
#' @param print Boolean. Do you wish to see the results table?
#' @export
dalex_local <- function(explainer, row, plot = TRUE, print = TRUE) {

  individual <- row %>% as.data.frame()
  breakdown <- prediction_breakdown(explainer, observation = individual)
  
  if (plot == TRUE) {
    print(plot(breakdown))
  }
  
  if (print == TRUE) {
    print(breakdown[1:10, 1:5])
  }
  
  return(breakdown)
  
}


####################################################################
#' DALEX Residuals
#' 
#' DALEX library function for residuals
#' 
#' @param explainer Object. Result from dalex_explainer function
#' @export
dalex_residuals <- function (explainer) {

  resids <- model_performance(explainer)
  
  p1 <- plot(resids)
  p2 <- plot(resids, geom = "boxplot")
  
  grid.arrange(p1, p2, nrow = 2)
  
}


####################################################################
#' DALEX Partial Dependency Plots (PDP)
#' 
#' DALEX library function for creating Partial Dependency Plots and study
#' variable's responses vs independent vector.
#' 
#' @param explainer Object. Result from dalex_explainer function
#' @param variable Character. Which character do you wish to study?
#' @param force_class Character. If you wish to force a class on your variable, which one do you need?
#' @export
dalex_variable <- function (explainer, variable, force_class = NA) {
  
  classes <- c('factor','numeric')
  if (force_class %in% classes) {
    class(explainer$data[[variable]]) <- force_class
    message("Change class to ", force_class)
  } else {
    if (!is.na(force_class)) {
      stop("Try using force_class: ", paste(shQuote(classes), collapse=", ")) 
    }
  }
  
  if (is.numeric(explainer$data[[variable]]) & 
      length(unique(explainer$data[[variable]])) > 6) {
    message(paste0("Calculating and plotting ", variable, "'s response... this might take some time!"))
  }
  
  pdp <- variable_response(explainer, variable = variable, type = "pdp")
  
  return(plot(pdp))
  
}
