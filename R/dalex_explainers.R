dalex_explainer <- function(df, model, label = "lares::h2o_automl") {
  require(DALEX) 
  
  x_valid <- as.data.frame(df) %>% select(-tag)
  y_valid <- as.integer(df$tag)
  
  pred <- function(model, newdata)  {
    h2o.no_progress()
    results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
    return(results[[3L]])
  }
  
  explainer <- explain(
    model = model,
    data = x_valid,
    y = y_valid,
    predict_function = pred,
    label = label
  )
  
  return(explainer)

}

dalex_local <- function(explainer, row, plot = TRUE, print = TRUE) {
  
  require(DALEX)
  
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
