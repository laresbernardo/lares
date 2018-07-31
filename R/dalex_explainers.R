############## DALEX explainer ############## 
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
# explainer <- dalex_explainer(results$datasets$train, results$model)

############## Local interpretations ############## 
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
# local <- dalex_local(explainer, results$datasets$train[15,], plot = TRUE, print = FALSE)


############## Model's residuals ############## 
dalex_residuals <- function (explainer) {
  
  require(DALEX)
  require(gridExtra)
  
  resids <- model_performance(explainer)
  
  p1 <- plot(resids)
  p2 <- plot(resids, geom = "boxplot")
  
  grid.arrange(p1, p2, nrow = 2)
  
}
