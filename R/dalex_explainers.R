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
# dalex_residuals(explainer)


############## Check specific important variables ############## 
dalex_variable <- function (explainer, variable, force_class = NA) {
  
  require(DALEX)
  
  var <- explainer$data[[variable]]
  if (is.na(force_class)) {
    type <- ifelse(is.numeric(var), "pdp", "factor") 
  } else {
    types <- c('factor','numeric','character')
    if (!force_class %in% types) {
      stop("Please, try any of the following:", paste(shQuote(types), collapse=", "))
    } else {
      type <- force_class
    }
  }
  message(paste0("Calculating and plotting ", 
                 variable, "'s response as a ", type, 
                 "... this might take some time!"))
  pdp <- variable_response(explainer, variable = variable, type = type)
  
  return(plot(pdp))
  
}
# dalex_variable(explainer, "Sex")
