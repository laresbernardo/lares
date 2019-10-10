####################################################################
#' DALEX Explainer for H2O
#' 
#' DALEX function to create an explainer object
#' 
#' @family Interpretability
#' @param df Dataframe. Must contain all columns and predictions
#' @param model Model object
#' @param y Character. Variable's column name
#' @param ignore Character vector. Which columns should be ignored?
#' @export
dalex_explainer <- function(df, model, y = "tag", ignore = NA) {
  
  try_require("DALEX")
  
  if (!any(grepl("H2O", class(model))))
    stop("This function currently works with h2o models only!")
  
  df <- data.frame(df)
  
  if (!y %in% colnames(df))
    stop(paste("The y value", y, "is not in your data.frame"))
  
  if (!is.na(ignore)[1])
    df <- df[,!(colnames(df) %in% ignore)]
  
  x_valid <- select(df, -y)
  y_valid <- df[y][,1]
  
  h2o <- function(model, newdata) {
    try_require("h2o")
    h2o.no_progress()
    results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
    return(results[[3L]])
  }
  
  explainer <- explain(
    model = model,
    data = x_valid,
    y = y_valid,
    predict_function = h2o,
    label = model@model_id)
  
  return(explainer)
  
}


####################################################################
#' DALEX Local
#' 
#' DALEX function for local interpretations
#' 
#' @family Interpretability
#' @param explainer Object. Result from dalex_explainer function
#' @param observation Data.frame. If you want to use an observation
#' that was not in the original explainer function, add here. Else, use row
#' @param row Dataframe. Row number from the data.frame used in explainer
#' @param plot Boolean. Do you wish to see the results plot?
#' @param print Boolean. Do you wish to see the results table?
#' @param alarm Boolean. Ping an alarm when ready! Needs beepr installed
#' @export
dalex_local <- function(explainer, observation = NA, row = 1, 
                        plot = TRUE, print = TRUE, alarm = TRUE) {
  
  try_require("DALEX")
  start <- Sys.time()
  
  if (is.na(observation)) {
    observation <- explainer$data[row,] 
  } else {
    observation <- select(observation, colnames(explainer$data))
  }
  
  breakdown <- prediction_breakdown(explainer, observation = observation)
  
  if (plot)
    plot(breakdown) + theme_lares2(legend = "none")
  
  if (print)
    print(breakdown[1:10, 1:5])
  
  if (alarm) {
    try_require("beepr", stop = FALSE)
    beep() 
  }
  
  aux <- round(difftime(Sys.time(), start, units = "secs"), 2)
  message(paste(Sys.time(), "| Duration:", aux, "s"))
  
  return(breakdown)
  
}


####################################################################
#' DALEX Residuals
#' 
#' DALEX function for residuals
#' 
#' @family Interpretability
#' @param explainer Object. Result from dalex_explainer function
#' @export
dalex_residuals <- function(explainer) {
  
  try_require("DALEX")
  
  resids <- model_performance(explainer)
  
  p1 <- plot(resids)
  p2 <- plot(resids, geom = "boxplot")
  
  grid.arrange(p1, p2, nrow = 2)
  
}


####################################################################
#' DALEX Partial Dependency Plots (PDP)
#' 
#' DALEX function for creating Partial Dependency Plots and study
#' variable's responses vs independent vector.
#' 
#' @family Interpretability
#' @param explainer Object. Result from dalex_explainer function
#' @param y Character. Which character do you wish to study?
#' @param force_class Character. If you wish to force a class on your 
#' y, which one do you need?
#' @param alarm Boolean. Ping an alarm when ready! Needs beepr installed
#' @export
dalex_variable <- function(explainer, y, force_class = NA, alarm = TRUE) {
  
  try_require("DALEX")
  try_require("factorMerger")
  start <- Sys.time()
  
  label <- explainer$label
  explainer$label <- NULL
  classes <- c('factor','numeric')
  if (force_class %in% classes) {
    class(explainer$data[[y]]) <- force_class
    message("Change class to ", force_class)
  } else {
    if (!is.na(force_class))
      stop("Try using force_class: ", vector2text(classes))
  }
  
  
  # Plot
  if (is.numeric(explainer$data[[y]])) {
    try_require("ingredients")
    message(paste0(
      ">>> Calculating and plotting ", y, "'s response... this might take some time!"))
    aux <- partial_dependency(explainer, variables = y)
    message("Done")
    p <- plot(aux) + 
      labs(title = paste("Partial Dependency Plot (PDP):", y), 
           subtitle = "model", x = NULL, y = "Average Prediction") +
      theme_lares2(pal = 2, legend = "none")
  } else {
    try_require("pdp")
    aux <- variable_response(explainer, y, type = "pdp")
    p <- plot(aux) + 
      labs(title = paste("Partial Dependency Plot (PDP):", y), subtitle = label) +
      theme_lares2(pal = 2, bg_colour = "white", legend = "none") +
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())
  }
  
  pdp <- list(pdp = aux, plot = p, y = y)

  aux <- round(difftime(Sys.time(), start, units = "secs"), 2)
  message(paste(Sys.time(), "| Duration:", aux, "s"))
  
  if (alarm) {
    try_require("beepr", stop = FALSE)
    beep() 
  }
  
  return(pdp)
  
}
