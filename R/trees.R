####################################################################
#' Recursive Partitioning and Regression Trees
#'
#' Fit and plot a rpart model for exploratory purposes using rpart
#' and rpart.plot libraries. Idea from explore library.
#'
#' @family Exploratory
#' @family Visualization
#' @param df Data frame
#' @param target Variable
#' @param max Integer. Maximal depth of the tree
#' @param min Integer. The minimum number of observations that must 
#' exist in a node in order for a split to be attempted
#' @param cp Numeric. Complexity parameter
#' @param size Numeric. Textsize of plot
#' @param ohse Boolean. Auto generate One Hot Smart Encoding?
#' @param plot Boolean. Return a plot? If not, \code{rpart} object
#' @param ... rpart.plot custom parameters
#' @return When \code{plot=TRUE} returns plot; when \code{plot=FALSE}
#' returns \code{rpart} fitted model.
#' @export
tree_var <- function(df, target, max = 3, min = 20, cp = 0, 
                     size = 0.7, ohse = TRUE, plot = TRUE, ...) {
  
  try_require("rpart.plot")
  
  if (ohse) df <- ohse(df, limit = min)
  
  # Check if main variable exists
  target_quo <- enquo(target)
  target_txt <- quo_name(target_quo)[[1]]
  if (!target_txt %in% colnames(df)) {
    message(paste("Not a valid input:", target_txt, "was transformed or does not exist."))
    maybes <- colnames(df)[grepl(target_txt, colnames(df))]
    if (length(maybes) > 0) message(paste("Maybe you meant one of:", vector2text(maybes)))
    stop()
  }
  
  formula_txt <- as.formula(paste(target_txt, "~ ."))
  
  mod <- rpart(formula_txt, data = df, 
               control = rpart.control(maxdepth = max, minsplit = min, cp = cp))
  
  if (plot) {
    if (nrow(mod$frame) > 1) {
      plot <- rpart.plot(
        mod, prefix = "target = ", type = 2, 
        yesno = 2, branch = 0, branch.type = 5, box.palette = "Blues", 
        shadow.col = 0, cex = size, ...)
    } else { 
      plot <- noPlot("Can't grow decision tree")
    }
    return(plot)
  } else {
    return(mod)
  }
}
