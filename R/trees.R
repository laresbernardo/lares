####################################################################
#' Recursive Partitioning and Regression Trees
#'
#' Fit and plot a \code{rpart} model for exploratory purposes using
#' \code{rpart} and \code{rpart.plot} libraries.
#'
#' @family Exploratory
#' @family Visualization
#' @inheritParams h2o_automl
#' @param df Data frame
#' @param max Integer. Maximal depth of the tree.
#' @param min Integer. The minimum number of observations that must 
#' exist in a node in order for a split to be attempted.
#' @param cp Numeric. Complexity parameter.
#' @param ohse Boolean. Auto generate One Hot Smart Encoding?
#' @param plot Boolean. Return a plot? If not, \code{rpart} object.
#' @param title,subtitle Character. Title and subtitle to include in plot.
#' Set to \code{NULL} to ignore.
#' @param ... Additional parameters passed to \code{model_metrics()}.
#' @return When \code{plot=TRUE} returns plot; when \code{plot=FALSE}
#' returns \code{rpart} fitted model.
#' @examples 
#' \dontrun{
#' data(dft)
#' tree <- tree_var(dft, Fare, subtitle = "Titanic dataset")
#' tree$plot() # tree plot
#' tree$tree # model
#' tree$performance # metrics
#' tree_var(dft, Survived_TRUE)$plot()
#' }
#' @export
tree_var <- function(df, y, max = 3, min = 15, cp = 0, 
                     ohse = TRUE, plot = TRUE,
                     title = NA, subtitle = NULL, ...) {
  
  if (ohse) df <- ohse(df, limit = min)
  
  # Check if main variable exists
  target_quo <- enquo(y)
  target_txt <- quo_name(target_quo)[[1]]
  if (!target_txt %in% colnames(df)) {
    message(paste("Not a valid input:", target_txt, "was transformed or does not exist."))
    maybes <- colnames(df)[grepl(target_txt, colnames(df))]
    if (length(maybes) > 0) message(paste("Maybe you meant one of:", vector2text(maybes)))
    stop()
  }
  
  # Train tree
  mod <- rpart(as.formula(paste(target_txt, "~ .")), data = df,
               control = rpart.control(maxdepth = max, minsplit = min, cp = cp))
  
  # Tree's performance
  aux <- df[!is.na(df[,target_txt]),]
  p <- unlist(predict(mod, aux))
  real <- unlist(aux[,target_txt])
  performance <- model_metrics(real, p, auto_n = FALSE, plots = FALSE,
                               quiet = TRUE, ...)
  
  interpret <- sprintf(paste(
    "Recursive partitioning and regression tree",
    "with a minimum split of %s observations per node and up to depth %s,",
    "with %s complexity parameter (doesn't necessarily increase R-squared at each step).",
    'On each leaf, upper number is "mean value" and lower number is "percentage of observations".',
    collapse = ""), min, max, cp)
  
  if (plot) {
    try_require("rpart.plot")
    if (nrow(mod$frame) > 1) {
      plot_tree <- function(tree, title = title, subtitle = subtitle, caption = interpret) {
        rpart.plot(tree, type = 5, roundint = FALSE)
        if (is.na(title)) mtext(side = 3, line = 3, at = -0.05, adj = 0, cex = 1.1,
              paste("Decision Tree for", target_txt))
        mtext(side = 3, line = 2, at = -0.05, adj = 0, cex = 0.9, subtitle)
        mtext(side = 1, line = 3, at = -0.05, adj = 0, cex = 0.6, caption)
      }
      tree.plot <- function() plot_tree(mod, title, subtitle, autoline(interpret, rel = 4.5))
    } else { 
      plots <- noPlot("Can't grow decision tree")
    }
  } else plot <- NULL
  
  return(invisible(list(
    plot = tree.plot,
    tree = mod,
    performance = performance,
    interpret = interpret)))
  
}

# data(dft)
# tree <- tree_var(dft, Survived_TRUE)
# tree$plot() # tree plot
# tree$tree # model
# tree$performance # metrics
