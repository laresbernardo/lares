####################################################################
#' Recursive Partitioning and Regression Trees
#'
#' Fit and plot a \code{rpart} model for exploratory purposes using
#' \code{rpart} and \code{rpart.plot} libraries.
#'
#' @family Exploratory
#' @family Visualization
#' @inheritParams h2o_automl
#' @inherit rpart::rpart
#' @inherit rpart::rpart.control
#' @inherit rpart.plot::rpart.plot
#' @param df Data frame
#' @param max Integer. Maximal depth of the tree.
#' @param min Integer. The minimum number of observations that must
#' exist in a node in order for a split to be attempted.
#' @param ohse Boolean. Auto generate One Hot Smart Encoding?
#' @param plot Boolean. Return a plot? If not, \code{rpart} object.
#' @param explain Boolean. Include a brief explanation on the bottom
#' part of the plot.
#' @param title,subtitle Character. Title and subtitle to include in plot.
#' Set to \code{NULL} to ignore.
#' @param ... Additional parameters passed to \code{rpart.plot()}.
#' @return (Invisible) list type 'tree_var' with plot (function), model,
#' predictions, performance metrics, and interpret auxiliary text.
#' @examples
#' data(dft)
#' # Regression Tree
#' tree <- tree_var(dft, Fare, subtitle = "Titanic dataset")
#' tree$plot() # tree plot
#' tree$model # rpart model object
#' tree$performance # metrics
#' # Binary Tree
#' tree_var(dft, Survived_TRUE, explain = FALSE, cex = 0.8)$plot()
#' # Multiclass tree
#' tree_var(dft[, c("Pclass", "Fare", "Age")], Pclass, ohse = FALSE)$plot()
#' @export
tree_var <- function(df, y, type = 2, max = 3, min = 20, cp = 0,
                     ohse = TRUE, plot = TRUE, explain = TRUE,
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
  mod <- rpart(as.formula(paste(target_txt, "~ .")),
    data = df,
    control = rpart.control(maxdepth = max, minsplit = min, cp = cp)
  )

  # Tree's performance
  aux <- df[!is.na(df[, target_txt]), ]
  p <- predict(mod, aux)
  real <- unlist(aux[, target_txt])
  if (!is.vector(p)) {
    multis <- p
    p <- colnames(p)[apply(p, 1, which.max)]
    thresh <- ncol(multis) - 1
  } else {
    multis <- NULL
  }
  performance <- model_metrics(real, p, multis, auto_n = FALSE, plots = FALSE, quiet = TRUE)
  preds <- as_tibble(bind_cols(tag = real, pred = p, multis))

  interpret <- sprintf(paste(
    "Recursive partitioning and regression tree",
    "with a minimum split of %s observations per node and up to depth %s,",
    "with %s complexity parameter (doesn't necessarily increase performance at each step).",
    "On each split or leaf, upper number is 'mean value' and lower number is",
    "'percentage of observations' per level.",
    collapse = ""
  ), min, max, cp)

  if (plot) {
    if (nrow(mod$frame) > 1) {
      bottom_mg <- ifelse(explain, 3.5, 1)
      plot_tree <- function(tree, title, subtitle, explain = TRUE) {
        font <- .font_global(Sys.getenv("LARES_FONT"), when_not = NULL, ...)
        rpart.plot(tree,
          type = type, roundint = FALSE, family = font,
          mar = c(bottom_mg, 1, 2.5, 1), fallen.leaves = FALSE, ...
        )
        if (is.na(title)) {
          mtext(
            side = 3, line = 2.5, at = -0.05, adj = 0, cex = 1.1,
            paste("Decision Tree for", target_txt), family = font
          )
        }
        mtext(side = 3, line = 1.6, at = -0.05, adj = 0, cex = 0.9, subtitle, family = font)
        if (explain) {
          mtext(
            side = 1, line = 3, at = -0.05, adj = 0, cex = 0.6,
            family = font, autoline(interpret, rel = 4.4)
          )
        }
      }
      tree.plot <- function() plot_tree(mod, title, subtitle, explain)
    } else {
      tree.plot <- noPlot("Can't grow decision tree")
    }
  } else {
    plot <- NULL
  }

  ret <- list(
    plot = tree.plot,
    model = mod,
    preds = preds,
    performance = performance,
    interpret = interpret
  )
  attr(ret, "type") <- "tree_var"
  return(invisible(ret))
}

plot.tree_var <- function(x, ...) {
  if (!inherits(x, "tree_var")) {
    stop("Object must be class tree_var")
  }
  if ("plot" %in% names(x)) x$plot()
}

# data(dft)
# tree_var(dft, Survived_TRUE)$plot()
# tree <- tree_var(dft, Fare)
# tree$plot() # tree plot
# tree$model # model
# tree$performance # metrics

# tree_var(dft[,c("Pclass","Fare","Age")], Pclass, ohse = FALSE)$plot()
# dft %>%
#   mutate(weird = Fare >= 26 & Age < 15 & Pclass == 3) %>%
#   filter(!is.na(weird)) %>%
#   freqs(weird)
# x <- tree_var(dft[,c("Pclass","Fare","Age")], Pclass, ohse = FALSE)

# ROC(x$preds$tag, x$preds$pred, x$preds[,3:5])
# tag = x$preds$tag
# score = x$preds$pred
# multis = x$preds[,3:5]
