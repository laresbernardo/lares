####################################################################
#' Plot methods for lares
#' @rdname print
#' @param x \code{h2o_automl} object
#' @param ... Additional parameters
#' @export
plot.h2o_automl <- function(x, ...) {
  if (!inherits(x, 'h2o_automl'))
    stop('Object must be class h2o_automl')
  if ("plots" %in% names(x)) {
    x$plots$dashboard 
  } else {
    message("Nothing to plot: set 'plots = TRUE' when using h2o_automl.") 
  } 
}
