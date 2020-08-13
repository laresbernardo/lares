####################################################################
#' Plot methods for lares
#' @rdname plot
#' @param x \code{h2o_automl} object
#' @param ... Additional parameters
#' @export
print.h2o_automl <- function(x, ...) {
  
  if (!inherits(x, 'h2o_automl'))
    stop('Object must be class h2o_automl')
  
  aux <- list()
  
  aux[["met"]] <- glued(
    "Test metrics: 
{v2t({met}, sep = '\n', quotes = FALSE)}", met = paste(
  "  ",
  names(x$metrics$metrics), "=",
  signif(x$metrics$metrics, 5)))
  
  if ("importance" %in% names(x)) {
    aux[["imp"]] <- glued(
      "Most important variables:
{v2t({imp}, sep = '\n', quotes = FALSE)}", imp = paste(
  "  ",
  x$importance %>% head(5) %>%
    mutate(label = sprintf(
      "%s (%s)", 
      .data$variable, 
      formatNum(100*.data$importance, 1, pos = "%"))) %>%
    pull(.data$label)))
  }
  
  print(glued("
Leader Model: {x$model_name}
Independent Variable: {x$y}
Type: {x$type}
Algorithm: {toupper(x$algorithm)}
Trained: {nrow(x$leaderboard)} models
Split: {round(100*x$split)}% training data (of {nrow(x$datasets$global)} observations)
Seed: {x$seed}

{aux$met}

{aux$imp}"))
  
}
