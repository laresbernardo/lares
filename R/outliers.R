####################################################################
#' Outliers: Z-score method
#' 
#' Z-score, also called a standard score, of an observation is a 
#' distance from the population center measured in number of normalization 
#' units. The default choice for center is sample mean and for 
#' normalization unit is standard deviation. Values are considered 
#' outliers based on z-score if its absolute value of 
#' default z-score is higher then the threshold (popular choice is 3).
#'
#' @family Outliers
#' @param x Numeric. Distribution
#' @param thresh Numeric. Z-Score threshold for n standard deviations.
#' @param mad Boolean. Use median absolute deviation instead?
#' @export
outlier_zscore <- function(x, thresh = 3, mad = FALSE) {
  if (mad == FALSE)
    !abs(x - mean(x, na.rm = TRUE)) <= thresh * sd(x, na.rm = TRUE)
  else
    !abs(x - median(x, na.rm = TRUE)) <= thresh * mad(x, na.rm = TRUE)
}

####################################################################
#' Outliers: Tukey’s fences
#' 
#' Tukey’s fences is a technique used in box plots. The non-outlier 
#' range is defined with \code{[Q1−k(Q3−Q1), Q3+k(Q3−Q1)]}, where Q1 and Q3 
#' are the lower and upper quartiles respectively, k - some non-negative 
#' constant (popular choice is 1.5). A value is an outlier based 
#' on Tukey’s fences when its value does not lie in non-outlier range.
#'
#' @family Outliers
#' @param x Numeric. Distribution
#' @param k Positive Numeric. K-multiplier
#' @export
outlier_turkey <- function(x, k = 1.5) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(quar)
  !((quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr))
}
