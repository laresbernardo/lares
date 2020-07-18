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
  if (mad == FALSE) {
    calc <- mean(x, na.rm = TRUE)
    std <- sd(x, na.rm = TRUE)
    z <- thresh * std
    ret <- !abs(x - calc) <= z
    attr(ret, "std") <- std
    attr(ret, "mean") <- calc
  } else {
    calc <- median(x, na.rm = TRUE)
    mad <- mad(x, na.rm = TRUE)
    z <- thresh * mad
    ret <- !abs(x - calc) <= z
    attr(ret, "mad") <- mad
    attr(ret, "median") <- calc
  }
  attr(ret, "zscore") <- z
  return(ret)
}


####################################################################
#' Outliers: Z-score method plot
#' 
#' Test several Z-score thresholds to visualize outliers. Tidyverse
#' friendly.
#'
#' @family Outliers
#' @param df Dataframe
#' @param var Numeric variable
#' @param thresh Numeric vector. Z-Score threshold for n standard deviations.
#' @param mad Boolean. Use median absolute deviation instead?
#' @param plot Boolean. Show plot?
#' @examples 
#' options("lares.font" = NA) # Temporal
#' data(dft) # Titanic dataset
#' x <- outlier_zscore_plot(dft, Fare)
#' x <- outlier_zscore_plot(dft, Age, thresh = 1:4)
#' lapply(x[-1], head)
#' @export
outlier_zscore_plot <- function(df, var, thresh = c(2, 3, 5), 
                                mad = FALSE, plot = TRUE) {
  
  var <- enquo(var)
  name <- as_label(var)
  
  zs <- ref <- c()
  for (i in thresh) {
    aux <- outlier_zscore(df[,name], i, mad = mad)
    df[,paste0("Z-", i)] <- aux
    zs <- c(zs, attr(aux, "zscore"))
  }
  
  centre <- ifelse(!mad, attr(aux, "mean"), attr(aux, "median"))
  std <- ifelse(!mad, attr(aux, "std"), attr(aux, "mad"))
  
  # # For geom_rects
  # temp <- data.frame(thresh, std)
  # aux2 <- lapply(thresh, function(t)
  #     data.frame(x = paste0("Z-", t), xmin = -Inf, xmax = Inf) %>%
  #       mutate(ymin = centre - t * temp$std[temp$thresh == t],
  #              ymax = centre + t * temp$std[temp$thresh == t])
  # )
  
  temp <- select(df, !!var, one_of(paste0("Z-", thresh))) %>%
    replaceall(c(TRUE, FALSE), c("purple", "#F79747")) %>%
    filter(!is.na(!!var))
  
  caption <- sprintf("Using %s absolute deviation (%s)", 
                     ifelse(mad, "median", "mean"),
                     signif(std, 4))
  
  p <- ggplot(temp) +
    lapply(thresh, function(t)
      lapply(select(temp, one_of(paste0("Z-", t))), function(x)
        geom_jitter(aes(
          x = paste0("Z-", str_pad(t, nchar(max(thresh)), pad = "0"),
                     sprintf("\n(%s)", sum(x == "purple"))),
          colour = x, y = !!var))
      )) +
    # lapply(aux2, function(r)
    #   geom_rect(data = r, 
    #             aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = x),
    #             fill = "grey", colour = "black", alpha = 0.1)) +
    scale_color_identity() +
    geom_hline(yintercept = centre, linetype = "dashed", alpha = 0.6) +
    labs(title = "Outliers (Z-Scores)",
         subtitle = sprintf("Variable: %s", name),
         caption = caption, x = NULL) +
    theme_lares(legend = "top")
  
  df <- select(df, !!var, one_of(paste0("Z-", thresh)))
  ret <- list(plot = p, data = df, zs = zs, ref = centre)
  if (plot) plot(p)
  return(invisible(ret))
}

####################################################################
#' Outliers: Tukey’s fences
#' 
#' Tukey’s fences is a technique used in box plots. The non-outlier 
#' range is defined with \code{[Q1-k(Q3-Q1), Q3+k(Q3-Q1)]}, where Q1 and Q3 
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
