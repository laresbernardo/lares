####################################################################
#' Outliers: Winsorize
#'
#' Winsorizing a vector means that a predefined quantum of the smallest
#' and/or the largest values are replaced by less extreme values.
#' Thereby the substitute values are the most extreme retained values.
#'
#' @family Outliers
#' @param x Numeric vector. Distribution to be winsorized.
#' @param thresh Numeric vector. Lower and upper quantiles thresholds.
#' Set values within [0,1].
#' @param na.rm Boolean. Should \code{NA} be omitted to calculate the quantiles?
#' Note that \code{NA} in \code{x} are preserved and left unchanged anyway.
#' @return Numeric vector transformed.
#' @export
winsorize <- function(x, thresh = c(0.05, 0.95), na.rm = FALSE) {
  if (length(thresh) != 2 | any(abs(thresh) > 1)) {
    stop("thresh: pass a valid numeric vector of length 2 and values within [0, 1]")
  }
  cut_point_bottom <- quantile(x, thresh[1], na.rm = na.rm)
  cut_point_top <- quantile(x, thresh[2], na.rm = na.rm)
  j <- which(x <= cut_point_bottom)
  x[j] <- cut_point_bottom
  i <- which(x >= cut_point_top)
  x[i] <- cut_point_top
  attr(x, "thresh") <- thresh
  return(x)
}

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
#' @return data.frame. Each row is an \code{x} observation with its
#' respective std/mean or mad/med calculations depending on \code{mad} input.
#' @export
outlier_zscore <- function(x, thresh = 3, mad = FALSE) {
  if (mad == FALSE) {
    calc <- mean(x, na.rm = TRUE)
    std <- sd(x, na.rm = TRUE)
    z <- thresh * std
    ret <- !abs(x - calc) <= z
    attr(ret, "values") <- data.frame(z = z, std = std, mean = calc)
  } else {
    calc <- median(x, na.rm = TRUE)
    mad <- mad(x, na.rm = TRUE)
    z <- thresh * mad
    ret <- !abs(x - calc) <= z
    attr(ret, "values") <- data.frame(z = z, mad = mad, med = calc)
  }
  return(ret)
}

####################################################################
#' Outliers: Z-score method plot
#'
#' Test several Z-score thresholds to visualize outliers. Tidyverse
#' friendly.
#'
#' @family Outliers
#' @param df Dataframe.
#' @param var Numeric variable.
#' @param group Categorical variable. Grouping variable.
#' @param thresh Numeric vector. Z-Score threshold for n standard deviations.
#' @param top Integer. Show only n most frequent categorical values when
#' using the \code{group} argument.
#' @return ggplot2 object
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dft) # Titanic dataset
#' outlier_zscore_plot(dft, Fare)
#' p <- outlier_zscore_plot(dft, Fare, Pclass, thresh = c(3, 5))
#' plot(p)
#' attr(p, "z_values")
#' head(attr(p, "labels"))
#' @export
outlier_zscore_plot <- function(df, var, group = NULL,
                                thresh = c(2, 3, 5),
                                top = 5) {
  var <- enquo(var)
  group <- enquo(group)
  grouped <- !rlang::quo_is_null(group)

  df <- filter(df, !is.na(!!var))
  original_n <- filtered_n <- nrow(df)

  if (grouped) {
    if (!is.na(top)) {
      f <- freqs(df, !!group) %>% head(top)
      use <- unlist(f[, as_label(group)])
      df <- filter(df, !!group %in% use)
      filtered_n <- nrow(df)
    }
    df <- group_by(df, !!group)
  }

  df <- df %>%
    mutate(
      outlier_std = sd(!!var),
      outlier_mean = mean(!!var),
      outlier_group = ifelse(grouped, !!group, "Across all observations")
    )

  zs <- ref <- centre <- NULL
  for (i in thresh) {
    aux <- df %>%
      group_by(.data$outlier_group) %>%
      mutate(outlier = outlier_zscore(!!var, i))
    df[, paste0("Z-", i)] <- aux$outlier
    zs <- rbind(zs, c(i, unique(aux$outlier_std) * i))
  }

  df <- ungroup(df)
  temp <- select(df, starts_with("outlier_"), !!var, one_of(paste0("Z-", thresh))) %>%
    replaceall(c(TRUE, FALSE), c("purple", "#F79747"))

  subtitle <- sprintf("Variable: %s", as_label(var))
  if (grouped) subtitle <- paste(subtitle, "| Grouped by:", as_label(group))

  p <- ggplot(temp) +
    facet_grid(outlier_group ~ ., scales = "free") +
    lapply(thresh, function(t) {
      lapply(select(temp, one_of(paste0("Z-", t))), function(x) {
        geom_jitter(aes(
          x = paste0(
            "Z-", str_pad(t, nchar(max(thresh)), pad = "0"),
            sprintf("\n(%s)", sum(x == "purple"))
          ),
          colour = x, y = !!var
        ), width = .2, alpha = 0.7)
      })
    }) +
    scale_color_identity() +
    geom_hline(aes(yintercept = .data$outlier_mean), linetype = "dashed", alpha = 0.7) +
    labs(
      title = "Outliers thresholds test (Z-Scores)",
      subtitle = subtitle, x = NULL
    ) +
    theme_lares()
  if (original_n != filtered_n) {
    p <- p + labs(caption = sprintf(
      "Showing only the %s most frequent grouping categories", top
    ))
  }

  attr(p, "labels") <- select(df, !!var, one_of(paste0("Z-", thresh)), starts_with("outlier_"))
  attr(p, "z_values") <- zs
  return(p)
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
#' @param k Positive Numeric. K-multiplier.
#' @return Boolean vector detecting outliers.
#' @export
outlier_turkey <- function(x, k = 1.5) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(quar)
  !((quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr))
}
