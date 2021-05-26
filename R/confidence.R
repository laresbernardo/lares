####################################################################
#' Lower/Upper Confidence Intervals
#' 
#' Calculate lower and upper confidence intervals given a mean,
#' standard deviation, sample size, and confidence level. You may
#' want to use \code{ci_var()} to calculate all values quickly.
#'
#' @family Confidence
#' @param mean Numeric. Mean: \code{mean(var, na.rm = TRUE)}
#' @param ssd Numeric. Standard deviation: \code{sd(var, na.rm = TRUE)}
#' @param n Integer. Amount of observations: \code{n()}
#' @param conf Numeric (0-1). Confidence level.
#' @return Vector with confidence limit value.
#' @examples 
#' ci_lower(100, 5, 10)
#' ci_upper(100, 5, 10)
#' @export
ci_lower <- function(mean, ssd, n, conf = 0.95){
  if (is.na(ssd)[1]) return(NA)
  se <- ssd / sqrt(n) 
  ci <- mean - qt(1 - ((1 - conf) / 2), n - 1) * se
  return(ci)
}
#' @rdname ci_lower
#' @export
ci_upper <- function(mean, ssd, n, conf = 0.95){
  if (is.na(ssd)[1]) return(NA)
  se <- ssd / sqrt(n) 
  ci <- mean + qt(1 - ((1 - conf) / 2), n - 1) * se
  return(ci)
}

####################################################################
#' Confidence Intervals on Dataframe
#'
#' Calculate confidence intervals for a continuous numerical column on
#' a dataframe, given a confidence level. You may also group results 
#' using another variable. Tidyverse friendly.
#'
#' @family Confidence
#' @param df Dataframe
#' @param var Variable name. Must be a numerical column.
#' @param group_var Variable name. Group results by another variable.
#' @param conf Numeric. Confidence level (0-1).
#' @return data.frame mean, standard deviation, counter, upper and lower CIs.
#' @examples 
#' data(dft) # Titanic dataset
#' ci_var(dft, Fare)
#' ci_var(dft, Fare, Pclass)
#' ci_var(dft, Fare, Pclass, conf = 0.99)
#' @export
ci_var <- function(df, var, group_var = NULL, conf = 0.95){
  
  var <- enquo(var)
  group_var <- enquo(group_var)
  
  if (as_label(group_var) != "NULL")
    df <- df %>% group_by(!!group_var)
  
  aux <- df %>%
    summarise(smean = mean(!!var, na.rm = TRUE),
              ssd = sd(!!var, na.rm = TRUE),
              n = n()) %>% 
    mutate(lower_ci = ci_lower(.data$smean, .data$ssd, .data$n, conf),
           upper_ci = ci_upper(.data$smean, .data$ssd, .data$n, conf))
  
  varname <- as_label(var)
  cols <- colnames(aux)
  colnames(aux)[cols == "smean"] <- sprintf("%s_mean", varname)
  colnames(aux)[cols == "ssd"] <- sprintf("%s_sd", varname)
  
  return(as_tibble(aux))
}
