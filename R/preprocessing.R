####################################################################
#' Auto-Preprocessing data for a Machine Learning Model
#'
#' This function helps us preprocess data automatically.
#'
#' @param df Dataframe. Dataframe to format, transform and input
#' @param num2fac Integer. Threshold for unique values on a
#' numerical variable to transform into factor
#' @param impute Boolena. Impute missing values with means?
#' @param ohe Boolean. One hot encoding on variables with 3 or
#' more cateogries?
#' @param scale Boolean. Scale the data (normalized)
#' @param logs Boolean. Automatically calculate log(values) for numerical
#' variables (not binaries)
#' @param print Boolean. Print results summary
#' @export
auto_preprocess <- function(df, num2fac = 7, impute = FALSE, ohe = FALSE,
                            scale = FALSE, logs = FALSE, print = FALSE) {

  require(recipes)
  require(tidyr)
  require(dplyr)
  require(caret)

  # Remove nearly zero variance columns
  NZV <- caret::nearZeroVar(df, freqCut = 99/1)
  df <- select(df, -NZV)

  # Which columns to transform
  df <- df %>% mutate_if(is.character, funs(as.factor(.)))
  num_2_factor_names <- df %>%
    select_if(is.numeric) %>%
    summarise_all(funs(n_distinct(.))) %>%
    tidyr::gather(.) %>%
    arrange(value) %>%
    filter(value <= num2fac) %>%
    pull(key) %>%
    as.character()

  # Create recipe's recipe for imputations
  rec <- recipe(~ ., data = df) %>%
    step_num2factor(num_2_factor_names)

  if (impute == TRUE) {
    rec <- rec %>%
      step_meanimpute(all_numeric()) %>%
      step_modeimpute(all_nominal())
  }

  if (logs == TRUE) {
    rec <- rec %>%
      mutate_if(is.numeric, funs(log(.)))
  }

  if (scale == TRUE) {
    rec <- rec %>%
      step_scale(all_numeric(), -all_outcomes())
  }

  if (ohe == TRUE) {
    rec <- rec %>%
      step_dummy(all_nominal(), -all_outcomes())
  }

  rec <- rec %>% prep(stringsAsFactors = FALSE)

  baker <- function(x) {
    recipes::bake(rec, x)
  }

  output <- list(processed = baker(df),
                 processor = baker)

  if (print == TRUE) {
    print(glimpse(output$processed))
  }

  return(output)

}
