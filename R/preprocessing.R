####################################################################
#' Auto-Preprocessing data for a Machine Learning Model
#' 
#' This function helps us preprocess data automatically.
#' 
#' @param df Dataframe. Dataframe to format, transform and input
#' @param num2fac Integer. Threshold for unique values on a numerical variable 
#' to transform into factor
#' @param return Character. Select if you wish the preprocessed data 'results' or 
#' recipe 'function' result
#' @export
auto_preprocess <- function(df, num2fac = 7, print = FALSE, return = "results") {
  
  require(recipes)
  require(tidyr)
  require(dplyr)
  
  returns <- c('results','function')
  if (!return %in% returns) {
    stop(paste("Please try one of theses return parameters:",lares::vector2text(returns)))
  }
  
  # Which columns to transform
  string_2_factor_names <- df %>% 
    select_if(is.character) %>% 
    names()
  num_2_factor_names <- df %>% 
    select_if(is.numeric) %>% 
    summarise_all(funs(n_distinct(.))) %>% 
    gather(.) %>% 
    arrange(value) %>% 
    filter(value <= num2fac) %>% 
    pull(key) %>% 
    as.character()
  
  # Create recipe's recipe for imputations
  rec <- recipe(~ ., data = df) %>%
    step_string2factor(string_2_factor_names) %>%
    step_num2factor(num_2_factor_names) %>%
    step_meanimpute(all_numeric()) %>%
    step_modeimpute(all_nominal()) %>%
    prep(stringsAsFactors = FALSE)
  
  if (return == "results") {
    processed <- bake(rec, df) 
    if (print == TRUE) {
      print(glimpse(processed))
    }
    return(processed)    
  }
  
  if (return == "function") {
    return(rec)    
  }
  
}
