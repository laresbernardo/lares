#' #' YOUR PROYECTS' TITLE
#' #' URL: https://github.com/laresbernardo/lares
#' #' 
#' #' Further explanations on the project
#' #' Something to declare
#' 
#' ############## 0. Global settings
#' 
#' project <- "My Personal Project"
#' wd <- "/Users/bernardo/Desktop"
#' trainingfile <- "Sample.csv"
#' variable <- "variable"
#' save <- FALSE
#' subdir <- NA
#' seed <- 0
#' 
#' ############## 1. Import libraries and data
#' 
#' library(lares)
#' library(dplyr)
#' library(ggplot2)
#' 
#' setwd(wd)
#' s <- read.csv(trainingfile)
#' s <- rename(s, "tag" = variable)
#' 
#' 
#' ############## 2. Quick Exploratory Data Analysis (EDA):
#' 
#' # Overall panorama:
#' dim(s)
#' head(s)
#' str(s)
#' df_str(s)
#' 
#' # Check for Correlations:
#' corr <- corr_var(s, "tag")
#' 
#' # Study particular intereseting variables:
#' freqs(s, variable, plot = T)
#' distr(s, tag, variable)
#' mplot_lineal(s$tag, s$variable)
#' 
#' 
#' ############## 3. Data transformations
#' 
#' df <- balance_data(s, "tag", rate = 1, seed = seed) %>%
#'   mutate_if(is.numeric, funs(log = log(.+1))) %>%
#'   select(-X, -tag_log, -Year_log, -Month_log)
#' 
#' # Final check:
#' df_str(df, plot = F)
#' 
#' 
#' ############## 4. Modeling
#' 
#' # Train a quick first model
#' results <- h2o_automl(df, max_time = 60, project = project, seed = seed)
#' 
#' # Let's take a look at the results:
#' mplot_importance(results$importance$variable,
#'                  results$importance$importance,
#'                  subtitle = project, save = save, subdir = subdir,
#'                  model_name = results$model_name)
#' mplot_full(tag = results$scores_test$tag,
#'            score = results$scores_test$score,
#'            subtitle = project, save = save, subdir = subdir,
#'            model_name = results$model_name)
#' 
#' # Further study on particular intereseting variables:
#' freqs(s, variable, plot = T, save = save, subdir = subdir)
#' distr(s, tag, variable, save = save, subdir = subdir)
#' mplot_lineal(s$tag, s$variable, save = save, subdir = subdir)
#' 
#' 
#' ############## 5. Export model and results
#' 
#' # Get all files into a folder:
#' if (save == TRUE) { export_results(results, subdir = NA, sample_size = 4) }
#' 
#' # Run another file with predictions:
#' prediction <- as.vector(h2o_predict_model(new_df, results$model)[,1])
