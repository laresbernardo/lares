####################################################################
#' Split a dataframe for training and testing sets
#'
#' This function splits automatically a dataframe into train and test datasets.
#' You can define a seed to get the same results every time, but has a default value.
#' You can prevent it from printing the split counter result.
#'
#' @param df Dataframe to split
#' @param size Split rate
#' @param seed Seed for random split
#' @param print Print summary results
#' @return A list with both datasets, summary, and split rate
#' @export
msplit <- function(df, size = 0.7, seed = NA, print=T) {
  
  if (!is.na(seed)) {
    set.seed(seed)
  }
  
  df <- data.frame(df)
  
  ind <- sample(seq_len(nrow(df)), size = floor(size * nrow(df)))
  train <- df[ind, ]
  test <- df[-ind, ]
  
  train_size <- dim(train)
  test_size <- dim(test)
  summary <- rbind(train_size, test_size)[,1]
  if (print == TRUE) {
    print(summary) 
  }
  
  sets <- list(train=train, test=test, summary=summary, split_size=size)
  
  return(sets)
  
}


####################################################################
#' Loggarithmic Loss Function for Binary Models
#'
#' This function calculates Loggarithmic Loss for Binary Models
#' You can define a seed to get the same results every time, but has a default value.
#' You can prevent it from printing the split counter result.
#'
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param eps Numeric. Epsilon value
#' @export
loglossBinary <- function(tag, score, eps = 1e-15) {
  
  # Note: 0.69315 - the classification is neutral; it assigns equal probability to both classes
  
  if (length(unique(tag)) != 2) {
    stop("Your 'tag' vector is not binary!")
  }
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", 
                       length(tag),
                       "rows and score has", 
                       length(score)))
    )
  }
  
  if (!is.numeric(tag)) {
    tag <- as.integer(tag) - 1
  }
  
  score <- pmin(pmax(score, eps), 1-eps)
  - (sum(tag * log(score) + (1 - tag) * log(1 - score))) / length(tag)
  
}


####################################################################
#' Automated H2O's AutoML
#'
#' This function lets the user create a robust and fast model, using 
#' H2O's AutoML function. The result is a list with the best model, 
#' its parameters, datasets, performance metrics, variables 
#' importances, and others. 
#'
#' @param df Dataframe. Dataframe containing all your data, including 
#' the independent variable labeled as 'tag'
#' @param train_test Character. If needed, df's column name with 'test' 
#' and 'train' values to split
#' @param split Numeric. Value between 0 and 1 to split as train/test 
#' datasets. Value is for training set.
#' @param seed Numeric. Seed for random stuff and reproducibility
#' @param thresh Integer. Threshold for selecting binary or regression 
#' models: this number is the threshold of unique values we should 
#' have in 'tag' (more than: regression; less than: classification)
#' @param max_time Numeric. Max seconds you wish for the function 
#' to iterate
#' @param max_models Numeric. Max models you wish for the function 
#' to create
#' @param alarm Boolean. Ping an alarm when ready!
#' @param export Boolean. Do you wish to save results into your 
#' working directory?
#' @param plot Boolean. Do you want to plot the results with 
#' lares::mplot_full function?
#' @param project Character. Your project's name
#' @export
h2o_automl <- function(df, 
                       train_test = NA,
                       split = 0.7,
                       seed = 0,
                       thresh = 5,
                       max_time = 5*60,
                       max_models = 25,
                       alarm = TRUE,
                       export = FALSE,
                       plot = FALSE,
                       project = "Machine Learning Model") {
  # require(dplyr)
  # require(h2o)
  # require(beepr)
  # require(pROC)
  options(warn=-1)
  
  start <- Sys.time()
  message(paste(start,"| Started process..."))
  
  df <- data.frame(df) %>% filter(!is.na(tag)) %>% mutate_if(is.character, as.factor)
  type <- ifelse(length(unique(df$tag)) <= as.integer(thresh), "Classifier", "Regression")
  
  ####### Validations to proceed #######
  if(!"tag" %in% colnames(df)){
    stop("You should have a 'tag' column in your data.frame!")
  }
  
  ####### Split datasets for training and testing #######
  if (is.na(train_test)) {
    splits <- lares::msplit(df, size = split, seed = seed)
    train <- splits$train
    test <- splits$test
    if (type == "Classifier") {
      print(table(train$tag)) 
      train$tag <- as.factor(as.character(train$tag))
    }
    if (type == "Regression") {
      print(summary(train$tag)) 
    }
  } else {
    # If we already have a default split for train and test (train_test)
    if ((!unique(train_test) %in% c('train', 'test')) & (length(unique(train_test)) != 2)) {
      stop("Your train_test column should have 'train' and 'test' values only!")
    }
    train <- df %>% filter(train_test == "train")
    test <- df %>% filter(train_test == "test")
    test$tag <- NULL
    print(table(train_test))
  }
  
  
  ####### Train model #######
  h2o::h2o.init(nthreads = -1, port=54321, min_mem_size="8g")
  #h2o.shutdown()
  h2o::h2o.removeAll()
  aml <- h2o::h2o.automl(x = setdiff(names(df), "tag"), 
                         y = "tag",
                         training_frame = as.h2o(train),
                         leaderboard_frame = as.h2o(test),
                         max_runtime_secs = max_time,
                         max_models = max_models,
                         exclude_algos = c("StackedEnsemble","DeepLearning"),
                         nfolds = 5, 
                         seed = seed)
  print(aml@leaderboard[,1:3])
  
  # Select model (Best one by default)
  m <- h2o::h2o.getModel(as.vector(aml@leaderboard$model_id[1]))  
  
  # Calculations and variables
  scores <- predict(m, as.h2o(test))
  scores_df <- as.vector(predict(m, as.h2o(df))[,1])
  imp <- data.frame(h2o.varimp(m)) %>%
  {if ("names" %in% colnames(.)) 
    dplyr::rename(., "variable" = "names", "importance" = "coefficients") else .
  } %>%
  {if ("percentage" %in% colnames(.)) 
    dplyr::rename(., "importance" = "percentage") else .
  }
  
  # CLASSIFICATION MODELS
  if (type == "Classifier") {
    results <- list(
      project = project,
      model = m,
      scores_test = data.frame(
        index = c(1:nrow(test)),
        tag = as.vector(test$tag),
        score = as.vector(scores[,3]),
        norm_score = lares::normalize(as.vector(scores[,3]))),
      scores_df = scores_df,
      scoring_history = data.frame(m@model$scoring_history),
      datasets = list(test = test, train = train),
      parameters = m@parameters,
      importance = imp,
      auc_test = NA, #h2o.auc(m, valid=TRUE)
      errors_test = NA,
      logloss_test = NA,
      model_name = as.vector(m@model_id),
      algorithm = m@algorithm,
      leaderboard = aml@leaderboard,
      seed = seed)
    roc <- pROC::roc(results$scores_test$tag, results$scores_test$score, ci=T)
    results$auc_test <- roc$auc
    if (length(unique(test$tag)) == 2) {
      results$errors_test <- errors(tag = results$scores_test$tag, 
                                    score = results$scores_test$score) 
      results$logloss_test <- lares::loglossBinary(tag = results$scores_test$tag, score = results$scores_test$score) 
    }
  } 
  
  if (type == "Regression") {
    results <- list(
      project = project,
      model = m,
      scores_test = data.frame(
        index = c(1:nrow(test)),
        tag = as.vector(test$tag),
        score = as.vector(scores$predict)),
      scores_df = scores_df,
      scoring_history = data.frame(m@model$scoring_history),
      datasets = list(test = test, train = train),
      parameters = m@parameters,
      importance = imp,
      rmse = h2o::h2o.rmse(m),
      model_name = as.vector(m@model_id),
      algorithm = m@algorithm,
      leaderboard = aml@leaderboard
    )
  }
  
  message(paste0("Training duration: ", round(difftime(Sys.time(), start, units="secs"), 2), "s"))
  
  if (export == TRUE) {
    lares::export_results(results)
  }
  
  if (plot == TRUE) {
    lares::mplot_full(tag = results$scores_test$tag,
                      score = results$scores_test$score,
                      subtitle = results$project,
                      model_name = results$model_name)
  }
  
  if (alarm == TRUE) {
    beepr::beep()
  }
  
  return(results)
  
}


####################################################################
#' Select Model from h2o_automl's Leaderboard
#' 
#' Select wich model from the h2o_automl function to use
#' 
#' @param results Object. h2o_automl output
#' @param which_model Integer. Which model from the leaderboard you wish to use?
#' @export
h2o_selectmodel <- function(results, which_model = 1) {
  
  # require(h2o)
  # require(pROC)
  
  # Select model (Best one by default)
  m <- h2o.getModel(as.vector(results$leaderboard$model_id[which_model]))  
  
  # Calculations and variables
  scores <- predict(m, as.h2o(results$datasets$test))
  output <- list(
    project = results$project,
    model = m,
    scores = data.frame(
      index = c(1:nrow(results$datasets$test)),
      tag = as.vector(results$datasets$test$tag),
      score = as.vector(scores[,3]),
      norm_score = lares::normalize(as.vector(scores[,3]))),
    importance = data.frame(h2o.varimp(m)),
    auc_test = NA,
    errors_test = NA,
    logloss_test = NA,
    model_name = as.vector(m@model_id),
    algorithm = m@algorithm)
  roc <- pROC::roc(output$scores$tag, output$scores$score, ci=T)
  output$auc_test <- roc$auc
  output$errors_test <- lares::errors(tag = results$scores_test$tag, 
                                      score = results$scores_test$score)
  output$logloss_test <- lares::loglossBinary(tag = results$scores_test$tag, 
                                              score = results$scores_test$score)
  return(output)
}


####################################################################
#' Export h2o_automl's Results
#' 
#' Export RDS, TXT, POJO, MOJO and all results from h2o_automl
#' 
#' @param results Object. h2o_automl output
#' @param txt Boolean. Do you wish to export the txt results?
#' @param rds Boolean. Do you wish to export the RDS results?
#' @param binary Boolean. Do you wish to export the Binary model?
#' @param pojo Boolean. Do you wish to export the POJO model?
#' @param mojo Boolean. Do you wish to export the MOJO model?
#' @param sample_size Integer. How many example rows do you want to show?
#' @param subdir Character. In which directory do you wish to save the results?
#' @export
export_results <- function(results, 
                           txt = TRUE, 
                           rds = TRUE, 
                           binary = TRUE,
                           pojo = TRUE, 
                           mojo = TRUE, 
                           sample_size = 10,
                           subdir = NA) {
  
  # require(h2o)
  options(warn=-1)
  
  # We create a directory to save all our results
  first <- paste0(round(100*results$auc_test, 2), signif(results$rmse, 4))
  subdirname <- paste0(first, "_", results$model_name)  
  if (!is.na(subdir)) {
    subdir <- paste0(subdir, "/", subdirname)
  } else {
    subdir <- subdirname
  }
  dir.create(subdir)
  
  # Export Results List
  if (rds == TRUE) {
    saveRDS(results, file=paste0(subdir, "/results.rds")) 
  }
  
  # Export Model as POJO & MOJO for Production
  if (pojo == TRUE) {
    h2o.download_pojo(results$model, path=subdir)  
  }
  if (mojo == TRUE) {
    h2o.download_mojo(results$model, path=subdir)  
  }
  
  # Export Binary
  if (mojo == TRUE) {
    h2o.saveModel(results$model, path=subdir, force=TRUE)
  }
  
  if (txt == TRUE) {
    tags <- c(as.character(results$datasets$test$tag), as.character(results$datasets$train$tag))
    tags_test <- results$datasets$test
    tags_train <- results$datasets$train
    random_sample <- sample(1:nrow(results$scores_test), sample_size)
    
    results_txt <- list(
      "Project" = results$project,
      "Model" = results$model_name,
      "Dimensions" = 
        list("Distribution" = table(tags),
             "Test vs Train"=c(paste(round(100*nrow(tags_test)/length(tags)),
                                     round(100*nrow(tags_train)/length(tags)), sep=" / "),
                               paste(nrow(tags_test), nrow(tags_train), sep=" vs. ")),
             "Total"=length(tags)),
      "Metrics" = 
        list("Test AUC" = results$auc_test,
             "Test Errors" = results$errors_test,
             "Test LogLoss" = results$logloss_test),
      "Variable Importance" = results$importance,
      "Model Results" = results$model,
      "Models Leaderboard" = results$leaderboard,
      "10 Scoring examples" = cbind(real = results$scores_test$tag[random_sample],
                                    score = results$scores_test$score[random_sample], 
                                    results$datasets$test[random_sample, names(results$datasets$test) != "tag"])
    )
    capture.output(results_txt, file = paste0(subdir, "/results.txt"))
  }
}


####################################################################
#' Iterate and Search for Best Seed
#' 
#' This functions lets the user iterate and search for best seed. Note that if
#' the results change a lot, you are having a high variance in your data.
#' 
#' @param df Dataframe. Dataframe with all your data you wish to model (see h2o_automl's df)
#' @param tries Integer. How many seed do you wish to try?
#' @export
iter_seeds <- function(df, tries = 10) {
  
  # require(h2o)
  
  seeds <- data.frame()
  
  for (i in 1:tries) {
    iter <- lares::h2o_automl(df, seed = i)
    seeds <- rbind(seeds, cbind(seed = as.integer(i), auc = iter$auc_test))
    seeds <- arrange(seeds, desc(auc))
    print(seeds)
  }
  return(seeds)
}

####################################################################
#' Root Mean Squared Error (RMSE)
#' 
#' This function lets the user calculate Root Mean Squared Error
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
rmse <- function(tag, score){
  error <- tag - score
  sqrt(mean(error^2))
}


####################################################################
#' Mean Absolute Error (MAE)
#' 
#' This function lets the user calculate Mean Absolute Error
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
mae <- function(tag, score){
  error <- tag - score
  mean(abs(error))
}


####################################################################
#' Mean Squared Error (MSE)
#' 
#' This function lets the user calculate Mean Squared Error
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
mse <- function(tag, score){ 
  error <- tag - score
  mean(error^2)
}


####################################################################
#' Mean Absolute Percentage Error (MAPE)
#' 
#' This function lets the user calculate Mean Squared Error
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
mape <- function(tag, score){ 
  error <- (tag - score) / tag
  error <- error[!is.infinite(error)]
  tag <- tag[tag != 0]
  mean(abs(error/tag))
}

####################################################################
#' Calculate Errors
#' 
#' This function lets the user calculate all errors simultaneously.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
errors <- function(tag, score){ 
  data.frame(
    rmse = lares::rmse(tag, score),
    mae = lares::mae(tag, score),
    mse = lares::mse(tag, score),
    mape = lares::mape(tag, score)
  )
}

####################################################################
#' H2O Predict using MOJO file
#' 
#' This function lets the user predict using the h2o .zip file 
#' containing the MOJO files. Note that it works with the files 
#' generated when using the function lares::export_results()
#' 
#' @param df Dataframe. Data to insert into the model
#' @param model_path Character. Relative model_path directory
#' @param sample Integer. How many rows should the function predict?
#' @export
h2o_predict_MOJO <- function(df, model_path, sample = NA){
  
  # require(jsonlite)
  # require(h2o)
  
  zip <- paste0(model_path, "/", gsub(".*-","",model_path), ".zip")
  
  if(!is.na(sample)) {
    json <- toJSON(df[1:sample, ])
  } else {
    json <- toJSON(df)
  }
  
  x <- h2o.predict_json(zip, json)
  
  if (length(x$error) >= 1) {
    stop("Error:", x$error)
  } else {
    score_MOJO <- as.vector(unlist(data.frame(x[,3])[2,])) 
    return(score_MOJO)
  }
}


####################################################################
#' H2O Predict using Binary file
#' 
#' This function lets the user predict using the h2o binary file.
#' Note that it works with the files generated when using the 
#' function lares::export_results(). Recommendation: use the 
#' h2o_predict_MOJO() function when possible - it let's you change
#' h2o's version without problem.
#' 
#' @param df Dataframe. Data to insert into the model
#' @param model_path Character. Relative model_path directory
#' @param sample Integer. How many rows should the function predict?
#' @export
h2o_predict_binary <- function(df, model_path, sample = NA){
  
  # require(jsonlite)
  # require(h2o)
  options(warn=-1)
  
  binary <- paste(model_path, gsub(".*-", "", model_path), sep="/")
  model <- h2o.loadModel(binary)
  
  if(!is.na(sample)) {
    df <- df[1:sample, ]
  }
  
  score_binary <- as.vector(predict(model, as.h2o(df))[,3])
  
  return(score_binary)
  
}


####################################################################
#' H2O Predict using API Service
#' 
#' This function lets the user get the score from an API service
#' 
#' @param df Dataframe/Vector. Data to insert into the model
#' @param api Character. API's URL
#' @export
h2o_predict_API <- function(df, api) {
  
  # require(httr)
  # require(dplyr)
  
  post <- function(df, api) {
    df <- df %>%
      removenacols() %>% 
      select(-contains("tag"))
    x <- POST(
      api, 
      add_headers('Content-Type'='application/json'), 
      body = as.list(df), 
      encode = "json", 
      verbose())
    return(content(x)$probabilityToOne)
  }
  
  batch <- c()
  for (i in 1:nrow(df)) {
    x <- df[i,]
    score <- post(x, api)
    batch <- rbind(batch, score)
  }
  
  return(as.vector(batch))
  
}


####################################################################
#' H2O Predict using H2O Model Object
#' 
#' This function lets the user get scores from a H2O Model Object
#' 
#' @param df Dataframe/Vector. Data to insert into the model
#' @param model H2o Object. Model
#' @export
h2o_predict_model <- function(df, model){
  # require(h2o)
  #model <- h2o.getModel(as.vector(aml@leaderboard$model_id[1]))
  scores <- predict(model, as.h2o(df))
  return(scores)
}
