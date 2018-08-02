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
# Loggarithmic Loss Function for Binary Models
# Note: 0.69315 - the classification is neutral; it assigns equal probability to both classes
loglossBinary = function(tag, score, eps = 1e-15) {
  
  if (length(unique(tag)) != 2) {
    stop("Your 'tag' vector is not binary!")
  }
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", length(tag),"rows and score has", length(score))))
  }
  
  if (!is.numeric(tag)) {
    tag <- as.integer(tag) - 1
  }
  
  score <- pmin(pmax(score, eps), 1-eps)
  - (sum(tag * log(score) + (1 - tag) * log(1 - score))) / length(tag)
  
}


####################################################################
# H2O function to run autoML and return a list of usefull results
h2o_automl <- function(df, 
                       train_test = NA,   # Column name with 'test' and 'train' values
                       split = 0.7,       # Test / Train split relation
                       seed = 0,          # For random stuff and reproducibility
                       max_time = 5*60,   # For how long we want to iterate
                       max_models = 25,   # How many models we wish to try
                       export = FALSE,    # Save results into our system
                       project = "Machine Learning Model") {
  require(dplyr)
  require(h2o)
  options(warn=-1)
  
  start <- Sys.time()

  df <- data.frame(df) %>% filter(!is.na(tag))
  type <- ifelse(length(unique(df$tag)) <= 6, "Classifier", "Regression")
  
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
  h2o.init(nthreads = -1, port=54321, min_mem_size="8g")
  #h2o.shutdown()
  h2o.removeAll()
  aml <- h2o.automl(x = setdiff(names(df), "tag"), 
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
  m <- h2o.getModel(as.vector(aml@leaderboard$model_id[1]))  
  
  # Calculations and variables
  scores <- predict(m, as.h2o(test))
  
  # CLASSIFICATION MODELS
  if (type == "Classifier") {
    require(pROC)
    results <- list(
      project = project,
      model = m,
      scores = data.frame(
        index = c(1:nrow(test)),
        tag = as.vector(test$tag),
        score = as.vector(scores[,3]),
        norm_score = lares::normalize(as.vector(scores[,3]))),
      scoring_history = data.frame(m@model$scoring_history),
      datasets = list(test = test, train = train),
      parameters = m@parameters,
      importance = data.frame(h2o.varimp(m)),
      auc_test = NA, #h2o.auc(m, valid=TRUE)
      logloss_test = NA,
      model_name = as.vector(m@model_id),
      algorithm = m@algorithm,
      leaderboard = aml@leaderboard,
      seed = seed)
    roc <- pROC::roc(results$scores$tag, results$scores$score, ci=T)
    results$auc_test <- roc$auc
    if (length(unique(test$tag)) == 2) {
      results$logloss_test <- lares::loglossBinary(tag = results$scores$tag, score = results$scores$score) 
    }
  } 
  
  if (type == "Regression") {
    results <- list(
      project = project,
      model = m,
      scores = data.frame(
        index = c(1:nrow(test)),
        tag = as.vector(test$tag),
        score = as.vector(scores$predict)),
      scoring_history = data.frame(m@model$scoring_history),
      datasets = list(test = test, train = train),
      parameters = m@parameters,
      importance = data.frame(h2o.varimp(m)),
      model_name = as.vector(m@model_id),
      algorithm = m@algorithm,
      leaderboard = aml@leaderboard
    )
  }

  message(paste0("Training duration: ", round(difftime(Sys.time(), start, units="secs"), 2), "s"))
  
  if (export == TRUE) {
    lares::export_results(results)
  }
  
  return(results)
  
}


####################################################################
# Select wich model from the h2o_automl function to use
h2o_selectmodel <- function(results, which_model = 1) {
  
  require(h2o)
  require(pROC)
  
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
    logloss_test = NA,
    model_name = as.vector(m@model_id),
    algorithm = m@algorithm)
  roc <- pROC::roc(output$scores$tag, output$scores$score, ci=T)
  output$auc_test <- roc$auc
  output$logloss_test <- lares::loglossBinary(tag = results$scores$tag, 
                                              score = results$scores$score)
  return(output)
}


####################################################################
# Export RDS, TXT, POJO, MOJO and all results from the h2o_automl function
export_results <- function(results, txt = TRUE, rds = TRUE, pojo = TRUE, mojo = TRUE, subdir = NA) {
  
  require(h2o)
  options(warn=-1)
  
  if (!is.na(subdir)) {
    subdir <- paste0(subdir, "/", round(100*results$auc_test, 2), "-", results$model_name)
  } else {
    subdir <- paste0(round(100*results$auc_test, 2), "-", results$model_name)  
  }
  
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
  
  if (txt == TRUE) {
    tags <- c(as.character(results$datasets$test$tag), as.character(results$datasets$train$tag))
    tags_test <- results$datasets$test
    tags_train <- results$datasets$train
    
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
             "Test LogLoss" = results$logloss_test),
      "Variable Importance" = results$importance,
      "Model Results" = results$model,
      "Models Leaderboard" = results$leaderboard,
      "10 Scoring examples" = cbind(real = results$scores$tag[1:10],
                                    score = results$scores$score[1:10], 
                                    results$datasets$test[1:10, names(results$datasets$test) != "tag"])
    )
    capture.output(results_txt, file = paste0(subdir, "/results.txt"))
  }
}

####################################################################
# Iterate and Search for Best Seed
iter_seeds <- function(tries = 10, data) {
  require(h2o)
  
  seeds <- data.frame()
  
  for (i in 1:tries) {
    iter <- lares::h2o_automl(data, seed = i)
    seeds <- rbind(seeds, cbind(seed = as.integer(i), auc = iter$auc_test))
    seeds <- arrange(seeds, desc(auc))
    print(seeds)
  }
  return(seeds)
}

####################################################################
# Some metrics to measure performance
# Root Mean Squared Error
rmse <- function(tag, score){
  error <- tag - score
  sqrt(mean(error^2))
}
# Mean Absolute Error
mae <- function(tag, score){
  error <- tag - score
  mean(abs(error))
}
# Mean Squared Error
mse <- function(sm){ 
  error <- tag - score
  mean(error^2)
}
