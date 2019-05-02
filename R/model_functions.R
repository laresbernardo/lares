####################################################################
#' Split a dataframe for training and testing sets
#'
#' This function splits automatically a dataframe into train and 
#' test datasets. You can define a seed to get the same results 
#' every time, but has a default value. You can prevent it from 
#' printing the split counter result.
#'
#' @param df Dataframe to split
#' @param size Numeric. Split rate value, between 0 and 1. If set to
#' 1, the train and test set will be the same.
#' @param seed Seed for random split
#' @param print Print summary results
#' @return A list with both datasets, summary, and split rate
#' @export
msplit <- function(df, size = 0.7, seed = 0, print=T) {
  
  if (size <= 0 | size > 1) {
    stop("Set size parameter to a value >0 and <=1") 
  }
  
  set.seed(seed)
  df <- data.frame(df)
  
  if (size == 1) {
    train <- test <- df
  }
  
  if (size < 1 & size > 0) {
    ind <- sample(seq_len(nrow(df)), size = floor(size * nrow(df)))
    train <- df[ind, ]
    test <- df[-ind, ] 
  }
  
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
#' This function calculates log loss/cross-entropy loss for binary 
#' models. NOTE: when result is 0.69315, the classification is neutral; 
#' it assigns equal probability to both classes.
#'
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param eps Numeric. Epsilon value
#' @export
loglossBinary <- function(tag, score, eps = 1e-15) {
  
  if (length(unique(tag)) != 2) {
    stop("Your 'tag' vector is not binary!")
  }
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", length(tag),
                       "rows and score has", length(score)))
    )
  }
  
  if (!is.numeric(tag)) {
    tag <- as.integer(tag) - 1
  }
  
  score <- pmax(pmin(score, 1 - eps), eps)
  LogLoss <- -mean(tag * log(score) + (1 - tag) * log(1 - score))
  
  return(LogLoss)
  
}


####################################################################
#' Automated H2O's AutoML
#'
#' This function lets the user create a robust and fast model, using 
#' H2O's AutoML function. The result is a list with the best model, 
#' its parameters, datasets, performance metrics, variables 
#' importances, and other useful metrics.
#' 
#' Full list of algorithms: "DRF" (Distributed Random Forest, including 
#' Random Forest (RF) and Extremely-Randomized Trees (XRT)), "GLM" 
#' (Generalized Linear Model), "XGBoost" (eXtreme Grading Boosting), 
#' "GBM" (Gradient Boosting Machine), "DeepLearning" (Fully-connected 
#' multi-layer artificial neural network) and "StackedEnsemble". 
#'
#' @param df Dataframe. Dataframe containing all your data, including 
#' the independent variable labeled as 'tag'
#' @param train_test Character. If needed, df's column name with 'test' 
#' and 'train' values to split
#' @param split Numeric. Value between 0 and 1 to split as train/test 
#' datasets. Value is for training set.
#' @param weight Column with observation weights. Giving some observation a
#' weight of zero is equivalent to excluding it from the dataset; giving an 
#' observation a relative weight of 2 is equivalent to repeating that 
#' row twice. Negative weights are not allowed.
#' @param balance Boolean. Auto-balance train dataset with under-sampling?
#' @param seed Numeric. Seed for random stuff and reproducibility
#' @param thresh Integer. Threshold for selecting binary or regression 
#' models: this number is the threshold of unique values we should 
#' have in 'tag' (more than: regression; less than: classification)
#' @param max_time Numeric. Max seconds you wish for the function 
#' to iterate
#' @param max_models Numeric. Max models you wish for the function 
#' to create
#' @param start_clean Boolean. Erase everything in the current h2o 
#' instance before we start to train models?
#' @param exclude_algos Vector of character strings. Algorithms to 
#' skip during the model-building phase.
#' @param alarm Boolean. Ping an alarm when ready!
#' @param save Boolean. Do you wish to save/export results into your 
#' working directory?
#' @param subdir Character. In which directory do you wish to save 
#' the results? Working directory as default.
#' @param plots Boolean. Do you want to plot the results metrics?
#' @param project Character. Your project's name
#' @export
h2o_automl <- function(df, 
                       train_test = NA,
                       split = 0.7,
                       weight = NULL,
                       balance = TRUE,
                       seed = 0,
                       thresh = 5,
                       max_time = 5*60,
                       max_models = 25,
                       start_clean = TRUE,
                       exclude_algos = c("StackedEnsemble","DeepLearning"),
                       alarm = TRUE,
                       save = FALSE,
                       subdir = NA,
                       project = "Machine Learning Model") {
  
  options(warn=-1)
  
  start <- Sys.time()
  message(paste(start,"| Started process..."))
  
  df <- data.frame(df) %>% filter(!is.na(tag)) %>% mutate_if(is.character, as.factor)
  type <- ifelse(length(unique(df$tag)) <= as.integer(thresh), "Classifier", "Regression")
  
  message("Model type: ", type)
  if (type == "Classifier") {
    print(data.frame(freqs(df, tag)))
  }
  if (type == "Regression") {
    print(summary(df$tag)) 
  }
  
  ####### Validations to proceed #######
  if(!"tag" %in% colnames(df)){
    stop("You should have a 'tag' column in your data.frame!")
  }
  
  ####### Split datasets for training and testing #######
  if (is.na(train_test)) {
    message("Splitting datasets...")
    splits <- msplit(df, size = split, seed = seed)
    train <- splits$train
    test <- splits$test
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
  # BALANCE TRAINING SET
  if (type == "Classifier" & balance == TRUE) {
    total <- nrow(train)
    cats <- length(unique(train$tag))
    min <- train %>% freqs(tag) %>% .$n %>% min()
    rel <- round(100*min*cats/total, 1)
    train <- train %>% group_by(tag) %>% sample_n(min)
    message(paste0("Training set balanced: ", min, 
                   " observations for each (",cats,") category; using ",
                   rel, "% of training data..."))
  }
  
  ####### Train model #######
  
  quiet(h2o.init(nthreads = -1, port=54321, min_mem_size="8g"))
  if (start_clean) {
    quiet(h2o.removeAll()) 
  } else {
    message("Previous trained models are not being erased. Use 'start_clean' parameter if needed.")
  }
  #h2o.shutdown()
  
  message(paste("Iterating until", max_models, "models or", max_time, "seconds..."))
  aml <- h2o::h2o.automl(x = setdiff(names(df), "tag"), 
                         y = "tag",
                         weights_column = weight,
                         training_frame = as.h2o(train),
                         leaderboard_frame = as.h2o(test),
                         max_runtime_secs = max_time,
                         max_models = max_models,
                         exclude_algos = exclude_algos,
                         nfolds = 5, 
                         seed = seed)
  if (nrow(aml@leaderboard) == 0) {
    stop("No models were trained! Please set max_models to at least 1.")
  } else {
    message(paste("Succesfully trained", nrow(aml@leaderboard), "models:")) 
  }
  print(aml@leaderboard[,1:3])
  flow <- "http://localhost:54321/flow/index.html"
  message("Check results in H2O Flow's nice interface: ", flow)
  
  # Select model (Best one by default)
  m <- h2o.getModel(as.vector(aml@leaderboard$model_id[1]))  
  
  # Calculations and variables
  scores <- quiet(h2o_predict_model(test, m))
  
  # Variables importances
  imp <- data.frame(h2o.varimp(m)) %>%
  {if ("names" %in% colnames(.)) 
    dplyr::rename(., "variable" = "names", "importance" = "coefficients") else .
  } %>%
  {if ("percentage" %in% colnames(.)) 
    dplyr::rename(., "importance" = "percentage") else .
  }
  
  # CLASSIFICATION MODELS
  if (type == "Classifier") {
    if (length(unique(train$tag)) == 2) {
      scores <- select_if(scores, is.numeric) %>% .[,1]
      multis <- NA
    } else {
      colnames(scores)[1] <- "score"
      multis <- scores %>% select(-score)
    }
    
    #crossval <- m@model$cross_validation_metrics_summary[,c(1,2)]
    #max_thresh <- m@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores
    
    results <- list(
      model = m,
      scores_test = data.frame(tag = as.vector(test$tag), scores),
      metrics = NA,
      datasets = list(test = test, train = train),
      parameters = m@parameters,
      importance = imp,
      project = project,
      model_name = as.vector(m@model_id),
      algorithm = m@algorithm,
      leaderboard = aml@leaderboard,
      scoring_history = data.frame(m@model$scoring_history),
      seed = seed)
  } 
  
  # REGRESION MODELS
  if (type == "Regression") {
    results <- list(
      project = project,
      model = m,
      scores_test = data.frame(tag = as.vector(test$tag), score = scores$predict),
      metrics = NA,
      scoring_history = data.frame(m@model$scoring_history),
      datasets = list(test = test, train = train),
      parameters = m@parameters,
      importance = imp,
      model_name = as.vector(m@model_id),
      algorithm = m@algorithm,
      leaderboard = aml@leaderboard
    )
  }
  
  results[["metrics"]] <- model_metrics(
    tag = results$scores_test$tag, 
    score = results$scores_test$score,
    multis = multis,
    plots = TRUE)
  print(results$metrics$metrics)
  
  message(paste0("Training duration: ", round(difftime(Sys.time(), start, units="secs"), 2), "s"))
  
  if (save) {
    export_results(results, subdir = subdir)
    message("Results and model files exported succesfully!")
  }
  
  # if (plots) {
  #   mplot_full(tag = results$scores_test$tag,
  #              score = results$scores_test$score,
  #              subtitle = results$project,
  #              model_name = results$model_name)
  # }
  
  if (alarm) {
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
      norm_score = normalize(as.vector(scores[,3]))),
    importance = data.frame(h2o.varimp(m)),
    auc_test = NA,
    errors_test = NA,
    logloss_test = NA,
    model_name = as.vector(m@model_id),
    algorithm = m@algorithm)
  roc <- pROC::roc(output$scores$tag, output$scores$score, ci=T)
  output$auc_test <- roc$auc
  output$errors_test <- errors(tag = results$scores_test$tag, 
                               score = results$scores_test$score)
  output$logloss_test <- loglossBinary(tag = results$scores_test$tag, 
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
#' @param save Boolean. Do you wish to save/export results?
#' @export
export_results <- function(results, 
                           txt = TRUE, 
                           rds = TRUE, 
                           binary = TRUE,
                           pojo = TRUE, 
                           mojo = TRUE, 
                           sample_size = 10,
                           subdir = NA,
                           save = TRUE) {
  
  if (save) {
    
    options(warn=-1)
    quiet(h2o.init(nthreads = -1, port = 54321, min_mem_size = "8g"))
    
    # We create a directory to save all our results
    if ("plot_ROC" %in% names(results$metrics)) {
      first <- round(100*results$metrics$metrics$AUC, 2)
    } else {
      first <- round(100*results$metrics$metrics$ACC, 2)
    }
    if (length(unique(results$scores_test$tag)) > 6) {
      first <- signif(results$errors_test$rmse, 4)
    }
    
    subdirname <- paste0(first, "-", results$model_name)  
    if (!is.na(subdir)) {
      subdir <- paste0(subdir, "/", subdirname)
    } else {
      subdir <- subdirname
    }
    dir.create(file.path(getwd(), subdir), recursive = T)
    
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
      
      tags <- c(as.character(results$datasets$test$tag), 
                as.character(results$datasets$train$tag))
      tags_test <- results$datasets$test
      tags_train <- results$datasets$train
      random_sample <- sample(1:nrow(results$scores_test), sample_size)
      
      results_txt <- list(
        "Project" = results$project,
        "Model" = results$model_name,
        "Dimensions" = 
          list("Distribution" = table(tags),
               "Test vs Train" = c(paste(round(100*nrow(tags_test)/length(tags)),
                                         round(100*nrow(tags_train)/length(tags)), sep=" / "),
                                   paste(nrow(tags_test), nrow(tags_train), sep=" vs. ")),
               "Total" = length(tags)),
        "Metrics" = model_metrics(results$scores_test$tag, 
                                  results$scores_test$score, plots = FALSE),
        "Variable Importance" = results$importance,
        "Model Results" = results$model,
        "Models Leaderboard" = results$leaderboard,
        "10 Scoring examples" = cbind(
          real = results$scores_test$tag[random_sample],
          score = results$scores_test$score[random_sample], 
          results$datasets$test[random_sample, names(results$datasets$test) != "tag"])
      )
      capture.output(results_txt, file = paste0(subdir, "/results.txt"))
    } 
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
  
  seeds <- data.frame()
  
  for (i in 1:tries) {
    iter <- h2o_automl(df, seed = i)
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
#' R Squared
#' 
#' This function lets the user calculate r squared
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
rsq <- function(tag, score){ 
  fit <- lm(score ~ tag)
  signif(summary(fit)$r.squared, 4)
}

####################################################################
#' Adjusted R Squared
#' 
#' This function lets the user calculate adjusted r squared
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
rsqa <- function(tag, score){ 
  fit <- lm(score ~ tag)
  signif(summary(fit)$adj.r.squared, 4)
}


####################################################################
#' Calculate Errors
#' 
#' This function lets the user calculate all errors and R squared 
#' simultaneously.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
errors <- function(tag, score){ 
  data.frame(
    rmse = rmse(tag, score),
    mae = mae(tag, score),
    mape = mape(tag, score),
    mse = mse(tag, score),
    rsq = rsq(tag, score),
    rsqa = rsqa(tag, score)
  )
}


####################################################################
#' H2O Predict using MOJO file
#' 
#' This function lets the user predict using the h2o .zip file 
#' containing the MOJO files. Note that it works with the files 
#' generated when using the function export_results()
#' 
#' @param df Dataframe. Data to insert into the model
#' @param model_path Character. Relative model_path directory
#' @param sample Integer. How many rows should the function predict?
#' @export
h2o_predict_MOJO <- function(df, model_path, sample = NA){
  
  df <- as.data.frame(df)
  zip <- paste0(model_path, "/", gsub(".*-","",model_path), ".zip")
  
  if(!is.na(sample)) {
    json <- toJSON(df[1:sample, ])
  } else {
    json <- toJSON(df)
  }
  
  quiet(h2o.init(nthreads = -1, port=54321, min_mem_size="8g"))
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
#' function export_results(). Recommendation: use the 
#' h2o_predict_MOJO() function when possible - it let's you change
#' h2o's version without problem.
#' 
#' @param df Dataframe. Data to insert into the model
#' @param model_path Character. Relative model_path directory or zip file
#' @param sample Integer. How many rows should the function predict?
#' @export
h2o_predict_binary <- function(df, model_path, sample = NA){
  
  options(warn=-1)
  quiet(h2o.init(nthreads = -1, port=54321, min_mem_size="8g"))
  
  if (!right(model_path, 4) == ".zip") {
    binary <- paste(model_path, gsub(".*-", "", model_path), sep="/")  
  } else {
    binary <- model_path
  }
  
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
  #model <- h2o.getModel(as.vector(aml@leaderboard$model_id[1]))
  scores <- as.data.frame(predict(model, as.h2o(df)))
  return(scores)
}


####################################################################
#' Calibrate Sampling Scores
#' 
#' This function lets the user calibrate a model's predictions when 
#' under or over sampling methods were applied when training it.
#' 
#' @param score Vector. Probability predictions of the model output
#' @param train Integer. Total row count in the training dataset
#' @param target Integer. Total row count of the target class 
#' in the training dataset
#' @param train_sample Integer. Total row count in the training 
#' dataset after sampling
#' @param target_sample Integer. Total row count of the target 
#' class in the training dataset after sampling
#' @export
calibrate <- function(score, train, target, train_sample, target_sample) {
  score <-
    (score * (target / train) / (target_sample / train_sample)) /
    ((
      (1 - score) * (1 - target / train) / (1 - target_sample / train_sample)
    ) +
      (
        score * (target / train) / (target_sample / train_sample)
      ))
  return(score)
}


####################################################################
#' Cumulative Gain, Lift and Response
#' 
#' This function calculates cumulative gain, lift, and response 
#' values for a predictive score of a specific target. You can use the 
#' mplot_gain() function to custom plot results.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param target Value. Which is your target positive value? If 
#' set to 'auto', the target with largest mean(score) will be 
#' selected. Change the value to overwrite.
#' @param splits Integer. Numer of percentiles to split the data
#' @param plot Boolean. Plot results?
#' @param quiet Boolean. Do not show message for auto target?
#' @export
gain_lift <- function(tag, score, target = "auto", splits = 10, 
                      plot = FALSE, quiet = FALSE) {
  
  if (splits <= 1) {
    stop("You must set more than 1 split")
  }
  
  df <- data.frame(tag = tag, score = score)
  
  if (target == "auto") {
    means <- df %>% group_by(tag) %>% summarise(mean = mean(score))
    target <- means$tag[means$mean == max(means$mean)]
    if (!quiet) {
      message(paste("Target value:", target)) 
    }
  }
  if (!target %in% unique(df$tag)) {
    stop(paste("Your target value", target, "is not valid. Possible other values:", 
               vector2text(unique(tag))))
  }
  
  df <- df %>% mutate(tag = ifelse(tag == target, TRUE, FALSE))
  
  sc <- df %>% arrange(desc(score)) %>%
    mutate(percentile = .bincode(
      score, quantile(score, probs = seq(0, 1, length = splits + 1), include.lowest = TRUE), 
      right = TRUE, include.lowest = TRUE)) %>%
    mutate(percentile = rev(factor(percentile, 1:splits)))
  
  wizard <- sc %>% filter(tag == TRUE) %>% 
    mutate(percentile = sc$percentile[1:length(sc$percentile[sc$tag==TRUE])]) %>%
    group_by(percentile) %>% tally() %>% 
    ungroup() %>% mutate(p = 100 * n/sum(n), pcum = cumsum(p)) %>% 
    select(percentile, pcum) %>% rename(optimal = pcum)
  
  gains <- sc %>% group_by(percentile) %>% 
    summarise(total = n(), target = sum(tag), score = 100 * min(score)) %>%
    left_join(wizard, "percentile") %>% replace(is.na(.), 100) %>% ungroup() %>%
    mutate(gain = 100*cumsum(target)/sum(target),
           random = 100*cumsum(total)/sum(total),
           lift = 100 * (gain/random - 1),
           response = 100 * target/sum(target)) %>%
    select(percentile, random, target, total, gain, optimal, lift, response, score)
  
  if (plot == TRUE) {
    mplot_gain(tag, score, target, splits = 10)
  }
  
  return(gains)
  
}


####################################################################
#' ROC Curves
#' 
#' This function calculates ROC Curves and AUC values with 95\% confidence 
#' range. It also works for multi-categorical models.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param multis Data.frame. Containing columns with each category score 
#' (only used when more than 2 categories coexist)
#' @export
ROC <- function (tag, score, multis = NA) {
  
  # require(pROC)
  # require(dplyr)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  if (!is.numeric(score) & is.na(multis)) {
    score <- as.numeric(score) 
    #stop("You should use the multis parameter to add each category's score")
  }
  
  if (is.na(multis)) {
    roc <- pROC::roc(tag, score, ci=T)
    coords <- data.frame(
      fpr = rev(roc$specificities),
      tpr = rev(roc$sensitivities)) %>%
      mutate(label = "2cats")
    ci <- data.frame(roc$ci, row.names = c("min","AUC","max"))
  } else {
    df <- data.frame(tag = tag, score = score, multis)
    cols <- colnames(df)
    coords <- c(); rocs <- list()
    for (i in 1:(length(cols)-2)) {
      which <- colnames(df)[2+i]
      res <- df[,c(which)]
      if (grepl("p+[[:digit:]]", which)) {
        which <- as.character(gsub("p","", which))
      }
      label <- ifelse(df[,1] == which, which, "other")
      roci <- pROC::roc(label, res, ci = TRUE)
      rocs[[paste(cols[i+2])]] <- roci
      iter <- data.frame(fpr = rev(roci$specificities),
                         tpr = rev(roci$sensitivities),
                         label = paste(round(100*roci$auc,2), which, sep="% | "))
      coords <- rbind(coords, iter)
    }
    ci <- data.frame(lapply(rocs, "[[", "ci")) %>% mutate(mean = rowMeans(.))
    row.names(ci) <- c("min","AUC","max")
  }
  ret <- list(ci = ci, roc = coords)
  if (!is.na(multis)) {
    ret[["rocs"]] <- rocs  
  }
  return(ret)
}


####################################################################
#' Confussion Matrix
#' 
#' This function calculates a Confussion Matrix using crosstab for
#' 2 or more categories. You can either set the score and threshold
#' or the labels you wish to cross with.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param thresh Numeric. Value which splits the results for the 
#' confusion matrix when binary.
#' @export
conf_mat <- function (tag, score, thresh = 0.5) {
  
  df <- data.frame(tag, score)
  
  # About tags
  labels <- df %>% group_by(tag) %>% tally() %>% arrange(desc(n)) %>% .$tag
  df <- df %>% mutate(tag = factor(tag, levels = unique(tag)))
  
  # About scores
  if (is.numeric(df$score) & length(unique(tag)) == 2) {
    df <- df %>% mutate(pred = ifelse(
      score >= thresh, as.character(labels[1]), as.character(labels[2])))
  } else {
    df <- df %>% mutate(pred = score)
  }
  
  # Confussion Matrix
  ret <- df %>% 
    rename(Real = tag, Pred = pred) %>%
    crosstab(Real, Pred, total = FALSE)
  
  return(ret)
}


####################################################################
#' Classification Model Metrics
#' 
#' This function lets the user get a confusion matrix and accuracy, and 
#' for for binary classification models: AUC, Precision, Sensitivity, and
#' Specificity.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param multis Data.frame. Containing columns with each category score 
#' (only used when more than 2 categories coexist)
#' @param thresh Numeric. Value which splits the results for the 
#' confusion matrix.
#' @param plots Boolean. Include plots?
#' @param subtitle Character. Subtitle for plots
#' @export
model_metrics <- function(tag, score, multis = NA, thresh = 0.5, plots = TRUE, subtitle = NA){
  
  metrics <- list()
  type <- ifelse(length(unique(tag)) <= 10, "Classification", "Regression")
  
  if (type == "Classification") {
    
    dic <- c("AUC: Area Under the Curve",
             "ACC: Accuracy",
             "PRC: Precision = Positive Predictive Value",
             "TPR: Sensitivity = Recall = Hit rate = True Positive Rate",
             "TNR: Specificity = Selectivity = True Negative Rate",
             "Logloss (Error): Logarithmic loss [Neutral classification: 0.69315]",
             "Gain: When best n deciles selected, what % of the real target observations are picked?",
             "Lift: When best n deciles selected, how much better than random is?")
    metrics[["dictionary"]] <- dic
    
    labels <- sort(unique(as.character(tag)))
    tag <- as.character(tag)
    
    if (is.numeric(score)) {
      new <- data.frame(score = score) %>%
        mutate(new = ifelse(score >= thresh, labels[1], labels[2])) %>% .$new
    } else {
      new <- score
    }
  
    conf_mat <- table(Real = as.character(tag), 
                      Pred = as.character(new))
    total <- sum(conf_mat)
    trues <- sum(diag(conf_mat))
    falses <- total - trues
    
    # For Binaries
    if (length(labels) == 2) {
      metrics[["confusion_matrix"]] <- conf_mat
      ROC <- pROC::roc(tag, as.numeric(score), ci=T)
      nums <- data.frame(
        AUC = ROC$auc,
        ACC = trues / total,
        PRC = conf_mat[2,2] / (conf_mat[2,2] + conf_mat[1,2]),
        TPR = conf_mat[2,2] / (conf_mat[2,2] + conf_mat[2,1]),
        TNR = conf_mat[1,1] / (conf_mat[1,1] + conf_mat[1,2]))
      metrics[["gain_lift"]] <- gain_lift(tag, score, "auto", quiet = FALSE)
      metrics[["metrics"]] <- signif(nums, 5)
    } else {
      
      # For Multi-Categories
      df <- data.frame(tag, score)
      metrics[["confusion_matrix"]] <- conf_mat(tag, score)
      AUCs <- t(ROC(tag, score, multis)$ci)[,2]
      m <- data.frame(
        AUC = mean(AUCs[1:length(labels)]),
        ACC = trues / total)
      metrics[["metrics"]] <- signif(m, 5)
      nums <- c()
      for (i in 1:length(labels)) {
        tagi <- ifelse(tag == labels[i], 1, 0)
        predi <- as.numeric(ifelse(score == labels[i], 1, 0))
        conf_mati <- table(Real = as.character(tagi), 
                           Pred = as.character(predi))
        total <- sum(conf_mati)
        trues <- sum(diag(conf_mati))
        falses <- total - trues
        numsi <- data.frame(
          tag = labels[i],
          ACC = trues / total,
          PRC = conf_mati[2,2] / (conf_mati[2,2] + conf_mati[1,2]),
          TPR = conf_mati[2,2] / (conf_mati[2,2] + conf_mati[2,1]),
          TNR = conf_mati[1,1] / (conf_mati[1,1] + conf_mati[1,2]))
        nums <- rbind(nums, numsi)
      }
      nums$AUC <- AUCs[1:length(labels)]
      nums <- left_join(freqs(df %>% select(tag), tag), nums, "tag") %>% 
        select(tag, n, p, AUC, everything(), -pcum)
      metrics[["metrics_tags"]] <- nums %>% mutate_if(is.numeric, funs(signif(., 5)))
    }
    
    if (plots == TRUE) {
      plots <- list()
      # CONFUSION MATRIX PLOT
      plots[["conf_matrix"]] <- mplot_conf(tag, score, thresh, subtitle = subtitle) 
      if (length(labels) == 2) {
        # ROC CURVE PLOT
        plot_roc <- invisible(mplot_roc(tag, score, subtitle = subtitle)) 
        # CUMULATIVE GAINS PLOT
        p <- mplot_gain(tag, score, target = "auto", splits = 10, 
                        highlight = "auto", quiet = TRUE)
        plots[["gains"]] <- p
        # CUMULATIVE RESPONSE PLOT
        p <- mplot_response(tag, score, target = "auto", 
                            splits = 10, highlight = "auto", quiet = TRUE)
        plots[["response"]] <- p
      } else {
        # ROC CURVES PLOT
        plot_roc <- invisible(mplot_roc(tag, score, multis, subtitle = subtitle)) 
      }
      plots[["ROC"]] <- plot_roc
      metrics[["plots"]] <- plots
    }
  }
  
  if (type == "Regression") {
    
    dic <- c("RMSE: Root Mean Squared Error",
             "MAE: Mean Average Error",
             "MAPE: Mean Absolute Percentage Error",
             "MSE: Mean Squared Error",
             "RSQ: R Squared",
             "RSQA: Adjusted R Squared")
    metrics[["dictionary"]] <- dic
    
    metrics[["metrics"]] <- errors(tag, score)
  }
  return(metrics)
}
