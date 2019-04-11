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
#' @param start_clean Boolean. Erase everything in the current h2o 
#' instance before we start to train models?
#' @param alarm Boolean. Ping an alarm when ready!
#' @param save Boolean. Do you wish to save/export results into your 
#' working directory?
#' @param subdir Character. In which directory do you wish to save 
#' the results? Working directory as default.
#' @param plot Boolean. Do you want to plot the results with 
#' mplot_full function?
#' @param project Character. Your project's name
#' @export
h2o_automl <- function(df, 
                       train_test = NA,
                       split = 0.7,
                       seed = 0,
                       thresh = 5,
                       max_time = 5*60,
                       max_models = 25,
                       start_clean = TRUE,
                       alarm = TRUE,
                       save = FALSE,
                       subdir = NA,
                       plot = FALSE,
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
                         training_frame = as.h2o(train),
                         leaderboard_frame = as.h2o(test),
                         max_runtime_secs = max_time,
                         max_models = max_models,
                         exclude_algos = c("StackedEnsemble","DeepLearning"),
                         nfolds = 5, 
                         seed = seed)
  message(paste("Succesfully trained", nrow(aml@leaderboard), "models:"))
  print(aml@leaderboard[,1:3])
  flow <- "http://localhost:54321/flow/index.html"
  message("Check results in H2O Flow's nice interface: ", flow)
  
  # Select model (Best one by default)
  m <- h2o.getModel(as.vector(aml@leaderboard$model_id[1]))  
  
  # Calculations and variables
  scores <- h2o_predict_model(test, m)
  #scores_df <- as.vector(h2o_predict_model(df, m)[,1])
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
      # Binaries
      scores <- scores[,2]
    } else {
      # More than 2 cateogies
      scores <- scores[,1]
    }
    results <- list(
      project = project,
      model = m,
      scores_test = data.frame(
        tag = as.vector(test$tag),
        score = scores),
      metrics = NA,
      datasets = list(test = test, train = train),
      parameters = m@parameters,
      importance = imp,
      auc_test = NA, #h2o.auc(m, valid=TRUE)
      errors_test = NA,
      model_name = as.vector(m@model_id),
      algorithm = m@algorithm,
      leaderboard = aml@leaderboard,
      scoring_history = data.frame(m@model$scoring_history),
      seed = seed)
    
    results$metrics <- model_metrics(
      tag = results$scores_test$tag, 
      score = results$scores_test$score,
      plots = TRUE)
    
    if (length(unique(train$tag)) == 2) {
      results$errors_test <- errors(
        tag = results$scores_test$tag, 
        score = results$scores_test$score) 
      results$auc_test <- pROC::roc(
        results$scores_test$tag, 
        results$scores_test$score, ci=T)
      
    } else {
      results$logloss_test <- NULL
      results$errors_test <- NULL
      results$auc_test <- NULL
    }
    
  } 
  
  # REGRESION MODELS
  if (type == "Regression") {
    results <- list(
      project = project,
      model = m,
      scores_test = data.frame(
        index = c(1:nrow(test)),
        tag = as.vector(test$tag),
        score = as.vector(scores$predict)),
      metrics = NULL,
      scoring_history = data.frame(m@model$scoring_history),
      datasets = list(test = test, train = train),
      parameters = m@parameters,
      importance = imp,
      rmse = h2o::h2o.rmse(m),
      model_name = as.vector(m@model_id),
      algorithm = m@algorithm,
      leaderboard = aml@leaderboard
    )
    
    results$metrics <- model_metrics(
      tag = results$scores_test$tag, 
      score = results$scores_test$score,
      plots = TRUE)
  }
  
  message(paste0("Training duration: ", round(difftime(Sys.time(), start, units="secs"), 2), "s"))
  
  if (save) {
    export_results(results, subdir = subdir)
    message("Results and model files exported succesfully!")
  }
  
  if (plot) {
    mplot_full(tag = results$scores_test$tag,
               score = results$scores_test$score,
               subtitle = results$project,
               model_name = results$model_name)
  }
  
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
    first <- ifelse(length(unique(results$scores_test$tag)) > 6,
                    signif(results$errors_test$rmse, 4),
                    round(100*results$metrics$metrics$AUC, 2))
    subdirname <- paste0(first, "-", results$model_name)  
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
#' Calculate R Squared
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
#' Calculate Adjusted R Squared
#' 
#' This function lets the user calculate adjusted r squared
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
rsqa <- function(tag, score){ 
  fit <- lm(score ~ tag)
  signif(summary(fit)$adjusted.r.squared, 4)
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
    mse = mse(tag, score),
    mape = mape(tag, score),
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
#' Classification Model Metrics
#' 
#' This function lets the user get a confusion matrix and accuracy, and 
#' for for binary classification models: AUC, Precision, Sensitivity, and
#' Specificity.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param thresh Numeric. Value which splits the results for the 
#' confusion matrix.
#' @param plots Boolean. Include plots?
#' @param subtitle Character. Subtitle for plots
#' @export
model_metrics <- function(tag, score, thresh = 0.5, 
                          plots = TRUE, subtitle = NA){
  
  metrics <- list()
  type <- ifelse(length(unique(tag)) <= 10, "Classification", "Regression")
  
  if (type == "Classification") {
    
    labels <- sort(unique(as.character(tag)))
    
    if (is.numeric(score)) {
      new <- data.frame(score = score) %>%
        mutate(score = ifelse(score >= thresh, labels[1], labels[2])) %>%
        .$score
    } else {
      new <- score
    }
    
    conf_mat <- table(Real = as.character(tag), 
                      Pred = as.character(new))
    
    metrics[["confusion_matrix"]] <- conf_mat
    
    total <- sum(conf_mat)
    trues <- sum(diag(conf_mat))
    falses <- total - trues
    
    # For Binaries
    if (length(unique(tag)) == 2) {
      dic <- c("AUC: Area Under the Curve",
               "ACC: Accuracy",
               "PRC: Precision = Positive Predictive Value",
               "TPR: Sensitivity = Recall = Hit rate = True Positive Rate",
               "TNR: Specificity = Selectivity = True Negative Rate",
               "Logloss (Error): Logarithmic loss [Neutral classification: 0.69315]")
      metrics[["dictionary"]] <- dic
      ROC <- pROC::roc(tag, score, ci=T)
      metrics[["metrics"]] <- data.frame(
        AUC = signif(ROC$auc, 5),
        ACC = signif(trues / total, 5),
        PRC = signif(conf_mat[2,2] / (conf_mat[2,2] + conf_mat[1,2]), 5),
        TPR = signif(conf_mat[2,2] / (conf_mat[2,2] + conf_mat[2,1]), 5),
        TNR = signif(conf_mat[1,1] / (conf_mat[1,1] + conf_mat[1,2]), 5),
        Logloss = loglossBinary(tag, score)
      )
      # ROC CURVE PLOT
      if (plots) {
        plot_roc <- invisible(mplot_roc(tag, score))
        if (!is.na(subtitle)) {
          plot_roc <- plot_roc + labs(subtitle = subtitle)
        }
        metrics[["plot_ROC"]] <- plot_roc
      }

    } else {
      metrics[["ACC"]] <- signif(trues / total, 5)
    }
    
    # CONFUSION MATRIX PLOT
    if (plots) {
      plot_cf <- data.frame(conf_mat) %>%
        mutate(perc = round(100 * Freq / sum(Freq), 2)) %>%
        mutate(label = paste0(formatNum(Freq, 0),"\n", perc,"%")) %>%
        ggplot(aes(
          y = factor(Real, levels = rev(labels)), 
          x = as.character(Pred), 
          fill= Freq, size=Freq, 
          label = label)) +
        geom_tile() + theme_lares2() +
        geom_text(colour="white") + 
        scale_size(range = c(3, 4)) + coord_equal() + 
        guides(fill=FALSE, size=FALSE, colour=FALSE) +
        labs(x="Predicted values", y="Real values",
             title = ifelse(length(unique(tag)) == 2,
                            paste("Confusion Matrix with Threshold =", thresh),
                            paste("Confusion Matrix for", length(unique(tag)), "Categories")),
             subtitle = paste0("Accuracy: ", round(100*(trues / total), 2), "%")) +
        theme(axis.text.x = element_text(angle=30, hjust=0)) +
        scale_x_discrete(position = "top") +
        theme(axis.text.x.bottom = element_blank(), 
              axis.ticks.x.bottom = element_blank(),
              axis.text.y.right = element_blank(),
              axis.ticks.y.right = element_blank()) +
        theme_lares2()
      
      if (!is.na(subtitle)) {
        plot_cf <- plot_cf + labs(subtitle = subtitle)
      }
      metrics[["plot_ConfMat"]] <- plot_cf 
    }
  }
  
  if (type == "Regression") {
    # Needs further improvements
    metrics[["errors"]] <- errors(tag, score)
  }
  return(metrics)
}
