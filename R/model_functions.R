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
#' multi-layer artificial neural network) and "StackedEnsemble". Read more: 
#' \url{http://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html}
#'
#' @family Machine Learning
#' @param df Dataframe. Dataframe containing all your data, including 
#' the independent variable labeled as 'tag'. If you want to define 
#' which variable should be used instead, use the y parameter.
#' @param y Character. Name of the independent variable
#' @param ignore Character vector. Force columns for the model to ignore
#' @param train_test Character. If needed, df's column name with 'test' 
#' and 'train' values to split
#' @param split Numeric. Value between 0 and 1 to split as train/test 
#' datasets. Value is for training set. Set value to 1 to train will all 
#' available data and test with same data (cross-validation will still be 
#' used when training)
#' @param weight Column with observation weights. Giving some observation a
#' weight of zero is equivalent to excluding it from the dataset; giving an 
#' observation a relative weight of 2 is equivalent to repeating that 
#' row twice. Negative weights are not allowed.
#' @param target Value. Which is your target positive value? If 
#' set to 'auto', the target with largest mean(score) will be 
#' selected. Change the value to overwrite. Only used when binary
#' categorical model.
#' @param balance Boolean. Auto-balance train dataset with under-sampling?
#' @param impute Boolean. Fill NA values with MICE?
#' @param center,scale Boolean. Using the base function scale, do you wish
#' to center and/or scale all numerical values? 
#' @param seed Integer. Set a seed for reproducibility. AutoML can only 
#' guarantee reproducibility if max_models is used because max_time is 
#' resource limited.
#' @param nfolds Integer. Number of folds for k-fold cross-validation of 
#' the models. If set to 0, the test data will be used as validation, and
#' cross-validation amd Stacked Ensembles disableded
#' @param thresh Integer. Threshold for selecting binary or regression 
#' models: this number is the threshold of unique values we should 
#' have in 'tag' (more than: regression; less than: classification)
#' @param max_models,max_time Numeric. Max number of models and seconds 
#' you wish for the function to iterate. Note that max_models guarantees
#' reproducibility and max_time not (because it depends entirely on your
#' machine's computational characteristics)
#' @param start_clean Boolean. Erase everything in the current h2o 
#' instance before we start to train models?
#' @param exclude_algos Vector of character strings. Algorithms to 
#' skip during the model-building phase. Set NULL to use all
#' @param plots Boolean. Create plots objects?
#' @param alarm Boolean. Ping an alarm when ready! Needs beepr installed
#' @param quiet Boolean. Quiet messages, warnings, recommendations?
#' @param save Boolean. Do you wish to save/export results into your 
#' working directory?
#' @param subdir Character. In which directory do you wish to save 
#' the results? Working directory as default.
#' @param project Character. Your project's name
#' @export
h2o_automl <- function(df, y = "tag",
                       ignore = c(),
                       train_test = NA,
                       split = 0.7,
                       weight = NULL,
                       target = "auto",
                       balance = FALSE,
                       impute = FALSE,
                       center = FALSE,
                       scale = FALSE,
                       seed = 0,
                       nfolds = 5,
                       thresh = 5,
                       max_models = 3,
                       max_time = 10*60,
                       start_clean = TRUE,
                       exclude_algos = c("StackedEnsemble","DeepLearning"),
                       plots = TRUE,
                       alarm = TRUE,
                       quiet = FALSE,
                       save = FALSE,
                       subdir = NA,
                       project = "ML Project") {
  
  tic(id = "h2o_automl")
  
  if (!quiet) message(paste(Sys.time(), "| Started process..."))
  
  # INDEPENDENT VARIABLE
  if (!y %in% colnames(df)) {
    stop(paste("You should have a 'tag' column in your data.frame or select",
               "an independent varialbe using the 'y' parameter."))
  }
  colnames(df)[colnames(df) == y] <- "tag"
  df <- data.frame(df) %>% 
    filter(!is.na(tag)) %>%
    mutate_if(is.character, as.factor)
  
  # MISSING VALUES
  m <- missingness(df)
  if (!is.null(m)) {
    m <- mutate(m, label = paste0(variable, " (", missingness, "%)"))
    if (!quiet) {
      top10 <- m %>% ungroup() %>% slice(10)
      which <- vector2text(top10$label)
      if (nrow(m) > 10)
        which <- paste(which, "and", nrow(m) - 10, "more...")
      message(paste0("NOTE: The following variables contain missing observations: ", which,
                     if (!impute & !quiet) ". Consider setting the impute parameter."))
    }
    if (impute) {
      if (!quiet) message(paste(">>> Imputing", sum(m$missing), "missing values..."))
      df <- impute(df, seed = seed, quiet = TRUE)
    }
  }
  
  # ONE HOT SMART ENCODING
  nums <- df_str(df, "names", quiet = TRUE)$nums
  if (length(nums) != ncol(df) & !quiet) 
    message(paste(
      "NOTE: There are", ncol(df) - length(nums), "non-numerical features.",
      "Consider using ohse() for One Hot Smart Encoding before automl if you want to custom your inputs."))
  if (scale | center & length(nums) > 0) {
    new <- data.frame(lapply(df[nums], function(x) scale(x, center = center, scale = scale)))
    colnames(new) <- nums; df[nums] <- new
    msg <- ifelse(scale & center, "scaled and centered", ifelse(scale, "scaled", "centered"))
    if (!quiet) message(paste0("All numerical features (", length(nums), ") were ", msg))
  }
  
  # MODEL TYPE
  cats <- unique(df$tag)
  model_type <- ifelse(length(cats) <= as.integer(thresh), "Classifier", "Regression")
  message("Model type: ", model_type)
  # If y variables is named as one of the categories, prediction values will be a problem
  if (model_type == "Classifier" & y %in% cats) {
    stop(paste("Your y parameter can't be named as any of the labels used.",
               "Please, rename", y, "into a valid column name next such as",
               paste0(y, "_labels for example.")))
  }
  # If target value is not an existing target value
  if (!target %in% cats & length(cats) == 2)
    if (!target %in% c(cats, "auto"))
      stop(paste("Your target value", target, "is not valid.",
                 "Possible other values:", vector2text(cats)))
  # When might seem numeric but is categorical
  if (model_type == "Classifier" & sum(grepl('^[0-9]', cats)) > 0)
    df <- mutate(df, tag = as.factor(as.character(
      ifelse(grepl('^[0-9]', tag), paste0("n_", tag), as.character(tag)))))
  # When is regression should always be numerical
  if (model_type == "Regression")
    df$tag <- as.numeric(df$tag)
  # Show a summary of our tags
  if (model_type == "Classifier" & !quiet) print(data.frame(freqs(df, tag)))
  if (model_type == "Regression" & !quiet) print(summary(df$tag)) 
  
  # SPLIT TRAIN AND TEST DATASETS
  if (is.na(train_test)) {
    if (!quiet) message(">>> Splitting datasets")
    splits <- msplit(df, size = split, seed = seed, print = !quiet)
    train <- splits$train
    test <- splits$test
  } else {
    # If we already have a default split for train and test (train_test)
    if (train_test %in% colnames(df)) {
      colnames(df)[colnames(df) == train_test] <- "train_test"
      if (all(unique(as.character(df$train_test)) %in% c('train', 'test'))) {
        train <- filter(df, train_test == "train")
        test <- filter(df, train_test == "test")
        ignore <- c(ignore, train_test)
        if (!quiet) print(table(df$train_test))
      } else stop("Your train_test column should have 'train' and 'test' values only!") 
    } else stop(paste("There is no column named", train_test))
  }
  if (nrow(train) > 10000)
    message("NOTE: Consider sampling your dataset for faster results")
  
  # BALANCE TRAINING SET
  if (model_type == "Classifier" & balance) {
    total <- nrow(train)
    min <- freqs(train, tag) %>% .$n %>% min(., na.rm = TRUE)
    train <- train %>% group_by(tag) %>% sample_n(min)
    if (!quiet) message(paste0("Training set balanced: ", min, 
                               " observations for each (",length(cats),") category; using ",
                               round(100*nrow(train)/total, 2), "% of training data..."))
  }
  
  # CHECK TRAIN/TEST VALUES
  if (model_type == "Classifier") {
    if (!all(unique(df$tag) %in% unique(train$tag)))
      stop(paste("You must train with all available tags:", vector2text(unique(df$tag))))
    if (!all(unique(df$tag) %in% unique(test$tag)))
      warning("You are training with tags that are not in your test set.")  
  }
  
  # TRAIN MODEL
  quiet(h2o.init(nthreads = -1, port = 54321, min_mem_size = "8g"))
  if (start_clean) {
    quiet(h2o.removeAll()) 
  } else {
    if (!quiet) 
      message(paste("Previous trained models are not being erased.",
                    "Use 'start_clean' parameter if needed."))
  }
  if (length(ignore) > 0)
    if (!quiet)
      message(paste("Ignored variables for training models:", vector2text(ignore)))
  
  # RUN AUTOML
  if (length(exclude_algos) > 0 & !quiet) 
    message(paste("Algorithms excluded:", vector2text(exclude_algos)))
  if (!quiet) 
    message(paste(">>> Iterating until", max_models, 
                  "models or", max_time, "seconds..."))
  aml <- quiet(h2o.automl(
    x = colnames(df)[!colnames(df) %in% c("tag", ignore)],
    y = "tag",
    training_frame = as.h2o(train),
    leaderboard_frame = as.h2o(test),
    weights_column = weight,
    max_runtime_secs = max_time,
    max_models = max_models,
    exclude_algos = exclude_algos,
    nfolds = nfolds, 
    #project_name = project,
    seed = seed))
  if (nrow(aml@leaderboard) == 0) {
    stop("NO MODELS TRAINED. Please set max_models to at least 1 and increase max_time")
  } else {
    if (!is.nan(aml@leaderboard[1,2]))
      if (!quiet) message(paste("Succesfully generated", nrow(aml@leaderboard), "models:")) 
  }
  if (!quiet) print(aml@leaderboard[,1:3])
  flow <- "http://localhost:54321/flow/index.html"
  if (!quiet) message("Check results in H2O Flow's nice interface: ", flow)
  
  # GET RESULTS AND PERFORMANCE
  results <- h2o_results(
    aml, test, train, y, which = 1, 
    model_type = model_type, target = target, 
    plots = plots, project = project, seed = seed, quiet = quiet)
  
  if (save) {
    export_results(results, subdir = subdir, thresh = thresh)
    if (!quiet) message("Results and model files exported succesfully!")
  }
  
  if (!quiet) toc(id = "h2o_automl", msg = "Process duration:")
  
  print(results$metrics$metrics)
  
  if (alarm & !quiet) {
    tryCatch({
      try_require("beepr", stop = FALSE)
      beep() 
    })
  }
  
  attr(results, "type") <- "h2o_automl"
  return(results)
  
}


####################################################################
#' Automated H2O's AutoML Results
#'
#' @family Machine Learning
#' @param h2o_object H2O Leaderboard (H2OFrame/H2OAutoML) or Model (h2o)
#' @param test,train Dataframe. Must have the same columns
#' @param y Character. Name of the independent variable
#' @param which Integer. Which model to select from leaderboard
#' @param model_type Character. Select "Classifier" or "Regression"
#' @param target Value. Which is your target positive value? If 
#' set to 'auto', the target with largest mean(score) will be 
#' selected. Change the value to overwrite. Only used when binary
#' categorical model.
#' @param plots Boolean. Create plots objects?
#' @param project Character. Your project's name
#' @param seed Integer. Set a seed for reproducibility. AutoML can only 
#' guarantee reproducibility if max_models is used because max_time is 
#' resource limited.
#' @param quiet Boolean. Quiet messages, warnings, recommendations?
#' @export
h2o_results <- function(h2o_object, test, train, y = "tag", which = 1,
                        model_type, target = "auto", plots = TRUE, 
                        project = NULL, seed = 0, quiet = FALSE) {
  
  # MODEL TYPE
  types <- c("Classifier", "Regression")
  if (!model_type %in% types) 
    stop(paste("model_type must be any of:", vector2text(types)))
  if (model_type == types[1]) thresh <- 100 else thresh <- 0
  
  # GLOBAL DATAFRAME FROM TEST AND TRAIN
  if (!all(colnames(test) == colnames(train)))
    stop("All columns from test and train datasets must be exactly the same")
  global <- data.frame(test) %>%  bind_rows(train) %>%
    mutate(train_test = c(rep("test", nrow(test)), rep("train", nrow(train))))
  colnames(global)[colnames(global) == "tag"] <- y
  if (model_type == "Classifier")
    cats <- unique(global[,colnames(global) == y]) else cats <- "None"
  
  # SELECT MODEL FROM h2o_automl()
  if (any(c("H2OFrame","H2OAutoML") %in% class(h2o_object))) {
    # Note: Best model from leaderboard is which = 1
    m <- h2o.getModel(as.vector(h2o_object@leaderboard$model_id[which]))     
    if (!quiet) message(paste("Model selected:", as.vector(m@model_id)))
  } else {
    m <- h2o_object
  }
  
  # VARIABLES IMPORTANCES
  if (sum(grepl("Stacked", as.vector(m@model_id))) > 0) {
    stacked <- TRUE
    if (!quiet) message("NOTE: No importance features for Stacked Ensemble Models")
  } else stacked <- FALSE
  if (!stacked) {
    imp <- data.frame(h2o.varimp(m)) %>%
      {if ("names" %in% colnames(.)) 
        rename(., "variable" = "names", "importance" = "coefficients") else .} %>%
      {if ("percentage" %in% colnames(.)) 
        rename(., "importance" = "percentage") else .}
    noimp <- filter(imp, importance < 0.015) %>% arrange(desc(importance))
    if (nrow(noimp) > 0) {
      top10 <- noimp %>% ungroup() %>% slice(10)
      which <- vector2text(top10$variable)
      if (nrow(noimp) > 10) 
        which <- paste(which, "and", nrow(noimp) - 10, "other...")
      if (!quiet) 
        message(paste("NOTE: The following variables were NOT important:", which))
    } 
  }
  
  # GET PREDICTIONS
  if (!quiet) message(paste0(">>> Running predictions for ", y, "..."))
  predictions <- quiet(h2o_predict_model(global, m))
  global <- cbind(global, predictions)
  # Change dots for space
  if (sum(grepl(" ", cats)) > 0)
    colnames(global) <- str_replace_all(colnames(global), "\\.", " ")
  # For test performance metrics
  scores <- predictions[1:nrow(test),]
  # Selected target value
  if (target != "auto" & length(cats) == 2)
    scores <- target_set(tag = as.vector(test$tag), 
                         score = scores[,2], 
                         target = target, 
                         quiet = TRUE)$df
  # Multis object and standard predictions output
  multis <- NA
  if (model_type == "Classifier") {
    if (length(cats) == 2) {
      scores <- select_if(scores, is.numeric) %>% .[,1]
    } else {
      colnames(scores)[1] <- "score"
      multis <- select(scores, -score)
    }
  } else {
    scores <- data.frame(score = as.vector(scores))
  }
  
  # GET ALL RESULTS INTO A LIST
  results <- list()
  results[["model"]] <- m
  results[["scores_test"]] <- data.frame(tag = as.vector(test$tag), scores)
  results[["metrics"]] <- model_metrics(
    tag = results$scores_test$tag, 
    score = results$scores_test$score,
    multis = multis,
    thresh = thresh,
    target = target,
    model_name = as.vector(m@model_id),
    plots = plots)
  if (model_type == "Classifier" & length(cats) == 2) 
    results$metrics[["max_metrics"]] <- data.frame(
      m@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores)
  if (!stacked) results[["importance"]] <- imp
  
  results[["datasets"]] <- list(
    global = global, test = filter(global, train_test == "test"))
  results[["scoring_history"]] <- data.frame(m@model$scoring_history)
  results[["parameters"]] <- m@parameters
  results[["type"]] <- model_type
  results[["model_name"]] <- as.vector(m@model_id)
  results[["algorithm"]] <- m@algorithm
  if (any(c("H2OFrame","H2OAutoML") %in% class(h2o_object)))
    results[["leaderboard"]] <- h2o_object@leaderboard
  results[["project"]] <- project
  results[["y"]] <- y
  results[["seed"]] <- seed
  results[["h2o"]] <- h2o.getVersion()
  
  if (plots) {
    if (!quiet) message(">>> Generating plots...")
    plots <- list()
    plots[["dashboard"]] <- mplot_full(
      tag = results$scores_test$tag,
      score = results$scores_test$score,
      multis = multis, 
      thresh = thresh,
      subtitle = results$project,
      model_name = results$model_name,
      plot = FALSE)
    if (!stacked) 
      plots[["importance"]] <- mplot_importance(
        var = results$importance$variable,
        imp = results$importance$importance,
        model_name = results$model_name,
        subtitle = results$project)
    plots <- append(plots, rev(as.list(results$metrics$plots)))
    results$plots <- plots
  } 
  
  attr(results, "type") <- "h2o_automl"
  return(results)
  
}


####################################################################
#' Select Model from h2o_automl's Leaderboard
#' 
#' Select wich model from the h2o_automl function to use
#' 
#' @family Machine Learning
#' @family Tools
#' @param results Object. h2o_automl output
#' @param which_model Integer. Which model from the leaderboard you wish to use?
#' @param plots Boolean. Create plots too?
#' @param quiet Boolean. Quiet messages, warnings, recommendations?
#' @export
h2o_selectmodel <- function(results, which_model = 1, plots = TRUE, quiet = FALSE) {
  
  check_attr(results, attr = "type", check = "h2o_automl")
  
  # Select model (Best one by default)
  m <- h2o.getModel(as.vector(results$leaderboard$model_id[which_model]))  
  
  # Calculate everything
  output <- h2o_results(m, 
                        test = results$datasets$test, 
                        # NOT filter(results$datasets$global, train_test == "train"),
                        train = results$datasets$test, 
                        y = results$y, 
                        which = which_model, 
                        model_type = results$model_type, 
                        plots = plots, 
                        project = results$project, 
                        seed = results$seed, 
                        quiet = quiet)
  return(output)
}


####################################################################
#' Export h2o_automl's Results
#' 
#' Export RDS, TXT, POJO, MOJO and all results from h2o_automl
#' 
#' @family Machine Learning
#' @family Tools
#' @param results Object. h2o_automl output
#' @param thresh Integer. Threshold for selecting binary or regression 
#' models: this number is the threshold of unique values we should 
#' have in 'tag' (more than: regression; less than: classification)
#' @param which Character vector. Select which file format to export:
#' Possible values: txt, csv, rds, binary, mojo, plots. You might also
#' use dev (txt, csv, rds) or production (binary, mojo) or simply don't use
#' parameter to export everything
#' @param note Character. Add a note to the txt file. Useful when lots of 
#' models are trained and saved to remember which one is which one
#' @param subdir Character. In which directory do you wish to save 
#' the results?
#' @param save Boolean. Do you wish to save/export results?
#' @export
export_results <- function(results, 
                           thresh = 10,
                           which = c("txt","csv","rds",
                                     "binary","mojo","plots",
                                     "dev","production"),
                           note = NA,
                           subdir = NA,
                           save = TRUE) {
  
  if (save) {
    
    check_attr(results, attr = "type", check = "h2o_automl")
    quiet(h2o.init(nthreads = -1, port = 54321, min_mem_size = "8g"))
    name <- results$model_name
    subdir <- paste0(ifelse(is.na(subdir), "", subdir), "/", name)
    
    # Directory to save all our results
    dir <- file.path(paste0(getwd(), "/", subdir))
    message(paste("Export directory:", dir))
    if (!dir.exists(dir)) dir.create(dir) 
    
    if ("dev" %in% which) which <- unique(c(which, "txt", "csv", "rds"))
    if ("production" %in% which) which <- unique(c(which, "binary", "mojo"))
    
    if ("txt" %in% which | !is.na(note)[1]) {
      set.seed(123)
      results_txt <- list(
        "Project" = results$project,
        "Note" = note,
        "Model Type" = results$type,
        "Algorithm" = results$algorithm,
        "Model name" = name,
        "Train/Test" = table(results$datasets$global$train_test),
        "Metrics Glossary" = results$metrics$dictionary,
        "Train Metrics" = results$metrics$metrics,
        "Train Metrics by labels" = if (length(results$metrics$metrics_tags) > 1)
          results$metrics$metrics_tags else NULL,
        "Train's Confusion Matrix" = if (length(results$metrics$confusion_matrix) > 1)
          results$metrics$confusion_matrix else NULL,
        "Variables Importance" = results$importance,
        "H2O Global Results" = results$model,
        "Leaderboard" = results$leaderboard,
        "10 Scoring examples" = sample_n(results$datasets$global, 10),
        "H20 Version" = results$h2o)
      if (is.na(note)[1]) results_txt$note <- NULL
      capture.output(results_txt, file = paste0(dir, "/", name, ".txt"))
      message(">>> Summary test file saved...")
    }
    
    # Export CSV with predictions and datasets
    if ("csv" %in% which) {
      write.csv(results$datasets$global, 
                paste0(dir, "/", name, ".csv"), 
                row.names = FALSE)
      message(">>> CSV file exported...")
    }
    
    # Export Results List
    if ("rds" %in% which) {
      saveRDS(results, file = paste0(dir, "/", name, ".rds")) 
      message(">>> RDS file exported...")
    }
    
    # Export Model as POJO & MOJO for Production
    if ("mojo" %in% which) {
      h2o.download_mojo(results$model, path = dir, get_genmodel_jar = TRUE)  
      message(">>> MOJO (zip + jar files) exported...")
    } 
    
    #if (pojo) h2o.download_pojo(results$model, path = dir)  
    
    # Export Binary
    if ("binary" %in% which) {
      h2o.saveModel(results$model, path = dir, force = TRUE)
      message(">>> Binary file saved...")
    }
    
    if ("plots" %in% which) {
      message(">>> Saving plots...")
      aux <- names(results$plots)
      for (i in 1:length(results$plots)) {
        export_plot(results$plots[[i]], 
                    name = aux[i],
                    width = 8, height = 6, res = 300,
                    dir = getwd(),
                    subdir = paste0(subdir, "/Plots"),
                    quiet = TRUE)
      }
      message(">>> Plots saved...")
    }
    message(paste("Succesfully exported files:", vector2text(which)))
  }
}


####################################################################
#' Split a dataframe for training and testing sets
#'
#' This function splits automatically a dataframe into train and 
#' test datasets. You can define a seed to get the same results 
#' every time, but has a default value. You can prevent it from 
#' printing the split counter result.
#'
#' @family Machine Learning
#' @family Tools
#' @param df Dataframe to split
#' @param size Numeric. Split rate value, between 0 and 1. If set to
#' 1, the train and test set will be the same.
#' @param seed Seed for random split
#' @param print Print summary results
#' @return A list with both datasets, summary, and split rate
#' @export
msplit <- function(df, size = 0.7, seed = 0, print=T) {
  
  if (size <= 0 | size > 1) stop("Set size parameter to a value >0 and <=1") 
  
  set.seed(seed)
  df <- data.frame(df)
  if (size == 1) train <- test <- df
  
  if (size < 1 & size > 0) {
    ind <- sample(seq_len(nrow(df)), size = floor(size * nrow(df)))
    train <- df[ind, ]
    test <- df[-ind, ] 
  }
  
  train_size <- dim(train)
  test_size <- dim(test)
  summary <- rbind(train_size, test_size)[,1]
  
  if (print == TRUE) print(summary) 
  
  sets <- list(train = train, test = test, summary = summary, split_size = size)
  
  return(sets)
  
}


####################################################################
#' Iterate and Search for Best Seed
#' 
#' This functions lets the user iterate and search for best seed. Note that if
#' the results change a lot, you are having a high variance in your data.
#' 
#' @family Machine Learning
#' @family Tools
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
#' @family Calculus
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
#' @family Calculus
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
#' @family Calculus
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
#' @family Calculus
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
#' @family Calculus
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
#' @family Calculus
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
#' @family Calculus
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
#' Loggarithmic Loss Function for Binary Models
#'
#' This function calculates log loss/cross-entropy loss for binary 
#' models. NOTE: when result is 0.69315, the classification is neutral; 
#' it assigns equal probability to both classes.
#'
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param eps Numeric. Epsilon value
#' @export
loglossBinary <- function(tag, score, eps = 0.0001) {
  
  if (length(unique(tag)) != 2)
    stop("Your 'tag' vector is not binary!")
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", length(tag),
                       "rows and score has", length(score)))
    )
  }
  
  if (!is.numeric(tag)) tag <- as.integer(tag) - 1
  
  score <- pmax(pmin(score, 1 - eps), eps)
  LogLoss <- -mean(tag * log(score) + (1 - tag) * log(1 - score))
  
  return(LogLoss)
  
}


####################################################################
#' H2O Predict using MOJO file
#' 
#' This function lets the user predict using the h2o .zip file 
#' containing the MOJO files. Note that it works with the files 
#' generated when using the function export_results()
#' 
#' @family Machine Learning
#' @family Tools
#' @param df Dataframe. Data to insert into the model
#' @param model_path Character. Relative path of directory
#' where your zip model file is
#' @param batch Integer. Run n batches at a time
#' @export
h2o_predict_MOJO <- function(df, model_path, batch = 300){
  
  quiet(h2o.init(nthreads = -1, port = 54321, min_mem_size = "8g"))
  
  df <- as.data.frame(df)
  file <- listfiles(model_path, regex = ".zip")$filename
  zip <- paste0(model_path,"/",as.character(file))
  
  aux <- ceiling(nrow(df)/batch)
  df$aux <- rep(1:aux, each = batch)[1:nrow(df)]
  output <- c()
  for (i in 1:aux) {
    dfi <- select(df[df$aux == i,], -aux)
    json <- toJSON(dfi)
    res <- h2o.predict_json(zip, json)  
    if (length(res$error) >= 1)
      break("Error:", res$error)
    output <- rbind(output, res)
    if (aux > 1) statusbar(i, aux)
  }
  return(output)
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
#' @family Machine Learning
#' @family Tools
#' @param df Dataframe. Data to insert into the model
#' @param model_path Character. Relative model_path directory or zip file
#' @param sample Integer. How many rows should the function predict?
#' @export
h2o_predict_binary <- function(df, model_path, sample = NA){
  
  message("Use of h2o_predict_MOJO instead highly recommended!")
  quiet(h2o.init(nthreads = -1, port = 54321, min_mem_size = "8g"))
  
  if (!right(model_path, 4) == ".zip") {
    binary <- paste(model_path, gsub(".*-", "", model_path), sep = "/")  
  } else {
    binary <- model_path
  }
  
  model <- h2o.loadModel(binary)
  
  if (!is.na(sample)) df <- df[1:sample, ]
  
  score_binary <- as.vector(predict(model, as.h2o(df))[,3])
  
  return(score_binary)
  
}


####################################################################
#' H2O Predict using API Service
#' 
#' This function lets the user get the score from an API service
#' 
#' @family Machine Learning
#' @family Tools
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
      add_headers('Content-Type' = 'application/json'), 
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
#' @family Machine Learning
#' @family Tools
#' @param df Dataframe/Vector. Data to insert into the model
#' @param model H2o Object. Model
#' @export
h2o_predict_model <- function(df, model){
  as.data.frame(predict(model, as.h2o(df)))
}


####################################################################
#' Calibrate Sampling Scores
#' 
#' This function lets the user calibrate a model's predictions when 
#' under or over sampling methods were applied when training it.
#' 
#' @family Data Wrangling
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
  (score * (target / train) / (target_sample / train_sample)) /
    (((1 - score) * (1 - target / train) / (1 - target_sample / train_sample)) +
       (score * (target / train) / (target_sample / train_sample)))
}


####################################################################
#' Set Target Value in Target Variable
#' 
#' This function detects or forces the target value when predicting
#' a categorical binary model.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param target Value. Which is your target positive value? If 
#' set to 'auto', the target with largest mean(score) will be 
#' selected. Change the value to overwrite. Only used when binary
#' categorical model.
#' @param quiet Boolean. Do not show message for auto target?
#' @export
target_set <- function(tag, score, target = "auto", quiet = FALSE) {
  
  df <- data.frame(tag = tag, score = score)
  
  # Validate inputs
  if (!is.numeric(score) | is.numeric(tag))
    stop("Your tag must be categorical. Your score must be numerical.") 
  
  # Get mean scores for each tag
  means <- df %>% group_by(tag) %>% summarise(mean = mean(score))
  auto <- means$tag[means$mean == max(means$mean)]
  if (target == "auto")
    target <- auto
  if (!target %in% unique(df$tag))
    stop(paste("Your target value", target, "is not valid.",
               "Possible other values:", vector2text(unique(df$tag))))
  if (!quiet) message(paste("Target value:", target)) 
  # If the forced target value is the "lower scores" value, invert scores
  if (auto != target) df$score <- df$score * (-1) + 1
  return(list(df = df, which = target))
}


####################################################################
#' Cumulative Gain, Lift and Response
#' 
#' This function calculates cumulative gain, lift, and response 
#' values for a predictive score of a specific target. You can use the 
#' mplot_gain() function to custom plot results.
#' 
#' @family Machine Learning
#' @family Exploratory
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param target Value. Which is your target positive value? If 
#' set to 'auto', the target with largest mean(score) will be 
#' selected. Change the value to overwrite. Only used when binary
#' categorical model.
#' @param splits Integer. Numer of percentiles to split the data
#' @param plot Boolean. Plot results?
#' @param quiet Boolean. Do not show message for auto target?
#' @export
gain_lift <- function(tag, score, target = "auto", splits = 10, 
                      plot = FALSE, quiet = FALSE) {
  
  if (splits <= 1) stop("You must set more than 1 split")
  aux <- target_set(tag, score, target, quiet)
  df <- aux$df
  which <- aux$which
  
  sc <- df %>% 
    mutate(tag = ifelse(tag == which, TRUE, FALSE)) %>%
    arrange(desc(score)) %>%
    mutate(percentile = .bincode(
      score, quantile(score, probs = seq(0, 1, length = splits + 1), include.lowest = TRUE), 
      right = TRUE, include.lowest = TRUE)) %>%
    mutate(percentile = rev(factor(percentile, 1:splits)))
  
  wizard <- sc %>% filter(tag == TRUE) %>% 
    mutate(percentile = sc$percentile[1:length(sc$percentile[sc$tag == TRUE])]) %>%
    group_by(percentile) %>% tally() %>% 
    ungroup() %>% mutate(p = 100 * n/sum(n), pcum = cumsum(p)) %>% 
    select(percentile, pcum) %>% rename(optimal = pcum)
  
  gains <- sc %>% group_by(percentile) %>% 
    summarise(total = n(), target = sum(tag), score = 100 * min(score)) %>%
    left_join(wizard, "percentile") %>% replace(is.na(.), 100) %>% ungroup() %>%
    mutate(gain = 100 * cumsum(target)/sum(target),
           random = 100 * cumsum(total)/sum(total),
           lift = 100 * (gain/random - 1),
           response = 100 * target/sum(target),
           value = which) %>%
    select(percentile, value, random, target, total, 
           gain, optimal, lift, response, score)
  
  if (plot) {
    plots <- list()
    plots[["gain"]] <- mplot_gain(
      tag, score, target = which, splits = splits)
    plots[["response"]] <- mplot_response(
      tag, score, target = which, splits = splits)
    return(plots) 
  }
  return(gains)
  
}


####################################################################
#' ROC Curves
#' 
#' This function calculates ROC Curves and AUC values with 95\% confidence 
#' range. It also works for multi-categorical models.
#' 
#' @family Machine Learning
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param multis Data.frame. Containing columns with each category score 
#' (only used when more than 2 categories coexist)
#' @export
ROC <- function(tag, score, multis = NA) {
  
  # require(pROC)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  if (!is.numeric(score) & is.na(multis)[1]) {
    score <- as.numeric(score) 
    warning("You should use the multis parameter to add each category's score")
  }
  
  if (is.na(multis)[1]) {
    roc <- pROC::roc(tag, score, ci = TRUE, quiet = TRUE)
    coords <- data.frame(
      fpr = rev(roc$specificities),
      tpr = rev(roc$sensitivities)) %>%
      mutate(label = "2cats")
    ci <- data.frame(roc$ci, row.names = c("min","AUC","max"))
  } else {
    df <- data.frame(tag = tag, score = score, multis)
    cols <- colnames(df)
    coords <- c(); rocs <- list()
    for (i in 1:(length(cols) - 2)) {
      which <- colnames(df)[2 + i]
      res <- df[,c(which)]
      label <- ifelse(df[,1] == which, which, "other")
      roci <- pROC::roc(label, res, ci = TRUE, quiet = TRUE)
      rocs[[paste(cols[i + 2])]] <- roci
      iter <- data.frame(fpr = rev(roci$specificities),
                         tpr = rev(roci$sensitivities),
                         label = paste(round(100*roci$auc,2), which, sep = "% | "))
      coords <- rbind(coords, iter)
    }
    ci <- data.frame(lapply(rocs, "[[", "ci")) %>% mutate(mean = rowMeans(.))
    row.names(ci) <- c("min","AUC","max")
  }
  ret <- list(ci = ci, roc = coords)
  if (!is.na(multis)[1]) {
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
#' @family Machine Learning
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param thresh Numeric. Value which splits the results for the 
#' confusion matrix when binary.
#' @param plot Boolean. Plot result?
#' @export
conf_mat <- function(tag, score, thresh = 0.5, plot = FALSE) {
  
  if (plot) return(mplot_conf(tag, score, thresh = thresh))
  
  df <- data.frame(tag, score)
  
  # About tags
  labels <- df %>% group_by(tag, .drop = FALSE) %>% tally(wt = NULL) %>% arrange(desc(n)) %>% .$tag
  df <- df %>% mutate(tag = factor(tag, levels = unique(tag)))
  
  # About scores
  if (is.numeric(df$score) & length(unique(tag)) == 2) {
    df <- mutate(df, pred = ifelse(score >= thresh, as.character(labels[1]), as.character(labels[2])))
  } else {
    df <- mutate(df, pred = score)
  }
  
  # Confussion Matrix
  ret <- df %>% 
    rename(Real = tag, Pred = pred) %>%
    crosstab(Real, Pred, total = FALSE)
  
  return(ret)
}


####################################################################
#' Model Metrics and Performance
#' 
#' This function lets the user get a confusion matrix and accuracy, and 
#' for for binary classification models: AUC, Precision, Sensitivity, and
#' Specificity.
#' 
#' @family Machine Learning
#' @family Calculus
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param multis Data.frame. Containing columns with each category score 
#' (only used when more than 2 categories coexist)
#' @param abc Boolean. Arrange columns and rows alphabetically 
#' when categorical values?
#' @param thresh Integer. Threshold for selecting binary or regression 
#' models: this number is the threshold of unique values we should 
#' have in 'tag' (more than: regression; less than: classification)
#' @param thresh_cm Numeric. Value to splits the results for the 
#' confusion matrix. Range of values: (0-1)
#' @param target Value. Which is your target positive value? If 
#' set to 'auto', the target with largest mean(score) will be 
#' selected. Change the value to overwrite. Only used when binary
#' categorical model.
#' @param model_name Character. Model's name
#' @param plots Boolean. Include plots?
#' @param subtitle Character. Subtitle for plots
#' @export
model_metrics <- function(tag, score, multis = NA, 
                          abc = TRUE,
                          thresh = 10, 
                          thresh_cm = 0.5, 
                          target = "auto",
                          model_name = NA,
                          plots = TRUE, subtitle = NA){
  
  metrics <- list()
  cats <- sort(unique(as.character(tag)))
  model_type <- ifelse(length(cats) <= thresh, "Classifier", "Regression")    
  
  # When seems numeric but is categorical
  if (model_type == "Classifier" & sum(grepl('^[0-9]', cats)) > 0)
    tag <- as.factor(as.character(ifelse(
      grepl('^[0-9]', tag), paste0("n_", tag), as.character(tag))))
  # When is regression should always be numerical
  if (model_type == "Regression")
    tag <- as.numeric(tag)
  
  if (model_type == "Classifier") {
    
    dic <- c("AUC: Area Under the Curve",
             "ACC: Accuracy",
             "PRC: Precision = Positive Predictive Value",
             "TPR: Sensitivity = Recall = Hit rate = True Positive Rate",
             "TNR: Specificity = Selectivity = True Negative Rate",
             "Logloss (Error): Logarithmic loss [Neutral classification: 0.69315]",
             "Gain: When best n deciles selected, what % of the real target observations are picked?",
             "Lift: When best n deciles selected, how much better than random is?")
    metrics[["dictionary"]] <- dic
    
    tag <- as.character(tag)
    
    if (is.numeric(score)) {
      new <- data.frame(score = score) %>%
        mutate(new = ifelse(score >= thresh_cm, cats[1], cats[2])) %>% .$new
    } else {
      new <- score
    }
    
    conf_mat <- table(Real = factor(tag, levels = unique(tag)), 
                      Pred = factor(new, levels = unique(tag)))
    total <- sum(conf_mat)
    trues <- sum(diag(conf_mat))
    falses <- total - trues
    
    # For Binaries
    if (length(cats) == 2) {
      metrics[["confusion_matrix"]] <- conf_mat
      ROC <- pROC::roc(tag, as.numeric(score), ci = TRUE, quiet = TRUE)
      nums <- data.frame(
        AUC = ROC$auc,
        ACC = trues / total,
        PRC = conf_mat[2,2] / (conf_mat[2,2] + conf_mat[1,2]),
        TPR = conf_mat[2,2] / (conf_mat[2,2] + conf_mat[2,1]),
        TNR = conf_mat[1,1] / (conf_mat[1,1] + conf_mat[1,2]))
      metrics[["gain_lift"]] <- gain_lift(tag, score, target = target, quiet = FALSE)
      metrics[["metrics"]] <- signif(nums, 5)
    } else {
      
      # For Multi-Categories
      tags <- sort(unique(tag))
      if (is.na(multis)[1]) 
        stop("You have to input a data.frame with each tag's probability into the multis parameter.")
      if (!all(as.character(tags) %in% colnames(multis)))
        stop(paste0("Your multis data.frame colums should be ", vector2text(tags),
                    " (not ", vector2text(colnames(multis)), ")"))
      if (!all(colnames(multis) %in% as.character(tags))) {
        multis <- multis[,colnames(multis) %in% as.character(tags)]
      }
      
      df <- data.frame(tag, score)
      metrics[["confusion_matrix"]] <- conf_mat(tag, score)
      AUCs <- t(ROC(tag, score, multis)$ci)[,2]
      m <- data.frame(
        AUC = mean(AUCs[1:length(cats)]),
        ACC = trues / total)
      metrics[["metrics"]] <- signif(m, 5)
      nums <- c()
      for (i in 1:length(cats)) {
        tagi <- ifelse(tag == cats[i], 1, 0)
        predi <- as.numeric(ifelse(score == cats[i], 1, 0))
        conf_mati <- table(Real = factor(tagi, levels = c(1, 0)), 
                           Pred = factor(predi, levels = c(1, 0)))
        if (nrow(data.frame(conf_mati)) == 4) {
          total <- sum(conf_mati)
          trues <- sum(diag(conf_mati))
          falses <- total - trues
          numsi <- data.frame(
            tag = cats[i],
            ACC = trues / total,
            PRC = conf_mati[2,2] / (conf_mati[2,2] + conf_mati[1,2]),
            TPR = conf_mati[2,2] / (conf_mati[2,2] + conf_mati[2,1]),
            TNR = conf_mati[1,1] / (conf_mati[1,1] + conf_mati[1,2]))
          nums <- rbind(nums, numsi) 
        }
      }
      nums$AUC <- AUCs[1:nrow(nums)]
      nums <- left_join(freqs(select(df, tag), tag), nums, "tag") %>% 
        select(tag, n, p, AUC, everything(), -pcum)
      metrics[["metrics_tags"]] <- mutate_if(nums, is.numeric, funs(signif(., 5)))
    }
    
    if (plots) {
      plots <- list()
      # CUMULATIVE GAINS PLOT
      plots[["gains"]] <- mplot_gain(
        tag, score, multis, target = "auto", splits = 10, quiet = TRUE)
      # CUMULATIVE RESPONSE PLOT
      plots[["response"]] <- mplot_response(
        tag, score, multis, target = "auto", splits = 10, highlight = "auto", quiet = TRUE)
      # CONFUSION MATRIX PLOT
      plots[["conf_matrix"]] <- mplot_conf(
        tag, score, thresh_cm, abc = abc, subtitle = subtitle, model_name = model_name) 
      # ROC CURVES PLOT
      plots[["ROC"]] <- invisible(mplot_roc(tag, score, multis, subtitle = subtitle)) 
      # Bring them all!
      metrics[["plots"]] <- plots
    }
  }
  
  if (model_type == "Regression") {
    
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
