####################################################################
#' Automated H2O's AutoML
#'
#' This function lets the user create a robust and fast model, using 
#' H2O's AutoML function. The result is a list with the best model, 
#' its parameters, datasets, performance metrics, variables 
#' importances, and plots. If the input is categorical, classification
#' models will be trained and if is a continuous variable, regression
#' models will be trained. 
#' 
#' @section List of algorithms:
#' \describe{
#'   \item{DRF}{Distributed Random Forest, including Random Forest (RF) and Extremely-Randomized Trees (XRT)}
#'   \item{GLM}{Generalized Linear Model}
#'   \item{XGBoost}{eXtreme Grading Boosting}
#'   \item{GBM}{Gradient Boosting Machine}
#'   \item{DeepLearning}{Fully-connected multi-layer artificial neural network}
#'   \item{StackedEnsemble}{Stacked Ensemble}
#' }
#' \href{http://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html}{Read more here}.
#' 
#' @section Methods:
#' \describe{
#'   \item{print}{Use `print` method to print models stats and summary}
#'   \item{plot}{Use `plot` method to plot results using \code{mplot_full()}}
#' }
#'
#' @family Machine Learning
#' @param df Dataframe. Dataframe containing all your data, including 
#' the independent variable labeled as 'tag'. If you want to define 
#' which variable should be used instead, use the y parameter.
#' @param y Variable or Character. Name of the independent variable.
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
#' @param no_outliers Boolean/Numeric. Remove y's outliers in the dataset? 
#' Will remove those values that are farther than n standard deviations from
#' the independent variable's mean (Z-score). Set to `TRUE` for default (3) 
#' or numeric to set a different multiplier.
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
#' @param ... Additional parameters on \code{h2o::h2o.automl}
#' @examples 
#' \dontrun{
#' data(dft) # Titanic dataset
#' dft <- subset(dft, select = -c(Ticket, PassengerId, Cabin))
#' 
#' # Classification: Binomial - 2 Classes
#' r <- h2o_automl(dft, y = Survived, max_models = 1, impute = FALSE, target = "TRUE")
#' print(r)
#' 
#' # Let's see all the stuff we have inside:
#' lapply(r, names)
#' 
#' # Classification: Multi-Categorical - 3 Classes
#' r <- h2o_automl(dft, Pclass, ignore = c("Fare", "Cabin"), max_time = 30, plots = FALSE)
#' print(r)
#' 
#' # Regression: Continuous Values
#' r <- h2o_automl(dft, y = "Fare", ignore = c("Pclass"), exclude_algos = NULL)
#' print(r)
#' 
#' # WITH PRE-DEFINED TRAIN/TEST DATAFRAMES
#' splits <- msplit(dft, size = 0.8)
#' splits$train$split <- "train"
#' splits$test$split <- "test"
#' df <- rbind(splits$train, splits$test)
#' r <- h2o_automl(df, "Survived", max_models = 1, train_test = "split")
#' print(r)
#' }
#' @export
h2o_automl <- function(df, y = "tag",
                       ignore = c(),
                       train_test = NA,
                       split = 0.7,
                       weight = NULL,
                       target = "auto",
                       balance = FALSE,
                       impute = FALSE,
                       no_outliers = TRUE,
                       center = FALSE,
                       scale = FALSE,
                       seed = 0,
                       nfolds = 5,
                       thresh = 10,
                       max_models = 3,
                       max_time = 10*60,
                       start_clean = TRUE,
                       exclude_algos = c("StackedEnsemble","DeepLearning"),
                       plots = TRUE,
                       alarm = TRUE,
                       quiet = FALSE,
                       save = FALSE,
                       subdir = NA,
                       project = "ML Project",
                       ...) {
  
  tic(id = "h2o_automl")
  
  if (!quiet) message(paste(Sys.time(), "| Started process..."))
  
  # Tidyverse friendly
  y <- gsub('"', "", as_label(enquo(y)))
  
  # INDEPENDENT VARIABLE
  if (!y %in% colnames(df)) {
    stop(paste("You should have a 'tag' column in your data.frame or select",
               "an independent varialbe using the 'y' parameter."))
  }
  
  if (!quiet) message(sprintf("Variable to predict: %s", y))
  
  colnames(df)[colnames(df) == y] <- "tag"
  df <- data.frame(df) %>% 
    filter(!is.na(.data$tag)) %>%
    mutate_if(is.character, as.factor)
  
  # MISSING VALUES
  m <- missingness(df)
  if (!is.null(m)) {
    m <- mutate(m, label = paste0(.data$variable, " (", .data$missingness, "%)"))
    if (!quiet) {
      top10 <- m %>% ungroup() %>% slice(1:10)
      which <- vector2text(top10$label, quotes = FALSE)
      if (nrow(m) > 10)
        which <- paste(which, "and", nrow(m) - 10, "other.")
      message(paste0("- NOTE: The following variables contain missing observations: ", which,
                     if (!impute & !quiet) ". Consider using the impute parameter."))
    }
    if (impute) {
      if (!quiet) message(paste(">>> Imputing", sum(m$missing), "missing values..."))
      df <- impute(df, seed = seed, quiet = TRUE)
    }
  }
  
  # OUTLIERS ON INDEPENDENT VARIABLE
  if (is.numeric(df$tag)) {
    thresh <- ifelse(is.numeric(no_outliers), no_outliers, 3)
    is_outlier <- outlier_zscore(df$tag, thresh = thresh)
    if (!quiet & !isTRUE(no_outliers))
      message(sprintf(
        "- NOTE: %s (%s) of %s values are considered outliers (Z-Score: >%ssd). %s",
        formatNum(100*sum(is_outlier)/nrow(df), 1, pos = "%"),
        formatNum(sum(is_outlier), 0), y, thresh,
        ifelse(no_outliers, 
               paste("Removing them from the dataset for better results.",
                     "To keep them, set the 'no_outliers' parameter."), 
               "Consider using the 'no_outliers' parameter to remove them.")))
    if (no_outliers) df <- df[!is_outlier,] 
  }
  
  # ONE HOT SMART ENCODING
  nums <- df_str(df, "names", quiet = TRUE)$nums
  if (length(nums) != ncol(df) & !quiet) 
    message(paste(
      "- NOTE: There are", ncol(df) - length(nums) - sum(ignore %in% colnames(df)), "non-numerical features.",
      "Consider using ohse() prior for One Hot Smart Encoding your categorical variables."))
  if (scale | center & length(nums) > 0) {
    new <- data.frame(lapply(df[nums], function(x) scale(x, center = center, scale = scale)))
    colnames(new) <- nums
    df[nums] <- new
    msg <- ifelse(scale & center, "scaled and centered", ifelse(scale, "scaled", "centered"))
    if (!quiet) message(paste0("All numerical features (", length(nums), ") were ", msg))
  }
  
  # MODEL TYPE
  cats <- unique(df$tag)
  model_type <- ifelse(length(cats) <= as.integer(thresh), "Classifier", "Regression")
  message("Model type: ", model_type)
  # Change spaces for dots as `multis` arguments may not match
  if (model_type == "Classifier") cats <- gsub(" ", ".", cats)
  # If y variables is named as one of the categories, prediction values will be a problem
  if (model_type == "Classifier" & y %in% cats) {
    stop(paste("Your y parameter can't be named as any of the labels used.",
               "Please, rename", y, "into a valid column name next such as",
               paste0(y, "_labels for example.")))
  }
  ignored <- ignore
  
  # If target value is not an existing target value
  if (!target %in% cats & length(cats) == 2)
    if (!target %in% c(cats, "auto"))
      stop(paste("Your target value", target, "is not valid.",
                 "Possible other values:", vector2text(cats)))
  # When might seem numeric but is categorical
  if (model_type == "Classifier" & sum(grepl('^[0-9]', cats)) > 0)
    df <- mutate(df, tag = as.factor(as.character(
      ifelse(grepl('^[0-9]', .data$tag), paste0("n_", .data$tag), as.character(.data$tag)))))
  # When is regression should always be numerical
  if (model_type == "Regression")
    df$tag <- as.numeric(df$tag)
  # Show a summary of our tags
  if (model_type == "Classifier" & !quiet) print(freqs(df, .data$tag))
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
        train <- filter(df, .data$train_test == "train")
        test <- filter(df, .data$train_test == "test")
        split <- nrow(train)/nrow(df)
        ignore <- c(ignore, train_test)
        if (!quiet) print(table(df$train_test))
      } else stop("Your train_test column should have 'train' and 'test' values only!") 
    } else stop(paste("There is no column named", train_test))
  }
  if (nrow(train) > 10000)
    message("- NOTE: Consider sampling your dataset for faster results")
  
  # BALANCE TRAINING SET
  if (model_type == "Classifier" & balance) {
    total <- nrow(train)
    min <- freqs(train, .data$tag) %>% .$n %>% min(., na.rm = TRUE)
    train <- train %>% group_by(.data$tag) %>% sample_n(min)
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
    message(sprintf(">>> Iterating until %s models or %s seconds...", max_models, max_time))
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
    seed = seed,
    ...))
  if (nrow(aml@leaderboard) == 0) {
    stop("NO MODELS TRAINED. Please set max_models to at least 1 and increase max_time")
  } else {
    if (!is.nan(aml@leaderboard[1,2]))
      if (!quiet) message(paste("Succesfully generated", nrow(aml@leaderboard), "models:")) 
  }
  if (!quiet) print(aml@leaderboard[,1:3])
  
  flow <- "http://localhost:54321/flow/index.html"
  if (!quiet & Sys.getenv('HOSTNAME') == "") 
    message("Check results in H2O Flow's nice interface: ", flow)
  
  # GET RESULTS AND PERFORMANCE
  results <- h2o_results(
    aml, test, train, y, which = 1,
    model_type = model_type, target = target, split = split,
    plots = plots, project = project, ignored = ignored,
    seed = seed, quiet = quiet)
  
  if (save) {
    export_results(results, subdir = subdir, thresh = thresh)
    if (!quiet) message("Results and model files exported succesfully!")
  }
  
  if (!quiet) toc(id = "h2o_automl", msg = "Process duration:")
  if (!quiet) print(results)
  
  if (alarm & !quiet) {
    try_require("beepr", stop = FALSE)
    try(beep())
  }
  
  attr(results, "type") <- "h2o_automl"
  return(results)
  
}

####################################################################
#' Plot methods for lares
#' @rdname plot
#' @param x Object
#' @param ... Additional parameters
#' @export
plot.h2o_automl <- function(x, ...) {
  if (!inherits(x, 'h2o_automl'))
    stop('Object must be class h2o_automl')
  if ("plots" %in% names(x)) {
    x$plots$dashboard 
  } else { message(
    "Nothing to plot: set 'plots = TRUE' when creating h2o_automl object") 
  } 
}

####################################################################
#' Print methods for lares
#' @rdname print
#' @param x Object
#' @param ... Additional parameters
#' @export
print.h2o_automl <- function(x, ...) {
  
  if (!inherits(x, 'h2o_automl'))
    stop('Object must be class h2o_automl')
  
  aux <- list()
  selected <- which(as.vector(x$leaderboard$model_id) == x$model_name)
  n_models <- nrow(x$leaderboard)
  data_points <- nrow(x$datasets$global)
  split <- round(100*x$split)
  
  aux[["met"]] <- glued(
    "Test metrics: 
{v2t({met}, sep = '\n', quotes = FALSE)}", met = paste(
  "  ",
  names(x$metrics$metrics), "=",
  signif(x$metrics$metrics, 5)))
  
  if ("importance" %in% names(x)) {
    aux[["imp"]] <- glued(
      "Most important variables:
{v2t({imp}, sep = '\n', quotes = FALSE)}", imp = paste(
  "  ",
  x$importance %>% head(5) %>%
    mutate(label = sprintf(
      "%s (%s)", 
      .data$variable, 
      formatNum(100*.data$importance, 1, pos = "%"))) %>%
    pull(.data$label)))
  }
  
  
  print(glued("
Main Model ({selected}/{n_models}): {x$model_name}
Independent Variable: {x$y}
Type: {x$type}
Algorithm: {toupper(x$algorithm)}
Split: {split}% training data (of {data_points} observations)
Seed: {x$seed}

{aux$met}

{aux$imp}"))
  
}


####################################################################
#' Automated H2O's AutoML Results
#' 
#' This is an auxiliary function to calculate predictions and results
#' when using the \code{h2o_automl()} function. 
#'
#' @inheritParams h2o_automl
#' @param h2o_object H2O Leaderboard (H2OFrame/H2OAutoML) or Model (h2o)
#' @param test,train Dataframe. Must have the same columns
#' @param which Integer. Which model to select from leaderboard
#' @param model_type Character. Select "Classifier" or "Regression"
#' @param ignored Character vector. Columns ignored.
#' @param leaderboard H2O's Leaderboard. Passed when using 
#' \code{h2o_selectmodel} as it contains plain model and no leader board.
#' @export
h2o_results <- function(h2o_object, test, train, y = "tag", which = 1,
                        model_type, target = "auto", split = 0.7,
                        ignored = c(), quiet = FALSE, 
                        project = "ML Project", seed = 0,
                        leaderboard = list(),
                        plots = TRUE, 
                        ...) {

  # MODEL TYPE
  types <- c("Classifier", "Regression")
  check_opts(model_type, types)
  thresh <- ifelse(model_type == types[1], 10000, 0)
  
  # When using h2o_select
  if ("train_test" %in% colnames(test)) {
    colnames(test)[colnames(test) == y] <- "tag"
    colnames(train)[colnames(train) == y] <- "tag"
    test <- test[,1:(which(colnames(test) == "train_test") - 1)]
    train <- train[,1:(which(colnames(train) == "train_test") - 1)]
  }
  
  # GLOBAL DATAFRAME FROM TEST AND TRAIN
  if (!all(colnames(test) == colnames(train)))
    stop("All columns from test and train datasets must be exactly the same")
  global <- data.frame(test) %>% bind_rows(train) %>%
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
  # https://docs.h2o.ai/h2o/latest-stable/h2o-docs/variable-importance.html
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
    noimp <- dplyr::filter(imp, .data$importance < 0.015) %>% arrange(desc(.data$importance))
    if (nrow(noimp) > 0) {
      top10 <- noimp %>% ungroup() %>% slice(1:10)
      which <- vector2text(top10$variable, quotes = FALSE)
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
  
  # For performance metrics
  scores_test <- get_scores(
    predictions, test, 
    model_type = model_type, 
    target = target, 
    cats = cats)
  multis <- scores_test$multis
  scores <- scores_test$scores
  
  # # Used for train metrics
  # scores_train <- get_scores(
  #   predictions, train, 
  #   model_type = model_type, 
  #   target = target, cats = cats)
  # scores_tr <- scores_train$scores
  # multis_tr <- scores_train$multis
  # scores_train <- data.frame(tag = as.vector(train$tag), scores_tr)
  
  # GET ALL RESULTS INTO A LIST
  results <- list()
  results[["model"]] <- m
  results[["y"]] <- y
  results[["scores_test"]] <- data.frame(tag = as.vector(test$tag), scores)
  results[["metrics"]] <- model_metrics(
    tag = results$scores_test$tag, 
    score = results$scores_test$score,
    multis = multis,
    thresh = thresh,
    target = target,
    model_name = as.vector(m@model_id),
    plots = plots)
  # cvresults <- m@model$cross_validation_metrics_summary
  # results$metrics[["cv"]] <- as_tibble(
  #   data.frame(metric = rownames(cvresults), cvresults))
  if (model_type == "Classifier" & length(cats) == 2) 
    results$metrics[["max_metrics"]] <- data.frame(
      m@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores)
  if (!stacked) results[["importance"]] <- imp
  results[["datasets"]] <- list(
    global = as_tibble(global), 
    test = filter(global, .data$train_test == "test"))
  # results[["metrics_train"]] <- model_metrics(
  #   tag = scores_train$tag, 
  #   score = scores_train$score,
  #   multis = multis_tr,
  #   thresh = thresh,
  #   target = target,
  #   model_name = as.vector(m@model_id),
  #   type = "train")
  results[["scoring_history"]] <- as_tibble(m@model$scoring_history)
  results[["parameters"]] <- m@parameters
  results[["categoricals"]] <- list_cats(filter(global, .data$train_test == "train"))
  results[["type"]] <- model_type
  results[["split"]] <- split
  if (model_type == "Classifier") 
    results[["threshold"]] <- thresh
  results[["model_name"]] <- as.vector(m@model_id)
  results[["algorithm"]] <- m@algorithm
  if (any(c("H2OFrame","H2OAutoML") %in% class(h2o_object)))
    results[["leaderboard"]] <- h2o_object@leaderboard
  if (length(leaderboard) > 0)
    results[["leaderboard"]] <- leaderboard
  results[["project"]] <- project
  results[["y"]] <- y
  results[["ignored"]] <- ignored
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
  class(results) <- c("h2o_automl", class(results))
  
  return(results)
  
}

get_scores <- function(predictions, 
                       traintest, 
                       model_type,
                       target = "auto", 
                       cats) {
  
  type <- deparse(substitute(traintest))
  
  # Train or Test data
  nrows <- nrow(traintest)
  if (type == "test")
    scores <- predictions[1:nrows,]
  if (type == "train")
    scores <- predictions[(nrows+1):nrows,]
  
  # Selected target value
  if (target != "auto" & length(cats) == 2)
    scores <- target_set(tag = as.vector(traintest$tag), 
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
      multis <- select(scores, -.data$score)
    }
  } else {
    scores <- data.frame(score = as.vector(scores))
  } 
  ret <- list(scores = scores, multis = multis)
  return(ret)
}


####################################################################
#' Select Model from h2o_automl's Leaderboard
#' 
#' Select wich model from the h2o_automl function to use
#' 
#' @family Machine Learning
#' @family Tools
#' @inheritParams h2o_automl
#' @param results \code{h2o_automl()} object.
#' @param which_model Integer. Which model from the leaderboard you wish to use?
#' @export
h2o_selectmodel <- function(results, which_model = 1, quiet = FALSE, ...) {
  
  check_attr(results, attr = "type", check = "h2o_automl")
  
  # Select model (Best one by default)
  ntop <- nrow(results$leaderboard)
  if (which_model > ntop)
    stop("Select a valid model ID. Range: 1 to ", ntop)
  model_id <- as.vector(results$leaderboard$model_id[which_model])
  if (!quiet) message("Model selected: ", model_id)
  m <- h2o.getModel(model_id)  
  d <- results$datasets
  
  # Calculate everything
  output <- h2o_results(
    m, 
    test = d$test, 
    train = d$global[d$global$train_test=="train",], 
    y = results$y, 
    which = which_model, 
    model_type = results$type, 
    project = results$project, 
    leaderboard = results$leaderboard,
    seed = results$seed, 
    quiet = TRUE,
    ...)
  
  if (!quiet) print(output)
  return(output)
}


####################################################################
#' Export h2o_automl's Results
#' 
#' Export RDS, TXT, POJO, MOJO and all results from \code{h2o_automl()}.
#' 
#' @family Machine Learning
#' @family Tools
#' @param results Object. h2o_automl output from \code{h2o_automl()}
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
          results$metrics$metrics_tags else "NA",
        "Train's Confusion Matrix" = if (length(results$metrics$confusion_matrix) > 1)
          results$metrics$confusion_matrix else "NA",
        "Variables Importance" = results$importance,
        "H2O Global Results" = results$model,
        "Leaderboard" = results$leaderboard,
        "10 Scoring examples" = sample_n(results$datasets$global, 10),
        "H20 Version" = results$h2o)
      if (is.na(note)[1]) results_txt$Note <- NULL
      capture.output(results_txt, file = paste0(dir, "/", name, ".txt"))
      cats <- lapply(results$categoricals, data.frame)
      aux <- cats[names(cats)[!names(cats) %in% results$ignored]]
      capture.output(aux, file = paste0(dir, "/", name, "_cats.txt"))
      message(">>> Summary text files saved...")
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
#' @examples 
#' data(dft) # Titanic dataset
#' splits <- msplit(dft, size = 0.7, seed = 123)
#' names(splits)
#' @export
msplit <- function(df, size = 0.7, seed = 0, print = TRUE) {
  
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
#' Iterate Seeds on AutoML
#' 
#' This functions lets the user iterate and search for best seed. Note that if
#' the results change a lot, you are having a high variance in your data.
#' 
#' @family Machine Learning
#' @inheritParams h2o_automl
#' @param tries Integer. Number of iterations
#' @param ... Additional arguments passed to \code{h2o_automl}
#' @export
iter_seeds <- function(df, y, tries = 10, ...) {
  seeds <- data.frame()
  for (i in 1:tries) {
    model <- h2o_automl(df, y, seed = i, quiet = TRUE, ...)
    seeds <- rbind(seeds, cbind(seed = i, model$metrics$metrics))
    seeds <- arrange(seeds, desc(2))
    statusbar(i, tries, seeds[1,1])
  }
  return(seeds)
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
  df <- mutate_if(df, is.logical, as.character)
  file <- listfiles(model_path, regex = ".zip")$filename
  zip <- paste0(model_path,"/",as.character(file))
  
  aux <- ceiling(nrow(df)/batch)
  df$aux <- rep(1:aux, each = batch)[1:nrow(df)]
  output <- c()
  for (i in 1:aux) {
    dfi <- select(df[df$aux == i,], -.data$aux)
    json <- toJSON(dfi)
    size <- nchar(json)
    if (size > 250000)
      stop(paste("JSON batch is too long. Please, try with a smaller 'batch' parameter.",
                 "Suggested size:", round(batch * 235000 / size)))
    res <- h2o.predict_json(zip, json, labels = TRUE)  
    if ("error" %in% names(res)) {
      message("\nERROR: There was an issue with one of the inputs to predict...")
      writeLines(res$error)
      break
    }
    output <- rbind(output, res)
    if (aux > 1) statusbar(i, aux, i * batch)
  }
  if ("classProbabilities" %in% names(output)) {
    aux <- flatten_list(output$classProbabilities, quiet = TRUE)
    colnames(aux) <- output$responseDomainValues[[1]]
    output <- cbind(output[,c(1,2)], aux)
  }
  return(as_tibble(output))
}

flatten_list <- function(x, quiet = FALSE) {
  n <- length(x)
  for (i in 1:n) {
    if (i == 1) ret <- c()
    values <- unlist(x[[i]])
    aux <- data.frame(t(values))
    ret <- suppressWarnings(bind_rows(ret, aux))
    if (n > 500 & !quiet) statusbar(i, n, i)
    if (i == n) ret <- as_tibble(ret)
  }  
  return(ret)
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
      encode = "json")
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
#' Set Target Value in Target Variable
#' 
#' This function detects or forces the target value when predicting
#' a categorical binary model. This is an auxiliary function.
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
  means <- df %>% group_by(.data$tag) %>% summarise(mean = mean(.data$score))
  auto <- means$tag[means$mean == max(means$mean)]
  if (target == "auto")
    target <- auto
  if (!target %in% unique(df$tag))
    stop(paste("Your target value", target, "is not valid.",
               "Possible other values:", vector2text(unique(df$tag))))
  if (!quiet) message(paste("Target value:", target)) 
  # If the forced target value is the "lower scores" value, invert scores
  if (auto != target) df$score <- df$score * (-1) + 1
  ret <- list(df = df, which = target)
  return(ret)
}
