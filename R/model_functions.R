####################################################################
#' Automated H2O's AutoML
#'
#' This function lets the user create a robust and fast model, using
#' H2O's AutoML function. The result is a list with the best model,
#' its parameters, datasets, performance metrics, variables
#' importance, and plots. Read more about the \code{h2o_automl()} pipeline
#' \href{https://laresbernardo.github.io/lares/articles/h2o_automl.html}{here}.
#'
#' @section List of algorithms:
#' \href{https://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html}{-> Read more here}
#' \describe{
#'   \item{DRF}{Distributed Random Forest, including Random Forest (RF)
#'   and Extremely-Randomized Trees (XRT)}
#'   \item{GLM}{Generalized Linear Model}
#'   \item{XGBoost}{eXtreme Grading Boosting}
#'   \item{GBM}{Gradient Boosting Machine}
#'   \item{DeepLearning}{Fully-connected multi-layer artificial neural network}
#'   \item{StackedEnsemble}{Stacked Ensemble}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{print}{Use \code{print} method to print models stats and summary}
#'   \item{plot}{Use \code{plot} method to plot results using \code{mplot_full()}}
#' }
#'
#' @family Machine Learning
#' @inheritParams h2o::h2o.automl
#' @inheritParams get_mp3
#' @param df Dataframe. Dataframe containing all your data, including
#' the dependent variable labeled as \code{'tag'}. If you want to define
#' which variable should be used instead, use the \code{y} parameter.
#' @param y Variable or Character. Name of the dependent variable or response.
#' @param ignore Character vector. Force columns for the model to ignore
#' @param train_test Character. If needed, \code{df}'s column name with 'test'
#' and 'train' values to split
#' @param split Numeric. Value between 0 and 1 to split as train/test
#' datasets. Value is for training set. Set value to 1 to train with all
#' available data and test with same data (cross-validation will still be
#' used when training). If \code{train_test} is set, value will be overwritten
#' with its real split rate.
#' @param weight Column with observation weights. Giving some observation a
#' weight of zero is equivalent to excluding it from the dataset; giving an
#' observation a relative weight of 2 is equivalent to repeating that
#' row twice. Negative weights are not allowed.
#' @param target Value. Which is your target positive value? If
#' set to \code{'auto'}, the target with largest \code{mean(score)} will be
#' selected. Change the value to overwrite. Only used when binary
#' categorical model.
#' @param balance Boolean. Auto-balance train dataset with under-sampling?
#' @param impute Boolean. Fill \code{NA} values with MICE?
#' @param no_outliers Boolean/Numeric. Remove \code{y}'s outliers from the dataset?
#' Will remove those values that are farther than n standard deviations from
#' the dependent variable's mean (Z-score). Set to \code{TRUE} for default (3)
#' or numeric to set a different multiplier.
#' @param unique_train Boolean. Keep only unique row observations for training data?
#' @param center,scale Boolean. Using the base function scale, do you wish
#' to center and/or scale all numerical values?
#' @param thresh Integer. Threshold for selecting binary or regression
#' models: this number is the threshold of unique values we should
#' have in \code{'tag'} (more than: regression; less than: classification)
#' @param seed Integer. Set a seed for reproducibility. AutoML can only
#' guarantee reproducibility if max_models is used because max_time is
#' resource limited.
#' @param max_models,max_time Numeric. Max number of models and seconds
#' you wish for the function to iterate. Note that max_models guarantees
#' reproducibility and max_time not (because it depends entirely on your
#' machine's computational characteristics)
#' @param start_clean Boolean. Erase everything in the current h2o
#' instance before we start to train models? You may want to keep other models
#' or not. To group results into a custom common AutoML project, you may
#' use \code{project_name} argument.
#' @param exclude_algos,include_algos Vector of character strings. Algorithms
#' to skip or include during the model-building phase. Set NULL to ignore.
#' When both are defined, only \code{include_algos} will be valid.
#' @param plots Boolean. Create plots objects?
#' @param alarm Boolean. Ping (sound) when done. Requires \code{beepr}.
#' @param print Boolean. Print summary when process ends?
#' @param save Boolean. Do you wish to save/export results into your
#' working directory?
#' @param subdir Character. In which directory do you wish to save
#' the results? Working directory as default.
#' @param project Character. Your project's name
#' @param ... Additional parameters on \code{h2o::h2o.automl}
#' @return List. Trained model, predicted scores and datasets used, performance
#' metrics, parameters, importance data.frame, seed, and plots when \code{plots=TRUE}.
#' @examples
#' \dontrun{
#' # CRAN
#' data(dft) # Titanic dataset
#' dft <- subset(dft, select = -c(Ticket, PassengerId, Cabin))
#'
#' # Classification: Binomial - 2 Classes
#' r <- h2o_automl(dft, y = Survived, max_models = 1, impute = FALSE, target = "TRUE", alarm = FALSE)
#'
#' # Let's see all the stuff we have inside:
#' lapply(r, names)
#'
#' # Classification: Multi-Categorical - 3 Classes
#' r <- h2o_automl(dft, Pclass, ignore = c("Fare", "Cabin"), max_time = 30, plots = FALSE)
#'
#' # Regression: Continuous Values
#' r <- h2o_automl(dft, y = "Fare", ignore = c("Pclass"), exclude_algos = NULL, quiet = TRUE)
#' print(r)
#'
#' # WITH PRE-DEFINED TRAIN/TEST DATAFRAMES
#' splits <- msplit(dft, size = 0.8)
#' splits$train$split <- "train"
#' splits$test$split <- "test"
#' df <- rbind(splits$train, splits$test)
#' r <- h2o_automl(df, "Survived", max_models = 1, train_test = "split")
#' }
#' @export
h2o_automl <- function(df, y = "tag",
                       ignore = NULL,
                       train_test = NA,
                       split = 0.7,
                       weight = NULL,
                       target = "auto",
                       balance = FALSE,
                       impute = FALSE,
                       no_outliers = TRUE,
                       unique_train = TRUE,
                       center = FALSE,
                       scale = FALSE,
                       thresh = 10,
                       seed = 0,
                       nfolds = 5,
                       max_models = 3,
                       max_time = 10 * 60,
                       start_clean = FALSE,
                       exclude_algos = c("StackedEnsemble", "DeepLearning"),
                       include_algos = NULL,
                       plots = TRUE,
                       alarm = TRUE,
                       quiet = FALSE,
                       print = TRUE,
                       save = FALSE,
                       subdir = NA,
                       project = "AutoML Results",
                       verbosity = NULL,
                       ...) {
  try_require("h2o")
  tic(id = "h2o_automl")
  on.exit(toc(id = "h2o_automl", msg = "Process duration:", quiet = quiet))

  if (!quiet) message(paste(Sys.time(), "| Started process..."))

  quiet(h2o.init(nthreads = -1, port = 54321))

  df <- as.data.frame(df)
  y <- gsub('"', "", as_label(enquo(y)))

  # PROCESS THE DATA
  processed <- model_preprocess(
    df,
    y = y,
    train_test = train_test,
    split = split,
    weight = weight,
    target = target,
    balance = balance,
    impute = impute,
    no_outliers = no_outliers,
    unique_train = unique_train,
    center = center,
    scale = scale,
    thresh = thresh,
    seed = seed,
    quiet = quiet
  )

  # PROCESSED DATA: TRAIN AND TEST
  df <- processed$data
  train <- df[processed$train_index, ]
  test <- df[-processed$train_index, ]
  if (nrow(test) == 0) test <- train

  # MODEL TYPE (based on inputs + thresh value)
  model_type <- processed$model_type

  # ALGORITHMS
  if (length(exclude_algos) > 0 && length(include_algos) == 0 && !quiet) {
    message(paste("- ALGORITHMS: excluded", vector2text(exclude_algos)))
  }
  if (length(include_algos) > 0 && !quiet) {
    message(paste("- ALGORITHMS: included", vector2text(include_algos)))
    exclude_algos <- NULL
  }

  # START FRESH?
  if (!quiet && !isTRUE(start_clean)) {
    message(sprintf(
      paste(
        "- CACHE: Previous models %s being erased.",
        "You may use 'start_clean' [clear] or 'project_name' [join]"
      ),
      ifelse(start_clean, "are", "are not")
    ))
  }
  if (start_clean) quiet(h2o.removeAll())

  # INFORMATIVE MSG ON FLOW's UI
  flow <- "http://localhost:54321/flow/index.html"
  if (!quiet && Sys.getenv("HOSTNAME") == "") {
    message("- UI: You may check results using H2O Flow's interactive platform: ", flow)
  }

  # RUN AUTOML
  if (!quiet) {
    message(sprintf(">>> Iterating until %s models or %s seconds...", max_models, max_time))
  }
  training <- .quiet_h2o(as.h2o(train), quiet = TRUE)
  aml <- .quiet_h2o(h2o.automl(
    x = colnames(df)[!colnames(df) %in% c("tag", ignore)],
    y = "tag",
    training_frame = training,
    weights_column = weight,
    max_runtime_secs = max_time,
    max_models = max_models,
    exclude_algos = exclude_algos,
    include_algos = include_algos,
    nfolds = nfolds,
    # project_name = project,
    seed = seed,
    verbosity = verbosity,
    ...
  ), quiet = quiet)

  if (nrow(aml@leaderboard) == 0) {
    warning("NO MODELS TRAINED. Please set max_models to at least 1 and increase max_time")
  } else {
    if (!is.nan(aml@leaderboard[1, 2])) {
      if (!quiet) {
        message(paste("- EUREKA: Succesfully generated", nrow(aml@leaderboard), "models"))
        if (print) print(head(aml@leaderboard, 3))
      }
    }
  }

  # GET RESULTS AND PERFORMANCE
  results <- h2o_results(
    aml, test, train, y,
    which = 1,
    model_type = model_type,
    target = target,
    split = split,
    plots = plots,
    project = project,
    ignore = ignore,
    seed = seed,
    quiet = quiet
  )

  if (save) {
    export_results(results, subdir = subdir, thresh = thresh)
    if (!quiet) message("- EXPORT: Results and model files exported succesfully!")
  }

  if (!quiet && print) print(results)

  if (alarm && !quiet) {
    try_require("beepr", stop = FALSE)
    try(beep())
  }

  attr(results, "type") <- "h2o_automl"
  results
}

#' @rdname h2o_automl
#' @aliases h2o_automl
#' @param x h2o_automl object
#' @export
plot.h2o_automl <- function(x, ...) {
  if (!inherits(x, "h2o_automl")) {
    stop("Object must be class h2o_automl")
  }
  if ("plots" %in% names(x)) {
    x$plots$dashboard
  } else {
    invisible(mplot_full(
      tag = x$scores_test$tag,
      score = x$scores_test$score,
      multis = select(x$scores_test, -.data$tag, -.data$score)
    ))
  }
}

#' @rdname h2o_automl
#' @aliases h2o_automl
#' @param importance Boolean. Print important variables?
#' @export
print.h2o_automl <- function(x, importance = TRUE, ...) {
  if (!inherits(x, "h2o_automl")) {
    stop("Object must be class h2o_automl")
  }
  aux <- list()
  selected <- which(as.vector(x$leaderboard$model_id) == x$model_name)
  n_models <- nrow(x$leaderboard)
  data_points <- nrow(x$datasets$global)
  split <- round(100 * x$split)

  if (x$type == "Classification") {
    cats <- filter(x$datasets$global, grepl("train", .data$train_test)) %>%
      .[, x$y] %>%
      unique() %>%
      nrow()
    x$type <- sprintf("%s (%s classes)", x$type, cats)
  }

  aux[["met"]] <- glued(
    "Test metrics:
{v2t({met}, sep = '\n', quotes = FALSE)}",
    met = paste(
      "  ",
      names(x$metrics$metrics), "=",
      signif(x$metrics$metrics, 5)
    )
  )

  if ("importance" %in% names(x) && importance == TRUE) {
    if (nrow(x$importance) > 0) {
      aux[["imp"]] <- glued(
        "Most important variables:
{v2t({imp}, sep = '\n', quotes = FALSE)}",
        imp = paste(
          "  ",
          x$importance %>% head(5) %>%
            mutate(label = sprintf(
              "%s (%s)",
              .data$variable,
              formatNum(100 * .data$importance, 1, pos = "%")
            )) %>%
            pull(.data$label)
        )
      )
    }
  }

  print(glued("
    Model ({selected}/{n_models}): {x$model_name}
    Dependent Variable: {x$y}
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
#' @param model_type Character. Select "Classification" or "Regression"
#' @param ignore Character vector. Columns too ignore
#' @param leaderboard H2O's Leaderboard. Passed when using
#' \code{h2o_selectmodel} as it contains plain model and no leader board.
#' @return List. Trained model, predicted scores and datasets used, performance
#' metrics, parameters, importance data.frame, seed, and plots when \code{plots=TRUE}.
#' @export
h2o_results <- function(h2o_object, test, train, y = "tag", which = 1,
                        model_type, target = "auto", split = 0.7,
                        ignore = NULL, quiet = FALSE,
                        project = "ML Project", seed = 0,
                        leaderboard = list(),
                        plots = TRUE,
                        ...) {
  # MODEL TYPE
  types <- c("Classification", "Regression")
  check_opts(model_type, types)
  thresh <- ifelse(model_type == types[1], 10000, 0)

  # When using h2o_select
  if ("train_test" %in% colnames(test)) {
    colnames(test)[colnames(test) == y] <- "tag"
    colnames(train)[colnames(train) == y] <- "tag"
    test <- test[, 1:(which(colnames(test) == "train_test") - 1)]
    train <- train[, 1:(which(colnames(train) == "train_test") - 1)]
    split <- round(nrow(train) / (nrow(train) + nrow(test)), 2)
  }

  # GLOBAL DATAFRAME FROM TEST AND TRAIN
  if (!all(colnames(train) %in% colnames(test))) {
    stop("All columns from train data must be present on test data as well!")
  }
  if (split == 1) {
    global <- train %>% mutate(train_test = "train_test")
  } else {
    global <- data.frame(test) %>%
      bind_rows(train) %>%
      mutate(train_test = c(rep("test", nrow(test)), rep("train", nrow(train))))
  }
  colnames(global)[colnames(global) == "tag"] <- y
  if (model_type == "Classification") {
    cats <- unique(global[, colnames(global) == y])
  } else {
    cats <- "None"
  }

  # SELECT MODEL FROM h2o_automl()
  if (any(c("H2OFrame", "H2OAutoML") %in% class(h2o_object))) {
    # Note: Best model from leaderboard is which = 1
    m <- h2o.getModel(as.vector(h2o_object@leaderboard$model_id[which]))
    if (!quiet) message(paste("SELECTED MODEL:", as.vector(m@model_id)))
  } else {
    m <- h2o_object
  }

  # VARIABLES IMPORTANCES
  # https://docs.h2o.ai/h2o/latest-stable/h2o-docs/variable-importance.html
  if (sum(grepl("Stacked", as.vector(m@model_id))) > 0) {
    stacked <- TRUE
    if (!quiet) message("- NOTE: No importance features for Stacked Ensemble Models")
  } else {
    stacked <- FALSE
  }
  if (!stacked) {
    imp <- data.frame(h2o.varimp(m)) %>%
      {
        if ("names" %in% colnames(.)) {
          rename(., "variable" = "names", "importance" = "coefficients")
        } else {
          .
        }
      } %>%
      {
        if ("percentage" %in% colnames(.)) {
          rename(., "importance" = "percentage")
        } else {
          .
        }
      }
    noimp <- if (nrow(imp) > 0) {
      dplyr::filter(imp, .data$importance < 1 / (nrow(imp) * 4)) %>%
        arrange(desc(.data$importance))
    } else {
      imp
    }
    if (nrow(noimp) > 0) {
      topn <- noimp %>%
        ungroup() %>%
        slice(1:8)
      which <- vector2text(topn$variable, quotes = FALSE)
      if (nrow(noimp) > 8) {
        which <- paste(which, "and", nrow(noimp) - 8, "other...")
      }
      if (!quiet) {
        message(paste("- NOTE: The following variables were the least important:", which))
      }
    }
  }

  # GET PREDICTIONS
  if (!quiet) message(paste0(">>> Running predictions for ", y, "..."))
  predictions <- .quiet_h2o(h2o_predict_model(global, m), quiet = TRUE)
  global <- cbind(global, predictions)
  # Change dots for space
  if (sum(grepl(" ", cats)) > 0) {
    colnames(global) <- str_replace_all(colnames(global), "\\.", " ")
  }

  # For performance metrics
  scores_test <- .get_scores(
    predictions, test,
    model_type = model_type,
    target = target,
    cats = cats
  )
  multis <- scores_test$multis
  scores <- scores_test$scores

  # # Used for train metrics
  # scores_train <- .get_scores(
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
    plots = plots
  )
  cvresults <- m@model$cross_validation_metrics_summary
  if (!is.null(cvresults)) {
    results$metrics[["cv_metrics"]] <- as_tibble(
      data.frame(
        metric = rownames(cvresults),
        mutate_all(cvresults, list(~ as.numeric(as.character(.))))
      )
    )
  }
  if (model_type == "Classification") {
    if (length(cats) == 2) {
      results$metrics[["max_metrics"]] <- data.frame(
        m@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores
      )
    }
    if (length(cats) > 2) {
      results$metrics[["hit_ratio"]] <- data.frame(
        m@model$cross_validation_metrics@metrics$hit_ratio_table
      )
    }
  }
  # results[["parameters"]] <- m@parameters[
  #   sapply(m@parameters, function(x) length(x) == 1)] %>%
  #   bind_rows() %>% tidyr::gather(key = "parameter")
  results[["parameters"]] <- m@parameters
  if (!stacked) results[["importance"]] <- imp
  results[["datasets"]] <- list(
    global = as_tibble(global),
    test = filter(global, grepl("test", .data$train_test))
  )
  # results[["metrics_train"]] <- model_metrics(
  #   tag = scores_train$tag,
  #   score = scores_train$score,
  #   multis = multis_tr,
  #   thresh = thresh,
  #   target = target,
  #   model_name = as.vector(m@model_id),
  #   type = "train")
  results[["scoring_history"]] <- as_tibble(m@model$scoring_history)
  results[["categoricals"]] <- list_cats(filter(global, grepl("train", .data$train_test)))
  results[["type"]] <- model_type
  results[["split"]] <- split
  if (model_type == "Classification") {
    results[["threshold"]] <- thresh
  }
  results[["model_name"]] <- as.vector(m@model_id)
  results[["algorithm"]] <- m@algorithm
  if (any(c("H2OFrame", "H2OAutoML") %in% class(h2o_object))) {
    results[["leaderboard"]] <- h2o_object@leaderboard
  }
  if (length(leaderboard) > 0) {
    results[["leaderboard"]] <- leaderboard
  }
  results[["project"]] <- project
  results[["y"]] <- y
  results[["ignored"]] <- ignore
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
      plot = FALSE
    )
    plots[["metrics"]] <- results$metrics$plots
    results$metrics$plots <- NULL
    if (length(multis) > 1) {
      plots[["top_cats"]] <- mplot_topcats(
        tag = results$scores_test$tag,
        score = results$scores_test$score,
        multis = multis,
        model_name = results$model_name
      )
    }
    if (!stacked) {
      plots[["importance"]] <- mplot_importance(
        var = results$importance$variable,
        imp = results$importance$importance,
        model_name = results$model_name,
        subtitle = results$project
      )
    }
    plots <- append(plots, rev(as.list(results$metrics$plots)))
    results$plots <- plots
  }

  attr(results, "type") <- "h2o_automl"
  class(results) <- c("h2o_automl", class(results))
  results
}

.get_scores <- function(predictions,
                        traintest,
                        model_type,
                        target = "auto",
                        cats) {
  type <- deparse(substitute(traintest))

  # Train or Test data
  nrows <- nrow(traintest)
  if (type == "test") {
    scores <- predictions[1:nrows, ]
  }
  if (type == "train") {
    scores <- predictions[(nrows + 1):nrows, ]
  }

  # Selected target value
  if (target != "auto" && length(cats) == 2) {
    scores <- target_set(
      tag = as.vector(traintest$tag),
      score = scores[, 2],
      target = target,
      quiet = TRUE
    )$df
  }

  # Multis object and standard predictions output
  multis <- NA
  if (model_type == "Classification") {
    if (length(cats) == 2) {
      scores <- select_if(scores, is.numeric) %>% .[, 1]
    } else {
      colnames(scores)[1] <- "score"
      multis <- select(scores, -.data$score)
    }
  } else {
    scores <- data.frame(score = as.vector(scores))
  }
  list(scores = scores, multis = multis)
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
#' @return H2O processed model
#' @export
h2o_selectmodel <- function(results, which_model = 1, quiet = FALSE, ...) {
  check_attr(results, attr = "type", check = "h2o_automl")

  # Select model (Best one by default)
  ntop <- nrow(results$leaderboard)
  if (which_model > ntop) {
    stop("Select a valid model ID. Range: 1 to ", ntop)
  }
  model_id <- as.vector(results$leaderboard$model_id[which_model])
  if (!quiet) message("Model selected: ", model_id)
  m <- h2o.getModel(model_id)
  d <- results$datasets

  # Calculate everything
  output <- h2o_results(
    m,
    test = d$test,
    train = filter(d$global, grepl("test", .data$train_test)),
    y = results$y,
    which = which_model,
    model_type = results$type,
    project = results$project,
    leaderboard = results$leaderboard,
    seed = results$seed,
    quiet = TRUE,
    ...
  )

  if (!quiet) print(output)
  output
}


####################################################################
#' Export h2o_automl's Results
#'
#' Export RDS, TXT, POJO, MOJO and all results from \code{h2o_automl()}.
#'
#' @family Machine Learning
#' @family Tools
#' @param results \code{h2o_automl} or \code{h2o} model
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
#' @param seed Numeric. For reproducible results and random splits.
#' @return No return value, called for side effects.
#' @export
export_results <- function(results,
                           thresh = 10,
                           which = c(
                             "txt", "csv", "rds",
                             "binary", "mojo", "plots",
                             "dev", "production"
                           ),
                           note = NA,
                           subdir = NA,
                           save = TRUE,
                           seed = 0) {
  if (save) {
    try_require("h2o")
    quiet(h2o.init(nthreads = -1, port = 54321))

    pass <- !is.null(attr(results, "type"))
    if (!pass) results <- list(model = results)
    stopifnot(grepl("H2O", class(results$model)))
    name <- ifelse(pass, results$model_name, results$model@model_id)
    subdir <- paste0(ifelse(is.na(subdir), "", subdir), "/", name)
    if (substr(subdir, 1, 1) == "/") subdir <- substr(subdir, 2, nchar(subdir))

    # Directory to save all our results
    dir <- file.path(subdir)
    message(paste("Export directory:", dir))
    if (!dir.exists(dir)) {
      message("Creating directory: ", subdir)
      dir.create(dir, recursive = TRUE)
    }

    if ("dev" %in% which) which <- unique(c(which, "txt", "csv", "rds"))
    if ("production" %in% which) which <- unique(c(which, "binary", "mojo"))

    if ("txt" %in% which || !is.na(note)[1] && pass) {
      on.exit(set.seed(seed))
      results_txt <- list(
        "Project" = results$project,
        "Note" = note,
        "Model Type" = results$type,
        "Algorithm" = results$algorithm,
        "Model name" = name,
        "Train/Test" = table(results$datasets$global$train_test),
        "Metrics Glossary" = results$metrics$dictionary,
        "Test Metrics" = results$metrics$metrics,
        "Test Metrics by labels" = if (length(results$metrics$metrics_tags) > 1) {
          results$metrics$metrics_tags
        } else {
          "NA"
        },
        "Test's Confusion Matrix" = if (length(results$metrics$confusion_matrix) > 1) {
          results$metrics$confusion_matrix
        } else {
          NULL
        },
        "Predicted Variable" = results$y,
        "Ignored Variables" = results$ignored,
        "Variables Importance" = results$importance,
        "H2O Global Results" = results$model,
        "Leaderboard" = results$leaderboard,
        "Data examples" = data.frame(sample_n(results$datasets$global, 10)),
        "Seed" = results$seed,
        "H20 Version" = results$h2o
      )
      if (is.na(note)[1]) results_txt$Note <- NULL
      capture.output(results_txt, file = paste0(dir, "/", name, ".txt"))
      cats <- lapply(results$categoricals, data.frame)
      aux <- cats[names(cats)[!names(cats) %in% results$ignore]]
      capture.output(aux, file = paste0(dir, "/", name, "_cats.txt"))
      message(">>> Summary text files saved...")
    }

    # Export CSV with predictions and datasets
    if ("csv" %in% which && pass) {
      write.csv(results$datasets$global,
        paste0(dir, "/", name, ".csv"),
        row.names = FALSE
      )
      message(">>> CSV file exported...")
    }

    # Export Results List
    if ("rds" %in% which) {
      saveRDS(results, file = paste0(dir, "/", name, ".rds"))
      message(">>> RDS file exported...")
    }

    # Export Model as POJO && MOJO for Production
    if ("mojo" %in% which) {
      h2o.download_mojo(results$model, path = dir, get_genmodel_jar = TRUE)
      message(">>> MOJO (zip + jar files) exported...")
    }

    # if (pojo) h2o.download_pojo(results$model, path = dir)

    # Export Binary
    if ("binary" %in% which) {
      h2o.saveModel(results$model, path = dir, force = TRUE)
      message(">>> Binary file saved...")
    }

    if ("plots" %in% which && "plots" %in% names(results) && pass) {
      message(">>> Saving plots...")
      # Metrics plots
      aux <- results$plots$metrics
      for (i in seq_along(aux)) {
        export_plot(aux[[i]],
          name = names(aux)[i],
          width = 8, height = 6, res = 300,
          subdir = paste0(subdir, "/Plots"),
          quiet = TRUE
        )
      }
      # Other plots
      aux <- results$plots
      aux$metrics <- NULL
      for (i in seq_along(aux)) {
        export_plot(aux[[i]],
          name = names(aux)[i],
          width = 8, height = 6, res = 300,
          subdir = paste0(subdir, "/Plots"),
          quiet = TRUE
        )
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
#' @param df Dataframe
#' @param size Numeric. Split rate value, between 0 and 1. If set to
#' 1, the train and test set will be the same.
#' @param seed Integer. Seed for random split
#' @param print Boolean. Print summary results?
#' @return List with both datasets, summary, and split rate.
#' @examples
#' data(dft) # Titanic dataset
#' splits <- msplit(dft, size = 0.7, seed = 123)
#' names(splits)
#' @export
msplit <- function(df, size = 0.7, seed = 0, print = TRUE) {
  if (size <= 0 || size > 1) stop("Set size parameter to a value >0 and <=1")

  on.exit(set.seed(seed))
  df <- data.frame(df)
  if (size == 1) train <- test <- df

  if (size < 1 && size > 0) {
    ind <- sample(seq_len(nrow(df)), size = floor(size * nrow(df)))
    train <- df[ind, ]
    test <- df[-ind, ]
  } else {
    ind <- seq_len(nrow(df))
  }

  train_size <- dim(train)
  test_size <- dim(test)
  summary <- rbind(train_size, test_size)[, 1]

  if (print == TRUE) print(summary)

  list(
    train = train,
    test = test,
    summary = summary,
    split_size = size,
    train_index = ind
  )
}


####################################################################
#' Set Target Value in Target Variable
#'
#' This function detects or forces the target value when predicting
#' a categorical binary model. This is an auxiliary function.
#'
#' @inheritParams get_mp3
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param target Value. Which is your target positive value? If
#' set to 'auto', the target with largest mean(score) will be
#' selected. Change the value to overwrite. Only used when binary
#' categorical model.
#' @return List. Contains original data.frame \code{df} and
#' \code{which} with the target variable.
#' @export
target_set <- function(tag, score, target = "auto", quiet = FALSE) {
  df <- data.frame(tag = tag, score = score)

  # Validate inputs
  if (!is.numeric(score) || is.numeric(tag)) {
    stop("Your tag must be categorical. Your score must be numerical.")
  }

  # Get mean scores for each tag
  means <- df %>%
    group_by(.data$tag) %>%
    summarise(mean = mean(.data$score))
  auto <- means$tag[means$mean == max(means$mean)]
  if (length(auto) > 1) {
    auto <- if (any(auto %in% target)) target else auto[1]
  }
  if (target == "auto") {
    target <- auto
  }
  if (!target %in% unique(df$tag)) {
    stop(paste(
      "Your target value", target, "is not valid.",
      "Possible other values:", vector2text(unique(df$tag))
    ))
  }
  if (!quiet) message(paste("Target value:", target))
  # If the forced target value is the "lower scores" value, invert scores
  if (auto != target) df$score <- df$score * (-1) + 1
  list(df = df, which = target)
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
#' @return data.frame with performance results by seed tried on every row.
#' @export
iter_seeds <- function(df, y, tries = 10, ...) {
  seeds <- data.frame()
  for (i in 1:tries) {
    model <- h2o_automl(df, y, seed = i, quiet = TRUE, ...)
    seeds <- rbind(seeds, cbind(seed = i, model$metrics$metrics))
    seeds <- arrange(seeds, desc(2))
    statusbar(i, tries, seeds[1, 1])
  }
  seeds
}

.quiet_h2o <- function(..., quiet = TRUE) {
  if (quiet) on.exit(h2o.no_progress())
  x <- eval(...)
  h2o.show_progress()
  x
}
