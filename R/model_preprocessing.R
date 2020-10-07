####################################################################
#' Automate Data Preprocess for Modeling
#' 
#' Pre-process your data before training a model. This is the prior step
#' on the \code{h2o_automl()} function's pipeline. Enabling for 
#' other use cases when wanting too  use any other framework, library, 
#' or custom algorithm.
#'
#' @inheritParams h2o_automl
#' @export
model_preprocess <- function(df, 
                             y = "tag",
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
                             thresh = 10,
                             seed = 0,
                             quiet = FALSE) {
  
  # INDEPENDENT VARIABLE
  if (!y %in% colnames(df)) {
    stop(paste("You should have a 'tag' column in your data.frame or select",
               "an independent varialbe using the 'y' parameter."))
  }
  
  if (!quiet) message(sprintf("- Variable to predict: %s", y))
  
  colnames(df)[colnames(df) == y] <- "tag"
  df <- data.frame(df) %>% 
    filter(!is.na(.data$tag)) %>%
    mutate_if(is.character, as.factor)
  
  # MODEL TYPE
  cats <- unique(df$tag)
  model_type <- ifelse(length(cats) <= as.integer(thresh), "Classifier", "Regression")
  message("- Model type: ", model_type)
  # Change spaces for dots as `multis` arguments may not match
  if (model_type == "Classifier") cats <- gsub(" ", ".", cats)
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
      ifelse(grepl('^[0-9]', .data$tag), paste0("n_", .data$tag), as.character(.data$tag)))))
  # When is regression should always be numerical
  if (model_type == "Regression")
    df$tag <- as.numeric(df$tag)
  # Show a summary of our tags
  if (model_type == "Classifier" & !quiet) print(freqs(df, .data$tag))
  if (model_type == "Regression" & !quiet) print(summary(df$tag)) 
  
  # MISSING VALUES
  m <- missingness(df, summary = FALSE)
  if (!is.null(m)) {
    m <- mutate(m, label = paste0(.data$variable, " (", .data$missingness, "%)"))
    if (!quiet) {
      top10 <- m %>% ungroup() %>% slice(1:10)
      which <- vector2text(top10$label, quotes = FALSE)
      if (nrow(m) > 10)
        which <- paste(which, "and", nrow(m) - 10, "other.")
      message(paste0("- The following variables contain missing observations: ", which,
                     if (!impute & !quiet) ". Consider using the impute parameter."))
    }
    if (impute) {
      if (!quiet) message(paste(">>> Imputing", sum(m$missing), "missing values..."))
      df <- impute(df, seed = seed, quiet = TRUE)
    }
  } else if (!quiet) message("- No missing values in your data")
  
  # ONE HOT SMART ENCODING
  nums <- df_str(df, "names", quiet = TRUE)$nums
  if (length(nums) != ncol(df) & !quiet) {
    transformable <- ncol(df) - length(nums) - 
      sum(ignore %in% colnames(df)) - 
      as.integer(model_type == "Classifier")
    if (transformable > 0) message(paste(
      "- There are", transformable, "non-numerical features.",
      "Consider using ohse() prior for One Hot Smart Encoding your categorical variables."))
  }
  if (scale | center & length(nums) > 0) {
    new <- data.frame(lapply(df[nums], function(x) scale(x, center = center, scale = scale)))
    colnames(new) <- nums
    df[nums] <- new
    msg <- ifelse(scale & center, "scaled and centered", ifelse(scale, "scaled", "centered"))
    if (!quiet) message(paste0("- All numerical features (", length(nums), ") were ", msg))
  }
  
  # OUTLIERS ON INDEPENDENT VARIABLE
  if (is.numeric(df$tag)) {
    thresh <- ifelse(is.numeric(no_outliers), no_outliers, 3)
    is_outlier <- outlier_zscore(df$tag, thresh = thresh)
    if (!quiet & !isTRUE(no_outliers))
      message(sprintf(
        "- %s (%s) of %s values are considered outliers (Z-Score: >%ssd). %s",
        formatNum(100*sum(is_outlier)/nrow(df), 1, pos = "%"),
        formatNum(sum(is_outlier), 0), y, thresh,
        ifelse(no_outliers, 
               paste("Removing them from the dataset for better results.",
                     "To keep them, set the 'no_outliers' parameter."), 
               "Consider using the 'no_outliers' parameter to remove them.")))
    if (no_outliers) df <- df[!is_outlier,] 
  }
  
  # SPLIT TRAIN AND TEST DATASETS
  if (is.na(train_test)) {
    if (!quiet) message(">>> Splitting datasets...")
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
    message("- Consider sampling your dataset for faster results")
  
  # BALANCE TRAINING SET
  if (model_type == "Classifier" & balance) {
    total <- nrow(train)
    min <- freqs(train, .data$tag) %>% .$n %>% min(., na.rm = TRUE)
    train <- train %>% group_by(.data$tag) %>% sample_n(min)
    if (!quiet) message(paste0(
      "- Training set balanced: ", min, " observations for each (",length(cats),
      ") category; using ", round(100*nrow(train)/total, 2), "% of training data"))
  }
  
  # CHECK TRAIN/TEST VALUES
  if (model_type == "Classifier") {
    if (!all(unique(df$tag) %in% unique(train$tag)))
      stop(paste("You must train with all available tags:", vector2text(unique(df$tag))))
    if (!all(unique(df$tag) %in% unique(test$tag)))
      warning("You are training with tags that are not in your test set.")  
  }
  
  return(invisible(list(data = df, test = test, train = train, model_type = model_type)))
  
}
