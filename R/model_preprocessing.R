####################################################################
#' Automate Data Preprocess for Modeling
#'
#' Pre-process your data before training a model. This is the prior step
#' on the \code{h2o_automl()} function's pipeline. Enabling for
#' other use cases when wanting too  use any other framework, library,
#' or custom algorithm.
#'
#' @family Machine Learning
#' @inheritParams h2o_automl
#' @param y Character. Column name for independent variable.
#' @return List. Contains original data.frame \code{df}, an index
#' to identify which observations with be part of the train dataset
#' \code{train_index}, and which model type should be \code{model_type}.
#' @examples
#' data(dft) # Titanic dataset
#'
#' model_preprocess(dft, "Survived", balance = TRUE)
#'
#' model_preprocess(dft, "Fare", split = 0.5, scale = TRUE)
#'
#' model_preprocess(dft, "Pclass", ignore = c("Fare", "Cabin"))
#'
#' model_preprocess(dft, "Pclass", quiet = TRUE)
#' @export
model_preprocess <- function(df,
                             y = "tag",
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
                             quiet = FALSE) {
  tic(id = "model_preprocess")

  # INDEPENDENT VARIABLE
  if (!y %in% colnames(df)) {
    stop(paste(
      "You should have a 'tag' column in your data.frame or select",
      "an independent varialbe using the 'y' parameter."
    ))
  }
  if (!quiet) message(sprintf("- INDEPENDENT VARIABLE: %s", y))

  colnames(df)[colnames(df) == y] <- "tag"
  df <- data.frame(df) %>%
    filter(!is.na(.data$tag)) %>%
    mutate_if(is.character, as.factor)

  # MODEL TYPE
  cats <- unique(df$tag)
  model_type <- ifelse(length(cats) <= as.integer(thresh), "Classification", "Regression")
  if (!quiet) message("- MODEL TYPE: ", model_type)
  # Change spaces for dots as 'multis' arguments may not match
  if (model_type == "Classification") cats <- gsub(" ", ".", cats)
  # If y variables is named as one of the categories, prediction values will be a problem
  if (model_type == "Classification" && y %in% cats) {
    stop(paste(
      "Your y parameter can't be named as any of the labels used.",
      "Please, rename", y, "into a valid column name next such as",
      paste0(y, "_labels for example.")
    ))
  }

  # If target value is not an existing target value
  if (!target %in% cats && length(cats) == 2) {
    if (!target %in% c(cats, "auto")) {
      stop(paste(
        "Your target value", target, "is not valid.",
        "Possible other values:", vector2text(cats)
      ))
    }
  }
  # When might seem numeric but is categorical
  if (model_type == "Classification" && sum(grepl("^[0-9]", cats)) > 0) {
    df <- mutate(df, tag = as.factor(as.character(
      ifelse(grepl("^[0-9]", .data$tag), paste0("n_", .data$tag), as.character(.data$tag))
    )))
  }
  # When is regression should always be numerical
  if (model_type == "Regression") {
    df$tag <- as.numeric(df$tag)
  }
  # Show a summary of our tags
  if (model_type == "Classification" && !quiet) print(freqs(df, .data$tag))
  if (model_type == "Regression" && !quiet) print(summary(df$tag))

  # MISSING VALUES
  m <- missingness(df, summary = FALSE)
  if (!is.null(m)) {
    m <- mutate(m, label = paste0(.data$variable, " (", .data$missingness, "%)"))
    if (!quiet) {
      top10 <- m %>%
        ungroup() %>%
        slice(1:10)
      which <- vector2text(top10$label, quotes = FALSE)
      if (nrow(m) > 10) {
        which <- paste(which, "and", nrow(m) - 10, "other.")
      }
      message(paste0(
        "- MISSINGS: The following variables contain missing observations: ", which,
        if (!impute && !quiet) ". Consider using the impute parameter."
      ))
    }
    if (impute) {
      if (!quiet) message(paste(">>> Imputing", sum(m$missing), "missing values..."))
      df <- impute(df, seed = seed, quiet = TRUE)
    }
  } else if (!quiet) message("- MISSINGS: No missing values in your data")

  # IGNORED VARIABLES
  if (length(ignore) > 0) {
    ignore <- ignore[ignore %in% colnames(df)]
    if (length(ignore) > 0 && !quiet) {
      message(paste("- SKIPPED: Ignored variables for training models:", vector2text(ignore)))
    }
  }

  # ONE HOT SMART ENCODING
  temp <- df[, !colnames(df) %in% c("tag", ignore)]
  nums <- df_str(temp, "names", quiet = TRUE)$nums
  if (length(nums) != ncol(temp) && !quiet) {
    message(paste(
      "- CATEGORICALS: There are", ncol(temp) - length(nums), "non-numerical features.",
      "Consider using ohse() or equivalent prior to encode categorical variables."
    ))
  }

  # ADDITIONAL TRANSFORMATIONS
  if (scale || center && length(nums) > 0) {
    new <- data.frame(lapply(df[nums], function(x) scale(x, center = center, scale = scale)))
    colnames(new) <- nums
    df[nums] <- new
    msg <- ifelse(scale & center, "scaled and centered", ifelse(scale, "scaled", "centered"))
    if (!quiet) message(paste0("- TRANSFORMATIONS: All numerical features (", length(nums), ") were ", msg))
  }

  # OUTLIERS ON INDEPENDENT VARIABLE
  if (is.numeric(df$tag)) {
    thresh <- ifelse(is.numeric(no_outliers), no_outliers, 3)
    is_outlier <- outlier_zscore(df$tag, thresh = thresh)
    if (!quiet && !isTRUE(no_outliers)) {
      message(sprintf(
        "- OUTLIERS: %s (%s) of %s values are considered outliers (Z-Score: >%ssd). %s",
        formatNum(100 * sum(is_outlier) / nrow(df), 1, pos = "%"),
        formatNum(sum(is_outlier), 0), y, thresh,
        ifelse(no_outliers,
          paste(
            "Removing them from the dataset for better results.",
            "To keep them, set the 'no_outliers' parameter."
          ),
          "Consider using the 'no_outliers' parameter to remove them."
        )
      ))
    }
    if (no_outliers) df <- df[!is_outlier, ]
  }

  # SPLIT TRAIN AND TEST DATASETS
  if (is.na(train_test)) {
    if (!quiet) message(sprintf(">>> Splitting data: train = %s &&  test = %s", split, 1 - split))
    splits <- msplit(df, size = split, seed = seed, print = !quiet)
    train <- splits$train
    test <- splits$test
    train_index <- splits$train_index
  } else {
    # If we already have a default split for train and test (train_test)
    if (train_test %in% colnames(df)) {
      colnames(df)[colnames(df) == train_test] <- "train_test"
      if (all(unique(as.character(df$train_test)) %in% c("train", "test"))) {
        train <- filter(df, .data$train_test == "train")
        test <- filter(df, .data$train_test == "test")
        split <- nrow(train) / nrow(df)
        ignore <- c(ignore, train_test)
        train_index <- seq_len(nrow(train))
        if (!quiet) print(table(df$train_test))
      } else {
        stop("Your train_test column should have 'train' and 'test' values only!")
      }
    } else {
      stop(paste("There is no column named", train_test))
    }
  }

  # CHECK TRAINING DATASET
  if (unique_train) {
    train_rows <- nrow(train)
    train <- distinct(train)
    if (nrow(train) != train_rows && !quiet) {
      message(paste(
        "- REPEATED: There were", train_rows - nrow(train),
        "repeated rows which are being suppressed from the train dataset"
      ))
    }
  }
  if (nrow(train) > 10000 && !quiet) {
    message("- SAMPLE: Consider sampling or reduce the 'split' argument for faster results")
  }

  # BALANCE TRAINING SET
  if (model_type == "Classification" && balance) {
    total <- nrow(train)
    min <- freqs(train, .data$tag) %>%
      .$n %>%
      min(., na.rm = TRUE)
    train <- train %>%
      group_by(.data$tag) %>%
      sample_n(min)
    if (!quiet) {
      message(paste0(
        "- BALANCE: Training set balanced: ", min, " observations for each (", length(cats),
        ") category; using ", round(100 * nrow(train) / total, 2), "% of training data"
      ))
    }
  }

  # CHECK TRAIN/TEST VALUES
  if (model_type == "Classification") {
    if (!all(unique(df$tag) %in% unique(train$tag))) {
      warning(paste("You SHOULD train with all available tags:", vector2text(unique(df$tag))))
      temp <- df[df$tag %in% unique(train$tag), ]
      message(sprintf(paste(
        "- CHECK: excluded %s observations from your test set because",
        "those labels were not in your train set"
      ), nrow(df) - nrow(temp)))
      df <- temp
    }
    if (!all(unique(df$tag) %in% unique(test$tag))) {
      warning("You are training with tags that are not in your test set.")
    }
  }

  results <- list(data = df, train_index = train_index, model_type = model_type)
  attr(results, "type") <- "model_preprocess"
  toc(id = "model_preprocess", msg = "Pre-processed in", quiet = TRUE)
  return(invisible(results))
}
