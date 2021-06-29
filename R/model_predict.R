####################################################################
#' H2O Predict using MOJO file
#'
#' This function lets the user predict using the h2o .zip file
#' containing the MOJO files. Note that it works with the files
#' generated when using the function export_results()
#'
#' @family Machine Learning
#' @family Tools
#' @param df Dataframe. Data to pass to the model.
#' @param model_path Character. Relative path of directory
#' where your zip model file is. If multiple zip files are found,
#' first one found will be used.
#' @param method Character. One of "mojo" or "json".
#' @param batch Integer. Run n batches at a time for "json" method.
#' @return data.frame with predicted results.
#' @export
h2o_predict_MOJO <- function(df, model_path, method = "mojo", batch = 300) {
  quiet(h2o.init(nthreads = -1, port = 54321))

  files <- list.files(model_path)
  file <- files[endsWith(files, ".zip")][1]
  zip <- paste0(model_path, "/", file)

  if (method == "mojo") {
    df <- quiet(as.h2o(df))
    mojo_model <- quiet(h2o.import_mojo(zip))
    output <- quiet(as.data.frame(h2o.predict(mojo_model, df)))
  }

  if (method == "json") {
    df <- as.data.frame(df)
    df <- mutate_if(df, is.logical, as.character)
    aux <- ceiling(nrow(df) / batch)
    df$aux <- rep(1:aux, each = batch)[seq_len(nrow(df))]
    output <- NULL
    for (i in 1:aux) {
      dfi <- select(df[df$aux == i, ], -.data$aux)
      json <- toJSON(dfi)
      size <- nchar(json)
      if (size > 250000) {
        stop(paste(
          "JSON batch is too long. Please, try with a smaller 'batch' parameter.",
          "Suggested size:", round(batch * 235000 / size)
        ))
      }
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
      aux <- .flatten_list(output$classProbabilities, quiet = TRUE)
      colnames(aux) <- output$responseDomainValues[[1]]
      output <- cbind(output[, c(1, 2)], aux)
    }
  }
  return(as_tibble(output))
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
#' @param df Dataframe. Data to insert into the model.
#' @param model_path Character. Relative model path directory or zip file.
#' @param sample Integer. How many rows should the function predict?
#' @return vector with predicted results.
#' @export
h2o_predict_binary <- function(df, model_path, sample = NA) {
  message("Use of h2o_predict_MOJO instead highly recommended!")
  quiet(h2o.init(nthreads = -1, port = 54321))

  if (!right(model_path, 4) == ".zip") {
    binary <- paste(model_path, gsub(".*-", "", model_path), sep = "/")
  } else {
    binary <- model_path
  }

  model <- h2o.loadModel(binary)

  if (!is.na(sample)) df <- df[1:sample, ]

  score_binary <- as.vector(predict(model, as.h2o(df))[, 3])

  return(score_binary)
}

####################################################################
#' H2O Predict using H2O Model Object
#'
#' This function lets the user get scores from a H2O Model Object.
#'
#' @family Machine Learning
#' @family Tools
#' @param df Dataframe/Vector. Data to insert into the model.
#' @param model h2o model Object
#' @return data.frame with predicted results.
#' @export
h2o_predict_model <- function(df, model) {
  as.data.frame(predict(model, as.h2o(df)))
}


####################################################################
#' H2O Predict using API Service
#'
#' This function lets the user get the score from an API service
#'
#' @family Machine Learning
#' @family Tools
#' @param df Dataframe/Vector. Data to insert into the model.
#' @param api Character. API URL.
#' @param exclude Character. Name of the variables to exclude.
#' @return vector with predicted results.
#' @export
h2o_predict_API <- function(df, api, exclude = "tag") {
  post <- function(df, api) {
    df <- df %>%
      removenacols() %>%
      select(-one_of(exclude))
    x <- POST(
      api,
      add_headers("Content-Type" = "application/json"),
      body = as.list(df),
      encode = "json"
    )
    return(content(x)$probabilityToOne)
  }

  batch <- NULL
  for (i in seq_len(nrow(df))) {
    x <- df[i, ]
    score <- post(x, api)
    batch <- rbind(batch, score)
  }

  return(as.vector(batch))
}


.flatten_list <- function(x, quiet = FALSE) {
  n <- length(x)
  for (i in 1:n) {
    if (i == 1) ret <- NULL
    values <- unlist(x[[i]])
    aux <- data.frame(t(values))
    ret <- suppressWarnings(bind_rows(ret, aux))
    if (n > 500 & !quiet) statusbar(i, n, i)
    if (i == n) ret <- as_tibble(ret)
  }
  return(ret)
}
