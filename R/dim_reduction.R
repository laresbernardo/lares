####################################################################
#' Reduce Dimensionality with PCA
#'
#' Principal component analysis or (PCA) is a method we can use to
#' reduce high-dimensional data to a low-dimensional space. In other words,
#' we cannot accurately visualize high-dimensional datasets because
#' we cannot visualize anything above 3 features. The main purpose behind
#' PCA is to transform datasets with more than 3 features (high-dimensional)
#' into typically a 2/3 column dataset. Despite the reduction into a
#' lower-dimensional space we still can retain most of the variance or
#' information from our original dataset.
#'
#' @family Dimensionality
#' @family Clusters
#' @inheritParams clusterKmeans
#' @param n Integer. Number of dimensions to reduce to.
#' @param plot Boolean. Create plots?
#' @param ... Additional parameters passed to \code{stats::prcomp}
#' @return List with reduced dataframe and possible plots.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data("iris")
#' df <- subset(iris, select = c(-Species))
#' df$id <- seq_len(nrow(df))
#' reduce_pca(df, n = 3, ignore = "id")
#' @export
reduce_pca <- function(df, n = NULL, ignore = NULL,
                       comb = c(1, 2), quiet = FALSE,
                       plot = TRUE, ...) {
  if (isTRUE(is.na(ignore)[1])) ignore <- NULL
  ignore <- unique(ignore)
  df <- .prepare_reduce(df, ignore = ignore, ...)

  if (sum(is.na(df)) > 0) {
    if (!quiet) message("Replacing NA values with column's means...")
    df <- mutate_all(df, ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  }

  PCA <- list()
  temp <- select(df, -any_of(ignore))
  pca <- prcomp(temp, ...)
  PCA$pca_explained <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 4)
  PCA$pcadf <- data.frame(pca$x)[, PCA$pca_explained > 0.01]
  PCA$pcadf <- as_tibble(cbind(select(as.data.frame(df), any_of(ignore)), PCA$pcadf))
  if (!is.null(n)) PCA$pcadf <- PCA$pcadf[, 1:n]

  if (plot) {
    # How much variance is explained by n PCA features?
    pca_explained <- PCA$pca_explained
    PCA$plot_explained <- data.frame(id = seq_along(pca_explained)) %>%
      mutate(
        PC = factor(paste0("PC", .data$id),
          levels = paste0("PC", seq_along(pca_explained))
        ),
        amount = pca_explained
      ) %>%
      mutate(aux = cumsum(.data$amount)) %>%
      ggplot(aes(x = .data$id, y = .data$aux)) +
      geom_col(alpha = 0.9) +
      geom_path() +
      geom_point() +
      labs(
        title = "Principal Component Analysis",
        subtitle = "Percentage of Variation Explained by Components",
        y = "Cumulative variation explained [%]", x = "PC(i)"
      ) +
      theme_lares()

    # Two most relevant PCAs xy plot
    temp <- PCA$pcadf[, comb]
    colnames(temp) <- c("PC1", "PC2")
    explained <- formatNum(pca_explained, 1, pos = "%")
    PCA$plot <- ggplot(PCA$pcadf, aes(x = .data$PC1, y = .data$PC2)) +
      geom_point() +
      labs(
        title = "Dimensions reduction with PCA",
        x = sprintf("PCA Dimension %s", comb[1]),
        y = sprintf("PCA Dimension %s", comb[2]),
        caption = sprintf(
          "Explaining %s of the variance with 2 PCA:\nPC%s (%s), PC%s (%s)",
          formatNum(sum(pca_explained[1:2]), 1, pos = "%"),
          comb[1], explained[comb[1]], comb[2], explained[comb[2]]
        )
      ) +
      theme_lares(pal = 2)
  }
  PCA
}


####################################################################
#' Reduce Dimensionality with t-SNE
#'
#' t-SNE takes high-dimensional data and reduces it to a low-dimensional
#' graph (1-3 dimensions). Unlike PCA, t-SNE can reduce dimensions with
#' non-linear relationships. PCA attempts to draw the best fitting line
#' through the distribution. T-SNE calculates a similarity measure
#' based on the distance between points instead of trying to maximize variance.
#'
#' @family Dimensionality
#' @family Clusters
#' @inheritParams reduce_pca
#' @param ... Additional parameters passed to \code{Rtsne::Rtsne}
#' @return List with reduced dataframe and possible plots.
#' @examples
#' \dontrun{
#' data("iris")
#' df <- subset(iris, select = c(-Species))
#' df$id <- seq_len(nrow(df))
#' reduce_tsne(df, ignore = "id", max_iter = 800, perplexity = 20)
#' }
#' @export
reduce_tsne <- function(df, n = 2, ignore = NULL,
                        quiet = FALSE,
                        plot = TRUE, ...) {
  try_require("Rtsne")
  if (isTRUE(is.na(ignore)[1])) ignore <- NULL
  ignore <- unique(ignore)
  df <- .prepare_reduce(df, ignore = ignore, ...)

  tSNE <- list()

  temp <- select(df, -any_of(ignore))
  tSNE$tsne <- Rtsne(temp, dims = n, verbose = FALSE, ...)

  tSNE$tsne$df <- as_tibble(
    data.frame(
      select(df, any_of(ignore)),
      tSNE$tsne$Y,
      cost = tSNE$tsne$costs
    )
  )

  tSNE$tsne$Y <- tSNE$tsne$costs <- tSNE$tsne$N <- NULL

  if (plot && n >= 2) {
    tSNE$tsne$plot <- tSNE$tsne$df %>%
      ggplot(aes(x = .data$X1, y = .data$X2)) +
      geom_point() +
      labs(
        title = "Dimensions reduction with t-SNE",
        colour = "Cluster",
        x = "t-SNE Dimension 1", y = "t-SNE Dimension 2"
      ) +
      theme_lares(pal = 2)
  }
  tSNE$tsne
}

.prepare_reduce <- function(df, ignore = NULL, quiet = FALSE, norm = TRUE, ...) {
  df <- select(df, -any_of(zerovar(df)))
  df <- ohse(df, quiet = TRUE, ignore = ignore, ...)
  temp <- which(!colnames(df) %in% ignore)
  new_df <- distinct_at(df, temp, .keep_all = TRUE)
  df <- new_df
  if (sum(is.na(df)) > 0) {
    if (!quiet) message(">>> Replacing NA values with column's means...")
    df <- mutate_all(df, ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  }
  if (norm) {
    df <- df %>%
      transmute_if(is.numeric, normalize) %>%
      replace(., is.na(.), 0)
  }
  df
}
