####################################################################
#' Automated K-Means Clustering + PCA/t-SNE
#'
#' This function lets the user cluster a whole data.frame automatically.
#' As you might know, the goal of kmeans is to group data points into
#' distinct non-overlapping subgroups. If needed, one hot encoding will
#' be applied to categorical values automatically with this function.
#' For consideration: Scale/standardize the data when applying kmeans.
#' Also, kmeans assumes spherical shapes of clusters and does not work well
#' when clusters are in different shapes such as elliptical clusters.
#'
#' @family Clusters
#' @inheritParams stats::kmeans
#' @param df Dataframe
#' @param k Integer. Number of clusters
#' @param wss_var Numeric. Used to pick automatic \code{k} value,
#' when \code{k} is \code{NULL} based on WSS variance while considering
#' \code{limit} clusters. Values between (0, 1). Default value could be
#' 0.05 to consider convergence.
#' @param limit Integer. How many clusters should be considered?
#' @param drop_na Boolean. Should NA rows be removed?
#' @param ignore Character vector. Names of columns to ignore.
#' @param ohse Boolean. Do you wish to automatically run one hot
#' encoding to non-numerical columns?
#' @param norm Boolean. Should the data be normalized?
#' @param dim_red Character. Select dimensionality reduction technique.
#' Pass any of: \code{c("PCA", "tSNE", "all", "none")}.
#' @param comb Vector. Which columns do you wish to plot? Select which
#' two variables by name or column position.
#' @param seed Numeric. Seed for reproducibility
#' @param quiet Boolean. Keep quiet? If not, print messages.
#' @param ... Additional parameters to pass sub-functions.
#' @return List. If no \code{k} is provided, contains \code{nclusters} and
#' \code{nclusters_plot} to determine optimal \code{k} given their WSS (Within
#' Groups Sum of Squares). If \code{k} is provided, additionally we get:
#' \itemize{
#'   \item \code{df} data.frame with original \code{df} plus \code{cluster} column
#'   \item \code{clusters} integer which is the same as \code{k}
#'   \item \code{fit} kmeans object used to fit clusters
#'   \item \code{means} data.frame with means and counts for each cluster
#'   \item \code{correlations} plot with correlations grouped by clusters
#'   \item \code{PCA} list with PCA results (when \code{dim_red="PCA"})
#'   \item \code{tSNE} list with t-SNE results (when \code{dim_red="tSNE"})
#' }
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data("iris")
#' df <- subset(iris, select = c(-Species))
#'
#' # If dataset has +5 columns, feel free to reduce dimenstionalities
#' # with reduce_pca() or reduce_tsne() first
#'
#' # Find optimal k
#' check_k <- clusterKmeans(df, limit = 10)
#' check_k$nclusters_plot
#' # Or pick k automatically based on WSS variance
#' check_k <- clusterKmeans(df, wss_var = 0.05, limit = 10)
#' # You can also use our other functions:
#' # clusterOptimalK(df) and clusterVisualK(df)
#'
#' # Run with selected k
#' clusters <- clusterKmeans(df, k = 3)
#' names(clusters)
#'
#' # Cross-Correlations for each cluster
#' plot(clusters$correlations)
#'
#' # PCA Results (when dim_red = "PCA")
#' plot(clusters$PCA$plot_explained)
#' plot(clusters$PCA$plot)
#' @export
clusterKmeans <- function(df, k = NULL, wss_var = 0, limit = 15, drop_na = TRUE,
                          ignore = NULL, ohse = TRUE, norm = TRUE,
                          algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
                          dim_red = "PCA", comb = c(1, 2), seed = 123,
                          quiet = FALSE, ...) {
  on.exit(set.seed(seed))
  check_opts(dim_red, c("PCA", "tSNE", "all", "none"))
  if ("all" %in% dim_red) dim_red <- c("PCA", "tSNE")

  if (isTRUE(is.na(ignore)[1])) ignore <- NULL
  ignore <- unique(ignore)
  df <- .prepare_cluster(df,
    drop_na = drop_na, ohse = ohse,
    norm = norm, quiet = quiet,
    ignore = ignore, ...
  )

  # Ignore some columns
  if (!is.null(ignore)) {
    order <- colnames(df)
    aux <- select(df, any_of(ignore))
    df <- select(df, -any_of(ignore))
    if (!quiet) message(paste("Ignored features:", v2t(ignore)))
  }

  results <- list()

  # Determine number of clusters (n) using WSS methodology
  wss <- sum(apply(df, 2, var)) * (nrow(df) - 1)
  limit <- min(nrow(df) - 1, limit)
  for (i in 2:limit) {
    wss[i] <- suppressWarnings(
      sum(kmeans(df, centers = i, algorithm = algorithm)$withinss)
    )
  }
  nclusters <- data.frame(n = c(1:limit), wss = wss)
  nclusters_plot <- ggplot(nclusters, aes(x = .data$n, y = .data$wss)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Total Number of Clusters",
      subtitle = "HINT: Where does the curve level?",
      x = "Number of Clusters",
      y = "Within Groups Sum of Squares"
    ) +
    scale_y_comma() +
    theme_lares()
  results[["nclusters"]] <- nclusters
  results[["nclusters_plot"]] <- nclusters_plot
  
  # Auto K selected by less X WSS variance (convergence)
  if (wss_var > 0 & is.null(k)) {
    k <- nclusters %>%
      mutate(pareto = .data$wss/.data$wss[1],
             dif = lag(.data$pareto) - .data$pareto) %>%
      filter(.data$dif > wss_var) %>% pull(.data$n) %>% max(.)
    if (!quiet) message(sprintf(
      ">> Auto selected k = %s (clusters) based on minimum WSS variance of %s%%",
      k, wss_var * 100))
  }

  # If n is already selected
  if (!is.null(k)) {
    if (!is.null(ignore)) {
      results[["df"]] <- bind_cols(df, aux) %>% select(any_of(order), everything())
    } else {
      results[["df"]] <- df
    }
    yintercept <- nclusters$wss[nclusters$n == k]
    nclusters_plot <- nclusters_plot +
      geom_hline(aes(yintercept = yintercept), colour = "red") +
      labs(subtitle = paste("Number of clusters selected:", k))
    results[["clusters"]] <- k
    results[["nclusters_plot"]] <- nclusters_plot

    # K-Means Cluster Analysis
    fit <- kmeans(df, k, algorithm = algorithm, iter.max = limit)
    results[["fit"]] <- fit
    # Append cluster assignment
    df <- data.frame(results[["df"]], cluster = as.factor(fit$cluster))
    results[["df"]] <- df
    # Get cluster means
    results[["means"]] <- df %>%
      group_by(.data$cluster) %>%
      summarise_if(is.numeric, list(mean)) %>%
      mutate(n = as.integer(table(df$cluster)))

    # Correlations
    results[["correlations"]] <- corr_cross(
      df,
      contains = "cluster_", quiet = TRUE, ignore = ignore
    ) +
      labs(subtitle = "Most relevant correlations grouped by cluster")

    # Dim reduction: PCA
    if ("PCA" %in% dim_red) {
      PCA <- reduce_pca(df, ignore = c(ignore, "cluster"), comb = comb, quiet = quiet, ...)
      PCA$plot <- PCA$plot +
        geom_point(aes(colour = df$cluster)) +
        labs(colour = "Cluster", title = "Clusters with Principal Component Analysis")
      results[["PCA"]] <- PCA
    }

    # Dim reduction: t-SNE
    if ("tSNE" %in% dim_red) {
      tsne <- reduce_tsne(df, ignore = c(ignore, "cluster"), quiet = quiet, ...)
      tsne$plot <- tsne$plot +
        geom_point(aes(colour = df$cluster)) +
        labs(colour = "Cluster", title = "Clusters with t-SNE")
      results[["tSNE"]] <- tsne
    }
  }

  return(results)
}


####################################################################
#' Visualize K-Means Clusters for Several K
#'
#' Visualize cluster data for assorted values of k.
#'
#' @family Clusters
#' @inheritParams clusterKmeans
#' @param ks Integer vector. Which k should be tested?
#' @param ... Additional parameters passed to \code{clusterKmeans}
#' @return List. Plot and data.frame results of clustering \code{df}
#' data.frame into \code{ks} integer clusters.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data("iris")
#' df <- subset(iris, select = c(-Species))
#' df <- df[sample(nrow(df)), ]
#'
#' # Calculate and plot
#' result <- clusterVisualK(df, ks = 2:4)
#' plot(result$plot)
#'
#' # You can use the data generated as well
#' lapply(result$data, function(x) head(x$cluster, 10))
#' @export
clusterVisualK <- function(df, ks = 2:6, ...) {
  clus_dat <- function(df, k, n = length(ks), ...) {
    clusters <- clusterKmeans(df, k, quiet = TRUE, ...)
    pca <- clusters$PCA$pcadf %>% 
      mutate(cluster = clusters$df$cluster, k = k)
    explained <<- clusters$PCA$pca_explained[1:2]
    return(pca)
  }
  clus_plot <- function(clus_dat) {
    clus_dat %>%
      ggplot(aes(x = .data$PC1, y = .data$PC2, colour = as.character(.data$cluster))) +
      geom_point() +
      guides(colour = "none") +
      labs(subtitle = glued("{clus_dat$k[1]} clusters")) +
      theme_lares(pal = 2)
  }
  dats <- lapply(ks, function(x) clus_dat(df, x, ...))
  plots <- lapply(dats, clus_plot)

  total <- formatNum(sum(explained), 1, pos = "%")
  explained <- formatNum(explained, 1, pos = "%")
  subtitle <- sprintf(
    "Explaining %s of the variance with 2 PCA:\nPC1 (%s), PC2 (%s)",
    total, explained[1], explained[2]
  )

  wrapped <- wrap_plots(plots) +
    plot_annotation(
      title = "Kmeans Clustering across potential number of clusters",
      subtitle = subtitle
    )

  ret <- list(plot = wrapped, data = dats)
  return(invisible(ret))
}

####################################################################
#' Visualize K-Means Clusters for Several K Methods
#'
#' Visualize cluster data for assorted values of k and methods such as
#' WSS, Silhouette and Gap Statistic. See \code{factoextra::fviz_nbclust}
#' for more.
#'
#' @family Clusters
#' @inheritParams clusterKmeans
#' @param method Character vector.
#' @param ... Additional parameters passed to \code{factoextra::fviz_nbclust}
#' @return Plot. Optimal number of clusters of \code{df} data.frame given a
#' selected \code{method}.
#' @examples
#' # You must have "factoextra" library to use this auxiliary function:
#' \dontrun{
#' data("iris")
#' df <- subset(iris, select = c(-Species))
#' # Calculate and plot optimal k clusters
#' clusterOptimalK(df)
#' }
#' @export
clusterOptimalK <- function(df, method = c("wss", "silhouette", "gap_stat"),
                            drop_na = TRUE, ohse = TRUE, norm = TRUE,
                            quiet = TRUE, ...) {
  try_require("factoextra")
  df <- .prepare_cluster(df, drop_na = drop_na, ohse = ohse, norm = norm, quiet = quiet)
  plots <- lapply(method, function(x) {
    fviz_nbclust(df, kmeans, method = x, ...) + theme_lares(pal = 2)
  })
  return(plots)
}

.prepare_cluster <- function(df, drop_na = TRUE, ohse = TRUE,
                             norm = TRUE, quiet = FALSE, ignore = NULL, ...) {

  # Leave some columns out of the logic
  if (!is.null(ignore)) {
    ignored <- select(df, any_of(ignore))
    df <- select(df, -any_of(ignore))
  } else {
    ignored <- NULL
  }

  # There should be no NAs
  df <- removenacols(df, all = TRUE)
  if (sum(is.na(df)) > 0) {
    if (drop_na) {
      df <- removenarows(df, all = FALSE)
      if (!quiet) {
        message(
          "Automatically removed rows with NA. To overwrite: fix NAs and set drop_na = FALSE"
        )
      }
    } else {
      stop(paste("There should be no NAs in your dataframe!",
        "You can manually fix it or set drop_na to TRUE to remove these rows.",
        sep = "\n"
      ))
    }
    if (nrow(df) == 0) {
      stop("There are no observations without NA values. Please, check your dataset")
    }
  }

  # Only numerical values
  nums <- df_str(df, return = "names", quiet = TRUE)$nums
  if (isTRUE(ohse) & length(nums) != ncol(df)) {
    df <- ohse(df, dates = TRUE, limit = 8, ignore = ignore, ...)
  } else {
    df <- data.frame(df) %>% select_if(is.numeric)
  }

  # Data should be normalized
  if (norm) {
    df <- df %>%
      transmute_if(is.numeric, list(normalize)) %>%
      replace(., is.na(.), 0)
  }

  # No duplicates
  df <- bind_cols(ignored, df)
  temp <- which(!colnames(df) %in% ignore)
  new_df <- distinct_at(df, temp, .keep_all = TRUE)
  if (nrow(new_df) != nrow(df)) {
    if (!quiet) {
      message(paste(">>> Removed duplicate obserations:", nrow(df) - nrow(new_df)))
    }
  }
  df <- new_df

  return(df)
}
