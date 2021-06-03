####################################################################
#' Automated K-Means Clustering + PCA
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
#' @param df Dataframe
#' @param k Integer. Number of clusters
#' @param limit Integer. How many clusters should be considered?
#' @param drop_na Boolean. Should NA rows be removed?
#' @param ignore Character vector. Which columns should be excluded
#' when calculating kmeans?
#' @param ohse Boolean. Do you wish to automatically run one hot
#' encoding to non-numerical columns?
#' @param norm Boolean. Should the data be normalized?
#' @param comb Vector. Which columns do you wish to plot? Select which
#' two variables by name or column position.
#' @param seed Numeric. Seed for reproducibility
#' @param quiet Boolean. Keep quiet? If not, print messages.
#' @return List. If no \code{k} is provided, contains \code{nclusters} and 
#' \code{nclusters_plot} to determine optimal \code{k} given their WSS (Within
#' Groups Sum of Squares). If \code{k} is provided, additionally we get:
#' \itemize{
#'   \item \code{df} data.frame with original \code{df} plus \code{cluster} column
#'   \item \code{clusters} integer which is the same as \code{k}
#'   \item \code{fit} kmeans object used to fit clusters
#'   \item \code{means} data.frame with means and counts for each cluster
#'   \item \code{correlations} plot with correlations grouped by clusters
#'   \item \code{PCA} list with PCA results
#' }
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data("iris")
#' df <- subset(iris, select = c(-Species))
#' 
#' # Find optimal k
#' check_k <- clusterKmeans(df, limit = 10)
#' check_k$nclusters_plot
#' # You can also use our other functions:
#' # clusterOptimalK() and clusterVisualK()
#' 
#' # Run with selected k
#' clusters <- clusterKmeans(df, k = 3)
#' names(clusters)
#' 
#' # Cross-Correlations for each cluster
#' plot(clusters$correlations)
#' 
#' # PCA Results
#' plot(clusters$PCA$plotVarExp)
#' plot(clusters$PCA$plot_1_2)
#' 
#' # You must have "ggforce" library to use this auxiliary function:
#' # 3D interactive plot
#' \dontrun{clusters$PCA$plot_1_2_3}
#' @export
clusterKmeans <- function(df, k = NA, limit = 20, drop_na = TRUE,
                          ignore = NA, ohse = TRUE, norm = TRUE,
                          comb = c(1, 2), seed = 123, quiet = TRUE){
  
  df <- prepare_data(df, drop_na = drop_na, ohse = ohse, norm = norm, quiet = quiet)
  
  # Ignore some columns
  if (!is.na(ignore)[1]) {
    order <- colnames(df)
    aux <- df[,colnames(df) %in% ignore]
    df <- df[,!colnames(df) %in% ignore]
    if (!quiet) message(paste("Ignored only for kmeans:", v2t(ignore)))
  }
  
  results <- list()
  
  # Determine number of clusters (n) using WSS methodology
  wss <- sum(apply(df, 2, var))*(nrow(df) - 1)
  for (i in 2:limit) wss[i] <- sum(kmeans(df, centers = i)$withinss)
  nclusters <- data.frame(n = c(1:limit), wss = wss)
  nclusters_plot <- ggplot(nclusters, aes(x = .data$n, y = .data$wss)) + 
    geom_line() + geom_point() +
    labs(title = "Total Number of Clusters",
         subtitle = "HINT: Where does the curve level?",
         x = "Number of Clusters",
         y = "Within Groups Sum of Squares") +
    scale_y_continuous(labels = scales::comma) +
    theme_lares()
  results[["nclusters"]] <- nclusters
  results[["nclusters_plot"]] <- nclusters_plot
  
  # If n is already selected
  if (!is.na(k)) {
    if (!is.na(ignore)[1])
      df <- cbind(df, aux) %>% select(one_of(order), everything())
    results[["df"]] <- df
    nclusters_plot <- nclusters_plot + 
      geom_hline(aes(yintercept = nclusters$wss[nclusters$n == k]), colour = "red") +
      labs(subtitle = paste("Number of clusters selected:", k))
    results[["clusters"]] <- k
    results[["nclusters_plot"]] <- nclusters_plot
    
    # K-Means Cluster Analysis
    on.exit(set.seed(seed))
    fit <- kmeans(df, k)
    results[["fit"]] <- fit
    # Append cluster assignment
    df <- data.frame(df, cluster = as.factor(fit$cluster))
    results[["df"]] <- df
    # Get cluster means
    results[["means"]] <- df %>% 
      group_by(.data$cluster) %>% 
      summarise_all(list(mean)) %>%
      mutate(n = as.integer(table(df$cluster)))
    
    # Correlations
    results[["correlations"]] <- corr_cross(df, contains = "cluster", quiet = quiet)
    
    # PCA
    PCA <- list()
    df <- df[,!colnames(df) %in% c(zerovar(df), "cluster")]
    pca <- prcomp(df, center = TRUE, scale. = TRUE)
    PCA$pcadf <- data.frame(pca$x, cluster = results$df$cluster)
    PCA$pca_explained <- round(100 * pca$sdev^2/sum(pca$sdev^2), 4)
    PCA$pcadf <- PCA$pcadf[,c(PCA$pca_explained > 0.1, TRUE)]
    PCA$plotVarExp <- data.frame(id = seq_along(PCA$pca_explained)) %>%
      mutate(PC = factor(paste0("PC", .data$id), 
                         levels = paste0("PC", seq_along(PCA$pca_explained))),
             amount = PCA$pca_explained) %>%
      mutate(aux = cumsum(.data$amount)) %>%
      ggplot(aes(x = .data$id, y = .data$aux)) +
      geom_col(alpha = 0.9) + geom_path() + geom_point() + 
      labs(title = "Principal Component Analysis",
           subtitle = "Percentage of Variation Explained by Components",
           y = "Cumulative variation explained [%]", x = "PC(i)") +
      scale_y_continuous(limits = c(0, 100), expand = c(0, 1)) +
      scale_x_continuous(expand = c(0, 1)) +
      theme_lares()
    
    explained <- formatNum(PCA$pca_explained, 1, pos = "%")
    subtitle <- sprintf("Explaining %s of the variance with 2 PCA:\nPC1 (%s), PC2 (%s)", 
                        formatNum(sum(PCA$pca_explained[1:2]), 1, pos = "%"), 
                        explained[1], explained[2])
    
    PCA$plot_1_2 <- ggplot(PCA$pcadf, aes(
      x = .data$PC1, y = .data$PC2, colour = .data$cluster)) +
      geom_point() +
      labs(title = "Principal Component Analysis",
           subtitle = subtitle) +
      theme_lares(pal = 2)
    
    if (length(find.package("ggforce", quiet = TRUE)) > 0) {
      try_require("ggforce")
      PCA$plot_1_2 <- PCA$plot_1_2 +
        geom_mark_ellipse(
          aes(group = .data$cluster, description = .data$cluster),
          label.fill = "black", label.colour = "white")
    } else if (!quiet) warning("Install ggforce for better visualization!")
    
    if (length(find.package("plotly", quiet = TRUE)) > 0) {
      try_require("plotly")
      PCA$plot_1_2_3 <- plot_ly(
        PCA$pcadf, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster, 
        colors = names(lares_pal()[[2]])[1:3]) %>%
        add_markers()
    } else warning("Install plotly to add a 3D visualization for PC1, PC2 and PC3")
    
    PCA$pca <- pca
    results[["PCA"]] <- PCA
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
#' 
#' # Calculate and plot
#' result <- clusterVisualK(df)
#' 
#' # You can use the data generated as well
#' lapply(result$data, function(x) head(x$cluster))
#' @export
clusterVisualK <- function(df, ks = 1:6, ...) {
  
  clus_dat <- function(df, k, n = length(ks), ...) {
    pca <- clusterKmeans(df, k, ...)$PCA
    x <- pca$pcadf %>% mutate(k = k)
    explained <<- pca$pca_explained[1:2]
    return(x)
  }
  
  clus_plot <- function(clus_dat) {
    clus_dat  %>%
      ggplot(aes(x = .data$PC1, y = .data$PC2, colour = .data$cluster)) +
      geom_point() +
      guides(colour = FALSE) +
      labs(subtitle = glued("{clus_dat$k[1]} clusters")) +
      theme_lares(pal = 2)
  }
  
  dats <- lapply(ks, function(x) clus_dat(df, x, ...))
  plots <- lapply(dats, clus_plot)
  
  total <- formatNum(sum(explained), 1, pos = "%")
  explained <- formatNum(explained, 1, pos = "%")
  subtitle <- sprintf(
    "Explaining %s of the variance with 2 PCA:\nPC1 (%s), PC2 (%s)", 
    total, explained[1], explained[2])
  
  wrapped <- wrap_plots(plots) +
    plot_annotation(title = "Kmeans Clustering across potential number of clusters",
                    subtitle = subtitle)
  
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
  df <- prepare_data(df, drop_na = drop_na, ohse = ohse, norm = norm, quiet = quiet)
  plots <- lapply(method, function(x) {
    fviz_nbclust(df, kmeans, method = x, ...) + theme_lares(pal = 2)
  })
  return(plots)
}

prepare_data <- function(df, drop_na = TRUE, ohse = TRUE, norm = TRUE, ...) {
  
  # There should not be NAs
  if (sum(is.na(df)) > 0) {
    if (drop_na) { 
      df <- removenarows(df, all = FALSE)
      if (!quiet) message(
        "Automatically removed rows with NA. To overwrite: fix NAs and set drop_na = FALSE")
    } else {
      stop(paste("There should be no NAs in your dataframe!",
                 "You can manually fix it or set drop_na to TRUE to remove these rows.", sep = "\n")) 
    }
  }
  
  # Only numerical values
  nums <- df_str(df, return = "names", quiet = TRUE)$nums
  if (isTRUE(ohse) & length(nums) != ncol(df)) {
    df <- ohse(df, dates = TRUE, limit = 8, ...)
  } else {
    df <- data.frame(df) %>% select_if(is.numeric)
  }
  
  # Data should be normalized
  if (norm) df <- df %>% 
    transmute_all(list(normalize)) %>% 
    replace(., is.na(.), 0)
  
  return(df)
}
