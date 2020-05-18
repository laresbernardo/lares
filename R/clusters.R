####################################################################
#' Automated K-Means Clustering + PCA
#' 
#' This function lets the user cluster a whole data.frame automatically.
#' As you might know, the goal of kmeans is to group data points into 
#' distinct non-overlapping subgroups. If needed, one hot encoding will 
#' be applied to categorical values automatically with this function. 
#' For consideration: Scale/standardize the data when applying kmeans.
#' Also, kmeans assumes spherical shapes of clusters and doesn’t work well 
#' when clusters are in different shapes such as elliptical clusters.
#' 
#' @family Machine Learning
#' @family Clusters
#' @family PCA
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
#' @examples 
#' options("lares.font" = NA) # Temporal
#' data(dft) # Titanic dataset
#' df <- subset(dft, select = -c(Ticket, PassengerId))
#' 
#' # Find optimal k
#' check_k <- clusterKmeans(df)
#' check_k$nclusters_plot
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
#' \dontrun{
#' # 3D interactive plot
#' clusters$PCA$plot_1_2_3
#' }
#' @export
clusterKmeans <- function(df, k = NA, limit = 20, drop_na = TRUE, 
                          ignore = NA, ohse = TRUE, norm = TRUE, 
                          comb = c(1, 2), seed = 123){
  
  results <- list()
  
  # There should not be NAs
  if (sum(is.na(df)) > 0) {
    if (drop_na) { 
      df <- removenarows(df, all = FALSE) 
      message("Automatically removed rows with NA. To overwrite: fix NAs and set drop_na = FALSE")
    } else {
      stop(paste("There should be no NAs in your dataframe!",
                 "You can manually fix it or set drop_na to TRUE to remove these rows.", sep = "\n")) 
    }
  }
  
  # Only numerical values
  nums <- df_str(df, return = "names", quiet = TRUE)$nums
  if (ohse & length(nums) != ncol(df)) {
    df <- ohse(df, redundant = FALSE, dates = TRUE, limit = 8)
    message("One hot encoding applied...")
  } else {
    df <- data.frame(df) %>% select_if(is.numeric)
  }
  
  # Data should be normalized for better results
  if (norm) df <- df %>% 
    transmute_all(list(normalize)) %>% 
    replace(., is.na(.), 0)
  
  # Ignore some columns
  if (!is.na(ignore)[1]) {
    order <- colnames(df)
    aux <- df[,colnames(df) %in% ignore]
    df <- df[,!colnames(df) %in% ignore]
    message(paste("Ignored only for kmeans:", vector2text(ignore)))
  }
  
  # Determine number of clusters (n)
  wss <- sum(apply(df, 2, var))*(nrow(df) - 1)
  for (i in 2:limit) wss[i] <- sum(kmeans(df, centers = i)$withinss)
  nclusters <- data.frame(n = c(1:limit), wss = wss)
  nclusters_plot <- ggplot(nclusters, aes(x = .data$n, y = .data$wss)) + 
    geom_line() + geom_point() +
    theme_minimal() +
    labs(title = "Total Number of Clusters",
         subtitle = "HINT: Where does the curve level?",
         x = "Number of Clusters",
         y = "Within Groups Sum of Squares") +
    scale_y_continuous(labels = comma) +
    theme_lares2()
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
    set.seed(seed)
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
    results[["correlations"]] <- corr_cross(df, contains = "cluster", redundant = FALSE)
    
    # PCA
    PCA <- list()
    df <- df[,!colnames(df) %in% c(zerovar(df), "cluster")]
    pca <- prcomp(df, center = TRUE, scale. = TRUE)
    PCA$pcadf <- data.frame(pca$x, cluster = results$df$cluster)
    PCA$pca_explained <- round(100 * pca$sdev^2/sum(pca$sdev^2), 4)
    PCA$pcadf <- PCA$pcadf[,c(PCA$pca_explained > 0.1, TRUE)]
    PCA$plotVarExp <- data.frame(id = 1:length(PCA$pca_explained)) %>%
      mutate(PC = factor(paste0("PC", .data$id), 
                         levels = paste0("PC", 1:length(PCA$pca_explained))),
             amount = PCA$pca_explained) %>%
      mutate(aux = cumsum(.data$amount)) %>%
      ggplot(aes(x = .data$id, y = .data$aux)) +
      geom_path() + geom_point() +
      labs(title = "Principal Component Analysis",
           subtitle = "Percentage of Variation Explained by Components",
           y = "Cumulative variation explained [%]", x = "PC(i)") +
      scale_y_continuous(limits = c(0, 100), expand = c(0, 1)) +
      scale_x_continuous(expand = c(0, 1)) +
      theme_lares2()
    
    PCA$plot_1_2 <- ggplot(PCA$pcadf, aes(
      x = .data$PC1, y = .data$PC2, colour = .data$cluster)) +
      geom_point() +
      labs(title = "Principal Component Analysis") +
      theme_lares2(pal = 2)
    
    if (length(find.package("ggforce", quiet = TRUE)) > 0) {
      try_require("ggforce")
      PCA$plot_1_2 <- PCA$plot_1_2 +
        geom_mark_ellipse(
          aes(group = .data$cluster, description = .data$cluster),
          label.fill = "black", label.colour = "white")
    } else warning("Install `ggforce` for better visualization!")
    
    if (length(find.package("plotly", quiet = TRUE)) > 0) {
      try_require("plotly")
      PCA$plot_1_2_3 <- plot_ly(
        PCA$pcadf, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster, 
        colors = names(lares_pal()[[2]])[1:3]) %>%
        add_markers()
    } else warning("Install `plotly` to add a 3D visualization for PC1, PC2 and PC3")
    
    PCA$pca <- pca
    results[["PCA"]] <- PCA
  }
  
  return(results)
}

# Testing new functions
if (FALSE) {
  df <- dft[1:100,]
  top <- 5
  clusters <- 3
  
  try_require("factoextra")
  dfp <- ohse(df)
  dfp <- quiet(impute(dfp))
  lasso <- lasso_vars(dfp, Survived_TRUE)
  which <- lasso$coef$names[1:top]
  most_imp_cols <- select(dfp, Survived_TRUE, one_of(which))
  
  # Optimal number of clusters
  #fviz_nbclust(most_imp_cols, kmeans, method = "wss")
  optimal <- clusterKmeans(most_imp_cols)
  optimal$nclusters_plot
  
  kclus <- kmeans(most_imp_cols, centers = clusters, nstart = 10)
  fviz_cluster(kclus, data = most_imp_cols, repel = TRUE, labelsize = 15)
  
  #Based on the coefficient, identified the best for this dataset (close to 1 is the best)
  hclust_output <- cluster::agnes(most_imp_cols, diss = FALSE, metric = "euclidiean", method = 'ward')
  fviz_dend(hclust_output, cex = 0.8, k = clusters, main = "Dendrogram", repel = TRUE)
  
}
