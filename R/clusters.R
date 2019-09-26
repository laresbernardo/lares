####################################################################
#' K-Means Clustering Automated
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
#' @param df Dataframe
#' @param k Integer. Number of clusters
#' @param limit Integer. How many clusters should be considered?
#' @param drop_na Boolean. Should NA rows be removed?
#' @param ohse Boolean. Do you wish to automatically run one hot
#' encoding to non-numerical columns?
#' @param norm Boolean. Should the data be normalized?
#' @param comb Vector. Which columns do you wish to plot? Select which
#' two variables by name or column position.
#' @param seed Numeric. Seed for reproducibility
#' @export
clusterKmeans <- function(df, k = NA, limit = 20, drop_na = TRUE, 
                          ohse = TRUE, norm = TRUE, comb = c(1,2,3),
                          seed = 123){
  
  results <- list()
  
  # There should not be NAs
  if (sum(is.na(df)) > 0) {
    if (drop_na) { 
      df <- df %>% removenarows(all = FALSE) 
      message("Automatically removed rows with NA. To overtwrite: fix NAs and set drop_na = FALSE")
    } else {
      stop(paste("There should be no NAs in your dataframe!",
                 "You can manually fix it or set drop_na to TRUE to remove these rows.", sep = "\n")) 
    }
  }
  
  # Only numerical values
  nums <- df_str(df, return = "names", quiet = TRUE)$nums
  if (ohse & length(nums) != ncol(df)) {
    df <- ohse(df, redundant = TRUE, dates = TRUE, limit = 6)
    message("One hot encoding applied...")
  } else {
    df <- data.frame(df) %>% select_if(is.numeric)
  }
  
  # Data should be normalized for better results
  if (norm) df <- df %>% transmute_all(funs(normalize)) %>% replace(., is.na(.), 0)
  
  # Determine number of clusters (n)
  wss <- sum(apply(df, 2, var))*(nrow(df) - 1)
  for (i in 2:limit) wss[i] <- sum(kmeans(df, centers = i)$withinss)
  nclusters <- data.frame(n = c(1:limit), wss = wss)
  nclusters_plot <- ggplot(nclusters, aes(x = n, y = wss)) + 
    geom_line() + geom_point() +
    theme_minimal() +
    labs(title = "Total Number of Clusters",
         subtitle = "Where does the curve level?",
         x = "Number of Clusters",
         y = "Within Groups Sum of Squares") +
    scale_y_continuous(labels = scales::comma) +
    theme_lares2()
  results[["nclusters"]] <- nclusters
  results[["nclusters_plot"]] <- nclusters_plot
  
  # If n is already selected
  if (!is.na(k)) {
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
    clusters <- df %>% 
      group_by(cluster) %>% 
      summarise_all(list(mean)) %>%
      mutate(n = as.integer(table(df$cluster)))
    results[["clusters"]] <- clusters
    
    # Plot clusters
    if (length(comb) == 2) {
      axisnames <- colnames(df[,comb])
      centers <- data.frame(
        cluster = clusters$cluster, 
        clusters[,-1][,comb],
        size = clusters$n)
      clusters_plot <- ggplot(df, aes(
        x = df[,comb[1]], y = df[,comb[2]], colour = df$cluster)) + 
        geom_point() + theme_minimal() + guides(size = FALSE) +
        geom_text(data = centers, 
                  aes_string(x = colnames(centers)[2], 
                             y = colnames(centers)[3], 
                             label = "cluster", 
                             size = "size"), 
                  colour = "black", fontface = "bold") +
        labs(title = "Clusters Plot",
             subtitle = paste("Number of clusters selected:", k),
             x = axisnames[1], y = axisnames[2],
             colour = "Cluster") + coord_flip() +
        theme_lares2(pal = 2)
    }
    
    if (length(comb) == 3) {
      try_require("plotly")
      clusters_plot <- plot_ly(x = df[,comb[1]], 
                               y = df[,comb[2]], 
                               z = df[,comb[3]],
                               color = df$cluster,
                               type = "scatter3d", mode = "markers")
    }
    
    if (exists("clusters_plot")) results[["clusters_plot"]] <- clusters_plot
    results[["correlations"]] <- corr_cross(df, contains = "cluster")  
  }
  
  return(results)
}
