####################################################################
#' K-Means Clustering Automated
#' 
#' This function lets the user cluster a whole data.frame automatically.
#' If needed, one hot encoding will be applied to categorical values.
#' 
#' @param df Dataframe
#' @param k Integer. Number of clusters
#' @param limit Integer. How many clusters should be considered?
#' @param ohse Boolean. Do you wish to automatically run one hot
#' encoding to non-numerical columns?
#' @param norm Boolean. Should the data be normalized?
#' @param comb Vector. Which columns do you wish to plot? Select which
#' two variables by name or column position.
#' @export
clusterKmeans <- function(df, k = NA, limit = 20, ohse = TRUE, norm = TRUE, comb = c(1,2)){
  
  results <- list()
  
  # Only numerical values
  nums <- df_str(df, return = "names", plot = F)$nums
  if (ohse & length(nums) != ncol(df)) {
    df <- ohse(df, redundant = TRUE, dates = TRUE, limit = 6)
    message("One hot encoding applied...")
  } else {
    df <- data.frame(df) %>% select_if(is.numeric)
  }
  
  # Data should be normalized for better results
  if (norm) {
    df <- df %>% transmute_all(funs(normalize))
  }
  
  results[["df"]] <- df
  
  # Determine number of clusters (n)
  wss <- sum(apply(df, 2, var))*(nrow(df)-1)
  for (i in 2:limit) wss[i] <- sum(kmeans(df, centers = i)$withinss)
  nclusters <- data.frame(n = c(1:limit), wss = wss)
  nclusters_plot <- ggplot(nclusters, aes(x=n, y=wss)) + 
    geom_line() + geom_point() +
    theme_minimal() +
    labs(title = "Total Number of Clusters",
         subtitle = "Where does the curve level?",
         x = "Number of Clusters",
         y = "Within Groups Sum of Squares")
  results[["nclusters"]] <- nclusters
  results[["nclusters_plot"]] <- nclusters_plot
  
  # If n is already selected
  if (!is.na(k)) {
    nclusters_plot <- nclusters_plot + 
      geom_hline(aes(yintercept = nclusters$wss[nclusters$n==k]), colour = "red") +
      labs(subtitle = paste("Number of clusters selected:", k))
    results[["clusters"]] <- k
    results[["nclusters_plot"]] <- nclusters_plot
    
    # K-Means Cluster Analysis
    set.seed(21132)
    fit <- kmeans(df, k)
    # Append cluster assignment
    df <- data.frame(df, cluster = as.factor(fit$cluster))
    # Get cluster means
    clusters <- df %>% 
      group_by(cluster) %>% 
      summarise_all(list(mean)) %>%
      mutate(n = as.integer(table(df$cluster)))
    results[["clusters"]] <- clusters
    # Plot clusters
    axisnames <- colnames(df[,comb])
    centers <- clusters[,-1][,comb]
    clusters_plot <- ggplot(df, aes(
      x=df[,comb[1]], y=df[,comb[2]], colour = df$cluster)) + 
      geom_point() + theme_minimal() +
      geom_point(data=centers, aes_string(x=colnames(centers)[1], y=colnames(centers)[2]), 
                 shape=18, colour="black") +
      labs(title = "Clusters Plot",
           subtitle = paste("Number of clusters selected:", k),
           x = axisnames[2], y = axisnames[1],
           colour = "Cluster")
    results[["clusters_plot"]] <- clusters_plot
  }
  return(results)
}
