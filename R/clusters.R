####################################################################
#' K-Means Clustering Automated
#' 
#' This function lets the user cluster a whole data.frame automatically.
#' If needed, one hot encoding will be applied to categorical values.
#' 
#' @param df Dataframe
#' @param force_n Integer. Number of clusters
#' @param limit Integer. How many clusters should be considered?
#' @param ohse Boolean. Do you wish to automatically run one hot
#' encoding to non-numerical columns?
#' @param comb Vector. Which columns do you wish to plot? Select which 2
#' by name or position.
#' @export
clusterKmeans <- function(df, force_n = NA, limit = 20, ohse = TRUE, comb = c(1,2)){
  
  # Only numerical values
  if (ohse) {
    df <- ohse(df)
    message("One hot encoding applied...")
  } else {
    df <- data.frame(df) %>% select_if(is.numeric)
  }
  
  results <- list()
  
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
  if (!is.na(force_n)) {
    nclusters_plot <- nclusters_plot + 
      geom_hline(aes(yintercept = nclusters$wss[nclusters$n==force_n]), colour = "red") +
      labs(subtitle = paste("Number of clusters selected:", force_n))
    results[["clusters"]] <- force_n
    results[["nclusters_plot"]] <- nclusters_plot
    
    # K-Means Cluster Analysis
    set.seed(21132)
    fit <- kmeans(df, force_n)
    # Append cluster assignment
    df <- data.frame(df, cluster = as.factor(fit$cluster))
    # Get cluster means
    results[["clusters"]] <- df %>% 
      group_by(cluster) %>% 
      summarise_all(list(mean)) %>%
      mutate(n = as.integer(table(df$cluster)))
    # Plot clusters
    axisnames <- colnames(df[,comb])
    clusters_plot <- ggplot(df, aes(x=df[,comb[1]], y=df[,comb[2]], colour = df$cluster)) + 
      geom_point() + theme_minimal() +
      labs(title = "Clusters Plot",
           subtitle = paste("Number of clusters selected:", force_n),
           x = axisnames[2], y = axisnames[1],
           colour = "Cluster")
    results[["clusters_plot"]] <- clusters_plot
  }
  return(results)
}
