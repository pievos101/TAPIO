# function to generate synthetic data around predefined cluster centers
get_synthetic_dataset <- function(cluster_centers, n_samples_per_cluster, sd = 0.2){
  k <- dim(cluster_centers)[1]
  n_feat <- dim(cluster_centers)[2]
  train <- data.frame()
  for (i in 1:k) { cluster_data <- matrix( rnorm(n_feat*n_samples_per_cluster, mean = cluster_centers[i,], sd = sd),
                                           nrow = n_samples_per_cluster, ncol = n_feat, byrow=TRUE )
    cluster_data <- as.data.frame(cluster_data)
    train <- rbind(train, cluster_data) 
  }
  return(train)
}