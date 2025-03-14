
source("~/GitHub/TAPIO/experiments/get_cluster_centers.R")
source("~/GitHub/TAPIO/experiments/get_synthetic_dataset.R")
library(aricode)
library(TAPIO)

cluster_number = 3 # cluster-specific features

# get cluster centers and define experiment parameters
cluster_centers <- get_cluster_centers(cluster_number)
k <- dim(cluster_centers)[1]
n_feat <- dim(cluster_centers)[2]
n_samples_per_cluster <- 50  
n_iter <- 30

# save training data, predictions and models
ress <- array(0, dim = c(n_samples_per_cluster*k, n_iter))
model_list <- list()
train_list <- list()

# generate synthetic data around predefined cluster centers and train uRF
for (n in 1:n_iter) {
  # generate synthetic data 
  train <- get_synthetic_dataset(cluster_centers, n_samples_per_cluster)
  train_list[[n]] <- train
  target <- rep(1:k, each = n_samples_per_cluster)
  #ggplot(train, aes(x = V2, y = V3, color = as.factor(target))) + geom_point() 
  
  
  res = TAPIO(as.matrix(train), k=k, n_trees=1000, levels=k)

  print(ARI(res$cl, target))
  print(NMI(res$cl, target))
  model_list[[n]] <- res
  ress[,n] <- res$cl
}
