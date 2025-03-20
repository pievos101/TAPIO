
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

  cat("Iterations: ",n,"of ",n_iter,"\n")
  #print(ARI(res$cl, target))
  #print(NMI(res$cl, target))
  model_list[[n]] <- res
  ress[,n] <- res$cl
}

## Get importance values
IMP_list = list()

for (xx in 1:length(model_list)){

   cat("Iterations: ",xx,"of ",length(model_list),"\n")
   IMP_list[[xx]] = importance(model_list[[xx]])

}

# Reorganize 
IMP_per_cluster = vector("list", nrow(IMP_list[[1]]))

for (xx in 1:length(IMP_per_cluster)){

  for(yy in 1:length(IMP_list)){

      IMP_per_cluster[[xx]] = rbind(IMP_per_cluster[[xx]], IMP_list[[yy]][xx,])
  }

}

names(IMP_per_cluster) = paste("cluster",1:length(IMP_per_cluster), sep="")

library(ggplot2)
library(reshape2)

IMP_melt = melt(IMP_per_cluster)
IMP_melt$Var2 = as.factor(IMP_melt$Var2)
IMP_melt$Var1 = as.factor(IMP_melt$Var1)


p = ggplot(IMP_melt, aes(x=Var2, y=value)) + 
  #geom_boxplot(notch=FALSE) +
  geom_bar(stat="identity") +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("Cluster-specific feature importance")+
  xlab("Features") +
  theme(text = element_text(size=15)) +
  facet_wrap(~Var1,ncol=2) 

