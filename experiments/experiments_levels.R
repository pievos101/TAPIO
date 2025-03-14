# experiments 
source("~/GitHub/TAPIO/TAPIO.R")
source("~/GitHub/TAPIO/get_dataset.R")
library(aricode)

#DATASETS = c("IONOSPHERE","GLASS", "WINE", 
#    "IRIS","WDBC","ZOO")

DATASET = "WDBC"

res = get_dataset(DATASET)
DATA  = res$train
labels = res$target
t = table(labels)
ids = which(t<=5)
LL = as.numeric(names(t[ids]))
ids = !is.element(labels, LL)
DATA = DATA[ids,]
labels = labels[ids]
labels = as.numeric(as.factor(labels))
K = length(unique(labels))

n_iter = 50 

# Normalization
#DATA = scale(DATA)
# custom function to implement min max scaling
minMax <- function(x) {
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}

DATA = as.data.frame(lapply(as.data.frame(DATA), minMax))
DATA = as.matrix(DATA)

#HC
hc = fastcluster::hclust(dist(DATA), method="ward.D2")
cl = cutree(hc, K)
HC_perf = ARI(cl, labels)
print(HC_perf)

n_levels = c(2, 3, 5, 10, 30)

RES = matrix(NaN, n_iter, length(n_levels))
colnames(RES) = n_levels

for(xx in 1:n_iter){
 for(yy in 1:length(n_levels)){

	# TAPIO
	res = TAPIO(DATA, k=K, levels=n_levels[yy])
	cl  = res$cl
	RES[xx,yy] = ARI(cl, labels)
 } 

print(RES)
print(HC_perf)

}



