# experiments 
source("~/GitHub/TAPIO/TAPIO.R")
source("~/GitHub/TAPIO/get_dataset.R")
library(aricode)


DATASET = "ZOO"

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

#DATA = scale(DATA)

#HC
hc = fastcluster::hclust(dist(DATA))
cl = cutree(hc, K)
HC_perf = ARI(cl, labels)
print(HC_perf)

n_trees = c(1, 2, 10, 100, 500, 1000)

RES = matrix(NaN, n_iter, length(n_trees))
colnames(RES) = n_trees

for(xx in 1:n_iter){
 for(yy in 1:length(n_trees)){

	# TAPIO
	res = TAPIO(DATA, k=K, n_trees=n_trees[yy])
	cl  = res$cl
	RES[xx,yy] = ARI(cl, labels)
 } 

print(RES)
print(HC_perf)

}



