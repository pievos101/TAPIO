# experiments 
source("~/GitHub/TAPIO/TAPIO.R")
source("~/GitHub/TAPIO/get_dataset.R")
library(aricode)


DATASET = "IRIS"

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

RES = matrix(NaN, n_iter, 2)
#DATA = scale(DATA)

for(xx in 1:n_iter){

	# TAPIO
	res = TAPIO(DATA, k=K)
	cl  = res$cl
	RES[xx,1] = ARI(cl, labels)


	#HC
	hc = fastcluster::hclust(dist(DATA))
	cl = cutree(hc, K)
	RES[xx,2] = ARI(cl, labels)

print(RES)

}



