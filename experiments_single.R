# experiments 
source("~/GitHub/TAPIO/TAPIO.R")
source("~/GitHub/TAPIO/get_dataset.R")
library(aricode)

#DATASETS = c("IONOSPHERE","GLASS", "WINE", 
#    "IRIS","WDBC","ZOO")

DATASET = "IONOSPHERE"

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

# TAPIO
res = TAPIO(DATA, k=K, n_trees=2000)
cl  = res$cl
TAPIO_perf = ARI(cl, labels)
	 
print(HC_perf)
print(TAPIO_perf)

I = importance(res)
NN = paste("cluster", 1:dim(I)[1])
rownames(I) = NN
colnames(I) = paste("V",1:dim(I)[2], sep="")

write.table(I, file=paste(DATASET,".txt", sep=""))

