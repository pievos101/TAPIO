library(TAPIO)
library(clusterMLD)
library(MASS)

load("Longdat.rda")
output = LongDataCluster(Longdat$Dat$obs,
                          Longdat$Dat[,paste("y", seq(5), sep = "_")],
                          Longdat$Dat$id)

CL = rep(NaN, length(unlist(output$Cluster.res)))
for(xx in 1:length(output$Cluster.res)){
    CL[output$Cluster.res[[xx]]] = xx
}

pp = TAPIO::MeanPlot(output)

#clusterMLD::DendroPlot(output)

res = longTAPIO_MLD(as.matrix(Longdat$Dat[,paste("y", seq(5), sep = "_")]),
              user_id = Longdat$Dat$id, obsTimes = Longdat$Dat$obs,
             k=4, levels=4, n_trees=20)

trueClusIDs  = aggregate(Longdat$Dat$label, function(x) return(x[1]), by = list(Longdat$Dat$id))[,2]
foundClusIDs = res$cl

library(aricode)
print(ARI(trueClusIDs,foundClusIDs))
print(ARI(trueClusIDs,CL))


IMP = importance(res)
print(IMP)