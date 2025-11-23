library(TAPIO)
library(clusterMLD)
library(MASS)
library(aricode)

load("/home/bastian/GitHub/TAPIO/experiments/Longdat_interpolated.rda")
output = LongDataCluster(Longdat$Dat$obs,
                          Longdat$Dat[,paste("y", seq(5), sep = "_")],
                          Longdat$Dat$id)

CL = rep(NaN, length(unlist(output$Cluster.res)))
for(xx in 1:length(output$Cluster.res)){
    CL[output$Cluster.res[[xx]]] = xx
}


trueClusIDs  = aggregate(Longdat$Dat$label, function(x) return(x[1]), 
                    by = list(Longdat$Dat$id))[,2]


#pp = TAPIO::MeanPlot(output)
#clusterMLD::DendroPlot(output)
#########################################################

# longTAPIO_trajectories
res = longTAPIO_trajectories(as.matrix(y_int[,1:5]), k = 4, 
                        user_id = y_int$id, levels=4, verbose = 1, n_trees=500)

foundClusIDs = res$cl
ari_trajectories  = ARI(trueClusIDs,foundClusIDs)

# longTAPIO_MLD
res = longTAPIO_MLD(as.matrix(Longdat$Dat[,paste("y", seq(5), sep = "_")]),
              user_id = Longdat$Dat$id, obsTimes = Longdat$Dat$obs,
             k=4, levels=4, n_trees=500)

foundClusIDs = res$cl


ari_MLD  = ARI(trueClusIDs,foundClusIDs)
ari_MLD_orig = ARI(trueClusIDs,CL)


#IMP = importance(res)
#print(IMP)