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


 trueClusIDs  = aggregate(Longdat$Dat$label, 
            function(x) return(x[1]), by = list(Longdat$Dat$id))[,2]

#pp = TAPIO::MeanPlot(output)
#clusterMLD::DendroPlot(output)

library(aricode)

niter = 30
ntrees = c(1, 5, 10, 20, 50, 100)
RES = matrix(NaN, niter, length(ntrees))
colnames(RES) = ntrees

for (xx in 1:length(ntrees)){

    for(yy in 1:niter){

        res = longTAPIO_MLD(as.matrix(Longdat$Dat[,paste("y", seq(5), sep = "_")]),
                    user_id = Longdat$Dat$id, obsTimes = Longdat$Dat$obs,
                    k=4, levels=4, n_trees=ntrees[xx])

       
        foundClusIDs = res$cl

        #print(ARI(trueClusIDs,foundClusIDs))
        #print(ARI(trueClusIDs,CL))

        RES[yy,xx] = ARI(trueClusIDs,foundClusIDs)
        print(RES)
    }
}

#IMP = importance(res)
#print(IMP)

### PLOTS


library(ggplot2)
library(reshape2)

RES_melt = melt(RES)
RES_melt$Var2 = as.factor(RES_melt$Var2)

p = ggplot(RES_melt, aes(x=Var2, y=value)) + 
  geom_boxplot(notch=FALSE) +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("Adjusted R-index")+
  xlab("Number of trees") +
  theme(text = element_text(size=15)) 
  