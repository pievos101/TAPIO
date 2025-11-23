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

MISSFRAC = 0.50
n_iter = 20
RES = matrix(NaN, n_iter, 2)
colnames(RES) = c("longTAPIO", "KML3D")


for(zz in 1:n_iter){

    IN = as.matrix(y_int[,1:5])

    ## Add missings to INPUT
    miss_frac = MISSFRAC
    if(miss_frac!=0){ 
        miss_perc = ceiling(length(IN)*miss_frac)
        r_ids = sample(1:length(IN), miss_perc)
        IN[r_ids] = NaN
    }

    # longTAPIO_trajectories
    res = longTAPIO_trajectories(IN, k = 4, 
                            user_id = y_int$id, levels=4, verbose = 1, 
                            n_trees=1000)

    foundClusIDs = res$cl
    #print(ARI(trueClusIDs,foundClusIDs))
    TAPIO_ari_trajectories  = ARI(trueClusIDs,foundClusIDs)


    ## Apply longTAPIO_MLD also
    #res2 = longTAPIO_MLD(
    #   IN,
    #   y_int$id,
    #   obsTimes = rep(1:10, length(trueClusIDs)), 
    #   k = 4,
    #   n_features = NaN,
    #   n_trees = 500,
    #   do.pca = TRUE,
    #   do.MFA = FALSE,
    #   do.leveling = TRUE,
    #   levels = 4,
    #   verbose = 1
    # )
    ###############################



    ############################
    # KML3D
    ############################
    library(kml3d)

    n_samples = length(unique(y_int$id))
    ## Plot the results
    tr1nn = array(NaN, dim = c(n_samples, 10, 5))

    for(xx in 1:5){

      tr1nn[,1,xx] = IN[seq(1,nrow(y_int),by=10),xx]
      tr1nn[,2,xx] = IN[seq(2,nrow(y_int),by=10),xx]
      tr1nn[,3,xx] = IN[seq(3,nrow(y_int),by=10),xx]
      tr1nn[,4,xx] = IN[seq(4,nrow(y_int),by=10),xx]
      tr1nn[,5,xx] = IN[seq(5,nrow(y_int),by=10),xx]
      tr1nn[,6,xx] = IN[seq(6,nrow(y_int),by=10),xx]
      tr1nn[,7,xx] = IN[seq(7,nrow(y_int),by=10),xx]
      tr1nn[,8,xx] = IN[seq(8,nrow(y_int),by=10),xx]
      tr1nn[,9,xx] = IN[seq(9,nrow(y_int),by=10),xx]
      tr1nn[,10,xx] = IN[seq(10,nrow(y_int),by=10),xx]

    }

    idAll = as.character(1:n_samples)
    time = 1:10

    object = clusterLongData3d(traj=tr1nn,
      idAll=idAll,
      time=time,
      varNames=paste("Marker", 1:5, sep=""),
      maxNA=9
    )

    kml3d(object, nbClusters = 4, 
            nbRedrawing = 20, toPlot = "none", parAlgo = parKml3d())

    cl = getClusters(object, 4)
    na_ids = which(is.na(cl))


    if(length(na_ids)>0){
    KMLD_ari = ARI(cl[-na_ids], trueClusIDs[-na_ids])
    TAPIO_ari_trajectories  = ARI(foundClusIDs[-na_ids],trueClusIDs[-na_ids])
    }else{
    KMLD_ari = ARI(cl, trueClusIDs)
    TAPIO_ari_trajectories  = ARI(foundClusIDs, trueClusIDs)
    }

    print("Results:")
    print(TAPIO_ari_trajectories)
    print(KMLD_ari)

print(zz)
RES[zz,1] = TAPIO_ari_trajectories
RES[zz,2] = KMLD_ari
print(RES)

}