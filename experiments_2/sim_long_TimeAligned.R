library(TAPIO)
library(clusterMLD)
library(MASS)
library(aricode)
library(reshape)

#######################################################################
#load("/home/bpfeif/GitHub/TAPIO/experiments/Longdat_interpolated.rda")


#output = LongDataCluster(Longdat$Dat$obs,
#                          Longdat$Dat[,paste("y", seq(5), sep = "_")],
#                          Longdat$Dat$id)

#CL = rep(NaN, length(unlist(output$Cluster.res)))
#for(xx in 1:length(output$Cluster.res)){
#    CL[output$Cluster.res[[xx]]] = xx
#}


#trueClusIDs  = aggregate(Longdat$Dat$label, function(x) return(x[1]), 
#                    by = list(Longdat$Dat$id))[,2]

######################################

n_iter = 50
MISSFRAC = 0 # fraction of missing data

RES = matrix(NaN, n_iter, 3)
colnames(RES) = c("KML3D", "TAPIO_sample", "TAPIO_trajectories")

for(ii in 1:n_iter){

    Longdat2 = simLongData(ranTimes = FALSE, 
                            n_i = 10, 
                            eta = 20)

    Longdat2_wide <- reshape(
    Longdat2,
    idvar = c("subject", "time", "cluster"),  # columns that identify each row
    timevar = "outcome",                      # the variable that will become columns
    direction = "wide"
    )

    # Interpolate
    #Longdat2_wide = TimeAlign_interpolate(Longdat2_wide)

    trueClusIDs  = aggregate(Longdat2_wide$cluster, function(x) return(x[1]), 
                        by = list(Longdat2_wide$subject))[,2]

    ################################################

    # Permute a variable 
    #id = sample(4:ncol(Longdat2_wide), 1)
    #vec = Longdat2_wide[,id]
    #idds = sample(1:length(vec), length(vec), replace=FALSE)
    #vec_perm = vec[idds]
    #Longdat2_wide[,id] = vec_perm

    # Add noise
    #Longdat2_wide = cbind(Longdat2_wide, 
    #                    Longdat2_wide[,ncol(Longdat2_wide)],
    #                    Longdat2_wide[,ncol(Longdat2_wide)],
    #                    Longdat2_wide[,ncol(Longdat2_wide)],
    #                    Longdat2_wide[,ncol(Longdat2_wide)],
    #                    Longdat2_wide[,ncol(Longdat2_wide)])
    ##########################################################

    ## Add missings to INPUT
    if(MISSFRAC!=0){
      IN = as.matrix(Longdat2_wide[,4:ncol(Longdat2_wide)])
      miss_frac = MISSFRAC
      if(miss_frac!=0){ 
          miss_perc = ceiling(length(IN)*miss_frac)
          r_ids = sample(1:length(IN), miss_perc)
          IN[r_ids] = NaN
      }
      Longdat2_wide[,4:ncol(Longdat2_wide)] = IN
    }
    ######################################

    ############################
    # KML3D
    ############################
    library(kml3d)

    print("KML3D")
    n_samples = length(unique(Longdat2_wide$subject))
    ## Plot the results
    tr1nn = array(NaN, dim = c(n_samples, 10, 5))

    IN = as.matrix(Longdat2_wide[,4:ncol(Longdat2_wide)])

    for(xx in 1:5){

      tr1nn[,1,xx] = IN[seq(1,nrow(Longdat2_wide),by=10),xx]
      tr1nn[,2,xx] = IN[seq(2,nrow(Longdat2_wide),by=10),xx]
      tr1nn[,3,xx] = IN[seq(3,nrow(Longdat2_wide),by=10),xx]
      tr1nn[,4,xx] = IN[seq(4,nrow(Longdat2_wide),by=10),xx]
      tr1nn[,5,xx] = IN[seq(5,nrow(Longdat2_wide),by=10),xx]
      tr1nn[,6,xx] = IN[seq(6,nrow(Longdat2_wide),by=10),xx]
      tr1nn[,7,xx] = IN[seq(7,nrow(Longdat2_wide),by=10),xx]
      tr1nn[,8,xx] = IN[seq(8,nrow(Longdat2_wide),by=10),xx]
      tr1nn[,9,xx] = IN[seq(9,nrow(Longdat2_wide),by=10),xx]
      tr1nn[,10,xx] = IN[seq(10,nrow(Longdat2_wide),by=10),xx]

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
            nbRedrawing = 10, toPlot = "none", 
            parAlgo = parKml3d(imputationMethod = "copyMean"))

    cl = getClusters(object, 4)
    
    na_ids = which(is.na(cl))
    if(length(na_ids)>0){
      #ari_KML3D = ARI(trueClusIDs, cl)
      ari_KML3D = ARI(trueClusIDs[-na_ids], cl[-na_ids])
    }else{
      ari_KML3D = ARI(trueClusIDs, cl)
    }

    
    # ClusterMLD
    #output = LongDataCluster(Longdat2_wide$time,
    #                        Longdat2_wide[,4:ncol(Longdat2_wide)],
    #                        Longdat2_wide$subject)
    #CL = rep(NaN, length(unlist(output$Cluster.res)))
    #for(xx in 1:length(output$Cluster.res)){
    #    CL[output$Cluster.res[[xx]]] = xx
    #}
    #ari_MLD  = ARI(trueClusIDs,CL)

    # longTAPIO_sample
    print("longTAPIO_sample")
    DD = as.matrix(Longdat2_wide[,4:ncol(Longdat2_wide)])
    rownames(DD) = sort(rep(1:200, 10))
    res = longTAPIO_sample(DD,
                         k = 4, levels=4, 
                         n_trees=1000, method="ward.D2",
                         n_features=NaN,
                         do.leveling=TRUE)

    foundClusIDs = res$cl
    # when KML3D produced NaNs
    if(length(na_ids)>0){
     #ari_TAPIO_sample  = ARI(trueClusIDs,foundClusIDs)
     ari_TAPIO_sample  = ARI(trueClusIDs[-na_ids],foundClusIDs[-na_ids])
    }else{
     ari_TAPIO_sample  = ARI(trueClusIDs,foundClusIDs)
    }
    
    # longTAPIO_trajectories
     print("longTAPIO_trajectories")
    DD = as.matrix(Longdat2_wide[,4:ncol(Longdat2_wide)])
    res = longTAPIO_trajectories(DD,
                         k = 4, user_id = Longdat2_wide$subject, 
                         levels=4, verbose = 1, 
                         n_trees=1000, method="ward.D2",
                         n_features=NaN, do.leveling=TRUE)

    foundClusIDs = res$cl
    # when KML3D produced NaNs
    if(length(na_ids)>0){
     #ari_TAPIO_trajectories  = ARI(trueClusIDs,foundClusIDs)
     ari_TAPIO_trajectories  = ARI(trueClusIDs[-na_ids],foundClusIDs[-na_ids])
    }else{
     ari_TAPIO_trajectories  = ARI(trueClusIDs,foundClusIDs)
    }
    
    # longTAPIO_MLD
    #res = longTAPIO_MLD(as.matrix(Longdat2_wide[,4:ncol(Longdat2_wide)]),
    #            user_id =  Longdat2_wide$subject, 
    #            obsTimes =  Longdat2_wide$time,
    #            k=4, levels=4, n_trees=500)

    #foundClusIDs = res$cl

    #ari_TAPIO_MLD  = ARI(trueClusIDs,foundClusIDs)


RES[ii,1] = ari_KML3D
RES[ii,2] = ari_TAPIO_sample
RES[ii,3] = ari_TAPIO_trajectories


print(RES)

}
    #IMP = importance(res)
    #print(IMP)

stop("All good!")

## PLOTS
library(ggplot2)
library(reshape)

RES_melted = melt(RES)

  
p = ggplot(RES_melted, aes(x=X2, y=value)) + 
  geom_boxplot(notch=FALSE, fill = "grey") +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("Adjusted R-index")+
  xlab("Method") +
  ylim(0,1) +
  theme_minimal()  + 
  theme(text = element_text(size=15)) 
  