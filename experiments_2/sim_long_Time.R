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

RES = matrix(NaN, n_iter, 2)
colnames(RES) = c("clusterMLD", "longTAPIO_MLD")

for(ii in 1:n_iter){

    Longdat2 = simLongData(ranTimes = TRUE, 
                            n_i = 10, 
                            eta = 5)

    Longdat2_wide <- reshape(
    Longdat2,
    idvar = c("subject", "time", "cluster"),  # columns that identify each row
    timevar = "outcome",                      # the variable that will become columns
    direction = "wide"
    )

    # Interpolate
    # Longdat2_wide = TimeAlign_interpolate(Longdat2_wide)

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


    # ClusterMLD
    output = LongDataCluster(Longdat2_wide$time,
                            Longdat2_wide[,4:ncol(Longdat2_wide)],
                            Longdat2_wide$subject, No.Class=4)
    CL = rep(NaN, length(unlist(output$Cluster.res)))
    for(xx in 1:length(output$Cluster.res)){
        CL[output$Cluster.res[[xx]]] = xx
    }
    ari_MLD  = ARI(trueClusIDs,CL)


    # longTAPIO_MLD
    res = longTAPIO_MLD(as.matrix(Longdat2_wide[,4:ncol(Longdat2_wide)]),
                user_id =  Longdat2_wide$subject, 
                obsTimes =  Longdat2_wide$time,
                k=4, levels=4, n_trees=10)

    foundClusIDs = res$cl

    ari_TAPIO_MLD  = ARI(trueClusIDs,foundClusIDs)



RES[ii,1] = ari_MLD
RES[ii,2] = ari_TAPIO_MLD


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
  #ylim(0.5,1) +
  theme_minimal()  + 
  theme(text = element_text(size=15)) 
  