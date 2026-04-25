library(TAPIO)
library(clusterMLD)
library(MASS)
library(aricode)
library(reshape)

n_iter = 50


RES = matrix(NaN, n_iter, 2)
colnames(RES) = c("longTAPIO_trajectories_PC1","longTAPIO_trajectories_PCwr")

for(ii in 1:n_iter){

    r_eta = 3 #sample(1:10,1)
    r_sigma_diag = rep(3,5) #sample(1:6, 5, replace=TRUE)
    id = sample(1:5, 1)
    r_sigma_diag[id] =  sample(3:20, 1)

    print(ii)
    print(r_sigma_diag)
    Longdat2 = simLongData(ranTimes = FALSE, 
                            n_i = 10, 
                            eta = r_eta,
                            sigma_diag = r_sigma_diag) 
                            #sigma_diag=rep(2,5))

    Longdat2_wide <- reshape(
    Longdat2,
    idvar = c("subject", "time", "cluster"),  # columns that identify each row
    timevar = "outcome",                      # the variable that will become columns
    direction = "wide"
    )

    
    trueClusIDs  = aggregate(Longdat2_wide$cluster, function(x) return(x[1]), 
                        by = list(Longdat2_wide$subject))[,2]

    
    ## Params
    set_levels = 4
    set_n_features = 5


    # longTAPIO_trajectories (first PCA)
     print("longTAPIO_trajectories_PCA_1")
    DD = as.matrix(Longdat2_wide[,4:ncol(Longdat2_wide)])
    res = longTAPIO_trajectories(DD,
                         k = 4, #max.k=6,
                         user_id = Longdat2_wide$subject, 
                         levels=set_levels, verbose = 1, 
                         n_trees=500, method="ward.D2",
                         n_features=set_n_features, do.leveling=TRUE,
                         pca_selection="first")

    foundClusIDs = res$cl
    ari_TAPIO_trajectories_1  = ARI(trueClusIDs,foundClusIDs)

    # longTAPIO_trajectories (weighted selected PCA)
     print("longTAPIO_trajectories_PCA_random_weighted")
    DD = as.matrix(Longdat2_wide[,4:ncol(Longdat2_wide)])
    res = longTAPIO_trajectories(DD,
                         k = 4, #max.k=6,
                         user_id = Longdat2_wide$subject, 
                         levels=set_levels, verbose = 1, 
                         n_trees=500, method="ward.D2",
                         n_features=set_n_features, do.leveling=TRUE,
                         pca_selection="random_weighted")

    foundClusIDs = res$cl
    ari_TAPIO_trajectories_random_weighted  = ARI(trueClusIDs,foundClusIDs)


RES[ii,1] = ari_TAPIO_trajectories_1
RES[ii,2] = ari_TAPIO_trajectories_random_weighted

print(RES)

}
    #IMP = importance(res)
    #print(IMP)

stop("All good!")

## PLOTS
library(ggplot2)
library(reshape)

RES = RES[,c(2,3,1)]
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
  
  ###

p = ggplot(RES_melted, aes(x=X2, y=value, fill=X2)) + 
  #geom_violin() +
  geom_boxplot(notch=FALSE) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  #theme_ipsum() +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("Adjusted R-index")+
  xlab("Methods") +
  ylim(0,1) +
  theme_minimal()  +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom") + 
  theme(text = element_text(size=17)) +
  scale_x_discrete(labels = NULL)
  