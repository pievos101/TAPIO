library(kml)
library(TAPIO)
#library(clusterMLD)

niter = 100 
RES = matrix(NaN, niter, 3)
colnames(RES) = c("longTAPIO_sample","longTAPIO_trajectories","KML")

for(xx in 1:niter){

   ex2 <- kml::generateArtificialLongData(
      meanTrajectories=list(function(t)0,function(t)-t,function(t)t),
      nbEachClusters=c(50,50,50),
      personalVariation = function(t){rnorm(1,0,3)},
      residualVariation=function(t){rnorm(1,0,1)} #0.35
   )

   trueClusIDs = rep(1:3,each=50)
   #plot(ex2,parTraj=parTRAJ(col=rep(2:4,each=50)))
   x =attr(ex2, "traj") #columns: time, rows: subjects
   user_ids = rep(1:nrow(x), each = ncol(x))
   obsTimes = rep(1:11, 150)


   #matplot(t(x),type="l",lty=1, col= trueClusIDs) ;grid()  

   ##########################################

   ## GGPLOT
   #library(ggplot2)
   #library(reshape)
   #plot_data = melt(x)
   #plot_data = cbind(plot_data, sort(rep(1:3,50)))
   #colnames(plot_data) = c("id","time","value","cluster")
   #plot_data$cluster = as.factor(plot_data$cluster)
   #plot_data$time = as.factor(plot_data$time)
   #p = ggplot(plot_data, aes(x=time, y=value, group=id, colour=cluster)) +
   #theme_minimal()  + 
   #theme(text = element_text(size=15)) +
   #geom_line()+
   #geom_point()

   ###################################
   library(aricode)


   # KML
   kml(ex2,3,10)
   foundClusIDs = getClusters(ex2,3)
   ari_kml = ARI(foundClusIDs, trueClusIDs)

   # Method 1 - row sampling
   DATA = matrix(as.vector(t(x)),ncol=1)
   rownames(DATA) = user_ids
   res2 = longTAPIO_sample(DATA, k = 3, levels=10, n_trees=1000, method="ward.D2")

   foundClusIDs = res2$cl
   #confusion matrix (well within the ambiguity of renumbering)
   ari_sample = ARI(foundClusIDs, trueClusIDs)

   # Method 2 - trajectories
   res = longTAPIO_trajectories(matrix(as.vector(t(x)),ncol=1), k = 3, 
                        user_id =user_ids, levels=10, verbose = 1, 
                        n_trees=1000, method="ward.D2")
   foundClusIDs = res$cl
   #confusion matrix
   ari_trajectories = ARI(foundClusIDs, trueClusIDs)

   # Method 3 - MLD 
   #res = longTAPIO_MLD(DATA,
   #              user_id = user_ids, obsTimes = obsTimes, 
   #              k=3, levels=3, n_trees=100)
   #foundClusIDs = res$cl
   #confusion matrix
   #ari_MLD = ARI(foundClusIDs, trueClusIDs)

RES[xx,1] = ari_sample
RES[xx,2] = ari_trajectories
RES[xx,3] = ari_kml

print(RES)

}

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
  theme(text = element_text(size=17)) 
  