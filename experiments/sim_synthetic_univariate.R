library(kml)
library(TAPIO)
library(clusterMLD)

ex2 <- kml::generateArtificialLongData(
   meanTrajectories=list(function(t)0,function(t)-t,function(t)t),
   nbEachClusters=c(50,50,50),
   residualVariation=function(t){rnorm(1,0,0.35)}
)

trueClusIDs = rep(1:3,each=50)
#plot(ex2,parTraj=parTRAJ(col=rep(2:4,each=50)))
x =attr(ex2, "traj") #columns: time, rows: subjects
user_ids = rep(1:nrow(x), each = ncol(x))



matplot(t(x),type="l",lty=1, col= trueClusIDs) ;grid()  





##########################################

## GGPLOT
library(ggplot2)
library(reshape)
plot_data = melt(x)
plot_data = cbind(plot_data, sort(rep(1:3,50)))
colnames(plot_data) = c("id","time","value","cluster")
plot_data$cluster = as.factor(plot_data$cluster)
plot_data$time = as.factor(plot_data$time)
p = ggplot(plot_data, aes(x=time, y=value, group=id, colour=cluster)) +
theme_minimal()  + 
theme(text = element_text(size=15)) +
geom_line()+
geom_point()