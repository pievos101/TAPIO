# aids analysis
library(TAPIO)
library(clusterMLD)
library(JMbayes2)

data(aids)

Longdat2_wide = aids[,c("patient","obstime","CD4","gender","prevOI","AZT")]
Longdat2_wide = aids[,c("patient","obstime","gender","CD4","AZT")]


Longdat2_wide$gender = as.numeric(Longdat2_wide$gender)
#Longdat2_wide$prevOI = as.numeric(Longdat2_wide$prevOI)
Longdat2_wide$AZT = as.numeric(Longdat2_wide$AZT)



# longTAPIO_MLD
res = longTAPIO_MLD(as.matrix(Longdat2_wide[,3:ncol(Longdat2_wide)]),
                user_id =  Longdat2_wide$patient, 
                obsTimes =  Longdat2_wide$obstime,
                k=2, levels=4, n_trees=5, n_features=3)

foundClusIDs = res$cl

# Split by id
split_list = split(Longdat2_wide, Longdat2_wide$id)

# Get unique rows for each id
trueLabels = lapply(split_list, function(x){unique(x$event)})
trueLabels = unlist(trueLabels) 
trueLabels[trueLabels==0] = 1

tmp = trueLabels
trueLabels[tmp==2] = 1
trueLabels[tmp==1] = 2

library(aricode)
ari_TAPIO_MLD  = ARI(trueLabels,foundClusIDs)

print(ari_TAPIO_MLD)

# ClusterMLD
output = LongDataCluster(Longdat2_wide$time,
                        Longdat2_wide[,3:ncol(Longdat2_wide)],
                        Longdat2_wide$id, No.Class=2)

CL = rep(NaN, length(unlist(output$Cluster.res)))

for(xx in 1:length(output$Cluster.res)){
    CL[output$Cluster.res[[xx]]] = xx
}

ari_MLD  = ARI(trueLabels,CL)
print(ari_MLD)