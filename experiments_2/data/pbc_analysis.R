library(TAPIO)
library(clusterMLD)
#detach("package:glmnet", unload = TRUE)
#detach("package:jomo", unload = TRUE)
#detach("package:survival", unload = TRUE)

#library(survival)

load("pbc2.RData")

Longdat2_wide <- pbc2[,c("id","time","years","event","serBilir","albumin","SGOT")]#,"albumin","prothrombin")]#,"alkaline")]#,"platelets","prothrombin","histologic")]
#Longdat2_wide <- pbc2[,c("id","time","event","SGOT","serBilir","albumin")]#,"alkaline")]


# Filter by number of measures
#tab = table(Longdat2_wide$id)
#idd = which(tab>=2)
#Longdat2_wide = Longdat2_wide[Longdat2_wide$id %in% idd, ]

# Get the event 
# Split by id
split_list = split(Longdat2_wide, Longdat2_wide$id)

# Get unique rows for each id
trueLabels = lapply(split_list, function(x){unique(x$event)})
trueLabels = unlist(trueLabels) 
trueLabels[trueLabels==2] = 1

###################################################

#Get the time to event
split_list = split(Longdat2_wide, Longdat2_wide$id)

# Get unique rows for each id
trueTime = lapply(split_list, function(x){unique(x$years)})
trueTime = unlist(trueTime)
############################################


survival = cbind(trueTime,trueLabels)
colnames(survival) = c("Survival","Death")
survival = as.data.frame(survival)

# longTAPIO_MLD
res = longTAPIO_MLD(as.matrix(Longdat2_wide[,5:ncol(Longdat2_wide)]),
                user_id =  Longdat2_wide$id, 
                obsTimes =  Longdat2_wide$time,
                k=NaN, levels=3, n_trees=50, n_features=NaN, max.k=5, 
                method="ward.D2", replace=TRUE, scale=TRUE)

foundClusIDs = res$cl

groups   <- factor(res$cl) 
names(groups) = rownames(survival)
coxFit <- survival::coxph(Surv(time = Survival, event = Death) ~ groups, data = survival, 
                                    ties="exact")
cox_snf  <- round(summary(coxFit)$sctest[3],digits = 40);
#print(cox_snf)
P_TAPIO = round(summary(coxFit)$sctest[3],digits = 40);
K_TAPIO = length(unique(groups))
C_TAPIO = summary(coxFit)$concordance 

print(P_TAPIO)
print(K_TAPIO)
print(C_TAPIO)


# ClusterMLD
output = LongDataCluster(Longdat2_wide$time,
                        Longdat2_wide[,5:ncol(Longdat2_wide)],
                        Longdat2_wide$id, No.Class=2)

CL = rep(NaN, length(unlist(output$Cluster.res)))

for(xx in 1:length(output$Cluster.res)){
    CL[output$Cluster.res[[xx]]] = xx
}


groups   <- factor(CL) 
names(groups) = rownames(survival)
coxFit <- survival::coxph(Surv(time = Survival, event = Death) ~ groups, data = survival, 
                                    ties="exact")
cox_snf  <- round(summary(coxFit)$sctest[3],digits = 40);
#print(cox_snf)
P_MLD = round(summary(coxFit)$sctest[3],digits = 40);
K_MLD = length(unique(groups))
C_MLD = summary(coxFit)$concordance 

print(P_MLD)
print(K_MLD)
print(C_MLD)

