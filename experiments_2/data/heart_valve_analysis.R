# heart.valve
library(TAPIO)
library(clusterMLD)
library(joineRML)

#Longdat2_wide = heart.valve[,c("num","time","fuyrs","status",
#            "grad","lvmi","ef","bsa","creat")] # grad has NaNs

Longdat2_wide = heart.valve[,c("num","time","fuyrs","status",
            "lvmi","ef","bsa","creat")] # only continous

#Longdat2_wide = heart.valve[,c("num","time","fuyrs","status",
#            "log.lvmi","ef","bsa","lvh","prenyha","redo","size","con.cabg",
#            "creat","dm","acei","lv","emergenc","hc","sten.reg.mix")]


#continuous_cols <- sapply(Longdat2_wide, function(col)
#  is.numeric(col) && length(unique(col)) > 10
#)

#n_iter = 50 
#RES = matrix(NaN, n_iter, 2)
#colnames(RES) = c("longTAPIO_MLD","clusterMLD")

#for(xx in 1:n_iter){

# Bootstrap
#num = Longdat2_wide$num
#num = unique(num)
#s_num = sample(num, 100, replace=FALSE)
#Longdat2_wide_s = Longdat2_wide[Longdat2_wide$num %in% s_num, ]

Longdat2_wide_s = Longdat2_wide

# Get the event 
# Split by id
split_list = split(Longdat2_wide_s, Longdat2_wide_s$num)

# Get unique rows for each id
trueLabels = lapply(split_list, function(x){unique(x$status)})
trueLabels = unlist(trueLabels) 

###################################################

#Get the time to event
split_list = split(Longdat2_wide_s, Longdat2_wide_s$num)

# Get unique rows for each id
trueTime = lapply(split_list, function(x){unique(x$fuyrs)})
trueTime = unlist(trueTime)
############################################


survival = cbind(trueTime,trueLabels)
colnames(survival) = c("Survival","Death")
survival = as.data.frame(survival)


# longTAPIO_MLD
res = longTAPIO_MLD(as.matrix(Longdat2_wide_s[,5:ncol(Longdat2_wide_s)]),
                user_id =  Longdat2_wide_s$num, 
                obsTimes =  Longdat2_wide_s$time,
                k=NaN, levels=5, n_trees=100, n_features=4, max.k=5,
                method="ward.D", replace=TRUE, scale=TRUE)

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
output = LongDataCluster(Longdat2_wide_s$time,
                        Longdat2_wide_s[,5:ncol(Longdat2_wide_s)],
                        Longdat2_wide_s$num)#, No.Class=2)

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

#print(RES)

