

DATASETS = c("IONOSPHERE","GLASS", "WINE", 
    "IRIS","WDBC","ZOO")


#DATASETS = c("WDBC", "WINE", 
#    "IRIS")

K = c(1,2,10,100,500,1000)
#K = c(2,5,10,30,50)

D_ALL = NULL

for (xx in 1:length(DATASETS)){
    for(yy in 1:length(K)){
        IN = paste(DATASETS[xx],".txt", sep="")    
        D = read.table(IN)
        #D = D[,1] - D[,2]
        D = cbind(DATASETS[xx], D)
        D_ALL = rbind(D_ALL, D)
    }
    
}

colnames(D_ALL) = c("data",K)

library(reshape)
library(ggplot2)

DATA = melt(D_ALL)
colnames(DATA) = c("data","ntrees","value")
DATA$ntrees = as.factor(DATA$ntrees)


add_lines1 <- data.frame(y = c(0.01, 0.13, 0.37, 0.64, 0.05, 0.83),
                       data = DATASETS) 
                       #effort  = rep(y, 3))

#add_lines2 <- data.frame(y = c(0.05, 0.37, 0.64),
#                       data = DATASETS) 
                       #effort  = rep(y, 3))


p <- ggplot(DATA, aes(x=ntrees, y=value)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("Adjusted R-Index (ARI)") +
  xlab("ntrees") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(text = element_text(size=12)) +
  ylim(0,1) +
  facet_wrap(~factor(data)) +
  geom_hline(aes(yintercept = y), data = add_lines1, color = "red")


  
################################################
colnames(DATA) = c("ID","Method","value")
p <- ggplot(DATA, aes(x=Method, y=value, fill=Method)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("c-index") +
  xlab("Subtyping Methods") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(text = element_text(size=14)) +
  #coord_flip() +
  geom_hline(yintercept=-log10(0.05), 
                color = "red", linewidth=1) +
  geom_hline(yintercept=-log10(0.01), linetype="dashed", 
                color = "red", linewidth=0.5) +
  ylim(0.4,0.8) 


  
