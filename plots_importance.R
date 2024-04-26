
library(reshape)
library(ggplot2)

DATASETS = c("IONOSPHERE","GLASS", "WINE", 
    "IRIS","WDBC","ZOO")


DATASETS = c("ZOO", "IRIS")

D_ALL = NULL

for (xx in 1:length(DATASETS)){
        IN = paste(DATASETS[xx],".txt", sep="")    
        D = read.table(IN)
        rownames(D) = paste("cluster", 1:dim(D)[1])
        D = cbind(rownames(D), D)
        #D = D[,1] - D[,2]
        D = cbind(DATASETS[xx], melt(D))
        D_ALL = rbind(D_ALL, D)
    
}

colnames(D_ALL) = c("data", "cluster", "variable","value")


D_ALL$variable = factor(D_ALL$variable)
D_ALL$cluster = factor(D_ALL$cluster)
D_ALL$value = as.numeric(D_ALL$value)


p <- ggplot(D_ALL, aes(x=variable, y=value, fill=cluster)) +
  geom_bar(position="dodge", stat="identity") +
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("Feature Importance") +
  xlab("Features") +  
  #theme_bw() +
  theme_minimal()  + 
  coord_flip() +
  theme(text = element_text(size=12)) +
  ylim(0,1) +
  facet_wrap(~factor(data), scales = "free") 
  

  