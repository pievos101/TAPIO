# plots
# Univariate 

one = read.table("res_univariate_050.txt")
two = read.table("res_univariate_1.txt")
three = read.table("res_univariate_2.txt")
four = read.table("res_univariate_3.txt")

ALL = list(one, two, three, four)
names(ALL) = c(0.50, 1, 2, 3)

library(ggplot2)
library(reshape)

ALL_melted = melt(ALL)

p = ggplot(ALL_melted, aes(x=L1, y=value, fill=variable)) + 
  geom_boxplot(notch=FALSE) +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("Adjusted R-index")+
  xlab("Within-Cluster Variation") +
  ylim(0,1) +
  theme_minimal()  + 
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size=17)) 
  
  #################################################
  # Multivariate

one = read.table("res_multivariate_3.txt")
two = read.table("res_multivariate_6.txt")
three = read.table("res_multivariate_9.txt")
four = read.table("res_multivariate_12.txt")

one = one[,c(2,3,1)]
two = two[,c(2,3,1)]
three = three[,c(2,3,1)]
four = four[,c(2,3,1)]

ALL = list(one, two, three, four)
names(ALL) = c(3,6,9,12)


library(ggplot2)
library(reshape)

ALL_melted = melt(ALL)
ALL_melted$L1 = factor(ALL_melted$L1, levels=c(3,6,9,12))

p = ggplot(ALL_melted, aes(x=L1, y=value, fill=variable)) + 
  geom_boxplot(notch=FALSE) +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("Adjusted R-index")+
  xlab("Measurenment error (eta)") +
  ylim(0,1) +
  theme_minimal()  + 
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size=17)) 

  ## MULTIVARIATE MLD


## PLOTS
library(ggplot2)
library(reshape)

RES1 = read.table("res_multivariate_mld_k4.txt")
colnames(RES1) = c("clusterMLD (k=4)", "longTAPIO_MLD (k=4)")


RES2 = read.table("res_multivariate_mld.txt")
colnames(RES2) = c("clusterMLD", "longTAPIO_MLD")

RES = cbind(RES1, RES2)

RES_melted = melt(RES)

p = ggplot(RES_melted, aes(x=variable, y=value, color=variable)) + 
  #geom_violin() +
  geom_boxplot(notch=FALSE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  #theme_ipsum() +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("Adjusted R-index")+
  xlab("Methods") +
  #ylim(0.5,1) +
  theme_minimal()  +
  theme(legend.title = element_blank()) +
  #theme(legend.position="bottom") + 
  theme(text = element_text(size=17)) +
  scale_x_discrete(labels = NULL)
  