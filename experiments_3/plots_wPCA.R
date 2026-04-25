# n_features
one = read.table("n_features_1.txt")
two = read.table("n_features_2.txt")
three = read.table("n_features_NaN.txt")
four = read.table("n_features_All.txt")


ALL = list(one, two, three, four)
names(ALL) = c(1, 2, "sqrt(p)", "p")

library(ggplot2)
library(reshape)

ALL_melted = melt(ALL)

ALL_melted$L1 = factor(ALL_melted$L1, levels=c(1, 2, "sqrt(p)", "p"))

p = ggplot(ALL_melted, aes(x=L1, y=value, fill=variable)) + 
  geom_boxplot(notch=FALSE) +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("Adjusted R-index")+
  xlab("Number of selected features") +
  ylim(0,1) +
  theme_minimal()  + 
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size=17)) 
  
  #################################################
  # Multivariate

###########################################################

# Levels
one = read.table("levels_2.txt")
two = read.table("levels_5.txt")
three = read.table("levels_10.txt")
four = read.table("levels_30.txt")
five = read.table("levels_50.txt")


ALL = list(one, two, three, four, five)
names(ALL) = c(2, 5, 10, 30, 50)

library(ggplot2)
library(reshape)

ALL_melted = melt(ALL)

ALL_melted$L1 = factor(ALL_melted$L1, levels=c(2, 5, 10, 30, 50))

p = ggplot(ALL_melted, aes(x=L1, y=value, fill=variable)) + 
  geom_boxplot(notch=FALSE) +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("Adjusted R-index")+
  xlab("Levels") +
  ylim(0,1) +
  theme_minimal()  + 
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size=17)) 
  
  #################################################
  # Multivariate
