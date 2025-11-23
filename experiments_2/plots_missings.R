# Plots missing and comparison with kml3d

zero = read.table("MISS_0.txt")
one = read.table("MISS_10.txt")
two = read.table("MISS_20.txt")
three = read.table("MISS_30.txt")
four = read.table("MISS_40.txt")
five = read.table("MISS_50.txt")


DATA = list(zero, one, two, three, four, five)
names(DATA) = as.factor(c(0, 10, 20, 30, 40, 50))

library(reshape)
library(ggplot2)

DATA_melt = melt(DATA)
colnames(DATA_melt) = c("Method","value","L1")

p <- ggplot(DATA_melt, aes(x=L1, y=value, fill=Method)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("Adjusted R-Index (ARI)") +
  xlab("Percentage of Missing Data") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(text = element_text(size=13)) +
  ylim(0.6,1) 