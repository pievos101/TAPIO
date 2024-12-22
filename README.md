# TAPIO: Hierarchical Clustering with an Ensemble of Principle Component Trees for Interpretable Patient Stratification 

## Required packages
TAPIO depends on the packages NbClust, fastcluster, and 
FactoMineR.

```{r}
library(NbClust)
library(fastcluster)
library(FactoMineR)
```

## Clustering using TAPIO

```{r}
source("~/GitHub/TAPIO/TAPIO.R")

data(iris)
D = as.matrix(iris[,1:4])
outcome = iris[,5]

res = TAPIO(D, k=3)

# Check the performance of clustering
library(aricode)

ARI(res$cl, outcome)

```

## Feature importance

```{r}
source("~/GitHub/TAPIO/importance.R")

fimp = importance(res)

rownames(fimp) = unique(outcome)
colnames(fimp) = colnames(iris)[1:4]

fimp

```