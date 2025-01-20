# TAPIO
<p align="center">
<img src="https://github.com/pievos101/TAPIO/blob/main/pic.jpg" width="400">
</p>

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
D = iris[,1:4]

# custom function to implement min max scaling
minMax <- function(x) {
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}

D_norm = as.data.frame(lapply(D, minMax))
D_norm = as.matrix(D_norm)

outcome = iris[,5]

res = TAPIO(D_norm, k=3, n_trees=1000, levels=3)

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