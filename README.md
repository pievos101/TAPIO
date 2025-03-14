# TAPIO
<p align="center">
<img src="https://github.com/pievos101/TAPIO/blob/main/pic.jpg" width="400">
</p>

## Required packages
TAPIO depends on the packages NbClust, fastcluster, and 
FactoMineR.

## Installation
TAPIO can be installed using the package devtools.

```{r}
install.packages("devtools")
library(devtools)

devtools::install_github("pievos101/TAPIO")
library(TAPIO)
```

## Clustering using TAPIO

```{r}
library(TAPIO)

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

fimp = importance(res)

rownames(fimp) = unique(outcome)
colnames(fimp) = colnames(iris)[1:4]

fimp

```

## Clustering longitudinal data using TAPIO
```{r}
library(TAPIO)

data(iris)
D = iris[,1:4]

# custom function to implement min max scaling
minMax <- function(x) {
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}

D_norm = as.data.frame(lapply(D, minMax))
D_norm = as.matrix(D_norm)

outcome = iris[,5]

## define the repeated measures (toy example with iris)
# Ten measures per sample
rownames(D_norm) = sort(rep(1:10, 15))

res = longTAPIO_sample(D_norm, k=3, n_trees=1000, levels=3)

res$cl

```