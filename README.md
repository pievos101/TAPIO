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

Generating some synthetic longitudinal data with regular time measures.

```{r}
library(TAPIO)

Longdat = simLongData(ranTimes = FALSE)
head(Longdat)

```
The data must be reorganized for longTAPIO.

```{r}

Longdat_wide <- reshape(
    Longdat,
    idvar = c("subject", "time", "cluster"),  # columns that identify each row
    timevar = "outcome",                      # the variable that will become columns
    direction = "wide"
)

head(Longdat_wide)

```
Now we run longTAPIO_sample

```{r}
DD = as.matrix(Longdat_wide[,4:ncol(Longdat_wide)]) # get the feature matrix
rownames(DD) = sort(rep(1:200, 10)) # set rownames according to the subjects
    
res_sample = longTAPIO_sample(DD, k = 4, levels=4, n_trees=1000)

# Get the clustering solution
res_sample$cl

```

Now we run longTAPIO_trajectories
```{r}
DD = as.matrix(Longdat_wide[,4:ncol(Longdat_wide)])

res_trajectories = longTAPIO_trajectories(DD, k = 4, 
                         user_id = Longdat_wide$subject, 
                         levels=4, verbose = 1, n_trees=1000)

# Get the clustering solution 
res_trajectories$cl
```

Let's generate some longitudinal data with irregeular temporal measures

```{r}
Longdat = simLongData(ranTimes = TRUE)
head(Longdat)

```
The data must be reorganized for longTAPIO.

```{r}

Longdat_wide <- reshape(
    Longdat,
    idvar = c("subject", "time", "cluster"),  # columns that identify each row
    timevar = "outcome",                      # the variable that will become columns
    direction = "wide"
)

head(Longdat_wide)

```

Now we run longTAPIO_MLD.

```{r}
# longTAPIO_MLD
res_MLD = longTAPIO_MLD(as.matrix(Longdat_wide[,4:ncol(Longdat_wide)]),
                user_id =  Longdat_wide$subject, 
                obsTimes =  Longdat_wide$time,
                k=4, levels=4, n_trees=10)

# Get the clustering solution 
res_MLD$cl
```

