#' @importFrom stats approx as.dist cutree dist kmeans prcomp reshape rnorm runif time window
#' @importFrom cluster silhouette
#' @importFrom zoo zoo na.approx
#' @importFrom MASS ginv mvrnorm
NULL

# Names evaluated in dplyr/ggplot2 data contexts.
utils::globalVariables(c("cluster", "grp", "mean_y", "outcome", "subject", "time", "x", "y", "Y"))
