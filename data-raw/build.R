# Rebuild bundled datasets from an upstream DynForest source data file.
# Usage: Rscript --vanilla data-raw/build.R /path/to/DynForest/data/pbc2.RData
args <- commandArgs(TRUE)
stopifnot(length(args)==1L, file.exists(args[1]))
e <- new.env(parent=emptyenv())
load(args[1], envir=e)
pbc2 <- e$pbc2
iris <- datasets::iris
stopifnot(nrow(pbc2)==1945L, ncol(pbc2)==19L, length(unique(pbc2$id))==312L)
dir.create("data", showWarnings=FALSE)
save(pbc2, file="data/pbc2.rda", compress="xz", version=2)
save(iris, file="data/iris.rda", compress="xz", version=2)
