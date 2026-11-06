library(iTAPIO)

# Fixed windows: aggregate duplicates, interpolate interior gaps, include upper edge.
x <- data.frame(marker=c(2, 4, 8, 10, 20, 26))
ids <- c("a", "a", "a", "a", "b", "b")
times <- c(0, 0.5, 2, 3, 0, 3)
disc <- discretize_longitudinal(x, ids, times, n_windows=3)
stopifnot(inherits(disc, "TAPIO_discretized"),
          identical(disc$breaks, c(0,1,2,3)),
          identical(disc$user_id, rep(c("a","b"), each=3)),
          identical(disc$window, rep(1:3, 2)),
          isTRUE(all.equal(disc$DATA$marker, c(3,6,9,20,23,26))),
          isTRUE(all.equal(disc$empty_fraction, 1/3)))
raw <- discretize_longitudinal(x, ids, times, breaks=disc$breaks, fill_empty=FALSE)
stopifnot(is.na(raw$DATA$marker[2]), is.na(raw$DATA$marker[5]))

# Multiple features and subject order must survive conversion back to long format.
multi <- discretize_longitudinal(cbind(x, second=2*x$marker), ids, times,
                                 breaks=disc$breaks)
stopifnot(isTRUE(all.equal(multi$DATA$second, 2*multi$DATA$marker)))

# Reuse training boundaries; do not estimate them from the new cohort's range.
new <- discretize_longitudinal(data.frame(marker=c(100,104)), c("new","new"),
                               c(1,3), breaks=disc$breaks)
stopifnot(identical(new$breaks, disc$breaks), identical(new$centers, disc$centers),
          isTRUE(all.equal(new$DATA$marker, c(100,100,104))))
no_extrap <- discretize_longitudinal(data.frame(marker=c(100,104)), c("new","new"),
                                     c(1,3), breaks=disc$breaks, extrapolate=FALSE)
stopifnot(is.na(no_extrap$DATA$marker[1]))
warned <- FALSE
outside <- withCallingHandlers(
    discretize_longitudinal(data.frame(marker=c(999,100,104)), rep("new",3),
                            c(-1,1,3), breaks=disc$breaks),
    warning=function(w) { warned <<- TRUE; invokeRestart("muffleWarning") }
)
stopifnot(warned, identical(outside$DATA, new$DATA))

# Installed-package integration: irregular cohort -> frozen windows -> fit/predict.
dat <- simLongData(n_total=20, ranTimes=TRUE)
wide <- reshape(dat, idvar=c("subject","time","cluster"),
                timevar="outcome", direction="wide")
features <- as.matrix(wide[,grep("^y\\.", names(wide))])
train <- wide$subject <= 16
reference <- discretize_longitudinal(features[train,], wide$subject[train],
                                     wide$time[train], n_windows=5)
test <- suppressWarnings(discretize_longitudinal(features[!train,], wide$subject[!train],
                                                 wide$time[!train], breaks=reference$breaks))
model <- longTAPIO(reference$DATA, user_id=reference$user_id, k=4,
                               levels=3, n_trees=10)
pred <- predict(model, test$DATA, user_id=test$user_id)
stopifnot(length(pred$cluster)==4L, all(pred$cluster %in% 1:4),
          all(is.finite(pred$cluster_scores)))

# Prospective preprocessing cannot see observations beyond the cutoff.
available <- !train & wide$time <= reference$breaks[3]
early <- discretize_longitudinal(features[available,], wide$subject[available],
                                 wide$time[available], breaks=reference$breaks)
prefix <- early$window <= 2
early_pred <- predict(model, early$DATA[prefix,], user_id=early$user_id[prefix], visits=2)
stopifnot(length(early_pred$cluster)==length(unique(wide$subject[available])),
          all(is.finite(early_pred$cluster_scores)))
cat("Longitudinal discretization tests passed.\n")
