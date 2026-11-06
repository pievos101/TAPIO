library(iTAPIO)
x <- as.matrix(iris[, 1:4])
train <- unlist(lapply(split(seq_len(nrow(x)), iris$Species), function(i) sample(i, 35)))
new <- x[-train, , drop=FALSE]
for (pc in c("random_weighted", "first")) {
    fit <- TAPIO(x[train, ], k=3, levels=3, n_trees=15, pca_selection=pc)
    frozen <- serialize(fit, NULL)
    pred <- predict(fit, new)
    stopifnot(inherits(fit, "inductiveTAPIO"), length(pred$cluster)==45L,
              all(pred$cluster %in% 1:3), all(is.finite(pred$cluster_scores)),
              identical(pred$cluster[1], predict(fit, new[1,,drop=FALSE])$cluster),
              identical(predict(unserialize(frozen), new), pred),
              identical(serialize(fit, NULL), frozen),
              all(is.finite(importance(fit))))
}
dat <- simLongData(n_total=20, ranTimes=FALSE, n_i=5)
wide <- reshape(dat, idvar=c("subject","time","cluster"), timevar="outcome", direction="wide")
wide <- wide[order(wide$subject, wide$time), ]
x <- as.matrix(wide[,grep("^y\\.", names(wide))])
ids <- wide$subject
training <- ids <= 16
for (pc in c("random_weighted", "first", "random_weighted_95")) {
    fit <- longTAPIO(x[training, ], ids[training], k=4,
                               levels=3, n_trees=10, pca_selection=pc)
    frozen <- serialize(fit, NULL)
    for (visits in c(1L,3L,5L)) {
        pred <- predict(fit, x[!training, ], ids[!training], visits=visits)
        stopifnot(length(pred$cluster)==4L, all(pred$patients==17:20),
                  all(pred$cluster %in% 1:4), all(is.finite(pred$margin)),
                  identical(pred$cluster[1], predict(fit, x[ids==17, ], ids[ids==17], visits=visits)$cluster))
    }
    stopifnot(identical(serialize(fit, NULL), frozen),
              all(is.finite(importance(fit)$global)))
    imp <- importance(fit, newdata=x[!training, ], user_id=ids[!training])
    stopifnot(identical(dim(imp$patient), c(4L, 5L, 5L)),
              all(is.finite(imp$patient)),
              identical(as.integer(imp$predicted_cluster[, 5]), as.integer(pred$cluster)))
}
stopifnot(inherits(try(importance(list()), silent=TRUE), "try-error"))
stopifnot(eval(formals(TAPIO)$pca_selection)[1]=="random_weighted",
          eval(formals(longTAPIO)$pca_selection)[1]=="random_weighted")
cat("Inductive package integration tests passed.\n")
