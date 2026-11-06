# One package per fresh process, avoiding namespace collisions between libraries.
args <- commandArgs(TRUE)
mode <- args[1]
if (mode == "main") {
    library(TAPIO, lib.loc=args[2])
} else {
    library(iTAPIO, lib.loc=args[2])
}
d <- readRDS(args[3])
set.seed(1000 + d$seed)

if (mode == "main") {
    elapsed <- system.time({
        if (d$case == "iris") {
            fit <- TAPIO(d$all, k=3, n_features=2, n_trees=100, levels=3,
                          pca_selection="random_weighted")
            predicted <- fit$cl[d$test]
        } else {
            fit <- longTAPIO_trajectories(d$all, user_id=d$all_id, k=4,
                                          n_features=2, n_trees=100, levels=3,
                                          scale=TRUE, pca_selection="random_weighted")
            predicted <- fit$cl[match(d$test_id, unique(d$all_id))]
        }
    })[["elapsed"]]
    fit_s <- elapsed
    predict_s <- 0
} else {
    fit_s <- system.time({
        if (d$case == "iris") {
            fit <- TAPIO(d$train, k=3, n_features=2,
                                   n_trees=100, levels=3, scale=FALSE)
        } else {
            fit <- longTAPIO(d$train, user_id=d$train_id, k=4,
                                       n_features=2, n_trees=100, levels=3, scale=TRUE)
        }
    })[["elapsed"]]
    predict_s <- system.time({
        if (d$case == "iris") predicted <- predict(fit, d$new)$cluster
        else {
            pred <- predict(fit, d$new, user_id=d$new_id)
            stopifnot(all(pred$patients == d$test_id))
            predicted <- pred$cluster
        }
    })[["elapsed"]]
}
stopifnot(length(predicted)==length(d$truth), !anyNA(predicted), !anyNA(d$truth))
result <- data.frame(case=d$case, seed=d$seed, method=mode,
                     n_train=d$n_train, n_test=length(d$truth),
                     ARI=aricode::ARI(d$truth, predicted),
                     NMI=aricode::NMI(d$truth, predicted),
                     fit_s=fit_s, predict_s=predict_s, total_s=fit_s+predict_s)
write.csv(result, args[4], row.names=FALSE)
