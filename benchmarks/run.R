# Usage: Rscript --vanilla benchmarks/run.R MAIN_LIB INDUCTIVE_LIB OUTPUT_DIR [N_REPS]
args <- commandArgs(TRUE)
stopifnot(length(args)>=3L)
main_lib <- normalizePath(args[1])
inductive_lib <- normalizePath(args[2])
output <- args[3]
reps <- if (length(args)>=4L) as.integer(args[4]) else 10L
stopifnot(reps>=1L)
dir.create(output, recursive=TRUE, showWarnings=FALSE)
output <- normalizePath(output)
library(iTAPIO, lib.loc=inductive_lib)
Sys.setenv(OPENBLAS_NUM_THREADS="1", OMP_NUM_THREADS="1")
rscript <- file.path(R.home("bin"), "Rscript")
worker <- normalizePath("benchmarks/worker.R")
results <- list()

for (seed in seq_len(reps)) {
    for (case in c("iris", "regular", "irregular")) {
        set.seed(seed)
        if (case == "iris") {
            x <- as.matrix(iris[,1:4])
            train <- sort(unlist(lapply(split(seq_len(nrow(x)), iris$Species),
                                       function(i) sample(i, 35))))
            # Fit min-max scaling on training samples only and reuse for both methods.
            lo <- apply(x[train,], 2, min)
            span <- apply(x[train,], 2, max) - lo
            x <- sweep(sweep(x, 2, lo), 2, span, "/")
            test <- setdiff(seq_len(nrow(x)), train)
            d <- list(case=case, seed=seed, all=x, train=x[train,], new=x[test,],
                      test=test, truth=iris$Species[test], n_train=length(train))
        } else {
            dat <- simLongData(n_total=80, ranTimes=case=="irregular", n_i=5)
            wide <- reshape(dat, idvar=c("subject","time","cluster"),
                            timevar="outcome", direction="wide")
            wide <- wide[order(wide$subject, wide$time), ]
            patients <- unique(wide[,c("subject","cluster")])
            train_ids <- sort(unlist(lapply(split(patients$subject, patients$cluster),
                                            function(i) sample(i, 14))))
            test_ids <- sort(setdiff(patients$subject, train_ids))
            training <- wide$subject %in% train_ids
            x <- as.matrix(wide[,grep("^y\\.", names(wide))])
            if (case == "irregular") {
                ref <- discretize_longitudinal(x[training,], wide$subject[training],
                                               wide$time[training], n_windows=5)
                # The warning is logged when held-out times exceed training boundaries.
                new <- discretize_longitudinal(x[!training,], wide$subject[!training],
                                               wide$time[!training], breaks=ref$breaks)
                train_x <- as.matrix(ref$DATA); new_x <- as.matrix(new$DATA)
                train_uid <- ref$user_id; new_uid <- new$user_id
            } else {
                train_x <- x[training,]; new_x <- x[!training,]
                train_uid <- wide$subject[training]; new_uid <- wide$subject[!training]
            }
            all_x <- rbind(train_x, new_x)
            all_uid <- c(train_uid, new_uid)
            order_rows <- order(all_uid) # Stable ordering retains within-patient windows.
            d <- list(case=case, seed=seed, all=all_x[order_rows,], all_id=all_uid[order_rows],
                      train=train_x, train_id=train_uid, new=new_x, new_id=new_uid,
                      test_id=test_ids, n_train=length(train_ids),
                      truth=patients$cluster[match(test_ids,patients$subject)])
        }
        stopifnot(all(is.finite(d$all)), all(is.finite(d$train)), all(is.finite(d$new)))
        dataset <- file.path(output, paste0(case, "-", seed, ".rds"))
        saveRDS(d, dataset)
        for (mode in c("main", "inductive")) {
            lib <- if (mode=="main") main_lib else inductive_lib
            result <- file.path(output,paste0(case,"-",seed,"-",mode,".csv"))
            log <- paste0(result,".log")
            status <- system2(rscript, c("--vanilla", shQuote(worker), mode,
                                        shQuote(lib), shQuote(dataset), shQuote(result)),
                              stdout=log, stderr=log)
            if (status != 0L) stop("Benchmark worker failed; see ",log)
            results[[length(results)+1L]] <- read.csv(result)
        }
        cat(case, "seed",seed,"completed\n")
        flush.console()
    }
}
results <- do.call(rbind,results)
write.csv(results,file.path(output,"results.csv"),row.names=FALSE)
metrics <- c("ARI","NMI","fit_s","predict_s","total_s")
means <- aggregate(results[,metrics], results[,c("case","method")], mean)
sds <- aggregate(results[,metrics], results[,c("case","method")], sd)
names(sds)[-(1:2)] <- paste0(metrics,"_sd")
summary <- merge(means,sds,by=c("case","method"))
write.csv(summary,file.path(output,"summary.csv"),row.names=FALSE)
capture.output(sessionInfo(), file=file.path(output,"sessionInfo.txt"))
versions <- data.frame(
    component=c("main TAPIO", "inductive iTAPIO", "aricode", "fastcluster"),
    version=c(as.character(packageVersion("TAPIO", lib.loc=main_lib)),
              as.character(packageVersion("iTAPIO", lib.loc=inductive_lib)),
              as.character(packageVersion("aricode")),
              as.character(packageVersion("fastcluster")))
)
write.csv(versions, file.path(output,"versions.csv"), row.names=FALSE)
print(summary)
