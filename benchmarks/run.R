# Rscript --vanilla benchmarks/run.R MAIN_LIB ITAPIO_LIB OUTPUT_DIR [N_REPS]
# Each run uses fresh randomness; no fixed RNG initialization is performed.
args <- commandArgs(TRUE)
stopifnot(length(args)>=3L)
main_lib <- normalizePath(args[1])
inductive_lib <- normalizePath(args[2])
output <- args[3]
reps <- if (length(args)>=4L) as.integer(args[4]) else 10L
stopifnot(reps>=2L)
dir.create(output, recursive=TRUE, showWarnings=FALSE)
output <- normalizePath(output)
library(iTAPIO, lib.loc=inductive_lib)
Sys.setenv(OPENBLAS_NUM_THREADS="1", OMP_NUM_THREADS="1")
rscript <- file.path(R.home("bin"), "Rscript")
worker <- normalizePath("benchmarks/worker.R")
results <- list()

for (case in c("regular", "irregular")) {
    for (eta in c(1,3,6,9)) {
        for (replicate in seq_len(reps)) {
            dat <- simLongData(n_total=120, ranTimes=case=="irregular", n_i=8, eta=eta)
            wide <- reshape(dat, idvar=c("subject","time","cluster"),
                            timevar="outcome", direction="wide")
            wide <- wide[order(wide$subject, wide$time), ]
            patients <- unique(wide[,c("subject","cluster")])
            train_ids <- sort(unlist(lapply(split(patients$subject, patients$cluster),
                                            function(i) sample(i, 21))))
            test_ids <- sort(setdiff(patients$subject, train_ids))
            training <- wide$subject %in% train_ids
            x <- as.matrix(wide[,grep("^y\\.", names(wide))])
            if (case == "irregular") {
                ref <- discretize_longitudinal(x[training,], wide$subject[training],
                                               wide$time[training], n_windows=8)
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
            order_rows <- order(all_uid)
            d <- list(case=case, eta=eta, replicate=replicate,
                      all=all_x[order_rows,], all_id=all_uid[order_rows],
                      train=train_x, train_id=train_uid, new=new_x, new_id=new_uid,
                      test_id=test_ids, n_train=length(train_ids),
                      truth=patients$cluster[match(test_ids,patients$subject)])
            stopifnot(all(is.finite(d$all)), all(is.finite(d$train)), all(is.finite(d$new)))
            prefix <- paste(case, eta, replicate, sep="-")
            dataset <- file.path(output, paste0(prefix,".rds"))
            saveRDS(d, dataset)
            for (mode in c("main", "inductive")) {
                lib <- if (mode=="main") main_lib else inductive_lib
                result <- file.path(output,paste0(prefix,"-",mode,".csv"))
                log <- paste0(result,".log")
                status <- system2(rscript, c("--vanilla", shQuote(worker), mode,
                                            shQuote(lib), shQuote(dataset), shQuote(result)),
                                  stdout=log, stderr=log)
                if (status != 0L) stop("Benchmark worker failed; see ",log)
                results[[length(results)+1L]] <- read.csv(result)
            }
            cat(case,"eta",eta,"run",replicate,"completed\n")
            flush.console()
        }
    }
}
results <- do.call(rbind,results)
write.csv(results,file.path(output,"results.csv"),row.names=FALSE)
metrics <- c("ARI","NMI","fit_s","predict_s","total_s")
groups <- c("case","eta","method")
means <- aggregate(results[,metrics], results[,groups], mean)
sds <- aggregate(results[,metrics], results[,groups], sd)
names(sds)[-(1:3)] <- paste0(metrics,"_sd")
summary <- merge(means,sds,by=groups)
summary$n <- reps
write.csv(summary,file.path(output,"summary.csv"),row.names=FALSE)
capture.output(sessionInfo(), file=file.path(output,"sessionInfo.txt"))
versions <- data.frame(component=c("main TAPIO","inductive iTAPIO","aricode","fastcluster"),
    version=c(as.character(packageVersion("TAPIO",lib.loc=main_lib)),
              as.character(packageVersion("iTAPIO",lib.loc=inductive_lib)),
              as.character(packageVersion("aricode")),as.character(packageVersion("fastcluster"))))
write.csv(versions,file.path(output,"versions.csv"),row.names=FALSE)
print(summary)
