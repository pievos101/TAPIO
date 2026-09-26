# =============================================================================
# INDUCTIVE longTAPIO
# PROGRESSIVE OUT-OF-SAMPLE BENCHMARK
# =============================================================================
#
# PURPOSE
#
# Demonstrate progressive assignment of completely unseen patients:
#
#       Visit 1
#       Visits 1:2
#       Visits 1:3
#       ...
#       Visits 1:10
#
# and simultaneously evaluate how feature importance evolves as additional
# visits become available.
#
#
# DESIGN
#
#   * N = 200 patients
#   * K = 4 true clusters
#   * 5 longitudinal features
#   * 10 REGULAR visits
#   * 70% training / 30% test
#   * stratified patient-level split
#   * 30 independent simulation runs
#
#
# STRICT INDUCTIVE SETTING
#
#   1. longTAPIO_inductive() is fitted ONLY on training patients.
#
#   2. Test patients are completely unseen during fitting.
#
#   3. The fitted model remains frozen.
#
#   4. importance_longTAPIO_inductive() evaluates unseen patients
#      progressively using prefixes 1,...,10.
#
#   5. True test labels are used ONLY for ARI/NMI evaluation.
#
#
# OUTPUT
#
#   Plot 1:
#       Out-of-sample ARI evolution across visits
#
#   Plot 2:
#       Overall feature-importance evolution across visits
#
#   Plot 3:
#       Feature-importance heatmap
#
#   Plot 4:
#       Cluster-specific feature-importance evolution
#
#   Plot 5:
#       Incremental feature importance
#
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

library(MASS)
library(aricode)
library(ggplot2)


# =============================================================================
# 1. CHECK EXISTING longTAPIO FUNCTIONS
# =============================================================================

if(!exists("longTAPIO_inductive")) {

    stop(
        "Please source/load longTAPIO_inductive() first."
    )
}


if(!exists("importance_longTAPIO_inductive")) {

    stop(
        "Please source/load importance_longTAPIO_inductive() first."
    )
}


if(!exists("generate_random_curve")) {

    stop(
        paste0(
            "generate_random_curve() is not available. ",
            "Please source your original function first."
        )
    )
}


# =============================================================================
# 2. GLOBAL SETTINGS
# =============================================================================

N_RUNS <- 30

N_TOTAL <- 200

TRAIN_FRACTION <- 0.70

K <- 4

OUTCOMES <- 5

N_VISITS <- 10

ETA <- 3

SIGMA_DIAG <- rep(
    3,
    OUTCOMES
)


# -----------------------------------------------------------------------------
# longTAPIO settings
# -----------------------------------------------------------------------------

N_TREES <- 500

LEVELS <- 4

PCA_SELECTION <- "random_weighted"


# =============================================================================
# 3. ORIGINAL simLongData()
# =============================================================================

simLongData <- function(
    n_total = 200,
    K = 4,
    outcomes = 5,
    eta = 3,
    cluster_sizes = rep(
        n_total / K,
        K
    ),
    ranTimes = TRUE,
    n_i = 10,
    sigma_diag = rep(
        3,
        5
    )
) {

    # =========================================================================
    # CLUSTER-SPECIFIC MEAN FUNCTIONS
    # =========================================================================

    mean_functions <- list(

        # ---------------------------------------------------------------------
        # Cluster 1
        # ---------------------------------------------------------------------

        list(
            function(t) 8 * t - 0.6 * t^2,
            function(t) t,
            function(t) -10 + 6 * t - 0.4 * t^2,
            function(t) -1 + t,
            function(t) -2 * t + 0.1 * t^2
        ),


        # ---------------------------------------------------------------------
        # Cluster 2
        # ---------------------------------------------------------------------

        list(
            function(t) 20 - 6 * t + 0.3 * t^2,
            function(t) -t,
            function(t) -10 + 6 * t - 0.4 * t^2,
            function(t) -1 + t,
            function(t) -2 * t + 0.1 * t^2
        ),


        # ---------------------------------------------------------------------
        # Cluster 3
        # ---------------------------------------------------------------------

        list(
            function(t) 0,
            function(t) -7 * t + 0.5 * t^2,
            function(t) 0.2 * t,
            function(t) -1 + t,
            function(t) -2 * t + 0.1 * t^2
        ),


        # ---------------------------------------------------------------------
        # Cluster 4
        # ---------------------------------------------------------------------

        list(
            function(t) 20,
            function(t) -20 + t,
            function(t) 0.2 * t,
            function(t) 10 + 2 * t - 0.2 * t^2,
            function(t) -2 * t + 0.1 * t^2
        )
    )


    # =========================================================================
    # RANDOM-EFFECT COVARIANCE
    # =========================================================================

    R <- matrix(
        c(
             1,    0.5,  0.3, -0.1, 0,
             0.5,  1,    0.2,  0.1, 0,
             0.3,  0.2,  1,    0.1, 0,
            -0.1,  0.1,  0.1,  1,   0,
             0,    0,    0,    0,   1
        ),
        nrow = outcomes
    )


    Sigma_sigma <-
        diag(
            sigma_diag
        ) %*%
        R %*%
        diag(
            sigma_diag
        )


    # =========================================================================
    # SIMULATE
    # =========================================================================

    sim_data <- list()

    subject_id <- 1


    for(k in 1:K) {

        for(i in 1:cluster_sizes[k]) {


            if(ranTimes) {

                n_i_subject <- sample(
                    4:12,
                    1
                )


                times <- sort(
                    c(
                        0,
                        sort(
                            runif(
                                n_i_subject - 1,
                                min = 0.5,
                                max = 11
                            )
                        )
                    )
                )

            } else {

                n_i_subject <- n_i

                times <- 1:n_i_subject
            }


            # -----------------------------------------------------------------
            # Subject-specific outcome effects
            # -----------------------------------------------------------------

            u_i <- MASS::mvrnorm(
                1,
                mu = rep(
                    0,
                    outcomes
                ),
                Sigma = Sigma_sigma
            )


            # -----------------------------------------------------------------
            # Visits
            # -----------------------------------------------------------------

            for(j in 1:n_i_subject) {

                t_ij <- times[j]


                random_effect_t <-
                    generate_random_curve(
                        t_ij
                    )


                # -------------------------------------------------------------
                # Outcomes
                # -------------------------------------------------------------

                for(h in 1:outcomes) {

                    mu <-
                        mean_functions[[k]][[h]](
                            t_ij
                        )


                    y_ijh <-
                        mu +
                        random_effect_t +
                        u_i[h] +
                        rnorm(
                            1,
                            mean = 0,
                            sd = eta
                        )


                    sim_data[[length(sim_data) + 1]] <-
                        data.frame(
                            subject = subject_id,
                            time = t_ij,
                            outcome = h,
                            y = y_ijh,
                            cluster = k
                        )
                }
            }


            subject_id <-
                subject_id +
                1
        }
    }


    sim_df <- do.call(
        rbind,
        sim_data
    )


    rownames(sim_df) <- NULL


    return(
        sim_df
    )
}


# =============================================================================
# 4. CONVERT LONG DATA TO PATIENT x VISIT x FEATURE ARRAY
# =============================================================================

long_to_array <- function(
    dat
) {

    subjects <- sort(
        unique(
            dat$subject
        )
    )


    times <- sort(
        unique(
            dat$time
        )
    )


    outcomes <- sort(
        unique(
            dat$outcome
        )
    )


    N <- length(
        subjects
    )

    V <- length(
        times
    )

    P <- length(
        outcomes
    )


    X <- array(
        NA_real_,
        dim = c(
            N,
            V,
            P
        ),
        dimnames = list(
            patient = as.character(
                subjects
            ),
            visit = as.character(
                times
            ),
            feature = paste0(
                "y",
                outcomes
            )
        )
    )


    truth <- integer(
        N
    )


    for(i in seq_along(subjects)) {

        id <- subjects[i]


        d_i <- dat[
            dat$subject == id,
            ,
            drop = FALSE
        ]


        truth[i] <- unique(
            d_i$cluster
        )[1]


        for(v in seq_along(times)) {

            for(p in seq_along(outcomes)) {

                z <- d_i$y[
                    d_i$time == times[v] &
                    d_i$outcome == outcomes[p]
                ]


                if(length(z) == 1) {

                    X[
                        i,
                        v,
                        p
                    ] <- z
                }
            }
        }
    }


    if(any(!is.finite(X))) {

        stop(
            "Non-finite values detected in regular longitudinal array."
        )
    }


    return(
        list(
            X = X,
            truth = truth,
            subjects = subjects,
            times = times,
            feature_names = paste0(
                "y",
                outcomes
            )
        )
    )
}


# =============================================================================
# 5. STRATIFIED PATIENT-LEVEL TRAIN / TEST SPLIT
# =============================================================================

make_stratified_split <- function(
    truth,
    train_fraction = 0.70,
    seed = 1
) {

    set.seed(
        seed
    )


    train_idx <- integer(
        0
    )


    classes <- sort(
        unique(
            truth
        )
    )


    for(k in classes) {

        ids <- which(
            truth == k
        )


        n_train_k <- floor(
            length(ids) *
            train_fraction
        )


        selected <- sample(
            ids,
            size = n_train_k,
            replace = FALSE
        )


        train_idx <- c(
            train_idx,
            selected
        )
    }


    train_idx <- sort(
        train_idx
    )


    test_idx <- setdiff(
        seq_along(
            truth
        ),
        train_idx
    )


    return(
        list(
            train = train_idx,
            test = test_idx
        )
    )
}


# =============================================================================
# 6. ARRAY -> longTAPIO DATA FORMAT
# =============================================================================

array_to_longTAPIO <- function(
    X
) {

    N <- dim(X)[1]

    V <- dim(X)[2]

    P <- dim(X)[3]


    DATA <- matrix(
        NA_real_,
        nrow = N * V,
        ncol = P
    )


    user_id <- rep(
        seq_len(N),
        each = V
    )


    for(i in seq_len(N)) {

        rows_i <- (
            (i - 1) * V + 1
        ):(
            i * V
        )


        DATA[
            rows_i,
        ] <- X[
            i,
            ,
        ]
    }


    DATA <- as.data.frame(
        DATA
    )


    names(DATA) <- paste0(
        "y",
        seq_len(P)
    )


    return(
        list(
            DATA = DATA,
            user_id = user_id
        )
    )
}


# =============================================================================
# 7. FIT TRAINING-ONLY longTAPIO
# =============================================================================

fit_training_longTAPIO <- function(
    X_train,
    seed
) {

    set.seed(
        seed
    )


    P <- dim(
        X_train
    )[3]


    prepared <- array_to_longTAPIO(
        X_train
    )


    # Same feature-sampling rule used in the previous benchmark

    m <- max(
        2,
        ceiling(
            sqrt(P)
        )
    )


    m <- min(
        m,
        P
    )


    fit <- longTAPIO_inductive(
        DATA = prepared$DATA,
        user_id = prepared$user_id,
        k = K,
        n_features = m,
        n_trees = N_TREES,
        levels = LEVELS,
        method = "ward.D2",
        scale = TRUE,
        replace = FALSE,
        pca_selection = PCA_SELECTION
    )


    return(
        fit
    )
}


# =============================================================================
# 8. ONE COMPLETE SIMULATION RUN
# =============================================================================

run_progressive_benchmark <- function(
    run
) {

    # =========================================================================
    # SEEDS
    # =========================================================================

    data_seed <-
        10000 +
        run


    split_seed <-
        20000 +
        run


    model_seed <-
        30000 +
        run


    # =========================================================================
    # SIMULATE REGULAR LONGITUDINAL DATA
    # =========================================================================

    set.seed(
        data_seed
    )


    dat <- simLongData(
        n_total = N_TOTAL,
        K = K,
        outcomes = OUTCOMES,
        eta = ETA,
        cluster_sizes = rep(
            N_TOTAL / K,
            K
        ),
        ranTimes = FALSE,
        n_i = N_VISITS,
        sigma_diag = SIGMA_DIAG
    )


    # =========================================================================
    # ARRAY REPRESENTATION
    # =========================================================================

    arr <- long_to_array(
        dat
    )


    X <- arr$X

    truth <- arr$truth


    # =========================================================================
    # TRAIN / TEST SPLIT
    # =========================================================================

    split <- make_stratified_split(
        truth = truth,
        train_fraction = TRAIN_FRACTION,
        seed = split_seed
    )


    X_train <- X[
        split$train,
        ,
        ,
        drop = FALSE
    ]


    X_test <- X[
        split$test,
        ,
        ,
        drop = FALSE
    ]


    truth_train <- truth[
        split$train
    ]


    truth_test <- truth[
        split$test
    ]


    # =========================================================================
    # PREPARE TEST DATA
    # =========================================================================
    #
    # IMPORTANT:
    #
    # We supply ALL 10 visits to importance_longTAPIO_inductive().
    #
    # The function itself progressively extracts:
    #
    #       H = 1
    #       H = 2
    #       ...
    #       H = 10
    #
    # =========================================================================

    test_prepared <- array_to_longTAPIO(
        X_test
    )


    # =========================================================================
    # INFORMATION
    # =========================================================================

    cat(
        "\n",
        paste(
            rep(
                "=",
                100
            ),
            collapse = ""
        ),
        "\n",
        "RUN ",
        run,
        "/",
        N_RUNS,
        "\n",
        "Training patients : ",
        length(
            split$train
        ),
        "\n",
        "Test patients     : ",
        length(
            split$test
        ),
        "\n",
        "Visits            : ",
        N_VISITS,
        "\n",
        "Features          : ",
        OUTCOMES,
        "\n",
        paste(
            rep(
                "=",
                100
            ),
            collapse = ""
        ),
        "\n",
        sep = ""
    )


    # =========================================================================
    # FIT MODEL ON TRAINING PATIENTS ONLY
    # =========================================================================

    cat(
        "\nFitting training-only longTAPIO model...\n"
    )


    t0 <- proc.time()[3]


    fit <- fit_training_longTAPIO(
        X_train = X_train,
        seed = model_seed
    )


    fitting_time <-
        proc.time()[3] -
        t0


    cat(
        sprintf(
            "Model fitted in %.2f seconds.\n",
            fitting_time
        )
    )


    # =========================================================================
    # PROGRESSIVE OUT-OF-SAMPLE IMPORTANCE
    # =========================================================================
    #
    # Your function simultaneously calculates:
    #
    #   IMP$predicted_cluster
    #       N_test x T
    #
    #   IMP$patient
    #       N_test x P x T
    #
    #   IMP$cluster_progressive
    #       K x P x T
    #
    #   IMP$incremental
    #       K x P x T
    #
    # =========================================================================

    cat(
        "\nCalculating progressive out-of-sample assignments and importance...\n"
    )


    t0 <- proc.time()[3]


    IMP <- importance_longTAPIO_inductive(
        res = fit,
        newdata = test_prepared$DATA,
        user_id = test_prepared$user_id,
        normalize = TRUE
    )


    importance_time <-
        proc.time()[3] -
        t0


    cat(
        sprintf(
            "Progressive evaluation completed in %.2f seconds.\n\n",
            importance_time
        )
    )


    # =========================================================================
    # CHECK OUTPUT DIMENSIONS
    # =========================================================================

    if(
        nrow(
            IMP$predicted_cluster
        ) !=
        length(
            truth_test
        )
    ) {

        stop(
            "Number of predicted test patients does not match truth_test."
        )
    }


    if(
        ncol(
            IMP$predicted_cluster
        ) !=
        N_VISITS
    ) {

        stop(
            "Unexpected number of progressive prediction visits."
        )
    }


    # =========================================================================
    # STORAGE
    # =========================================================================

    performance_rows <- list()

    patient_importance_rows <- list()

    cluster_importance_rows <- list()

    incremental_rows <- list()


    performance_counter <- 1L

    patient_counter <- 1L

    cluster_counter <- 1L

    incremental_counter <- 1L


    # =========================================================================
    # VISIT-BY-VISIT PERFORMANCE
    # =========================================================================

    for(H in seq_len(N_VISITS)) {

        pred_H <- IMP$predicted_cluster[
            ,
            H
        ]


        ari_H <- aricode::ARI(
            truth_test,
            pred_H
        )


        nmi_H <- aricode::NMI(
            truth_test,
            pred_H
        )


        cat(
            sprintf(
                "Visit %2d | Test ARI = %.3f | Test NMI = %.3f\n",
                H,
                ari_H,
                nmi_H
            )
        )


        performance_rows[[performance_counter]] <-
            data.frame(
                run = run,
                visit = H,
                n_train = length(
                    truth_train
                ),
                n_test = length(
                    truth_test
                ),
                ARI = ari_H,
                NMI = nmi_H,
                fitting_seconds = fitting_time,
                importance_seconds = importance_time,
                stringsAsFactors = FALSE
            )


        performance_counter <-
            performance_counter +
            1L


        # =====================================================================
        # PATIENT-LEVEL FEATURE IMPORTANCE
        # =====================================================================
        #
        # IMP$patient:
        #
        #       patient x feature x visit
        #
        # Store individual patient values. This means that later we can
        # calculate:
        #
        #       overall importance
        #       variability
        #       patient-specific plots
        #
        # without rerunning the model.
        #
        # =====================================================================

        for(p in seq_len(OUTCOMES)) {

            imp_values <- IMP$patient[
                ,
                p,
                H
            ]


            for(i in seq_along(imp_values)) {

                patient_importance_rows[[patient_counter]] <-
                    data.frame(
                        run = run,
                        visit = H,
                        patient = i,
                        feature = paste0(
                            "y",
                            p
                        ),
                        importance = imp_values[i],
                        predicted_cluster = pred_H[i],
                        true_cluster = truth_test[i],
                        stringsAsFactors = FALSE
                    )


                patient_counter <-
                    patient_counter +
                    1L
            }
        }


        # =====================================================================
        # CLUSTER-PROGRESSIVE IMPORTANCE
        # =====================================================================

        for(k_id in seq_len(K)) {

            for(p in seq_len(OUTCOMES)) {

                cluster_importance_rows[[cluster_counter]] <-
                    data.frame(
                        run = run,
                        visit = H,
                        cluster = k_id,
                        feature = paste0(
                            "y",
                            p
                        ),
                        importance =
                            IMP$cluster_progressive[
                                k_id,
                                p,
                                H
                            ],
                        stringsAsFactors = FALSE
                    )


                cluster_counter <-
                    cluster_counter +
                    1L
            }
        }


        # =====================================================================
        # INCREMENTAL IMPORTANCE
        # =====================================================================

        for(k_id in seq_len(K)) {

            for(p in seq_len(OUTCOMES)) {

                incremental_rows[[incremental_counter]] <-
                    data.frame(
                        run = run,
                        visit = H,
                        cluster = k_id,
                        feature = paste0(
                            "y",
                            p
                        ),
                        incremental_importance =
                            IMP$incremental[
                                k_id,
                                p,
                                H
                            ],
                        stringsAsFactors = FALSE
                    )


                incremental_counter <-
                    incremental_counter +
                    1L
            }
        }
    }


    # =========================================================================
    # RETURN
    # =========================================================================

    return(
        list(

            performance = do.call(
                rbind,
                performance_rows
            ),

            patient_importance = do.call(
                rbind,
                patient_importance_rows
            ),

            cluster_importance = do.call(
                rbind,
                cluster_importance_rows
            ),

            incremental = do.call(
                rbind,
                incremental_rows
            )
        )
    )
}


# =============================================================================
# 9. RUN COMPLETE BENCHMARK
# =============================================================================

cat(
    "\n",
    paste(
        rep(
            "=",
            110
        ),
        collapse = ""
    ),
    "\n",
    "PROGRESSIVE INDUCTIVE longTAPIO BENCHMARK\n",
    "\n",
    "Runs             : ",
    N_RUNS,
    "\n",
    "Patients/run     : ",
    N_TOTAL,
    "\n",
    "Training fraction: ",
    TRAIN_FRACTION,
    "\n",
    "Visits           : ",
    N_VISITS,
    "\n",
    "Features         : ",
    OUTCOMES,
    "\n",
    "Trees            : ",
    N_TREES,
    "\n",
    "PCA selection    : ",
    PCA_SELECTION,
    "\n",
    paste(
        rep(
            "=",
            110
        ),
        collapse = ""
    ),
    "\n",
    sep = ""
)


ALL_PERFORMANCE <- list()

ALL_PATIENT_IMPORTANCE <- list()

ALL_CLUSTER_IMPORTANCE <- list()

ALL_INCREMENTAL <- list()


for(run in seq_len(N_RUNS)) {

    z <- run_progressive_benchmark(
        run = run
    )


    ALL_PERFORMANCE[[run]] <-
        z$performance


    ALL_PATIENT_IMPORTANCE[[run]] <-
        z$patient_importance


    ALL_CLUSTER_IMPORTANCE[[run]] <-
        z$cluster_importance


    ALL_INCREMENTAL[[run]] <-
        z$incremental
}


performance_results <- do.call(
    rbind,
    ALL_PERFORMANCE
)


patient_importance_results <- do.call(
    rbind,
    ALL_PATIENT_IMPORTANCE
)


cluster_importance_results <- do.call(
    rbind,
    ALL_CLUSTER_IMPORTANCE
)


incremental_results <- do.call(
    rbind,
    ALL_INCREMENTAL
)


# =============================================================================
# 10. PERFORMANCE SUMMARY
# =============================================================================

performance_summary <- do.call(
    rbind,
    lapply(
        seq_len(N_VISITS),
        function(H) {

            d <- performance_results[
                performance_results$visit == H,
                ,
                drop = FALSE
            ]


            data.frame(

                visit = H,

                mean_ARI = mean(
                    d$ARI,
                    na.rm = TRUE
                ),

                sd_ARI = sd(
                    d$ARI,
                    na.rm = TRUE
                ),

                se_ARI =
                    sd(
                        d$ARI,
                        na.rm = TRUE
                    ) /
                    sqrt(
                        sum(
                            is.finite(
                                d$ARI
                            )
                        )
                    ),

                mean_NMI = mean(
                    d$NMI,
                    na.rm = TRUE
                ),

                sd_NMI = sd(
                    d$NMI,
                    na.rm = TRUE
                ),

                stringsAsFactors = FALSE
            )
        }
    )
)


performance_summary$lower_ARI <- pmax(
    0,
    performance_summary$mean_ARI -
        performance_summary$sd_ARI
)


performance_summary$upper_ARI <- pmin(
    1,
    performance_summary$mean_ARI +
        performance_summary$sd_ARI
)


cat(
    "\n\n",
    paste(
        rep(
            "=",
            100
        ),
        collapse = ""
    ),
    "\n",
    "OUT-OF-SAMPLE PERFORMANCE BY VISIT\n",
    paste(
        rep(
            "=",
            100
        ),
        collapse = ""
    ),
    "\n",
    sep = ""
)


print(
    performance_summary,
    row.names = FALSE
)


# =============================================================================
# 11. OVERALL FEATURE IMPORTANCE SUMMARY
# =============================================================================
#
# Average patient-specific importance over all unseen test patients and runs.
#
# =============================================================================

feature_names <- paste0(
    "y",
    seq_len(OUTCOMES)
)


importance_summary_rows <- list()

counter <- 1L


for(H in seq_len(N_VISITS)) {

    for(feature_name in feature_names) {

        d <- patient_importance_results[
            patient_importance_results$visit == H &
            patient_importance_results$feature == feature_name,
            ,
            drop = FALSE
        ]


        importance_summary_rows[[counter]] <-
            data.frame(

                visit = H,

                feature = feature_name,

                mean_importance = mean(
                    d$importance,
                    na.rm = TRUE
                ),

                sd_importance = sd(
                    d$importance,
                    na.rm = TRUE
                ),

                stringsAsFactors = FALSE
            )


        counter <-
            counter +
            1L
    }
}


importance_summary <- do.call(
    rbind,
    importance_summary_rows
)


importance_summary$feature <- factor(
    importance_summary$feature,
    levels = feature_names
)


# =============================================================================
# 12. CLUSTER-SPECIFIC IMPORTANCE SUMMARY
# =============================================================================

cluster_summary_rows <- list()

counter <- 1L


for(H in seq_len(N_VISITS)) {

    for(k_id in seq_len(K)) {

        for(feature_name in feature_names) {

            d <- cluster_importance_results[
                cluster_importance_results$visit == H &
                cluster_importance_results$cluster == k_id &
                cluster_importance_results$feature == feature_name,
                ,
                drop = FALSE
            ]


            cluster_summary_rows[[counter]] <-
                data.frame(

                    visit = H,

                    cluster = paste0(
                        "Cluster ",
                        k_id
                    ),

                    feature = feature_name,

                    mean_importance = mean(
                        d$importance,
                        na.rm = TRUE
                    ),

                    sd_importance = sd(
                        d$importance,
                        na.rm = TRUE
                    ),

                    stringsAsFactors = FALSE
                )


            counter <-
                counter +
                1L
        }
    }
}


cluster_importance_summary <- do.call(
    rbind,
    cluster_summary_rows
)


cluster_importance_summary$feature <- factor(
    cluster_importance_summary$feature,
    levels = feature_names
)


cluster_importance_summary$cluster <- factor(
    cluster_importance_summary$cluster,
    levels = paste0(
        "Cluster ",
        seq_len(K)
    )
)


# =============================================================================
# 13. INCREMENTAL IMPORTANCE SUMMARY
# =============================================================================

incremental_summary_rows <- list()

counter <- 1L


for(H in seq_len(N_VISITS)) {

    for(k_id in seq_len(K)) {

        for(feature_name in feature_names) {

            d <- incremental_results[
                incremental_results$visit == H &
                incremental_results$cluster == k_id &
                incremental_results$feature == feature_name,
                ,
                drop = FALSE
            ]


            incremental_summary_rows[[counter]] <-
                data.frame(

                    visit = H,

                    cluster = paste0(
                        "Cluster ",
                        k_id
                    ),

                    feature = feature_name,

                    mean_incremental = mean(
                        d$incremental_importance,
                        na.rm = TRUE
                    ),

                    sd_incremental = sd(
                        d$incremental_importance,
                        na.rm = TRUE
                    ),

                    stringsAsFactors = FALSE
                )


            counter <-
                counter +
                1L
        }
    }
}


incremental_summary <- do.call(
    rbind,
    incremental_summary_rows
)


incremental_summary$feature <- factor(
    incremental_summary$feature,
    levels = feature_names
)


incremental_summary$cluster <- factor(
    incremental_summary$cluster,
    levels = paste0(
        "Cluster ",
        seq_len(K)
    )
)


# =============================================================================
# 14. PLOT 1
# PROGRESSIVE OUT-OF-SAMPLE ARI
# =============================================================================

p_ari <- ggplot(
    performance_summary,
    aes(
        x = visit,
        y = mean_ARI
    )
) +

    geom_ribbon(
        aes(
            ymin = lower_ARI,
            ymax = upper_ARI
        ),
        alpha = 0.18
    ) +

    geom_line(
        linewidth = 1.15
    ) +

    geom_point(
        size = 3
    ) +

    scale_x_continuous(
        breaks = seq_len(
            N_VISITS
        )
    ) +

    scale_y_continuous(
        limits = c(
            0,
            1
        ),
        breaks = seq(
            0,
            1,
            by = 0.1
        ),
        expand = expansion(
            mult = c(
                0,
                0.02
            )
        )
    ) +

    labs(
        x = "Number of observed visits",
        y = "Adjusted Rand Index",
        title = "Progressive out-of-sample clustering",
        subtitle = paste0(
            "Unseen patients assigned using visits 1:h; ",
            "mean +/- SD across ",
            N_RUNS,
            " simulation runs"
        )
    ) +

    theme_classic(
        base_size = 13
    ) +

    theme(

        plot.title = element_text(
            face = "bold",
            size = 15
        ),

        plot.subtitle = element_text(
            size = 10.5,
            margin = margin(
                b = 10
            )
        ),

        axis.title = element_text(
            face = "bold"
        )
    )


print(
    p_ari
)


# =============================================================================
# 15. PLOT 2
# OVERALL FEATURE-IMPORTANCE EVOLUTION
# =============================================================================

p_importance <- ggplot(
    importance_summary,
    aes(
        x = visit,
        y = mean_importance,
        color = feature,
        group = feature
    )
) +

    geom_line(
        linewidth = 1.1
    ) +

    geom_point(
        size = 2.6
    ) +

    scale_x_continuous(
        breaks = seq_len(
            N_VISITS
        )
    ) +

    scale_y_continuous(
        limits = c(
            0,
            1
        ),
        breaks = seq(
            0,
            1,
            by = 0.1
        )
    ) +

    labs(
        x = "Number of observed visits",
        y = "Mean feature importance",
        color = "Feature",
        title = "Evolution of feature importance for unseen patients",
        subtitle = paste0(
            "Patient-specific longTAPIO importance averaged across test patients and ",
            N_RUNS,
            " simulation runs"
        )
    ) +

    theme_classic(
        base_size = 13
    ) +

    theme(

        plot.title = element_text(
            face = "bold",
            size = 15
        ),

        plot.subtitle = element_text(
            size = 10.5,
            margin = margin(
                b = 10
            )
        ),

        axis.title = element_text(
            face = "bold"
        ),

        legend.position = "right"
    )


print(
    p_importance
)


# =============================================================================
# 16. PLOT 3
# FEATURE-IMPORTANCE HEATMAP
# =============================================================================

p_heatmap <- ggplot(
    importance_summary,
    aes(
        x = visit,
        y = feature,
        fill = mean_importance
    )
) +

    geom_tile(
        color = "white",
        linewidth = 0.5
    ) +

    geom_text(
        aes(
            label = sprintf(
                "%.2f",
                mean_importance
            )
        ),
        size = 3.5
    ) +

    scale_x_continuous(
        breaks = seq_len(
            N_VISITS
        )
    ) +

    labs(
        x = "Number of observed visits",
        y = "Feature",
        fill = "Importance",
        title = "Progressive feature importance",
        subtitle = "Mean patient-specific importance for unseen patients"
    ) +

    theme_classic(
        base_size = 13
    ) +

    theme(

        plot.title = element_text(
            face = "bold",
            size = 15
        ),

        plot.subtitle = element_text(
            size = 10.5,
            margin = margin(
                b = 10
            )
        ),

        axis.title = element_text(
            face = "bold"
        )
    )


print(
    p_heatmap
)


# =============================================================================
# 17. PLOT 4
# CLUSTER-SPECIFIC PROGRESSIVE FEATURE IMPORTANCE
# =============================================================================
#
# Your importance function defines cluster_progressive using FINAL predicted
# cluster membership:
#
# "For patients ultimately assigned to cluster k, how did feature importance
# evolve from visits 1,...,T?"
#
# =============================================================================

p_cluster_importance <- ggplot(
    cluster_importance_summary,
    aes(
        x = visit,
        y = mean_importance,
        color = feature,
        group = feature
    )
) +

    geom_line(
        linewidth = 1.0
    ) +

    geom_point(
        size = 2.1
    ) +

    facet_wrap(
        ~ cluster,
        ncol = 2
    ) +

    scale_x_continuous(
        breaks = seq_len(
            N_VISITS
        )
    ) +

    scale_y_continuous(
        limits = c(
            0,
            1
        )
    ) +

    labs(
        x = "Number of observed visits",
        y = "Mean feature importance",
        color = "Feature",
        title = "Cluster-specific evolution of feature importance",
        subtitle = "Patients grouped according to their final longTAPIO assignment"
    ) +

    theme_classic(
        base_size = 12
    ) +

    theme(

        plot.title = element_text(
            face = "bold",
            size = 15
        ),

        plot.subtitle = element_text(
            size = 10.5,
            margin = margin(
                b = 10
            )
        ),

        strip.text = element_text(
            face = "bold"
        ),

        axis.title = element_text(
            face = "bold"
        ),

        legend.position = "bottom"
    )


print(
    p_cluster_importance
)


# =============================================================================
# 18. PLOT 5
# INCREMENTAL FEATURE IMPORTANCE
# =============================================================================
#
# This shows:
#
#       importance(H) - importance(H-1)
#
# Positive:
#       feature becomes more important after the additional visit.
#
# Negative:
#       feature becomes less important after the additional visit.
#
# =============================================================================

incremental_plot_data <- incremental_summary[
    incremental_summary$visit >= 2,
    ,
    drop = FALSE
]


p_incremental <- ggplot(
    incremental_plot_data,
    aes(
        x = visit,
        y = mean_incremental,
        color = feature,
        group = feature
    )
) +

    geom_hline(
        yintercept = 0,
        linetype = "dashed",
        linewidth = 0.5
    ) +

    geom_line(
        linewidth = 1
    ) +

    geom_point(
        size = 2
    ) +

    facet_wrap(
        ~ cluster,
        ncol = 2
    ) +

    scale_x_continuous(
        breaks = 2:N_VISITS
    ) +

    labs(
        x = "Newly available visit",
        y = "Change in feature importance",
        color = "Feature",
        title = "Incremental feature importance",
        subtitle = "Change in importance when one additional visit becomes available"
    ) +

    theme_classic(
        base_size = 12
    ) +

    theme(

        plot.title = element_text(
            face = "bold",
            size = 15
        ),

        plot.subtitle = element_text(
            size = 10.5,
            margin = margin(
                b = 10
            )
        ),

        strip.text = element_text(
            face = "bold"
        ),

        axis.title = element_text(
            face = "bold"
        ),

        legend.position = "bottom"
    )


print(
    p_incremental
)


# =============================================================================
# 19. OPTIONAL PLOT
# SHOW INDIVIDUAL ARI RUNS
# =============================================================================

p_ari_runs <- ggplot(
    performance_results,
    aes(
        x = visit,
        y = ARI,
        group = run
    )
) +

    geom_line(
        alpha = 0.18,
        linewidth = 0.45
    ) +

    geom_line(
        data = performance_summary,
        aes(
            x = visit,
            y = mean_ARI,
            group = 1
        ),
        inherit.aes = FALSE,
        linewidth = 1.3
    ) +

    geom_point(
        data = performance_summary,
        aes(
            x = visit,
            y = mean_ARI
        ),
        inherit.aes = FALSE,
        size = 2.8
    ) +

    scale_x_continuous(
        breaks = seq_len(
            N_VISITS
        )
    ) +

    scale_y_continuous(
        limits = c(
            0,
            1
        )
    ) +

    labs(
        x = "Number of observed visits",
        y = "Adjusted Rand Index",
        title = "Out-of-sample clustering across follow-up",
        subtitle = "Thin lines show individual simulation runs"
    ) +

    theme_classic(
        base_size = 13
    ) +

    theme(

        plot.title = element_text(
            face = "bold"
        ),

        axis.title = element_text(
            face = "bold"
        )
    )


print(
    p_ari_runs
)


# =============================================================================
# 20. SAVE PLOTS
# =============================================================================

ggsave(
    filename = "longTAPIO_inductive_ARI_by_visit.pdf",
    plot = p_ari,
    width = 8,
    height = 5.5
)


ggsave(
    filename = "longTAPIO_inductive_ARI_by_visit.png",
    plot = p_ari,
    width = 8,
    height = 5.5,
    dpi = 400
)


ggsave(
    filename = "longTAPIO_inductive_importance_by_visit.pdf",
    plot = p_importance,
    width = 8,
    height = 5.5
)


ggsave(
    filename = "longTAPIO_inductive_importance_by_visit.png",
    plot = p_importance,
    width = 8,
    height = 5.5,
    dpi = 400
)


ggsave(
    filename = "longTAPIO_inductive_importance_heatmap.pdf",
    plot = p_heatmap,
    width = 8,
    height = 4.5
)


ggsave(
    filename = "longTAPIO_inductive_importance_heatmap.png",
    plot = p_heatmap,
    width = 8,
    height = 4.5,
    dpi = 400
)


ggsave(
    filename = "longTAPIO_inductive_cluster_importance.pdf",
    plot = p_cluster_importance,
    width = 10,
    height = 8
)


ggsave(
    filename = "longTAPIO_inductive_cluster_importance.png",
    plot = p_cluster_importance,
    width = 10,
    height = 8,
    dpi = 400
)


ggsave(
    filename = "longTAPIO_inductive_incremental_importance.pdf",
    plot = p_incremental,
    width = 10,
    height = 8
)


ggsave(
    filename = "longTAPIO_inductive_incremental_importance.png",
    plot = p_incremental,
    width = 10,
    height = 8,
    dpi = 400
)


ggsave(
    filename = "longTAPIO_inductive_ARI_individual_runs.pdf",
    plot = p_ari_runs,
    width = 8,
    height = 5.5
)


# =============================================================================
# 21. SAVE NUMERICAL RESULTS
# =============================================================================

write.csv(
    performance_results,
    "longTAPIO_inductive_progressive_performance_raw.csv",
    row.names = FALSE
)


write.csv(
    performance_summary,
    "longTAPIO_inductive_progressive_performance_summary.csv",
    row.names = FALSE
)


write.csv(
    patient_importance_results,
    "longTAPIO_inductive_patient_importance_raw.csv",
    row.names = FALSE
)


write.csv(
    importance_summary,
    "longTAPIO_inductive_importance_summary.csv",
    row.names = FALSE
)


write.csv(
    cluster_importance_results,
    "longTAPIO_inductive_cluster_importance_raw.csv",
    row.names = FALSE
)


write.csv(
    cluster_importance_summary,
    "longTAPIO_inductive_cluster_importance_summary.csv",
    row.names = FALSE
)


write.csv(
    incremental_results,
    "longTAPIO_inductive_incremental_importance_raw.csv",
    row.names = FALSE
)


write.csv(
    incremental_summary,
    "longTAPIO_inductive_incremental_importance_summary.csv",
    row.names = FALSE
)


# =============================================================================
# 22. FINAL CONSOLE OUTPUT
# =============================================================================

cat(
    "\n\n",
    paste(
        rep(
            "=",
            100
        ),
        collapse = ""
    ),
    "\n",
    "PROGRESSIVE INDUCTIVE BENCHMARK COMPLETE\n",
    paste(
        rep(
            "=",
            100
        ),
        collapse = ""
    ),
    "\n\n",
    "Training:\n",
    "  longTAPIO fitted once on training patients only.\n\n",
    "Out-of-sample evaluation:\n",
    "  Completely unseen test patients.\n",
    "  Progressive prefixes from visit 1 to visit ",
    N_VISITS,
    ".\n\n",
    "Performance:\n",
    "  ARI and NMI calculated at every prefix.\n\n",
    "Feature importance:\n",
    "  Uses importance_longTAPIO_inductive() directly.\n",
    "  No permutation importance and no refitting.\n\n",
    sep = ""
)