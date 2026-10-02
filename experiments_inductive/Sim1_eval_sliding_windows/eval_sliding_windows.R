# =============================================================================
# IRREGULAR-TIME BENCHMARK
#
# PURPOSE
# -------
# Evaluate whether the overlapping-window representation used by
# inductive longTAPIO preserves longitudinal cluster structure when
# observation times become increasingly irregular.
#
# METHODS
# -------
# 1. longTAPIO-window
#       Raw irregular observations
#           -> frozen overlapping-window representation
#           -> longTAPIO_inductive()
#
# 2. clusterMLD
#       Raw irregular observations and their exact times
#           -> LongDataCluster()
#
# PRIMARY COMPARISON
# ------------------
# Both methods cluster exactly the same subjects:
#
#       longTAPIO-window reference ARI
#       clusterMLD reference ARI
#
# This directly tests whether window discretization loses information
# relative to native irregular-time clustering.
#
# SECONDARY COMPARISON
# --------------------
# longTAPIO is additionally evaluated with a strict 70/30
# train/test split:
#
#       train subjects -> define windows + fit longTAPIO
#       test subjects  -> SAME frozen windows + fitted model
#
# IMPORTANT
# ---------
# The window specification and fallback means are learned ONLY from
# the training/reference cohort.
#
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

library(aricode)
library(clusterMLD)
library(ggplot2)


if (!exists("longTAPIO_inductive")) {
    stop("Please source/load longTAPIO_inductive() first.")
}


# =============================================================================
# 1. GLOBAL SETTINGS
# =============================================================================

N_RUNS <- 30

N_TOTAL <- 200

TRAIN_FRAC <- 0.70

K <- 4

P <- 5

TIME_MIN <- 0

TIME_MAX <- 10

N_TREES <- 500

LEVELS <- 4

N_BINS <- 10

WINDOW_OVERLAP <- 0.50


# -----------------------------------------------------------------------------
# Longitudinal noise
# -----------------------------------------------------------------------------

BIOMARKER_RHO <- 0.65

BIOMARKER_SD <- 0.07

MEASUREMENT_SD <- 0.095


# -----------------------------------------------------------------------------
# Irregularity scenarios
# -----------------------------------------------------------------------------

SCENARIOS <- c(
    "regular",
    "mild",
    "moderate",
    "strong",
    "very_strong"
)


METHODS_REFERENCE <- c(
    "longTAPIO-window",
    "clusterMLD"
)


# =============================================================================
# 2. LATENT DISEASE TRAJECTORIES
# =============================================================================

latent_curve <- function(
    t,
    cluster,
    phase = 0
) {

    tt <- t + phase

    if (cluster == 1) {

        # Stable / slowly progressive
        value <-
            0.17 +
            0.020 * tt

    } else if (cluster == 2) {

        # Progressive
        value <-
            0.08 +
            0.080 * tt

    } else if (cluster == 3) {

        # Relapsing / fluctuating
        value <-
            0.45 +
            0.30 *
            sin(
                0.82 * tt -
                0.60
            )

    } else {

        # Acute flare followed by recovery
        value <-
            0.16 +
            0.68 *
            exp(
                -0.5 *
                (
                    (tt - 5.0) /
                    1.30
                )^2
            )
    }

    value
}


# =============================================================================
# 3. BIOMARKER-SPECIFIC TEMPORAL DEVIATION
# =============================================================================

simulate_biomarker_deviation <- function(
    n,
    rho = BIOMARKER_RHO,
    innovation_sd = BIOMARKER_SD
) {

    u <- numeric(n)

    initial_sd <-
        innovation_sd /
        sqrt(
            1 - rho^2
        )

    u[1] <- rnorm(
        1,
        mean = 0,
        sd = initial_sd
    )

    if (n >= 2) {

        for (tt in 2:n) {

            u[tt] <-
                rho *
                u[tt - 1] +
                rnorm(
                    1,
                    mean = 0,
                    sd = innovation_sd
                )
        }
    }

    u
}


# =============================================================================
# 4. OBSERVATION-TIME GENERATOR
# =============================================================================
#
# The latent disease process is IDENTICAL between scenarios.
#
# Only the observation process changes.
#
# regular:
#       exactly 10 equally spaced visits
#
# mild:
#       10 visits around regular grid with small jitter
#
# moderate:
#       7-10 completely irregular visits
#
# strong:
#       5-8 completely irregular visits
#
# very_strong:
#       3-6 completely irregular visits
#
# =============================================================================

generate_visit_times <- function(
    scenario
) {

    scenario <- match.arg(
        scenario,
        SCENARIOS
    )

    if (scenario == "regular") {

        ti <- seq(
            TIME_MIN,
            TIME_MAX,
            length.out = 10
        )

    } else if (scenario == "mild") {

        base <- seq(
            TIME_MIN,
            TIME_MAX,
            length.out = 10
        )

        jitter <- rnorm(
            length(base),
            mean = 0,
            sd = 0.25
        )

        jitter[1] <- 0
        jitter[length(jitter)] <- 0

        ti <- base + jitter

        ti <- pmax(
            TIME_MIN,
            pmin(
                TIME_MAX,
                ti
            )
        )

        ti <- sort(ti)

        # Prevent exact duplicates after clipping
        for (j in 2:length(ti)) {

            if (ti[j] <= ti[j - 1]) {

                ti[j] <-
                    min(
                        TIME_MAX,
                        ti[j - 1] + 1e-4
                    )
            }
        }

    } else if (scenario == "moderate") {

        n_i <- sample(
            7:10,
            size = 1
        )

        ti <- sort(
            runif(
                n_i,
                min = TIME_MIN,
                max = TIME_MAX
            )
        )

    } else if (scenario == "strong") {

        n_i <- sample(
            5:8,
            size = 1
        )

        ti <- sort(
            runif(
                n_i,
                min = TIME_MIN,
                max = TIME_MAX
            )
        )

    } else {

        n_i <- sample(
            3:6,
            size = 1
        )

        ti <- sort(
            runif(
                n_i,
                min = TIME_MIN,
                max = TIME_MAX
            )
        )
    }

    ti
}


# =============================================================================
# 5. SIMULATE DATA
# =============================================================================

simulate_irregular_benchmark <- function(
    scenario,
    seed = 1
) {

    scenario <- match.arg(
        scenario,
        SCENARIOS
    )

    set.seed(seed)


    # -------------------------------------------------------------------------
    # Balanced true clusters
    # -------------------------------------------------------------------------

    sizes <- rep(
        floor(N_TOTAL / K),
        K
    )

    sizes[1] <-
        sizes[1] +
        (
            N_TOTAL -
            sum(sizes)
        )

    truth <- rep(
        seq_len(K),
        times = sizes
    )

    truth <- sample(truth)


    # -------------------------------------------------------------------------
    # Subject heterogeneity
    # -------------------------------------------------------------------------

    subject_intercept <- rnorm(
        N_TOTAL,
        mean = 0,
        sd = 0.13
    )

    subject_scale <- rlnorm(
        N_TOTAL,
        meanlog = 0,
        sdlog = 0.07
    )

    subject_phase <- rnorm(
        N_TOTAL,
        mean = 0,
        sd = 0.14
    )


    # -------------------------------------------------------------------------
    # Biomarker loadings
    # -------------------------------------------------------------------------

    signal_loadings <- seq(
        0.90,
        1.25,
        length.out = P
    )

    feature_offsets <- seq(
        -0.10,
        0.10,
        length.out = P
    )


    # -------------------------------------------------------------------------
    # Generate subjects
    # -------------------------------------------------------------------------

    rows <- vector(
        "list",
        N_TOTAL
    )

    n_visits <- integer(
        N_TOTAL
    )


    for (i in seq_len(N_TOTAL)) {

        ti <- generate_visit_times(
            scenario
        )

        n_visits[i] <- length(ti)


        # ---------------------------------------------------------------------
        # Latent trajectory evaluated at ACTUAL observation times
        # ---------------------------------------------------------------------

        latent <- vapply(
            ti,
            function(tt) {

                latent_curve(
                    t = tt,
                    cluster = truth[i],
                    phase = subject_phase[i]
                )
            },
            numeric(1)
        )


        Xi <- matrix(
            NA_real_,
            nrow = length(ti),
            ncol = P
        )


        # ---------------------------------------------------------------------
        # Five informative biomarkers
        # ---------------------------------------------------------------------

        for (j in seq_len(P)) {

            biomarker_deviation <-
                simulate_biomarker_deviation(
                    n = length(ti)
                )

            Xi[, j] <-
                feature_offsets[j] +
                subject_intercept[i] +
                subject_scale[i] *
                signal_loadings[j] *
                latent +
                biomarker_deviation +
                rnorm(
                    length(ti),
                    mean = 0,
                    sd = MEASUREMENT_SD
                )
        }


        tmp <- data.frame(
            id = i,
            time = ti
        )

        for (j in seq_len(P)) {

            tmp[[paste0("y", j)]] <-
                Xi[, j]
        }

        rows[[i]] <- tmp
    }


    dat <- do.call(
        rbind,
        rows
    )

    rownames(dat) <- NULL


    list(
        data = dat,
        truth = truth,
        n_visits = n_visits,
        scenario = scenario
    )
}


# =============================================================================
# 6. STRATIFIED TRAIN / TEST SPLIT
# =============================================================================

make_train_test_split <- function(
    truth,
    train_fraction = TRAIN_FRAC,
    seed = 1
) {

    set.seed(seed)

    train_ids <- integer(0)

    for (g in sort(unique(truth))) {

        ids_g <- which(
            truth == g
        )

        n_train_g <- floor(
            length(ids_g) *
            train_fraction
        )

        train_ids <- c(
            train_ids,
            sample(
                ids_g,
                n_train_g
            )
        )
    }

    train_ids <- sort(train_ids)

    test_ids <- setdiff(
        seq_along(truth),
        train_ids
    )

    list(
        train = train_ids,
        test = test_ids
    )
}


# =============================================================================
# 7. LEARN FROZEN WINDOW SPECIFICATION
# =============================================================================
#
# IMPORTANT:
#
# Everything here is estimated from REFERENCE/TRAINING data only.
#
# The resulting object is then reused unchanged for test patients.
#
# =============================================================================

fit_window_spec <- function(
    dat_train,
    n_bins = N_BINS,
    overlap = WINDOW_OVERLAP
) {

    features <- grep(
        "^y[0-9]+$",
        names(dat_train),
        value = TRUE
    )


    # -------------------------------------------------------------------------
    # Fixed study-time domain
    #
    # We know the benchmark domain by design.
    #
    # In a real application these could instead be learned from the
    # reference cohort and stored in the fitted object.
    # -------------------------------------------------------------------------

    t_min <- TIME_MIN

    t_max <- TIME_MAX


    total_range <-
        t_max -
        t_min


    window_size <-
        total_range /
        n_bins


    step <-
        window_size *
        (
            1 -
            overlap
        )


    starts <- seq(
        t_min,
        t_max - window_size,
        by = step
    )


    # Make sure final temporal region is represented

    final_start <-
        t_max -
        window_size

    if (
        length(starts) == 0 ||
        abs(
            tail(starts, 1) -
            final_start
        ) > 1e-10
    ) {

        starts <- c(
            starts,
            final_start
        )
    }


    starts <- sort(
        unique(starts)
    )


    centers <-
        starts +
        window_size / 2


    # -------------------------------------------------------------------------
    # TRAIN-ONLY fallback means
    # -------------------------------------------------------------------------

    global_means <- vapply(
        features,
        function(feature) {

            mean(
                dat_train[[feature]],
                na.rm = TRUE
            )
        },
        numeric(1)
    )


    list(
        features = features,
        t_min = t_min,
        t_max = t_max,
        window_size = window_size,
        step = step,
        starts = starts,
        centers = centers,
        global_means = global_means,
        n_bins = n_bins,
        overlap = overlap
    )
}


# =============================================================================
# 8. APPLY FROZEN WINDOW SPECIFICATION
# =============================================================================

apply_window_spec <- function(
    dat,
    window_spec
) {

    features <- window_spec$features

    starts <- window_spec$starts

    centers <- window_spec$centers

    window_size <- window_spec$window_size

    global_means <- window_spec$global_means


    ids <- sort(
        unique(dat$id)
    )


    N <- length(ids)

    V <- length(starts)

    P_local <- length(features)


    X <- array(
        NA_real_,
        dim = c(
            N,
            V,
            P_local
        ),
        dimnames = list(
            as.character(ids),
            as.character(centers),
            features
        )
    )


    for (ii in seq_along(ids)) {

        d_i <- dat[
            dat$id == ids[ii],
            ,
            drop = FALSE
        ]

        d_i <- d_i[
            order(d_i$time),
            ,
            drop = FALSE
        ]


        for (j in seq_along(features)) {

            feature <- features[j]


            values <- rep(
                NA_real_,
                V
            )


            for (v in seq_len(V)) {

                lo <- starts[v]

                hi <-
                    starts[v] +
                    window_size


                idx <-
                    d_i$time >= lo &
                    d_i$time <= hi


                if (any(idx)) {

                    values[v] <- mean(
                        d_i[[feature]][idx],
                        na.rm = TRUE
                    )
                }
            }


            observed <- which(
                is.finite(values)
            )


            # -----------------------------------------------------------------
            # Interpolate missing windows
            # -----------------------------------------------------------------

            if (length(observed) >= 2) {

                values <- approx(
                    x = centers[observed],
                    y = values[observed],
                    xout = centers,
                    rule = 2
                )$y

            } else if (length(observed) == 1) {

                values[] <-
                    values[observed]

            } else {

                # TRAIN-derived fallback
                values[] <-
                    global_means[j]
            }


            X[ii, , j] <- values
        }
    }


    if (any(!is.finite(X))) {

        stop(
            "Non-finite values found in window representation."
        )
    }


    list(
        X = X,
        ids = ids,
        time = centers,
        features = features
    )
}


# =============================================================================
# 9. ARRAY -> LONGTAPIO FORMAT
# =============================================================================

array_to_longTAPIO <- function(
    X
) {

    N <- dim(X)[1]

    V <- dim(X)[2]

    P_local <- dim(X)[3]


    DATA <- matrix(
        NA_real_,
        nrow = N * V,
        ncol = P_local
    )


    user_id <- rep(
        seq_len(N),
        each = V
    )


    for (i in seq_len(N)) {

        rows_i <- (
            (i - 1) * V + 1
        ):(
            i * V
        )

        DATA[rows_i, ] <-
            X[i, , ]
    }


    DATA <- as.data.frame(
        DATA
    )


    names(DATA) <- paste0(
        "y",
        seq_len(P_local)
    )


    list(
        DATA = DATA,
        user_id = user_id
    )
}


# =============================================================================
# 10. EXTRACT LONGTAPIO TRAINING PARTITION
# =============================================================================

extract_longTAPIO_cluster <- function(
    fit,
    N
) {

    candidates <- c(
        "train_cluster",
        "train_clusters",
        "cluster",
        "clusters",
        "clustering",
        "labels",
        "membership"
    )


    for (nm in candidates) {

        z <- fit[[nm]]

        if (
            !is.null(z) &&
            length(z) == N
        ) {

            return(
                as.integer(z)
            )
        }
    }


    if (
        !is.null(fit$result) &&
        is.list(fit$result)
    ) {

        for (nm in candidates) {

            z <- fit$result[[nm]]

            if (
                !is.null(z) &&
                length(z) == N
            ) {

                return(
                    as.integer(z)
                )
            }
        }
    }


    stop(
        "Could not extract longTAPIO training partition."
    )
}


# =============================================================================
# 11. FIT LONGTAPIO
# =============================================================================

fit_longTAPIO_window <- function(
    representation,
    seed = NULL
) {

    if (!is.null(seed)) {
        set.seed(seed)
    }


    X <- representation$X

    N <- dim(X)[1]

    P_local <- dim(X)[3]


    prepared <- array_to_longTAPIO(
        X
    )


    m <- max(
        2,
        ceiling(
            sqrt(P_local)
        )
    )

    m <- min(
        m,
        P_local
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
        pca_selection = "random_weighted"
    )


    cluster <- extract_longTAPIO_cluster(
        fit = fit,
        N = N
    )


    list(
        fit = fit,
        cluster = cluster,
        prepared = prepared,
        m = m
    )
}


# =============================================================================
# 12. OPTIONAL INDUCTIVE PREDICTION WRAPPER
# =============================================================================
#
# EDIT ONLY THIS FUNCTION if your prediction function has another name.
#
# The benchmark itself does not otherwise depend on your prediction API.
#
# Expected output:
#
#       integer vector of length N_test
#
# containing assignments to the FIXED training clusters.
#
# =============================================================================

predict_longTAPIO_wrapper <- function(
    fit,
    representation_test
) {

    prepared_test <- array_to_longTAPIO(
        representation_test$X
    )


    # -------------------------------------------------------------------------
    # OPTION A:
    #
    # If your implementation provides predict.longTAPIO_inductive(),
    # this should normally work.
    # -------------------------------------------------------------------------

    pred <- try(
        predict(
            fit,
            newdata = prepared_test$DATA,
            user_id = prepared_test$user_id
        ),
        silent = TRUE
    )


    if (!inherits(pred, "try-error")) {

        if (is.atomic(pred)) {

            pred <- as.integer(pred)

            if (
                length(pred) ==
                dim(representation_test$X)[1]
            ) {

                return(pred)
            }
        }


        if (is.list(pred)) {

            candidates <- c(
                "cluster",
                "clusters",
                "prediction",
                "predictions",
                "membership",
                "assigned_cluster"
            )

            for (nm in candidates) {

                z <- pred[[nm]]

                if (
                    !is.null(z) &&
                    length(z) ==
                    dim(representation_test$X)[1]
                ) {

                    return(
                        as.integer(z)
                    )
                }
            }
        }
    }


    # -------------------------------------------------------------------------
    # OPTION B:
    #
    # If your implementation uses a dedicated function called
    # predict_longTAPIO_inductive(), try it.
    # -------------------------------------------------------------------------

    if (
        exists(
            "predict_longTAPIO_inductive",
            mode = "function"
        )
    ) {

        pred <- try(
            predict_longTAPIO_inductive(
                res = fit,
                DATA = prepared_test$DATA,
                user_id = prepared_test$user_id
            ),
            silent = TRUE
        )


        if (!inherits(pred, "try-error")) {

            if (is.atomic(pred)) {

                pred <- as.integer(pred)

                if (
                    length(pred) ==
                    dim(representation_test$X)[1]
                ) {

                    return(pred)
                }
            }


            if (is.list(pred)) {

                candidates <- c(
                    "cluster",
                    "clusters",
                    "prediction",
                    "predictions",
                    "membership",
                    "assigned_cluster"
                )

                for (nm in candidates) {

                    z <- pred[[nm]]

                    if (
                        !is.null(z) &&
                        length(z) ==
                        dim(representation_test$X)[1]
                    ) {

                        return(
                            as.integer(z)
                        )
                    }
                }
            }
        }
    }


    stop(
        paste0(
            "Could not call the inductive longTAPIO prediction function. ",
            "Edit only predict_longTAPIO_wrapper() to match your current API."
        )
    )
}


# =============================================================================
# 13. CLUSTERMLD LABEL HELPER
# =============================================================================

cluster_list_to_labels <- function(
    cluster_list,
    ids
) {

    labels <- rep(
        NA_integer_,
        length(ids)
    )

    names(labels) <-
        as.character(ids)


    for (g in seq_along(cluster_list)) {

        members <-
            cluster_list[[g]]

        labels[
            as.character(members)
        ] <- g
    }


    if (anyNA(labels)) {

        stop(
            "clusterMLD failed to assign subjects."
        )
    }


    as.integer(labels)
}


# =============================================================================
# 14. FIT CLUSTERMLD
# =============================================================================

fit_clusterMLD <- function(
    dat,
    k = K
) {

    features <- grep(
        "^y[0-9]+$",
        names(dat),
        value = TRUE
    )


    ids <- sort(
        unique(dat$id)
    )


    fit <- clusterMLD::LongDataCluster(
        x = dat$time,
        Y = dat[
            ,
            features,
            drop = FALSE
        ],
        id = dat$id,
        No.Class = k,
        parallel = FALSE
    )


    fixed_k <-
        fit$Cluster.Lists[[k]]


    cluster_list_to_labels(
        cluster_list = fixed_k,
        ids = ids
    )
}


# =============================================================================
# 15. METRICS
# =============================================================================

get_scores <- function(
    truth,
    pred
) {

    c(
        ARI = aricode::ARI(
            truth,
            pred
        ),
        NMI = aricode::NMI(
            truth,
            pred
        )
    )
}


# =============================================================================
# 16. RESULT HELPER
# =============================================================================

make_result <- function(
    scenario,
    run,
    method,
    evaluation,
    truth,
    pred,
    seconds
) {

    score <- get_scores(
        truth,
        pred
    )


    data.frame(
        scenario = scenario,
        run = run,
        method = method,
        evaluation = evaluation,
        ARI = unname(
            score["ARI"]
        ),
        NMI = unname(
            score["NMI"]
        ),
        seconds = seconds,
        stringsAsFactors = FALSE
    )
}


# =============================================================================
# 17. RUN ONE REPLICATE
# =============================================================================

run_one <- function(
    scenario,
    run
) {

    scenario_number <- match(
        scenario,
        SCENARIOS
    )


    data_seed <-
        50000 +
        scenario_number * 1000 +
        run


    split_seed <-
        75000 +
        scenario_number * 1000 +
        run


    model_seed <-
        100000 +
        scenario_number * 1000 +
        run


    # =========================================================================
    # GENERATE ONE DATASET
    # =========================================================================

    sim <- simulate_irregular_benchmark(
        scenario = scenario,
        seed = data_seed
    )


    dat <- sim$data

    truth <- sim$truth


    cat(
        "\n",
        paste(
            rep("=", 100),
            collapse = ""
        ),
        "\n",
        toupper(scenario),
        " | RUN ",
        run,
        "/",
        N_RUNS,
        " | mean visits=",
        sprintf(
            "%.2f",
            mean(sim$n_visits)
        ),
        " | range=",
        min(sim$n_visits),
        "-",
        max(sim$n_visits),
        "\n",
        paste(
            rep("=", 100),
            collapse = ""
        ),
        "\n",
        sep = ""
    )


    output <- list()


    # =========================================================================
    # A. REFERENCE / TRANSDUCTIVE COMPARISON
    # =========================================================================
    #
    # Both methods see EXACTLY the same patients and observations.
    #
    # longTAPIO uses overlapping windows.
    # clusterMLD uses raw irregular times.
    #
    # =========================================================================


    # -------------------------------------------------------------------------
    # A1. longTAPIO-window
    # -------------------------------------------------------------------------

    cat(
        "\nA1. longTAPIO-window: full/reference clustering\n"
    )


    # Full dataset defines windows here because this endpoint is explicitly
    # the transductive/reference comparison.

    full_window_spec <- fit_window_spec(
        dat_train = dat
    )


    full_representation <- apply_window_spec(
        dat = dat,
        window_spec = full_window_spec
    )


    t0 <- proc.time()[3]


    lt_full <- try(
        fit_longTAPIO_window(
            representation = full_representation,
            seed = model_seed
        ),
        silent = TRUE
    )


    sec_lt_full <-
        proc.time()[3] -
        t0


    if (!inherits(lt_full, "try-error")) {

        score <- get_scores(
            truth,
            lt_full$cluster
        )


        cat(
            sprintf(
                "   ARI=%.3f | NMI=%.3f | %.2f sec\n",
                score["ARI"],
                score["NMI"],
                sec_lt_full
            )
        )


        output[[length(output) + 1L]] <-
            make_result(
                scenario = scenario,
                run = run,
                method = "longTAPIO-window",
                evaluation = "reference",
                truth = truth,
                pred = lt_full$cluster,
                seconds = sec_lt_full
            )

    } else {

        cat(
            "   FAILED: ",
            as.character(lt_full),
            "\n"
        )
    }


    # -------------------------------------------------------------------------
    # A2. clusterMLD
    # -------------------------------------------------------------------------

    cat(
        "\nA2. clusterMLD: raw irregular-time clustering\n"
    )


    t0 <- proc.time()[3]


    cl <- try(
        fit_clusterMLD(
            dat = dat,
            k = K
        ),
        silent = TRUE
    )


    sec_cl <-
        proc.time()[3] -
        t0


    if (!inherits(cl, "try-error")) {

        score <- get_scores(
            truth,
            cl
        )


        cat(
            sprintf(
                "   ARI=%.3f | NMI=%.3f | %.2f sec\n",
                score["ARI"],
                score["NMI"],
                sec_cl
            )
        )


        output[[length(output) + 1L]] <-
            make_result(
                scenario = scenario,
                run = run,
                method = "clusterMLD",
                evaluation = "reference",
                truth = truth,
                pred = cl,
                seconds = sec_cl
            )

    } else {

        cat(
            "   FAILED: ",
            as.character(cl),
            "\n"
        )
    }


    # =========================================================================
    # B. STRICT INDUCTIVE LONGTAPIO
    # =========================================================================

    cat(
        "\nB. longTAPIO-window: strict 70/30 inductive evaluation\n"
    )


    split <- make_train_test_split(
        truth = truth,
        train_fraction = TRAIN_FRAC,
        seed = split_seed
    )


    train_ids <- split$train

    test_ids <- split$test


    dat_train <- dat[
        dat$id %in% train_ids,
        ,
        drop = FALSE
    ]

    dat_test <- dat[
        dat$id %in% test_ids,
        ,
        drop = FALSE
    ]


    # -------------------------------------------------------------------------
    # Re-index IDs because longTAPIO expects compact subject indices
    # -------------------------------------------------------------------------

    train_map <- setNames(
        seq_along(train_ids),
        train_ids
    )

    test_map <- setNames(
        seq_along(test_ids),
        test_ids
    )


    dat_train$id <- as.integer(
        train_map[
            as.character(dat_train$id)
        ]
    )


    dat_test$id <- as.integer(
        test_map[
            as.character(dat_test$id)
        ]
    )


    truth_train <-
        truth[train_ids]

    truth_test <-
        truth[test_ids]


    # -------------------------------------------------------------------------
    # CRITICAL:
    # Learn windows ONLY from training/reference patients
    # -------------------------------------------------------------------------

    train_window_spec <- fit_window_spec(
        dat_train = dat_train
    )


    representation_train <- apply_window_spec(
        dat = dat_train,
        window_spec = train_window_spec
    )


    # -------------------------------------------------------------------------
    # Apply EXACT SAME frozen windows and TRAIN fallback means to test patients
    # -------------------------------------------------------------------------

    representation_test <- apply_window_spec(
        dat = dat_test,
        window_spec = train_window_spec
    )


    # -------------------------------------------------------------------------
    # Fit reference model
    # -------------------------------------------------------------------------

    t0 <- proc.time()[3]


    lt_train <- try(
        fit_longTAPIO_window(
            representation = representation_train,
            seed = model_seed + 500000
        ),
        silent = TRUE
    )


    sec_train <-
        proc.time()[3] -
        t0


    if (!inherits(lt_train, "try-error")) {

        train_score <- get_scores(
            truth_train,
            lt_train$cluster
        )


        cat(
            sprintf(
                "   TRAIN ARI=%.3f | NMI=%.3f\n",
                train_score["ARI"],
                train_score["NMI"]
            )
        )


        output[[length(output) + 1L]] <-
            make_result(
                scenario = scenario,
                run = run,
                method = "longTAPIO-window",
                evaluation = "train",
                truth = truth_train,
                pred = lt_train$cluster,
                seconds = sec_train
            )


        # ---------------------------------------------------------------------
        # Predict unseen subjects
        # ---------------------------------------------------------------------

        t1 <- proc.time()[3]


        pred_test <- try(
            predict_longTAPIO_wrapper(
                fit = lt_train$fit,
                representation_test = representation_test
            ),
            silent = TRUE
        )


        sec_test <-
            proc.time()[3] -
            t1


        if (!inherits(pred_test, "try-error")) {

            test_score <- get_scores(
                truth_test,
                pred_test
            )


            cat(
                sprintf(
                    "   TEST  ARI=%.3f | NMI=%.3f\n",
                    test_score["ARI"],
                    test_score["NMI"]
                )
            )


            output[[length(output) + 1L]] <-
                make_result(
                    scenario = scenario,
                    run = run,
                    method = "longTAPIO-window",
                    evaluation = "test",
                    truth = truth_test,
                    pred = pred_test,
                    seconds = sec_test
                )

        } else {

            cat(
                "   TEST prediction unavailable.\n",
                "   Edit predict_longTAPIO_wrapper() for your current API.\n"
            )
        }

    } else {

        cat(
            "   TRAIN FIT FAILED: ",
            as.character(lt_train),
            "\n"
        )
    }


    # =========================================================================
    # RETURN
    # =========================================================================

    if (length(output) == 0) {

        return(NULL)
    }


    do.call(
        rbind,
        output
    )
}


# =============================================================================
# 18. RUN COMPLETE BENCHMARK
# =============================================================================

cat(
    "\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    "IRREGULAR-TIME BENCHMARK: longTAPIO WINDOWS vs clusterMLD\n",
    "N = ",
    N_TOTAL,
    " | K = ",
    K,
    " | P = ",
    P,
    " | trees = ",
    N_TREES,
    " | bins = ",
    N_BINS,
    " | overlap = ",
    WINDOW_OVERLAP,
    "\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    sep = ""
)


all_results <- list()

counter <- 1L


for (scenario in SCENARIOS) {

    for (run in seq_len(N_RUNS)) {

        z <- run_one(
            scenario = scenario,
            run = run
        )


        if (!is.null(z)) {

            all_results[[counter]] <- z

            counter <- counter + 1L
        }
    }
}


results <- do.call(
    rbind,
    all_results
)


# =============================================================================
# 19. SUMMARY
# =============================================================================

mean_sd <- function(
    x
) {

    x <- x[
        is.finite(x)
    ]


    if (length(x) == 0) {

        return("NA")
    }


    if (length(x) == 1) {

        return(
            sprintf(
                "%.3f",
                x
            )
        )
    }


    sprintf(
        "%.3f +/- %.3f",
        mean(x),
        sd(x)
    )
}


summary_rows <- list()

cc <- 1L


for (scenario in SCENARIOS) {

    evaluations <- unique(
        results$evaluation[
            results$scenario == scenario
        ]
    )


    for (evaluation in evaluations) {

        methods <- unique(
            results$method[
                results$scenario == scenario &
                results$evaluation == evaluation
            ]
        )


        for (method in methods) {

            d <- results[
                results$scenario == scenario &
                results$evaluation == evaluation &
                results$method == method,
                ,
                drop = FALSE
            ]


            if (nrow(d) == 0) {
                next
            }


            summary_rows[[cc]] <- data.frame(
                scenario = scenario,
                evaluation = evaluation,
                method = method,
                n = nrow(d),
                ARI = mean_sd(
                    d$ARI
                ),
                NMI = mean_sd(
                    d$NMI
                ),
                seconds = mean_sd(
                    d$seconds
                ),
                mean_ARI = mean(
                    d$ARI,
                    na.rm = TRUE
                ),
                sd_ARI = sd(
                    d$ARI,
                    na.rm = TRUE
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


            cc <- cc + 1L
        }
    }
}


summary_table <- do.call(
    rbind,
    summary_rows
)


cat(
    "\n\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    "FINAL SUMMARY\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    sep = ""
)


print(
    summary_table,
    row.names = FALSE
)


# =============================================================================
# 20. PAIRED REFERENCE COMPARISON
# =============================================================================

paired_rows <- list()

cc <- 1L


for (scenario in SCENARIOS) {

    lt <- results[
        results$scenario == scenario &
        results$evaluation == "reference" &
        results$method == "longTAPIO-window",
        c(
            "run",
            "ARI",
            "NMI"
        ),
        drop = FALSE
    ]


    cl <- results[
        results$scenario == scenario &
        results$evaluation == "reference" &
        results$method == "clusterMLD",
        c(
            "run",
            "ARI",
            "NMI"
        ),
        drop = FALSE
    ]


    names(lt) <- c(
        "run",
        "ARI_longTAPIO",
        "NMI_longTAPIO"
    )


    names(cl) <- c(
        "run",
        "ARI_clusterMLD",
        "NMI_clusterMLD"
    )


    paired <- merge(
        lt,
        cl,
        by = "run"
    )


    if (nrow(paired) == 0) {
        next
    }


    delta_ari <-
        paired$ARI_longTAPIO -
        paired$ARI_clusterMLD


    delta_nmi <-
        paired$NMI_longTAPIO -
        paired$NMI_clusterMLD


    paired_rows[[cc]] <- data.frame(
        scenario = scenario,
        n = nrow(paired),
        longTAPIO_ARI = mean(
            paired$ARI_longTAPIO
        ),
        clusterMLD_ARI = mean(
            paired$ARI_clusterMLD
        ),
        delta_ARI = mean(
            delta_ari
        ),
        sd_delta_ARI = sd(
            delta_ari
        ),
        longTAPIO_NMI = mean(
            paired$NMI_longTAPIO
        ),
        clusterMLD_NMI = mean(
            paired$NMI_clusterMLD
        ),
        delta_NMI = mean(
            delta_nmi
        ),
        sd_delta_NMI = sd(
            delta_nmi
        ),
        longTAPIO_better = sum(
            delta_ari > 0
        ),
        equal = sum(
            abs(delta_ari) < 1e-12
        ),
        clusterMLD_better = sum(
            delta_ari < 0
        ),
        stringsAsFactors = FALSE
    )


    cc <- cc + 1L
}


paired_summary <- do.call(
    rbind,
    paired_rows
)


cat(
    "\n\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    "PAIRED REFERENCE COMPARISON\n",
    "delta = longTAPIO-window - clusterMLD\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    sep = ""
)


print(
    paired_summary,
    row.names = FALSE
)


# =============================================================================
# 21. SAVE RESULTS
# =============================================================================

write.csv(
    results,
    "longTAPIO_irregular_time_results.csv",
    row.names = FALSE
)


write.csv(
    summary_table,
    "longTAPIO_irregular_time_summary.csv",
    row.names = FALSE
)


write.csv(
    paired_summary,
    "longTAPIO_vs_clusterMLD_paired.csv",
    row.names = FALSE
)


# =============================================================================
# 22. PUBLICATION PLOT:
#     REFERENCE CLUSTERING
# =============================================================================

reference_results <- results[
    results$evaluation == "reference",
    ,
    drop = FALSE
]


reference_results$scenario_plot <- factor(
    reference_results$scenario,
    levels = SCENARIOS,
    labels = c(
        "Regular",
        "Mild",
        "Moderate",
        "Strong",
        "Very strong"
    )
)


reference_results$method_plot <- factor(
    reference_results$method,
    levels = c(
        "longTAPIO-window",
        "clusterMLD"
    ),
    labels = c(
        "longTAPIO",
        "clusterMLD"
    )
)


p_reference <- ggplot(
    reference_results,
    aes(
        x = scenario_plot,
        y = ARI,
        group = method_plot,
        linetype = method_plot,
        shape = method_plot
    )
) +

    stat_summary(
        fun = mean,
        geom = "line",
        linewidth = 0.9
    ) +

    stat_summary(
        fun = mean,
        geom = "point",
        size = 3
    ) +

    stat_summary(
        fun.data = mean_se,
        geom = "errorbar",
        width = 0.10
    ) +

    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(
            0,
            1,
            by = 0.2
        )
    ) +

    labs(
        x = "Degree of visit-time irregularity",
        y = "Adjusted Rand Index",
        linetype = "Method",
        shape = "Method",
        title = "Robustness to irregular observation times",
        subtitle = paste0(
            "longTAPIO uses ",
            N_BINS,
            " overlapping windows; clusterMLD uses exact observation times"
        )
    ) +

    theme_classic(
        base_size = 12
    ) +

    theme(
        plot.title = element_text(
            face = "bold"
        ),
        axis.title = element_text(
            face = "bold"
        ),
        legend.position = "bottom"
    )


print(
    p_reference
)


ggsave(
    filename = "longTAPIO_vs_clusterMLD_irregularity.pdf",
    plot = p_reference,
    width = 8,
    height = 5
)


ggsave(
    filename = "longTAPIO_vs_clusterMLD_irregularity.png",
    plot = p_reference,
    width = 8,
    height = 5,
    dpi = 400
)


# =============================================================================
# 23. OPTIONAL: INDUCTIVE TEST PERFORMANCE PLOT
# =============================================================================

test_results <- results[
    results$evaluation == "test" &
    results$method == "longTAPIO-window",
    ,
    drop = FALSE
]


if (nrow(test_results) > 0) {

    test_results$scenario_plot <- factor(
        test_results$scenario,
        levels = SCENARIOS,
        labels = c(
            "Regular",
            "Mild",
            "Moderate",
            "Strong",
            "Very strong"
        )
    )


    p_test <- ggplot(
        test_results,
        aes(
            x = scenario_plot,
            y = ARI,
            group = 1
        )
    ) +

        stat_summary(
            fun = mean,
            geom = "line",
            linewidth = 0.9
        ) +

        stat_summary(
            fun = mean,
            geom = "point",
            size = 3
        ) +

        stat_summary(
            fun.data = mean_se,
            geom = "errorbar",
            width = 0.10
        ) +

        scale_y_continuous(
            limits = c(0, 1),
            breaks = seq(
                0,
                1,
                by = 0.2
            )
        ) +

        labs(
            x = "Degree of visit-time irregularity",
            y = "Test Adjusted Rand Index",
            title = "Inductive longTAPIO under irregular observation times",
            subtitle = "70% reference cohort / 30% unseen subjects"
        ) +

        theme_classic(
            base_size = 12
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
        p_test
    )


    ggsave(
        filename = "longTAPIO_inductive_irregularity.pdf",
        plot = p_test,
        width = 8,
        height = 5
    )
}


cat(
    "\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    "BENCHMARK COMPLETE\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    sep = ""
)