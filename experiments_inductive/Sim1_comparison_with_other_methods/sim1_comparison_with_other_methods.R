# =============================================================================
# LONGTAPIO ROBUSTNESS BENCHMARK
#
# Disease-inspired longitudinal trajectories with
# biomarker-specific temporal variation
#
# longTAPIO variants:
#
#   1. random_weighted
#   2. random_weighted_95
#   3. first
#   4. screened
#
# Baselines:
#
#   5. clusterMLD
#   6. kml3d
#
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

library(aricode)
library(clusterMLD)
library(kml3d)
library(longitudinalData)


if (!exists("longTAPIO_inductive")) {
    stop("Please source/load longTAPIO_inductive() first.")
}


if (!exists("importance_longTAPIO_inductive")) {
    stop("Please source/load importance_longTAPIO_inductive() first.")
}


# =============================================================================
# 1. GLOBAL SETTINGS
# =============================================================================

N_RUNS <- 30

K <- 4

N_VISITS <- 10

TIME_MIN <- 0

TIME_MAX <- 10

N_TREES <- 500

LEVELS <- 4

N_BINS <- 10

WINDOW_OVERLAP <- 0.50


# -----------------------------------------------------------------------------
# Biomarker-specific temporal deviation
# -----------------------------------------------------------------------------

BIOMARKER_RHO <- 0.65

BIOMARKER_SD <- 0.07


# -----------------------------------------------------------------------------
# Scenarios
# -----------------------------------------------------------------------------

SCENARIOS <- c(
    "nuisance50",
    "nuisance100",
    "standard",
    "irregular",
    "imbalanced"
)


# -----------------------------------------------------------------------------
# Methods
# -----------------------------------------------------------------------------

METHODS <- c(
    "longTAPIO-random_weighted",
    "longTAPIO-random_weighted_95",
    "longTAPIO-first",
    "longTAPIO-screened",
    "clusterMLD",
    "kml3d"
)


# =============================================================================
# 2. LATENT DISEASE TRAJECTORY
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
# 4. SIMULATION
# =============================================================================

simulate_benchmark <- function(
    scenario = SCENARIOS,
    seed = 1
) {

    scenario <- match.arg(scenario)

    set.seed(seed)


    # =========================================================================
    # SCENARIO SETTINGS
    # =========================================================================

    if (scenario == "nuisance50") {

        N <- 160
        P <- 50
        P_signal <- 5

        proportions <- c(
            0.25,
            0.25,
            0.25,
            0.25
        )

        irregular <- FALSE


    } else if (scenario == "nuisance100") {

        N <- 160
        P <- 100
        P_signal <- 5

        proportions <- c(
            0.25,
            0.25,
            0.25,
            0.25
        )

        irregular <- FALSE


    } else if (scenario == "standard") {

        N <- 160
        P <- 5
        P_signal <- 5

        proportions <- c(
            0.25,
            0.25,
            0.25,
            0.25
        )

        irregular <- FALSE


    } else if (scenario == "irregular") {

        N <- 160
        P <- 5
        P_signal <- 5

        proportions <- c(
            0.25,
            0.25,
            0.25,
            0.25
        )

        irregular <- TRUE


    } else {

        N <- 160
        P <- 10
        P_signal <- 5

        proportions <- c(
            0.55,
            0.25,
            0.125,
            0.075
        )

        irregular <- FALSE
    }


    # =========================================================================
    # TRUE CLUSTERS
    # =========================================================================

    sizes <- floor(
        N * proportions
    )


    sizes[1] <-
        sizes[1] +
        (
            N -
            sum(sizes)
        )


    truth <- rep(
        seq_len(K),
        times = sizes
    )


    truth <- sample(truth)


    # =========================================================================
    # SUBJECT-SPECIFIC HETEROGENEITY
    # =========================================================================

    subject_intercept <- rnorm(
        N,
        mean = 0,
        sd = 0.13
    )


    subject_scale <- rlnorm(
        N,
        meanlog = 0,
        sdlog = 0.07
    )


    subject_phase <- rnorm(
        N,
        mean = 0,
        sd = 0.14
    )


    # =========================================================================
    # SIGNAL PARAMETERS
    # =========================================================================

    signal_loadings <- seq(
        0.90,
        1.25,
        length.out = P_signal
    )


    feature_offsets <- seq(
        -0.10,
        0.10,
        length.out = P_signal
    )


    # =========================================================================
    # GENERATE SUBJECTS
    # =========================================================================

    rows <- vector(
        "list",
        N
    )


    for (i in seq_len(N)) {


        # =====================================================================
        # OBSERVATION TIMES
        # =====================================================================

        if (!irregular) {

            ti <- seq(
                TIME_MIN,
                TIME_MAX,
                length.out = N_VISITS
            )


        } else {

            n_i <- sample(
                7:12,
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


        # =====================================================================
        # LATENT DISEASE TRAJECTORY
        # =====================================================================

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


        # =====================================================================
        # INFORMATIVE BIOMARKERS
        # =====================================================================

        for (j in seq_len(P_signal)) {


            biomarker_deviation <-
                simulate_biomarker_deviation(
                    n = length(ti),
                    rho = BIOMARKER_RHO,
                    innovation_sd = BIOMARKER_SD
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
                    sd = 0.095
                )
        }


        # =====================================================================
        # NUISANCE BIOMARKERS
        # =====================================================================

        if (P > P_signal) {

            for (j in (P_signal + 1):P) {


                nuisance_frequency <- runif(
                    1,
                    min = 0.20,
                    max = 0.90
                )


                nuisance_phase <- runif(
                    1,
                    min = 0,
                    max = 2 * pi
                )


                nuisance_amplitude <- runif(
                    1,
                    min = 0.03,
                    max = 0.12
                )


                nuisance_offset <- rnorm(
                    1,
                    mean = 0,
                    sd = 0.10
                )


                Xi[, j] <-
                    nuisance_offset +
                    nuisance_amplitude *
                    sin(
                        nuisance_frequency *
                        ti +
                        nuisance_phase
                    ) +
                    rnorm(
                        length(ti),
                        mean = 0,
                        sd = 0.25
                    )
            }
        }


        # =====================================================================
        # LONG FORMAT
        # =====================================================================

        tmp <- data.frame(
            id = i,
            time = ti
        )


        for (j in seq_len(P)) {
            tmp[[paste0("y", j)]] <- Xi[, j]
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
        N = N,
        P = P,
        P_signal = P_signal,
        irregular = irregular
    )
}


# =============================================================================
# 5. REGULAR DATA -> ARRAY
# =============================================================================

regular_to_array <- function(
    dat
) {

    features <- grep(
        "^y[0-9]+$",
        names(dat),
        value = TRUE
    )


    ids <- sort(
        unique(dat$id)
    )


    times <- sort(
        unique(dat$time)
    )


    N <- length(ids)
    V <- length(times)
    P <- length(features)


    X <- array(
        NA_real_,
        dim = c(
            N,
            V,
            P
        ),
        dimnames = list(
            as.character(ids),
            as.character(times),
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


        X[ii, , ] <- as.matrix(
            d_i[
                ,
                features,
                drop = FALSE
            ]
        )
    }


    if (any(!is.finite(X))) {
        stop("Non-finite values found in regular representation.")
    }


    list(
        X = X,
        ids = ids,
        time = times,
        features = features
    )
}


# =============================================================================
# 6. IRREGULAR DATA -> OVERLAPPING WINDOWS
# =============================================================================

irregular_to_array <- function(
    dat,
    n_bins = N_BINS,
    overlap = WINDOW_OVERLAP
) {

    features <- grep(
        "^y[0-9]+$",
        names(dat),
        value = TRUE
    )


    ids <- sort(
        unique(dat$id)
    )


    P <- length(features)


    t_min <- min(
        dat$time
    )


    t_max <- max(
        dat$time
    )


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
        t_max,
        by = step
    )


    centers <-
        starts +
        window_size / 2


    V <- length(starts)
    N <- length(ids)


    X <- array(
        NA_real_,
        dim = c(
            N,
            V,
            P
        ),
        dimnames = list(
            as.character(ids),
            as.character(centers),
            features
        )
    )


    global_means <- vapply(
        features,
        function(feature) {

            mean(
                dat[[feature]],
                na.rm = TRUE
            )

        },
        numeric(1)
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

                values[] <-
                    global_means[j]
            }


            X[ii, , j] <- values
        }
    }


    if (any(!is.finite(X))) {
        stop("Non-finite values found in irregular representation.")
    }


    list(
        X = X,
        ids = ids,
        time = centers,
        features = features
    )
}


# =============================================================================
# 7. PREPARE REPRESENTATION
# =============================================================================

prepare_representation <- function(
    sim
) {

    if (sim$irregular) {

        irregular_to_array(
            sim$data
        )

    } else {

        regular_to_array(
            sim$data
        )
    }
}


# =============================================================================
# 8. ARRAY -> LONGTAPIO FORMAT
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
        seq_len(P)
    )


    list(
        DATA = DATA,
        user_id = user_id
    )
}


# =============================================================================
# 9. EXTRACT LONGTAPIO CLUSTER
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
        "Could not extract longTAPIO partition."
    )
}


# =============================================================================
# 10. FIT LONGTAPIO
# =============================================================================
#
# pca_selection can now be:
#
#   "random_weighted"
#   "random_weighted_95"
#   "first"
#
# =============================================================================

fit_longTAPIO_mode <- function(
    representation,
    pca_selection,
    k = K,
    seed = NULL
) {

    if (!is.null(seed)) {
        set.seed(seed)
    }


    X <- representation$X


    N <- dim(X)[1]
    P <- dim(X)[3]


    prepared <- array_to_longTAPIO(
        X
    )


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
        k = k,
        n_features = m,
        n_trees = N_TREES,
        levels = LEVELS,
        method = "ward.D2",
        scale = TRUE,
        replace = FALSE,
        pca_selection = pca_selection
    )


    cluster <- extract_longTAPIO_cluster(
        fit = fit,
        N = N
    )


    list(
        fit = fit,
        cluster = cluster,
        m = m
    )
}


# =============================================================================
# 11. GLOBAL IMPORTANCE
# =============================================================================

extract_global_importance <- function(
    fit,
    P
) {

    IMP <- importance_longTAPIO_inductive(
        res = fit,
        normalize = TRUE
    )


    if (
        is.list(IMP) &&
        !is.null(IMP$global)
    ) {

        G <- IMP$global


    } else if (is.matrix(IMP)) {

        G <- IMP


    } else {

        stop(
            "Could not extract global importance."
        )
    }


    G <- as.matrix(G)


    if (ncol(G) == P) {

        score <- colMeans(
            G,
            na.rm = TRUE
        )


    } else if (nrow(G) == P) {

        score <- rowMeans(
            G,
            na.rm = TRUE
        )


    } else {

        stop(
            "Importance dimensions incompatible with P."
        )
    }


    score[!is.finite(score)] <- 0

    score[score < 0] <- 0


    total <- sum(score)


    if (total <= 0) {

        score <- rep(
            1 / P,
            P
        )

    } else {

        score <-
            score /
            total
    }


    score
}


# =============================================================================
# 12. GAP SCREENING
# =============================================================================

gap_screen <- function(
    importance,
    no_screen_P = 5
) {

    P <- length(importance)


    if (P <= no_screen_P) {

        return(
            list(
                selected = seq_len(P),
                P_selected = P,
                order = order(
                    importance,
                    decreasing = TRUE
                ),
                gap = NA_real_
            )
        )
    }


    ord <- order(
        importance,
        decreasing = TRUE
    )


    imp_sorted <-
        importance[ord]


    positive_values <-
        imp_sorted[
            imp_sorted > 0
        ]


    if (length(positive_values) > 0) {

        eps <- max(
            1e-12,
            min(positive_values) *
            1e-6
        )

    } else {

        eps <- 1e-12
    }


    log_imp <- log(
        imp_sorted +
        eps
    )


    gaps <-
        log_imp[
            seq_len(P - 1)
        ] -
        log_imp[
            2:P
        ]


    min_rank <- 2L


    max_rank <- max(
        min_rank,
        floor(
            0.50 * P
        )
    )


    max_rank <- min(
        max_rank,
        P - 1L
    )


    candidates <-
        min_rank:max_rank


    best_rank <-
        candidates[
            which.max(
                gaps[candidates]
            )
        ]


    selected <-
        ord[
            seq_len(best_rank)
        ]


    list(
        selected = selected,
        P_selected = length(selected),
        order = ord,
        gap = gaps[best_rank]
    )
}


# =============================================================================
# 13. SCREEN + RANDOM-WEIGHTED REFIT
# =============================================================================

fit_screened_longTAPIO <- function(
    representation,
    first_fit,
    seed = NULL
) {

    X <- representation$X

    P <- dim(X)[3]


    if (P <= 5) {
        return(NULL)
    }


    importance <- extract_global_importance(
        fit = first_fit$fit,
        P = P
    )


    screen <- gap_screen(
        importance = importance
    )


    selected <- sort(
        screen$selected
    )


    reduced <- representation


    reduced$X <- X[
        ,
        ,
        selected,
        drop = FALSE
    ]


    if (!is.null(representation$features)) {

        reduced$features <-
            representation$features[selected]
    }


    final <- fit_longTAPIO_mode(
        representation = reduced,
        pca_selection = "random_weighted",
        k = K,
        seed = seed
    )


    list(
        importance = importance,
        selected = selected,
        P_selected = length(selected),
        cluster = final$cluster,
        final = final
    )
}


# =============================================================================
# 14. CLUSTERMLD LABEL HELPER
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

        members <- cluster_list[[g]]


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
# 15. FIT CLUSTERMLD
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
# 16. KML3D CONSTRUCTOR
# =============================================================================

make_cld3d <- function(
    X,
    time
) {

    if (
        exists(
            "cld3d",
            envir = asNamespace("kml3d"),
            inherits = FALSE
        )
    ) {

        fun <- get(
            "cld3d",
            envir = asNamespace("kml3d")
        )


        return(
            fun(
                traj = X,
                time = time
            )
        )
    }


    if (
        exists(
            "clusterLongData3d",
            envir = asNamespace("longitudinalData"),
            inherits = FALSE
        )
    ) {

        fun <- get(
            "clusterLongData3d",
            envir = asNamespace("longitudinalData")
        )


        return(
            fun(
                traj = X,
                time = time
            )
        )
    }


    if (
        exists(
            "clusterLongData3d",
            envir = asNamespace("kml3d"),
            inherits = FALSE
        )
    ) {

        fun <- get(
            "clusterLongData3d",
            envir = asNamespace("kml3d")
        )


        return(
            fun(
                traj = X,
                time = time
            )
        )
    }


    stop(
        "Could not locate ClusterLongData3d constructor."
    )
}


# =============================================================================
# 17. FIT KML3D
# =============================================================================

fit_kml3d <- function(
    representation,
    k = K
) {

    X <- representation$X

    N <- dim(X)[1]


    cld <- make_cld3d(
        X = X,
        time = representation$time
    )


    invisible(
        capture.output(
            suppressMessages(
                suppressWarnings(
                    kml3d::kml3d(
                        cld,
                        nbClusters = k,
                        nbRedrawing = 20,
                        toPlot = "none"
                    )
                )
            )
        )
    )


    cluster_name <- paste0(
        "c",
        k
    )


    part <- cld[
        cluster_name
    ][[1]]


    cluster <- as.integer(
        part[
            "clustersAsInteger"
        ]
    )


    if (length(cluster) != N) {

        stop(
            "Unexpected number of kml3d labels."
        )
    }


    cluster
}


# =============================================================================
# 18. METRICS
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
# 19. RESULT HELPER
# =============================================================================

make_result <- function(
    scenario,
    run,
    method,
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
# 20. RUN ONE REPLICATE
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


    rw_seed <-
        100000 +
        scenario_number * 1000 +
        run


    rw95_seed <-
        150000 +
        scenario_number * 1000 +
        run


    first_seed <-
        200000 +
        scenario_number * 1000 +
        run


    screened_seed <-
        300000 +
        scenario_number * 1000 +
        run


    # =========================================================================
    # DATA
    # =========================================================================

    sim <- simulate_benchmark(
        scenario = scenario,
        seed = data_seed
    )


    dat <- sim$data

    truth <- sim$truth


    representation <- prepare_representation(
        sim
    )


    P <- sim$P

    P_signal <- sim$P_signal


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
        " | N=",
        sim$N,
        " | P=",
        P,
        " | signal=",
        P_signal,
        " | nuisance=",
        P - P_signal,
        "\n",
        paste(
            rep("=", 100),
            collapse = ""
        ),
        "\n",
        sep = ""
    )


    output <- list()

    screening_row <- NULL


    # =========================================================================
    # 1. RANDOM WEIGHTED
    # =========================================================================

    cat(
        "\n1. longTAPIO-random_weighted\n"
    )


    t0 <- proc.time()[3]


    rw <- try(
        fit_longTAPIO_mode(
            representation = representation,
            pca_selection = "random_weighted",
            k = K,
            seed = rw_seed
        ),
        silent = TRUE
    )


    sec_rw <-
        proc.time()[3] -
        t0


    if (!inherits(rw, "try-error")) {

        score <- get_scores(
            truth,
            rw$cluster
        )


        cat(
            sprintf(
                "   ARI=%.3f | NMI=%.3f | m=%d | %.2f sec\n",
                score["ARI"],
                score["NMI"],
                rw$m,
                sec_rw
            )
        )


        output[[length(output) + 1L]] <-
            make_result(
                scenario,
                run,
                "longTAPIO-random_weighted",
                truth,
                rw$cluster,
                sec_rw
            )
    }


    # =========================================================================
    # 2. RANDOM WEIGHTED 95
    # =========================================================================

    cat(
        "\n2. longTAPIO-random_weighted_95\n"
    )


    t0 <- proc.time()[3]


    rw95 <- try(
        fit_longTAPIO_mode(
            representation = representation,
            pca_selection = "random_weighted_95",
            k = K,
            seed = rw95_seed
        ),
        silent = TRUE
    )


    sec_rw95 <-
        proc.time()[3] -
        t0


    if (!inherits(rw95, "try-error")) {

        score <- get_scores(
            truth,
            rw95$cluster
        )


        cat(
            sprintf(
                "   ARI=%.3f | NMI=%.3f | m=%d | %.2f sec\n",
                score["ARI"],
                score["NMI"],
                rw95$m,
                sec_rw95
            )
        )


        output[[length(output) + 1L]] <-
            make_result(
                scenario,
                run,
                "longTAPIO-random_weighted_95",
                truth,
                rw95$cluster,
                sec_rw95
            )
    }


    # =========================================================================
    # 3. FIRST
    # =========================================================================

    cat(
        "\n3. longTAPIO-first\n"
    )


    t0 <- proc.time()[3]


    first <- try(
        fit_longTAPIO_mode(
            representation = representation,
            pca_selection = "first",
            k = K,
            seed = first_seed
        ),
        silent = TRUE
    )


    sec_first <-
        proc.time()[3] -
        t0


    if (!inherits(first, "try-error")) {

        score <- get_scores(
            truth,
            first$cluster
        )


        cat(
            sprintf(
                "   ARI=%.3f | NMI=%.3f | m=%d | %.2f sec\n",
                score["ARI"],
                score["NMI"],
                first$m,
                sec_first
            )
        )


        output[[length(output) + 1L]] <-
            make_result(
                scenario,
                run,
                "longTAPIO-first",
                truth,
                first$cluster,
                sec_first
            )
    }


    # =========================================================================
    # 4. SCREENED
    # =========================================================================

    cat(
        "\n4. longTAPIO-screened\n"
    )


    if (P <= 5) {

        # Screening is unnecessary when all variables are signal.
        # Keep exactly the random_weighted result for direct comparison.

        if (!inherits(rw, "try-error")) {

            screened_cluster <-
                rw$cluster


            sec_screen <-
                sec_rw


            score <- get_scores(
                truth,
                screened_cluster
            )


            cat(
                sprintf(
                    "   No screening | ARI=%.3f | NMI=%.3f\n",
                    score["ARI"],
                    score["NMI"]
                )
            )


            output[[length(output) + 1L]] <-
                make_result(
                    scenario,
                    run,
                    "longTAPIO-screened",
                    truth,
                    screened_cluster,
                    sec_screen
                )
        }


    } else if (!inherits(first, "try-error")) {


        t0 <- proc.time()[3]


        screened <- try(
            fit_screened_longTAPIO(
                representation = representation,
                first_fit = first,
                seed = screened_seed
            ),
            silent = TRUE
        )


        sec_screen <-
            sec_first +
            (
                proc.time()[3] -
                t0
            )


        if (!inherits(screened, "try-error")) {

            score <- get_scores(
                truth,
                screened$cluster
            )


            signals_retained <- sum(
                screened$selected <=
                P_signal
            )


            false_positives <- sum(
                screened$selected >
                P_signal
            )


            cat(
                sprintf(
                    paste0(
                        "   P*=%d",
                        " | signals=%d/%d",
                        " | false positives=%d",
                        " | ARI=%.3f",
                        " | NMI=%.3f\n"
                    ),
                    screened$P_selected,
                    signals_retained,
                    P_signal,
                    false_positives,
                    score["ARI"],
                    score["NMI"]
                )
            )


            output[[length(output) + 1L]] <-
                make_result(
                    scenario,
                    run,
                    "longTAPIO-screened",
                    truth,
                    screened$cluster,
                    sec_screen
                )


            signal_mass <- sum(
                screened$importance[
                    seq_len(P_signal)
                ]
            )


            nuisance_mean <- mean(
                screened$importance[
                    (P_signal + 1):P
                ]
            )


            signal_mean <- mean(
                screened$importance[
                    seq_len(P_signal)
                ]
            )


            enrichment <- if (
                is.finite(nuisance_mean) &&
                nuisance_mean > 0
            ) {

                signal_mean /
                    nuisance_mean

            } else {

                NA_real_
            }


            screening_row <- data.frame(
                scenario = scenario,
                run = run,
                P = P,
                P_signal = P_signal,
                P_selected = screened$P_selected,
                signals_retained = signals_retained,
                false_positives = false_positives,
                signal_mass = signal_mass,
                enrichment = enrichment,
                stringsAsFactors = FALSE
            )
        }
    }


    # =========================================================================
    # 5. CLUSTERMLD
    # =========================================================================

    cat(
        "\n5. clusterMLD\n"
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
                scenario,
                run,
                "clusterMLD",
                truth,
                cl,
                sec_cl
            )
    }


    # =========================================================================
    # 6. KML3D
    # =========================================================================

    cat(
        "\n6. kml3d\n"
    )


    t0 <- proc.time()[3]


    km <- try(
        fit_kml3d(
            representation = representation,
            k = K
        ),
        silent = TRUE
    )


    sec_km <-
        proc.time()[3] -
        t0


    if (!inherits(km, "try-error")) {

        score <- get_scores(
            truth,
            km
        )


        cat(
            sprintf(
                "   ARI=%.3f | NMI=%.3f | %.2f sec\n",
                score["ARI"],
                score["NMI"],
                sec_km
            )
        )


        output[[length(output) + 1L]] <-
            make_result(
                scenario,
                run,
                "kml3d",
                truth,
                km,
                sec_km
            )
    }


    list(
        results = do.call(
            rbind,
            output
        ),
        screening = screening_row
    )
}


# =============================================================================
# 21. RUN BENCHMARK
# =============================================================================

cat(
    "\n",
    paste(
        rep("=", 120),
        collapse = ""
    ),
    "\n",
    "LONGTAPIO ROBUSTNESS BENCHMARK\n",
    "Biomarker AR(1) deviation: rho = ",
    BIOMARKER_RHO,
    ", innovation SD = ",
    BIOMARKER_SD,
    "\n",
    paste(
        rep("=", 120),
        collapse = ""
    ),
    "\n\n",
    sep = ""
)


all_results <- list()

all_screening <- list()


counter_result <- 1L

counter_screen <- 1L


for (scenario in SCENARIOS) {

    for (run in seq_len(N_RUNS)) {

        z <- run_one(
            scenario = scenario,
            run = run
        )


        if (!is.null(z$results)) {

            all_results[[counter_result]] <-
                z$results

            counter_result <-
                counter_result +
                1L
        }


        if (!is.null(z$screening)) {

            all_screening[[counter_screen]] <-
                z$screening

            counter_screen <-
                counter_screen +
                1L
        }
    }
}


results <- do.call(
    rbind,
    all_results
)


if (length(all_screening) > 0) {

    screening_results <- do.call(
        rbind,
        all_screening
    )

} else {

    screening_results <- NULL
}


# =============================================================================
# 22. SUMMARY HELPER
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


# =============================================================================
# 23. FINAL PERFORMANCE SUMMARY
# =============================================================================

summary_rows <- list()

cc <- 1L


for (scenario in SCENARIOS) {

    for (method in METHODS) {

        d <- results[
            results$scenario == scenario &
            results$method == method,
            ,
            drop = FALSE
        ]


        if (nrow(d) == 0) {
            next
        }


        summary_rows[[cc]] <- data.frame(
            scenario = scenario,
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


summary_table <- do.call(
    rbind,
    summary_rows
)


cat(
    "\n\n",
    paste(
        rep("=", 120),
        collapse = ""
    ),
    "\n",
    "FINAL RESULTS\n",
    paste(
        rep("=", 120),
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
# 24. SCREENING SUMMARY
# =============================================================================

if (!is.null(screening_results)) {

    cat(
        "\n\n",
        paste(
            rep("=", 120),
            collapse = ""
        ),
        "\n",
        "SCREENING SUMMARY\n",
        paste(
            rep("=", 120),
            collapse = ""
        ),
        "\n",
        sep = ""
    )


    screening_scenarios <- unique(
        screening_results$scenario
    )


    screen_summary <- do.call(
        rbind,
        lapply(
            screening_scenarios,
            function(sc) {

                d <- screening_results[
                    screening_results$scenario == sc,
                    ,
                    drop = FALSE
                ]


                data.frame(
                    scenario = sc,
                    P = unique(d$P)[1],
                    nuisance_fraction =
                        1 -
                        unique(d$P_signal)[1] /
                        unique(d$P)[1],
                    P_selected = mean_sd(
                        d$P_selected
                    ),
                    signals_retained = mean_sd(
                        d$signals_retained
                    ),
                    false_positives = mean_sd(
                        d$false_positives
                    ),
                    enrichment = mean_sd(
                        d$enrichment
                    ),
                    signal_mass = mean_sd(
                        d$signal_mass
                    ),
                    stringsAsFactors = FALSE
                )
            }
        )
    )


    print(
        screen_summary,
        row.names = FALSE
    )
}


# =============================================================================
# 25. PAIRED COMPARISON HELPER
# =============================================================================

paired_difference <- function(
    scenario,
    method1,
    method2,
    metric = "ARI"
) {

    d1 <- results[
        results$scenario == scenario &
        results$method == method1,
        c(
            "run",
            metric
        ),
        drop = FALSE
    ]


    d2 <- results[
        results$scenario == scenario &
        results$method == method2,
        c(
            "run",
            metric
        ),
        drop = FALSE
    ]


    names(d1)[2] <- "x"

    names(d2)[2] <- "y"


    d <- merge(
        d1,
        d2,
        by = "run"
    )


    if (nrow(d) == 0) {

        return(
            c(
                mean = NA_real_,
                sd = NA_real_,
                improved = NA_real_,
                equal = NA_real_,
                worse = NA_real_
            )
        )
    }


    delta <-
        d$x -
        d$y


    c(
        mean = mean(
            delta,
            na.rm = TRUE
        ),
        sd = sd(
            delta,
            na.rm = TRUE
        ),
        improved = sum(
            delta > 0,
            na.rm = TRUE
        ),
        equal = sum(
            abs(delta) < 1e-12,
            na.rm = TRUE
        ),
        worse = sum(
            delta < 0,
            na.rm = TRUE
        )
    )
}


# =============================================================================
# 26. GENERIC PAIRED SUMMARY
# =============================================================================

print_paired_comparison <- function(
    method1,
    method2
) {

    cat(
        "\n\n",
        paste(
            rep("=", 120),
            collapse = ""
        ),
        "\n",
        method1,
        " - ",
        method2,
        "\n",
        paste(
            rep("=", 120),
            collapse = ""
        ),
        "\n",
        sep = ""
    )


    for (scenario in SCENARIOS) {

        ari <- paired_difference(
            scenario = scenario,
            method1 = method1,
            method2 = method2,
            metric = "ARI"
        )


        nmi <- paired_difference(
            scenario = scenario,
            method1 = method1,
            method2 = method2,
            metric = "NMI"
        )


        cat(
            sprintf(
                paste0(
                    "%-13s",
                    " | delta ARI %+0.3f +/- %.3f",
                    " | delta NMI %+0.3f +/- %.3f",
                    " | improved %d",
                    " | equal %d",
                    " | worse %d\n"
                ),
                scenario,
                ari["mean"],
                ari["sd"],
                nmi["mean"],
                nmi["sd"],
                as.integer(ari["improved"]),
                as.integer(ari["equal"]),
                as.integer(ari["worse"])
            )
        )
    }
}


# =============================================================================
# 27. PAIRED COMPARISONS
# =============================================================================


# -----------------------------------------------------------------------------
# random_weighted_95 versus random_weighted
# -----------------------------------------------------------------------------

print_paired_comparison(
    method1 = "longTAPIO-random_weighted_95",
    method2 = "longTAPIO-random_weighted"
)


# -----------------------------------------------------------------------------
# first versus random_weighted
# -----------------------------------------------------------------------------

print_paired_comparison(
    method1 = "longTAPIO-first",
    method2 = "longTAPIO-random_weighted"
)


# -----------------------------------------------------------------------------
# first versus random_weighted_95
# -----------------------------------------------------------------------------

print_paired_comparison(
    method1 = "longTAPIO-first",
    method2 = "longTAPIO-random_weighted_95"
)


# -----------------------------------------------------------------------------
# screened versus random_weighted
# -----------------------------------------------------------------------------

print_paired_comparison(
    method1 = "longTAPIO-screened",
    method2 = "longTAPIO-random_weighted"
)


# -----------------------------------------------------------------------------
# screened versus random_weighted_95
# -----------------------------------------------------------------------------

print_paired_comparison(
    method1 = "longTAPIO-screened",
    method2 = "longTAPIO-random_weighted_95"
)


# -----------------------------------------------------------------------------
# screened versus first
# -----------------------------------------------------------------------------

print_paired_comparison(
    method1 = "longTAPIO-screened",
    method2 = "longTAPIO-first"
)


# =============================================================================
# 28. SAVE RESULTS
# =============================================================================

write.csv(
    results,
    "longTAPIO_biomarker_deviation_results.csv",
    row.names = FALSE
)


write.csv(
    summary_table,
    "longTAPIO_biomarker_deviation_summary.csv",
    row.names = FALSE
)


if (!is.null(screening_results)) {

    write.csv(
        screening_results,
        "longTAPIO_biomarker_deviation_screening_results.csv",
        row.names = FALSE
    )


    write.csv(
        screen_summary,
        "longTAPIO_biomarker_deviation_screening_summary.csv",
        row.names = FALSE
    )
}


cat(
    "\n",
    paste(
        rep("=", 120),
        collapse = ""
    ),
    "\n",
    "BENCHMARK COMPLETE\n",
    paste(
        rep("=", 120),
        collapse = ""
    ),
    "\n",
    sep = ""
)


#### PLOTS 
# =============================================================================
# PUBLICATION-STYLE GGPLOT OF BENCHMARK RESULTS
# =============================================================================

library(ggplot2)


# -----------------------------------------------------------------------------
# Clean labels
# -----------------------------------------------------------------------------

plot_data <- results


plot_data$method_plot <- factor(
    plot_data$method,
    levels = c(
        "longTAPIO-random_weighted",
        "longTAPIO-random_weighted_95",
        "longTAPIO-first",
        "longTAPIO-screened",
        "clusterMLD",
        "kml3d"
    ),
    labels = c(
        "longTAPIO-RW",
        "longTAPIO-RW95",
        "longTAPIO-PC1",
        "longTAPIO-Screened",
        "clusterMLD",
        "kml3d"
    )
)


plot_data$scenario_plot <- factor(
    plot_data$scenario,
    levels = c(
        "standard",
        "irregular",
        "imbalanced",
        "nuisance50",
        "nuisance100"
    ),
    labels = c(
        "Regular visits",
        "Irregular visits",
        "Imbalanced\n clusters",
        "50 biomarkers\n(5 informative)",
        "100 biomarkers\n(5 informative)"
    )
)


# -----------------------------------------------------------------------------
# ARI plot
# -----------------------------------------------------------------------------

p_ari <- ggplot(
    plot_data,
    aes(
        x = method_plot,
        y = ARI,
        fill = method_plot
    )
) +

    geom_boxplot(
        width = 0.65,
        outlier.shape = NA,
        alpha = 0.75,
        linewidth = 0.45
    ) +

    geom_jitter(
        aes(
            color = method_plot
        ),
        width = 0.13,
        height = 0,
        size = 1.1,
        alpha = 0.35,
        show.legend = FALSE
    ) +

    stat_summary(
        fun = mean,
        geom = "point",
        shape = 23,
        size = 3.2,
        fill = "white",
        color = "black",
        stroke = 0.7
    ) +

    facet_wrap(
        ~ scenario_plot,
        nrow = 1
    ) +

    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(
            0,
            1,
            by = 0.2
        ),
        expand = expansion(
            mult = c(
                0,
                0.02
            )
        )
    ) +

    labs(
        x = NULL,
        y = "Adjusted Rand Index",
        title = "Clustering performance across benchmark scenarios",
        subtitle = "Boxes show distributions across 30 simulation runs; white diamonds indicate means",
        fill = "Method"
    ) +

    theme_classic(
        base_size = 12
    ) +

    theme(
        plot.title = element_text(
            face = "bold",
            size = 15,
            margin = margin(
                b = 4
            )
        ),

        plot.subtitle = element_text(
            size = 10.5,
            margin = margin(
                b = 12
            )
        ),

        axis.title.y = element_text(
            face = "bold",
            margin = margin(
                r = 8
            )
        ),

        axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            vjust = 1,
            size = 8.5
        ),

        strip.text = element_text(
            face = "bold",
            size = 10
        ),

        strip.background = element_rect(
            fill = "grey95",
            color = NA
        ),

        panel.spacing = unit(
            1.0,
            "lines"
        ),

        legend.position = "bottom",

        legend.title = element_blank(),

        legend.text = element_text(
            size = 9
        ),

        plot.margin = margin(
            10,
            10,
            10,
            10
        )
    )


print(
    p_ari
)


# -----------------------------------------------------------------------------
# Save publication-quality versions
# -----------------------------------------------------------------------------

ggsave(
    filename = "longTAPIO_benchmark_ARI.pdf",
    plot = p_ari,
    width = 14,
    height = 5.2
)


ggsave(
    filename = "longTAPIO_benchmark_ARI.png",
    plot = p_ari,
    width = 14,
    height = 5.2,
    dpi = 400
)