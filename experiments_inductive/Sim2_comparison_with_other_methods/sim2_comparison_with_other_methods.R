# =============================================================================
# LONGTAPIO ROBUSTNESS BENCHMARK
# ORIGINAL simLongData() SYNTHETIC DATA
#
# METHODS
#
#   1. longTAPIO-random_weighted
#   2. longTAPIO-random_weighted_95
#   3. longTAPIO-first
#   4. longTAPIO-screened
#   5. clusterMLD
#   6. kml3d
#
# SCENARIOS
#
#   standard
#   irregular
#   noisy
#   imbalanced
#   nuisance50
#   nuisance100
#
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

library(MASS)
library(aricode)
library(clusterMLD)
library(kml3d)
library(longitudinalData)
library(ggplot2)


if (!exists("longTAPIO_inductive")) {
    stop("Please source/load longTAPIO_inductive() first.")
}


if (!exists("importance_longTAPIO_inductive")) {
    stop("Please source/load importance_longTAPIO_inductive() first.")
}


if (!exists("generate_random_curve")) {
    stop(
        paste0(
            "generate_random_curve() is not available. ",
            "Please source the original function used by simLongData()."
        )
    )
}


# =============================================================================
# 1. GLOBAL SETTINGS
# =============================================================================

N_RUNS <- 30

N_TOTAL <- 200

K <- 4

N_VISITS <- 10

N_TREES <- 500

LEVELS <- 4

N_BINS <- 10

WINDOW_OVERLAP <- 0.50


ETA_STANDARD <- 3

ETA_NOISY <- 15


SIGMA_DIAG <- rep(
    3,
    5
)


SCENARIOS <- c(
    "standard",
    "irregular",
    "noisy",
    "imbalanced",
    "nuisance50",
    "nuisance100"
)


METHODS <- c(
    "longTAPIO-random_weighted",
    "longTAPIO-random_weighted_95",
    "longTAPIO-first",
    "longTAPIO-screened",
    "clusterMLD",
    "kml3d"
)


# =============================================================================
# 2. ORIGINAL simLongData()
# =============================================================================

simLongData <- function(
    n_total = 200,
    K = 4,
    outcomes = 5,
    eta = 3,
    cluster_sizes = rep(n_total / K, K),
    ranTimes = TRUE,
    n_i = 10,
    sigma_diag = rep(3, 5)
) {

    # -------------------------------------------------------------------------
    # Fixed-effect mean functions
    # -------------------------------------------------------------------------

    mean_functions <- list(

        # Cluster 1
        list(
            function(t) 8 * t - 0.6 * t^2,
            function(t) t,
            function(t) -10 + 6 * t - 0.4 * t^2,
            function(t) -1 + t,
            function(t) -2 * t + 0.1 * t^2
        ),

        # Cluster 2
        list(
            function(t) 20 - 6 * t + 0.3 * t^2,
            function(t) -t,
            function(t) -10 + 6 * t - 0.4 * t^2,
            function(t) -1 + t,
            function(t) -2 * t + 0.1 * t^2
        ),

        # Cluster 3
        list(
            function(t) 0,
            function(t) -7 * t + 0.5 * t^2,
            function(t) 0.2 * t,
            function(t) -1 + t,
            function(t) -2 * t + 0.1 * t^2
        ),

        # Cluster 4
        list(
            function(t) 20,
            function(t) -20 + t,
            function(t) 0.2 * t,
            function(t) 10 + 2 * t - 0.2 * t^2,
            function(t) -2 * t + 0.1 * t^2
        )
    )


    # -------------------------------------------------------------------------
    # Covariance structure
    # -------------------------------------------------------------------------

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
        diag(sigma_diag) %*%
        R %*%
        diag(sigma_diag)


    # -------------------------------------------------------------------------
    # Simulate
    # -------------------------------------------------------------------------

    sim_data <- list()

    subject_id <- 1


    for (k in 1:K) {

        for (i in 1:cluster_sizes[k]) {


            if (ranTimes) {

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


            for (j in 1:n_i_subject) {

                t_ij <- times[j]


                random_effect_t <-
                    generate_random_curve(
                        t_ij
                    )


                for (h in 1:outcomes) {

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


    return(
        sim_df
    )
}


# =============================================================================
# 3. ADD NUISANCE VARIABLES
# =============================================================================
#
# Nuisance variables contain structured longitudinal variation but are
# independent of the true cluster.
#
# They are deliberately not simple white-noise columns.
#
# =============================================================================

add_nuisance_outcomes <- function(
    dat,
    total_outcomes,
    signal_outcomes = 5,
    noise_sd = ETA_STANDARD
) {

    if (total_outcomes <= signal_outcomes) {
        return(dat)
    }


    subjects <- sort(
        unique(dat$subject)
    )


    nuisance_rows <- list()

    counter <- 1L


    for (id in subjects) {

        d_i <- dat[
            dat$subject == id,
            ,
            drop = FALSE
        ]


        times <- sort(
            unique(d_i$time)
        )


        cluster_i <- unique(
            d_i$cluster
        )[1]


        if (length(times) > 1) {

            time_scaled <-
                (
                    times -
                    min(times)
                ) /
                (
                    max(times) -
                    min(times)
                )

        } else {

            time_scaled <- rep(
                0,
                length(times)
            )
        }


        for (h in (signal_outcomes + 1):total_outcomes) {

            intercept <- rnorm(
                1,
                mean = 0,
                sd = 5
            )


            slope <- rnorm(
                1,
                mean = 0,
                sd = 2
            )


            quadratic <- rnorm(
                1,
                mean = 0,
                sd = 0.8
            )


            amplitude <- runif(
                1,
                min = 0,
                max = 3
            )


            frequency <- runif(
                1,
                min = 0.5,
                max = 2
            )


            phase <- runif(
                1,
                min = 0,
                max = 2 * pi
            )


            residual_sd <- max(
                0.5,
                0.75 * noise_sd
            )


            y <-
                intercept +
                slope * time_scaled +
                quadratic * time_scaled^2 +
                amplitude *
                sin(
                    2 *
                    pi *
                    frequency *
                    time_scaled +
                    phase
                ) +
                rnorm(
                    length(times),
                    mean = 0,
                    sd = residual_sd
                )


            nuisance_rows[[counter]] <-
                data.frame(
                    subject = id,
                    time = times,
                    outcome = h,
                    y = y,
                    cluster = cluster_i
                )


            counter <- counter + 1L
        }
    }


    nuisance <- do.call(
        rbind,
        nuisance_rows
    )


    out <- rbind(
        dat,
        nuisance
    )


    out <- out[
        order(
            out$subject,
            out$time,
            out$outcome
        ),
        ,
        drop = FALSE
    ]


    rownames(out) <- NULL


    out
}


# =============================================================================
# 4. SIMULATE BENCHMARK SCENARIO
# =============================================================================

simulate_scenario <- function(
    scenario,
    seed
) {

    set.seed(
        seed
    )


    # =========================================================================
    # STANDARD
    # =========================================================================

    if (scenario == "standard") {

        cluster_sizes <- rep(
            N_TOTAL / K,
            K
        )


        dat <- simLongData(
            n_total = N_TOTAL,
            K = K,
            outcomes = 5,
            eta = ETA_STANDARD,
            cluster_sizes = cluster_sizes,
            ranTimes = FALSE,
            n_i = N_VISITS,
            sigma_diag = SIGMA_DIAG
        )


        P <- 5

        P_signal <- 5

        irregular <- FALSE
    }


    # =========================================================================
    # IRREGULAR
    # =========================================================================

    else if (scenario == "irregular") {

        cluster_sizes <- rep(
            N_TOTAL / K,
            K
        )


        dat <- simLongData(
            n_total = N_TOTAL,
            K = K,
            outcomes = 5,
            eta = ETA_STANDARD,
            cluster_sizes = cluster_sizes,
            ranTimes = TRUE,
            n_i = N_VISITS,
            sigma_diag = SIGMA_DIAG
        )


        P <- 5

        P_signal <- 5

        irregular <- TRUE
    }


    # =========================================================================
    # NOISY
    # =========================================================================

    else if (scenario == "noisy") {

        cluster_sizes <- rep(
            N_TOTAL / K,
            K
        )


        dat <- simLongData(
            n_total = N_TOTAL,
            K = K,
            outcomes = 5,
            eta = ETA_NOISY,
            cluster_sizes = cluster_sizes,
            ranTimes = FALSE,
            n_i = N_VISITS,
            sigma_diag = SIGMA_DIAG
        )


        P <- 5

        P_signal <- 5

        irregular <- FALSE
    }


    # =========================================================================
    # IMBALANCED
    # =========================================================================

    else if (scenario == "imbalanced") {

        cluster_sizes <- c(
            110,
            50,
            25,
            15
        )


        dat <- simLongData(
            n_total = N_TOTAL,
            K = K,
            outcomes = 5,
            eta = ETA_STANDARD,
            cluster_sizes = cluster_sizes,
            ranTimes = FALSE,
            n_i = N_VISITS,
            sigma_diag = SIGMA_DIAG
        )


        P <- 5

        P_signal <- 5

        irregular <- FALSE
    }


    # =========================================================================
    # 50 VARIABLES
    # =========================================================================

    else if (scenario == "nuisance50") {

        cluster_sizes <- rep(
            N_TOTAL / K,
            K
        )


        dat <- simLongData(
            n_total = N_TOTAL,
            K = K,
            outcomes = 5,
            eta = ETA_STANDARD,
            cluster_sizes = cluster_sizes,
            ranTimes = FALSE,
            n_i = N_VISITS,
            sigma_diag = SIGMA_DIAG
        )


        dat <- add_nuisance_outcomes(
            dat = dat,
            total_outcomes = 50,
            signal_outcomes = 5,
            noise_sd = ETA_STANDARD
        )


        P <- 50

        P_signal <- 5

        irregular <- FALSE
    }


    # =========================================================================
    # 100 VARIABLES
    # =========================================================================

    else if (scenario == "nuisance100") {

        cluster_sizes <- rep(
            N_TOTAL / K,
            K
        )


        dat <- simLongData(
            n_total = N_TOTAL,
            K = K,
            outcomes = 5,
            eta = ETA_STANDARD,
            cluster_sizes = cluster_sizes,
            ranTimes = FALSE,
            n_i = N_VISITS,
            sigma_diag = SIGMA_DIAG
        )


        dat <- add_nuisance_outcomes(
            dat = dat,
            total_outcomes = 100,
            signal_outcomes = 5,
            noise_sd = ETA_STANDARD
        )


        P <- 100

        P_signal <- 5

        irregular <- FALSE
    }


    else {

        stop(
            "Unknown scenario."
        )
    }


    # =========================================================================
    # TRUE LABELS
    # =========================================================================

    truth_df <- unique(
        dat[
            ,
            c(
                "subject",
                "cluster"
            )
        ]
    )


    truth_df <- truth_df[
        order(
            truth_df$subject
        ),
        ,
        drop = FALSE
    ]


    truth <- truth_df$cluster


    list(
        data = dat,
        truth = truth,
        N = length(truth),
        P = P,
        P_signal = P_signal,
        irregular = irregular
    )
}


# =============================================================================
# 5. LONG -> WIDE LONGITUDINAL DATA
# =============================================================================

long_to_wide <- function(
    dat
) {

    wide <- reshape(
        dat[
            ,
            c(
                "subject",
                "time",
                "outcome",
                "y"
            )
        ],
        idvar = c(
            "subject",
            "time"
        ),
        timevar = "outcome",
        direction = "wide"
    )


    names(wide) <- sub(
        "^y\\.",
        "y",
        names(wide)
    )


    wide <- wide[
        order(
            wide$subject,
            wide$time
        ),
        ,
        drop = FALSE
    ]


    rownames(wide) <- NULL


    wide
}


# =============================================================================
# 6. REGULAR DATA -> ARRAY
# =============================================================================

regular_to_array <- function(
    dat
) {

    wide <- long_to_wide(
        dat
    )


    features <- grep(
        "^y[0-9]+$",
        names(wide),
        value = TRUE
    )


    ids <- sort(
        unique(wide$subject)
    )


    times <- sort(
        unique(wide$time)
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

        d_i <- wide[
            wide$subject == ids[ii],
            ,
            drop = FALSE
        ]


        d_i <- d_i[
            order(
                d_i$time
            ),
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

        stop(
            "Non-finite values in regular representation."
        )
    }


    list(
        X = X,
        ids = ids,
        time = times,
        features = features
    )
}


# =============================================================================
# 7. IRREGULAR DATA -> OVERLAPPING WINDOWS
# =============================================================================

irregular_to_array <- function(
    dat,
    n_bins = N_BINS,
    overlap = WINDOW_OVERLAP
) {

    wide <- long_to_wide(
        dat
    )


    features <- grep(
        "^y[0-9]+$",
        names(wide),
        value = TRUE
    )


    ids <- sort(
        unique(wide$subject)
    )


    P <- length(features)


    t_min <- min(
        wide$time
    )


    t_max <- max(
        wide$time
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
                wide[[feature]],
                na.rm = TRUE
            )

        },
        numeric(1)
    )


    for (ii in seq_along(ids)) {

        d_i <- wide[
            wide$subject == ids[ii],
            ,
            drop = FALSE
        ]


        d_i <- d_i[
            order(
                d_i$time
            ),
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
                is.finite(
                    values
                )
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

        stop(
            "Non-finite values in irregular representation."
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
# 8. PREPARE REPRESENTATION
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
# 9. ARRAY -> LONGTAPIO FORMAT
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
# 10. EXTRACT LONGTAPIO CLUSTERS
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
# 11. FIT LONGTAPIO
# =============================================================================

fit_longTAPIO_mode <- function(
    representation,
    pca_selection,
    k = K,
    seed = NULL
) {

    if (!is.null(seed)) {

        set.seed(
            seed
        )
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
# 12. GLOBAL IMPORTANCE
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


    G <- as.matrix(
        G
    )


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


    total <- sum(
        score
    )


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
# 13. GAP SCREENING
# =============================================================================

gap_screen <- function(
    importance,
    no_screen_P = 5
) {

    P <- length(
        importance
    )


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
            min(
                positive_values
            ) *
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
            seq_len(
                best_rank
            )
        ]


    list(
        selected = selected,
        P_selected = length(selected),
        order = ord,
        gap = gaps[best_rank]
    )
}


# =============================================================================
# 14. SCREENED LONGTAPIO
# =============================================================================

fit_screened_longTAPIO <- function(
    representation,
    first_fit,
    seed = NULL
) {

    X <- representation$X

    P <- dim(X)[3]


    if (P <= 5) {

        return(
            NULL
        )
    }


    importance <- extract_global_importance(
        fit = first_fit$fit,
        P = P
    )


    screen <- gap_screen(
        importance
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
# 15. CLUSTERMLD LABEL HELPER
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


    as.integer(
        labels
    )
}


# =============================================================================
# 16. FIT CLUSTERMLD
# =============================================================================

fit_clusterMLD <- function(
    dat,
    k = K
) {

    wide <- long_to_wide(
        dat
    )


    features <- grep(
        "^y[0-9]+$",
        names(wide),
        value = TRUE
    )


    ids <- sort(
        unique(wide$subject)
    )


    fit <- clusterMLD::LongDataCluster(
        x = wide$time,
        Y = wide[
            ,
            features,
            drop = FALSE
        ],
        id = wide$subject,
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
# 17. KML3D CONSTRUCTOR
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
# 18. FIT KML3D
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
# 19. METRICS
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


    sim <- simulate_scenario(
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
# 21. RUN COMPLETE BENCHMARK
# =============================================================================

cat(
    "\n",
    paste(
        rep("=", 120),
        collapse = ""
    ),
    "\n",
    "LONGTAPIO simLongData ROBUSTNESS BENCHMARK\n",
    "\n",
    "Runs          : ",
    N_RUNS,
    "\n",
    "Subjects      : ",
    N_TOTAL,
    "\n",
    "Clusters      : ",
    K,
    "\n",
    "Standard eta  : ",
    ETA_STANDARD,
    "\n",
    "Noisy eta     : ",
    ETA_NOISY,
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
# 23. FINAL RESULTS
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


    print(
        screen_summary,
        row.names = FALSE
    )
}


# =============================================================================
# 25. PAIRED DIFFERENCE
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
# 26. PRINT PAIRED COMPARISON
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
                as.integer(
                    ari["improved"]
                ),
                as.integer(
                    ari["equal"]
                ),
                as.integer(
                    ari["worse"]
                )
            )
        )
    }
}


# =============================================================================
# 27. PAIRED COMPARISONS
# =============================================================================

print_paired_comparison(
    "longTAPIO-random_weighted_95",
    "longTAPIO-random_weighted"
)


print_paired_comparison(
    "longTAPIO-first",
    "longTAPIO-random_weighted"
)


print_paired_comparison(
    "longTAPIO-first",
    "longTAPIO-random_weighted_95"
)


print_paired_comparison(
    "longTAPIO-screened",
    "longTAPIO-random_weighted"
)


print_paired_comparison(
    "longTAPIO-screened",
    "longTAPIO-random_weighted_95"
)


print_paired_comparison(
    "longTAPIO-screened",
    "longTAPIO-first"
)


# =============================================================================
# 28. PUBLICATION-STYLE GGPLOT
# =============================================================================

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
        "noisy",
        "imbalanced",
        "nuisance50",
        "nuisance100"
    ),
    labels = c(
        "Standard",
        "Irregular visits",
        "High noise",
        "Imbalanced clusters",
        "50 outcomes\n(5 informative)",
        "100 outcomes\n(5 informative)"
    )
)


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
        nrow = 2
    ) +

    scale_y_continuous(
        limits = c(
            0,
            1
        ),
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
        title = "Clustering performance across longitudinal benchmark scenarios",
        subtitle = paste0(
            "simLongData benchmark; ",
            N_RUNS,
            " simulation runs per scenario; white diamonds indicate means"
        ),
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
            1,
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


ggsave(
    filename = "longTAPIO_simLongData_ARI.pdf",
    plot = p_ari,
    width = 14,
    height = 8
)


ggsave(
    filename = "longTAPIO_simLongData_ARI.png",
    plot = p_ari,
    width = 14,
    height = 8,
    dpi = 400
)


# =============================================================================
# 29. SAVE NUMERICAL RESULTS
# =============================================================================

write.csv(
    results,
    "longTAPIO_simLongData_results.csv",
    row.names = FALSE
)


write.csv(
    summary_table,
    "longTAPIO_simLongData_summary.csv",
    row.names = FALSE
)


if (!is.null(screening_results)) {

    write.csv(
        screening_results,
        "longTAPIO_simLongData_screening_results.csv",
        row.names = FALSE
    )


    write.csv(
        screen_summary,
        "longTAPIO_simLongData_screening_summary.csv",
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