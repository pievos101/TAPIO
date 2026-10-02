# =============================================================================
# LONGTAPIO TEMPORAL REPRESENTATION BENCHMARK
#
# Question:
# Does the simple overlapping-window representation lose information relative
# to a smoother continuous-time representation when visits are irregular?
#
# METHODS
# -------
# 1. longTAPIO-window
#       irregular observations
#       -> overlapping temporal windows
#       -> common temporal grid
#       -> longTAPIO
#
# 2. longTAPIO-spline
#       irregular observations
#       -> cubic B-spline regression
#       -> common temporal grid
#       -> longTAPIO
#
# 3. clusterMLD
#       irregular observations + exact observation times
#       -> LongDataCluster()
#
#
# IMPORTANT DESIGN PRINCIPLE
# --------------------------
# longTAPIO-window and longTAPIO-spline use EXACTLY the same:
#
#   * patients
#   * biomarkers
#   * number of output time points
#   * longTAPIO parameters
#   * number of trees
#   * feature sampling
#   * PCA selection: random_weighted
#
# Only the temporal representation differs.
#
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

library(aricode)
library(clusterMLD)
library(splines)
library(ggplot2)


if (!exists("longTAPIO_inductive")) {
    stop("Please source/load longTAPIO_inductive() first.")
}


# =============================================================================
# 1. GLOBAL SETTINGS
# =============================================================================

N_RUNS <- 30

N_TOTAL <- 200

K <- 4

P <- 5

TIME_MIN <- 0

TIME_MAX <- 10


# -----------------------------------------------------------------------------
# longTAPIO
# -----------------------------------------------------------------------------

N_TREES <- 500

LEVELS <- 4

PCA_SELECTION <- "random_weighted"


# -----------------------------------------------------------------------------
# Common output representation
# -----------------------------------------------------------------------------

N_GRID <- 10


# -----------------------------------------------------------------------------
# Sliding windows
# -----------------------------------------------------------------------------

N_BINS <- 10

WINDOW_OVERLAP <- 0.50


# -----------------------------------------------------------------------------
# Cubic B-splines
#
# df = 4 is deliberately conservative.
#
# We do NOT want an extremely flexible smoother that simply interpolates noise.
#
# For very sparse patients, spline complexity is automatically reduced.
# -----------------------------------------------------------------------------

SPLINE_DF <- 4

SPLINE_DEGREE <- 3


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


METHODS <- c(
    "longTAPIO-window",
    "longTAPIO-spline",
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
# ONLY the observation process changes between scenarios.
#
# regular:
#       10 fixed visits
#
# mild:
#       10 approximately regular visits with jitter
#
# moderate:
#       7-10 irregular visits
#
# strong:
#       5-8 irregular visits
#
# very_strong:
#       3-6 irregular visits
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
            1
        )


        ti <- sort(
            runif(
                n_i,
                TIME_MIN,
                TIME_MAX
            )
        )


    } else if (scenario == "strong") {

        n_i <- sample(
            5:8,
            1
        )


        ti <- sort(
            runif(
                n_i,
                TIME_MIN,
                TIME_MAX
            )
        )


    } else {

        n_i <- sample(
            3:6,
            1
        )


        ti <- sort(
            runif(
                n_i,
                TIME_MIN,
                TIME_MAX
            )
        )
    }


    ti
}


# =============================================================================
# 5. SIMULATE LONGITUDINAL DATA
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
    # Balanced clusters
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
    # Subject-level heterogeneity
    # -------------------------------------------------------------------------

    subject_intercept <- rnorm(
        N_TOTAL,
        0,
        0.13
    )


    subject_scale <- rlnorm(
        N_TOTAL,
        meanlog = 0,
        sdlog = 0.07
    )


    subject_phase <- rnorm(
        N_TOTAL,
        0,
        0.14
    )


    # -------------------------------------------------------------------------
    # Biomarker properties
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
    # Generate patients
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


        for (j in seq_len(P)) {

            biomarker_deviation <-
                simulate_biomarker_deviation(
                    length(ti)
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
                    0,
                    MEASUREMENT_SD
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
# 6. WINDOW SPECIFICATION
# =============================================================================

fit_window_spec <- function(
    dat,
    n_bins = N_BINS,
    overlap = WINDOW_OVERLAP
) {

    features <- grep(
        "^y[0-9]+$",
        names(dat),
        value = TRUE
    )


    total_range <-
        TIME_MAX -
        TIME_MIN


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
        TIME_MIN,
        TIME_MAX - window_size,
        by = step
    )


    final_start <-
        TIME_MAX -
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


    list(
        features = features,
        starts = starts,
        centers = centers,
        window_size = window_size,
        global_means = global_means
    )
}


# =============================================================================
# 7. WINDOW REPRESENTATION
# =============================================================================

apply_window_spec <- function(
    dat,
    spec
) {

    ids <- sort(
        unique(dat$id)
    )


    features <- spec$features

    starts <- spec$starts

    centers <- spec$centers

    window_size <- spec$window_size


    N <- length(ids)

    V <- length(starts)

    P_local <- length(features)


    X <- array(
        NA_real_,
        dim = c(
            N,
            V,
            P_local
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

            values <- rep(
                NA_real_,
                V
            )


            for (v in seq_len(V)) {

                lo <- starts[v]

                hi <-
                    lo +
                    window_size


                idx <-
                    d_i$time >= lo &
                    d_i$time <= hi


                if (any(idx)) {

                    values[v] <- mean(
                        d_i[[features[j]]][idx],
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
                    spec$global_means[j]
            }


            X[ii, , j] <- values
        }
    }


    if (any(!is.finite(X))) {

        stop(
            "Non-finite values in window representation."
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
# 8. SPLINE REPRESENTATION
# =============================================================================
#
# Each patient's biomarker is independently represented using a cubic
# regression spline.
#
# The fitted curve is evaluated on a common N_GRID-point temporal grid.
#
# IMPORTANT:
#
# * no cluster labels
# * no other patients
# * no test information
#
# are involved in fitting an individual patient's spline.
#
# Sparse trajectories automatically use simpler representations.
#
# =============================================================================

spline_to_array <- function(
    dat,
    n_grid = N_GRID,
    spline_df = SPLINE_DF
) {

    ids <- sort(
        unique(dat$id)
    )


    features <- grep(
        "^y[0-9]+$",
        names(dat),
        value = TRUE
    )


    grid <- seq(
        TIME_MIN,
        TIME_MAX,
        length.out = n_grid
    )


    N <- length(ids)

    P_local <- length(features)


    X <- array(
        NA_real_,
        dim = c(
            N,
            n_grid,
            P_local
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


            tt <- d_i$time

            yy <- d_i[[feature]]


            ok <-
                is.finite(tt) &
                is.finite(yy)


            tt <- tt[ok]

            yy <- yy[ok]


            # -----------------------------------------------------------------
            # Remove duplicate times if necessary
            # -----------------------------------------------------------------

            if (length(tt) > 0) {

                tmp <- aggregate(
                    yy,
                    by = list(
                        time = tt
                    ),
                    FUN = mean
                )


                tt <- tmp$time

                yy <- tmp$x
            }


            n_obs <- length(tt)


            # -----------------------------------------------------------------
            # No observations
            # -----------------------------------------------------------------

            if (n_obs == 0) {

                pred <- rep(
                    global_means[j],
                    n_grid
                )


            # -----------------------------------------------------------------
            # One observation
            # -----------------------------------------------------------------

            } else if (n_obs == 1) {

                pred <- rep(
                    yy[1],
                    n_grid
                )


            # -----------------------------------------------------------------
            # Two observations:
            # linear interpolation / extrapolation
            # -----------------------------------------------------------------

            } else if (n_obs == 2) {

                fit <- lm(
                    yy ~ tt
                )


                pred <- predict(
                    fit,
                    newdata = data.frame(
                        tt = grid
                    )
                )


            # -----------------------------------------------------------------
            # Three observations:
            # quadratic polynomial
            #
            # A cubic spline is not identifiable from only 3 observations.
            # -----------------------------------------------------------------

            } else if (n_obs == 3) {

                fit <- lm(
                    yy ~ poly(
                        tt,
                        degree = 2,
                        raw = TRUE
                    )
                )


                pred <- predict(
                    fit,
                    newdata = data.frame(
                        tt = grid
                    )
                )


            # -----------------------------------------------------------------
            # >= 4 observations:
            # cubic B-spline regression
            # -----------------------------------------------------------------

            } else {

                df_use <- min(
                    spline_df,
                    n_obs - 1
                )


                df_use <- max(
                    df_use,
                    3
                )


                spline_fit <- try(
                    lm(
                        yy ~ splines::bs(
                            tt,
                            df = df_use,
                            degree = min(
                                SPLINE_DEGREE,
                                df_use
                            ),
                            Boundary.knots = c(
                                TIME_MIN,
                                TIME_MAX
                            )
                        )
                    ),
                    silent = TRUE
                )


                if (!inherits(
                    spline_fit,
                    "try-error"
                )) {

                    pred <- try(
                        predict(
                            spline_fit,
                            newdata = data.frame(
                                tt = grid
                            )
                        ),
                        silent = TRUE
                    )


                } else {

                    pred <- structure(
                        "spline failure",
                        class = "try-error"
                    )
                }


                # -------------------------------------------------------------
                # Safe fallback
                # -------------------------------------------------------------

                if (
                    inherits(
                        pred,
                        "try-error"
                    ) ||
                    any(!is.finite(pred))
                ) {

                    pred <- approx(
                        x = tt,
                        y = yy,
                        xout = grid,
                        rule = 2
                    )$y
                }
            }


            # -----------------------------------------------------------------
            # Final safety
            # -----------------------------------------------------------------

            if (any(!is.finite(pred))) {

                pred <- approx(
                    x = tt,
                    y = yy,
                    xout = grid,
                    rule = 2
                )$y
            }


            X[ii, , j] <- pred
        }
    }


    if (any(!is.finite(X))) {

        stop(
            "Non-finite values in spline representation."
        )
    }


    list(
        X = X,
        ids = ids,
        time = grid,
        features = features
    )
}


# =============================================================================
# 9. ARRAY -> LONGTAPIO
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
# 10. EXTRACT LONGTAPIO PARTITION
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
#
# SAME FUNCTION for windows and splines.
#
# Therefore the ONLY methodological difference is X.
#
# =============================================================================

fit_longTAPIO <- function(
    representation,
    seed
) {

    set.seed(seed)


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
        pca_selection = PCA_SELECTION
    )


    cluster <- extract_longTAPIO_cluster(
        fit,
        N
    )


    list(
        fit = fit,
        cluster = cluster,
        m = m
    )
}


# =============================================================================
# 12. CLUSTERMLD
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
            "clusterMLD failed to assign all subjects."
        )
    }


    as.integer(labels)
}


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
        fixed_k,
        ids
    )
}


# =============================================================================
# 13. METRICS
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
# 14. RESULT HELPER
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
# 15. RUN ONE REPLICATE
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


    # -------------------------------------------------------------------------
    # IMPORTANT
    #
    # Window and spline longTAPIO get the SAME random seed.
    #
    # This makes feature/tree randomization paired as closely as possible.
    # -------------------------------------------------------------------------

    model_seed <-
        100000 +
        scenario_number * 1000 +
        run


    # =========================================================================
    # DATA
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
            rep("=", 105),
            collapse = ""
        ),
        "\n",
        toupper(scenario),
        " | RUN ",
        run,
        "/",
        N_RUNS,
        " | mean visits = ",
        sprintf(
            "%.2f",
            mean(sim$n_visits)
        ),
        " | range = ",
        min(sim$n_visits),
        "-",
        max(sim$n_visits),
        "\n",
        paste(
            rep("=", 105),
            collapse = ""
        ),
        "\n",
        sep = ""
    )


    output <- list()


    # =========================================================================
    # REPRESENTATIONS
    # =========================================================================


    # -------------------------------------------------------------------------
    # Window representation
    # -------------------------------------------------------------------------

    window_spec <- fit_window_spec(
        dat
    )


    window_rep <- apply_window_spec(
        dat,
        window_spec
    )


    # -------------------------------------------------------------------------
    # Spline representation
    # -------------------------------------------------------------------------

    spline_rep <- spline_to_array(
        dat,
        n_grid = N_GRID,
        spline_df = SPLINE_DF
    )


    # =========================================================================
    # 1. LONGTAPIO WINDOW
    # =========================================================================

    cat(
        "\n1. longTAPIO-window\n"
    )


    t0 <- proc.time()[3]


    fit_window <- try(
        fit_longTAPIO(
            representation = window_rep,
            seed = model_seed
        ),
        silent = TRUE
    )


    sec_window <-
        proc.time()[3] -
        t0


    if (!inherits(
        fit_window,
        "try-error"
    )) {

        score <- get_scores(
            truth,
            fit_window$cluster
        )


        cat(
            sprintf(
                "   ARI=%.3f | NMI=%.3f | %.2f sec\n",
                score["ARI"],
                score["NMI"],
                sec_window
            )
        )


        output[[length(output) + 1L]] <-
            make_result(
                scenario,
                run,
                "longTAPIO-window",
                truth,
                fit_window$cluster,
                sec_window
            )


    } else {

        cat(
            "   FAILED\n"
        )
    }


    # =========================================================================
    # 2. LONGTAPIO SPLINE
    # =========================================================================

    cat(
        "\n2. longTAPIO-spline\n"
    )


    t0 <- proc.time()[3]


    fit_spline <- try(
        fit_longTAPIO(
            representation = spline_rep,
            seed = model_seed
        ),
        silent = TRUE
    )


    sec_spline <-
        proc.time()[3] -
        t0


    if (!inherits(
        fit_spline,
        "try-error"
    )) {

        score <- get_scores(
            truth,
            fit_spline$cluster
        )


        cat(
            sprintf(
                "   ARI=%.3f | NMI=%.3f | %.2f sec\n",
                score["ARI"],
                score["NMI"],
                sec_spline
            )
        )


        output[[length(output) + 1L]] <-
            make_result(
                scenario,
                run,
                "longTAPIO-spline",
                truth,
                fit_spline$cluster,
                sec_spline
            )


    } else {

        cat(
            "   FAILED\n"
        )
    }


    # =========================================================================
    # 3. CLUSTERMLD
    # =========================================================================

    cat(
        "\n3. clusterMLD\n"
    )


    t0 <- proc.time()[3]


    fit_mld <- try(
        fit_clusterMLD(
            dat,
            K
        ),
        silent = TRUE
    )


    sec_mld <-
        proc.time()[3] -
        t0


    if (!inherits(
        fit_mld,
        "try-error"
    )) {

        score <- get_scores(
            truth,
            fit_mld
        )


        cat(
            sprintf(
                "   ARI=%.3f | NMI=%.3f | %.2f sec\n",
                score["ARI"],
                score["NMI"],
                sec_mld
            )
        )


        output[[length(output) + 1L]] <-
            make_result(
                scenario,
                run,
                "clusterMLD",
                truth,
                fit_mld,
                sec_mld
            )


    } else {

        cat(
            "   FAILED\n"
        )
    }


    do.call(
        rbind,
        output
    )
}


# =============================================================================
# 16. RUN BENCHMARK
# =============================================================================

cat(
    "\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    "LONGTAPIO TEMPORAL REPRESENTATION BENCHMARK\n",
    "\n",
    "longTAPIO PCA selection : ",
    PCA_SELECTION,
    "\n",
    "Trees                    : ",
    N_TREES,
    "\n",
    "Window overlap           : ",
    WINDOW_OVERLAP,
    "\n",
    "Spline df                : ",
    SPLINE_DF,
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
            scenario,
            run
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
# 17. SUMMARY HELPER
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
# 18. FINAL SUMMARY
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
# 19. PAIRED COMPARISON FUNCTION
# =============================================================================

paired_comparison <- function(
    method1,
    method2
) {

    output <- list()

    cc <- 1L


    for (scenario in SCENARIOS) {

        d1 <- results[
            results$scenario == scenario &
            results$method == method1,
            c(
                "run",
                "ARI",
                "NMI"
            ),
            drop = FALSE
        ]


        d2 <- results[
            results$scenario == scenario &
            results$method == method2,
            c(
                "run",
                "ARI",
                "NMI"
            ),
            drop = FALSE
        ]


        names(d1) <- c(
            "run",
            "ARI_1",
            "NMI_1"
        )


        names(d2) <- c(
            "run",
            "ARI_2",
            "NMI_2"
        )


        d <- merge(
            d1,
            d2,
            by = "run"
        )


        if (nrow(d) == 0) {
            next
        }


        delta_ari <-
            d$ARI_1 -
            d$ARI_2


        delta_nmi <-
            d$NMI_1 -
            d$NMI_2


        # ---------------------------------------------------------------------
        # 95% CI for paired difference
        # ---------------------------------------------------------------------

        se_ari <-
            sd(delta_ari) /
            sqrt(length(delta_ari))


        se_nmi <-
            sd(delta_nmi) /
            sqrt(length(delta_nmi))


        ci_ari <-
            qt(
                0.975,
                df = length(delta_ari) - 1
            ) *
            se_ari


        ci_nmi <-
            qt(
                0.975,
                df = length(delta_nmi) - 1
            ) *
            se_nmi


        output[[cc]] <- data.frame(
            scenario = scenario,
            method1 = method1,
            method2 = method2,
            n = nrow(d),

            mean_method1_ARI = mean(
                d$ARI_1
            ),

            mean_method2_ARI = mean(
                d$ARI_2
            ),

            delta_ARI = mean(
                delta_ari
            ),

            lower_ARI = mean(
                delta_ari
            ) - ci_ari,

            upper_ARI = mean(
                delta_ari
            ) + ci_ari,

            mean_method1_NMI = mean(
                d$NMI_1
            ),

            mean_method2_NMI = mean(
                d$NMI_2
            ),

            delta_NMI = mean(
                delta_nmi
            ),

            lower_NMI = mean(
                delta_nmi
            ) - ci_nmi,

            upper_NMI = mean(
                delta_nmi
            ) + ci_nmi,

            method1_better = sum(
                delta_ari > 0
            ),

            equal = sum(
                abs(delta_ari) < 1e-12
            ),

            method2_better = sum(
                delta_ari < 0
            ),

            stringsAsFactors = FALSE
        )


        cc <- cc + 1L
    }


    do.call(
        rbind,
        output
    )
}


# =============================================================================
# 20. WINDOWS vs SPLINES
# =============================================================================

window_vs_spline <- paired_comparison(
    "longTAPIO-window",
    "longTAPIO-spline"
)


cat(
    "\n\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    "PAIRED COMPARISON: WINDOWS - SPLINES\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    sep = ""
)


print(
    window_vs_spline,
    row.names = FALSE
)


# =============================================================================
# 21. WINDOWS vs CLUSTERMLD
# =============================================================================

window_vs_mld <- paired_comparison(
    "longTAPIO-window",
    "clusterMLD"
)


cat(
    "\n\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    "PAIRED COMPARISON: WINDOWS - clusterMLD\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    sep = ""
)


print(
    window_vs_mld,
    row.names = FALSE
)


# =============================================================================
# 22. SPLINES vs CLUSTERMLD
# =============================================================================

spline_vs_mld <- paired_comparison(
    "longTAPIO-spline",
    "clusterMLD"
)


cat(
    "\n\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    "PAIRED COMPARISON: SPLINES - clusterMLD\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    sep = ""
)


print(
    spline_vs_mld,
    row.names = FALSE
)


# =============================================================================
# 23. SAVE RESULTS
# =============================================================================

write.csv(
    results,
    "longTAPIO_time_representation_results.csv",
    row.names = FALSE
)


write.csv(
    summary_table,
    "longTAPIO_time_representation_summary.csv",
    row.names = FALSE
)


write.csv(
    window_vs_spline,
    "longTAPIO_window_vs_spline.csv",
    row.names = FALSE
)


write.csv(
    window_vs_mld,
    "longTAPIO_window_vs_clusterMLD.csv",
    row.names = FALSE
)


write.csv(
    spline_vs_mld,
    "longTAPIO_spline_vs_clusterMLD.csv",
    row.names = FALSE
)


# =============================================================================
# 24. PLOT DATA
# =============================================================================

plot_data <- results


plot_data$scenario_plot <- factor(
    plot_data$scenario,
    levels = SCENARIOS,
    labels = c(
        "Regular",
        "Mild",
        "Moderate",
        "Strong",
        "Very strong"
    )
)


plot_data$method_plot <- factor(
    plot_data$method,
    levels = METHODS,
    labels = c(
        "longTAPIO - windows",
        "longTAPIO - splines",
        "clusterMLD"
    )
)


# =============================================================================
# 25. ARI PLOT
# =============================================================================

p_ari <- ggplot(
    plot_data,
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
        limits = c(
            0,
            1
        ),
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
        title = "Temporal representation under irregular observation times",
        subtitle = paste0(
            "longTAPIO uses random-weighted PC selection; ",
            "clusterMLD uses exact observation times"
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

        legend.position = "bottom",

        legend.title = element_blank()
    )


print(
    p_ari
)


# =============================================================================
# 26. NMI PLOT
# =============================================================================

p_nmi <- ggplot(
    plot_data,
    aes(
        x = scenario_plot,
        y = NMI,
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
        limits = c(
            0,
            1
        ),
        breaks = seq(
            0,
            1,
            by = 0.2
        )
    ) +

    labs(
        x = "Degree of visit-time irregularity",
        y = "Normalized Mutual Information",
        linetype = "Method",
        shape = "Method",
        title = "Temporal representation under irregular observation times"
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

        legend.position = "bottom",

        legend.title = element_blank()
    )


print(
    p_nmi
)


# =============================================================================
# 27. SAVE FIGURES
# =============================================================================

ggsave(
    filename = "longTAPIO_time_representation_ARI.pdf",
    plot = p_ari,
    width = 8.5,
    height = 5.3
)


ggsave(
    filename = "longTAPIO_time_representation_ARI.png",
    plot = p_ari,
    width = 8.5,
    height = 5.3,
    dpi = 400
)


ggsave(
    filename = "longTAPIO_time_representation_NMI.pdf",
    plot = p_nmi,
    width = 8.5,
    height = 5.3
)


ggsave(
    filename = "longTAPIO_time_representation_NMI.png",
    plot = p_nmi,
    width = 8.5,
    height = 5.3,
    dpi = 400
)


# =============================================================================
# 28. FINAL INTERPRETATION TABLE
# =============================================================================

interpretation_table <- merge(
    window_vs_spline[
        ,
        c(
            "scenario",
            "delta_ARI",
            "lower_ARI",
            "upper_ARI"
        )
    ],
    window_vs_mld[
        ,
        c(
            "scenario",
            "delta_ARI",
            "lower_ARI",
            "upper_ARI"
        )
    ],
    by = "scenario",
    suffixes = c(
        "_window_spline",
        "_window_MLD"
    )
)


cat(
    "\n\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    "KEY REPRESENTATION COMPARISON\n",
    "\n",
    "Positive Window-Spline delta = windows better\n",
    "Negative Window-Spline delta = splines better\n",
    "\n",
    "Positive Window-MLD delta = longTAPIO-window better\n",
    "Negative Window-MLD delta = clusterMLD better\n",
    paste(
        rep("=", 110),
        collapse = ""
    ),
    "\n",
    sep = ""
)


print(
    interpretation_table,
    row.names = FALSE
)


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