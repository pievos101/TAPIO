# ======================================================================
# longTAPIO vs kml3d
#
# FAIR SAME-DATASET LONGITUDINAL CLUSTERING BENCHMARK
# ======================================================================
#
# BOTH methods cluster exactly the SAME complete dataset.
#
# There is:
#
#   * NO train/test split
#   * NO inductive test prediction
#   * NO artificial nearest-centroid extension of kml3d
#
# We evaluate the clustering partitions directly against the known
# simulation labels using:
#
#   * Adjusted Rand Index (ARI)
#   * Normalized Mutual Information (NMI)
#
#
# EXPERIMENT 1
# ------------
# Dimensionality / sample-size experiment
#
#   n = 80, 120, 200
#   p = 10, 25, 50, 100
#
# Approximately 40% of variables are informative at every p.
#
#
# EXPERIMENT 2
# ------------
# Nuisance-feature experiment
#
#   n = 120
#   informative variables = 8 (fixed)
#   p = 8, 16, 32, 64, 100
#
# Increasing p therefore ONLY adds nuisance variables.
#
#
# BOTH experiments:
#
#   A. balanced clusters
#   B. imbalanced clusters
#
# Balanced:
#   0.25 / 0.25 / 0.25 / 0.25
#
# Imbalanced:
#   0.55 / 0.25 / 0.125 / 0.075
#
#
# IMPORTANT:
#
# Source your current implementation of:
#
#   longTAPIO_inductive()
#
# before running this script.
#
# ======================================================================


# ======================================================================
# PACKAGES
# ======================================================================

packages <- c(
    "kml3d",
    "aricode"
)

for(pkg in packages) {

    if(!requireNamespace(pkg, quietly = TRUE)) {

        install.packages(pkg)
    }
}

library(kml3d)
library(aricode)


# ======================================================================
# SOURCE YOUR longTAPIO IMPLEMENTATION HERE IF NECESSARY
# ======================================================================

# source("longTAPIO_inductive.R")


# ======================================================================
# GLOBAL CONFIGURATION
# ======================================================================

N_RUNS <- 20

K <- 4

N_VISITS <- 10

BASE_SEED <- 42


# ======================================================================
# longTAPIO CONFIGURATION
# ======================================================================

N_TREES <- 500

LEVELS <- 4

PCA_SELECTION <- "random_weighted"


# ======================================================================
# kml3d CONFIGURATION
# ======================================================================

KML_REDRAWINGS <- 20


# ======================================================================
# CLUSTER PROPORTIONS
# ======================================================================

CLUSTER_STRUCTURES <- list(

    BALANCED = c(
        0.25,
        0.25,
        0.25,
        0.25
    ),

    IMBALANCED = c(
        0.55,
        0.25,
        0.125,
        0.075
    )
)


# ======================================================================
# EXPERIMENT 1
#
# n x p factorial
#
# Constant informative fraction = 40%
# ======================================================================

EXP1_N <- c(
    80,
    120,
    200
)

EXP1_P <- c(
    10,
    25,
    50,
    100
)

SIGNAL_FRACTION <- 0.40


# ======================================================================
# EXPERIMENT 2
#
# Fixed signal dimensionality.
#
# Increasing p adds nuisance variables only.
# ======================================================================

EXP2_N <- 120

EXP2_SIGNAL <- 8

EXP2_P <- c(
    8,
    16,
    32,
    64,
    100
)


# ======================================================================
# LONGITUDINAL PHENOTYPE FUNCTIONS
# ======================================================================
#
# Four different trajectory shapes.
#
# Cluster 1:
#   stable / weak progression
#
# Cluster 2:
#   progressive
#
# Cluster 3:
#   oscillatory
#
# Cluster 4:
#   transient flare
#
# ======================================================================

trajectory_function <- function(
    phenotype,
    t
) {

    if(phenotype == 1) {

        return(
            0.20 +
            0.025 * t
        )
    }


    if(phenotype == 2) {

        return(
            0.10 +
            0.075 * t
        )
    }


    if(phenotype == 3) {

        return(
            0.45 +
            0.28 *
            sin(
                0.85 * t -
                0.60
            )
        )
    }


    if(phenotype == 4) {

        return(
            0.20 +
            0.65 *
            exp(
                -0.5 *
                (
                    (
                        t -
                        5.0
                    ) /
                    1.35
                )^2
            )
        )
    }


    stop("Unknown phenotype.")
}


# ======================================================================
# EXACT CLUSTER SIZES
# ======================================================================

make_cluster_sizes <- function(
    n,
    proportions
) {

    raw_sizes <-
        n *
        proportions


    sizes <-
        floor(
            raw_sizes
        )


    remainder <-
        n -
        sum(
            sizes
        )


    if(remainder > 0) {

        fractional <-
            raw_sizes -
            sizes


        idx <-
            order(
                fractional,
                decreasing = TRUE
            )


        for(i in seq_len(remainder)) {

            sizes[
                idx[i]
            ] <-
                sizes[
                    idx[i]
                ] +
                1
        }
    }


    sizes
}


# ======================================================================
# LONGITUDINAL DATA GENERATOR
# ======================================================================
#
# X:
#
#   subject x visit x variable
#
#
# The first n_signal variables contain phenotype information.
#
# Remaining variables contain longitudinal structure but NO phenotype
# information.
#
#
# Subject-level heterogeneity:
#
#   * random intercept
#   * random trajectory scaling
#   * temporal phase variation
#   * AR(1) temporal noise
#
# ======================================================================

simulate_longitudinal_data <- function(
    n,
    p,
    n_signal,
    proportions,
    n_visits = 10,
    seed = 1
) {

    set.seed(seed)


    # ==================================================================
    # CHECKS
    # ==================================================================

    if(n_signal > p) {

        stop(
            "n_signal cannot exceed p."
        )
    }


    if(length(proportions) != K) {

        stop(
            "proportions must have length K."
        )
    }


    # ==================================================================
    # TRUE CLUSTERS
    # ==================================================================

    cluster_sizes <-

        make_cluster_sizes(
            n = n,
            proportions = proportions
        )


    y <- rep(
        seq_len(K),
        times = cluster_sizes
    )


    y <- sample(y)


    # ==================================================================
    # TIME
    # ==================================================================

    time <- seq(
        0,
        10,
        length.out = n_visits
    )


    # ==================================================================
    # DATA ARRAY
    # ==================================================================

    X <- array(
        0,
        dim = c(
            n,
            n_visits,
            p
        )
    )


    # ==================================================================
    # VARIABLE-SPECIFIC SIGNAL LOADINGS
    #
    # Generated once for the dataset.
    #
    # The ranges deliberately avoid near-zero signal variables.
    # ==================================================================

    signal_loading <-
        runif(
            n_signal,
            min = 0.75,
            max = 1.25
        )


    signal_baseline <-
        rnorm(
            n_signal,
            mean = 0,
            sd = 0.20
        )


    # ==================================================================
    # GENERATE SUBJECTS
    # ==================================================================

    for(i in seq_len(n)) {

        phenotype <- y[i]


        # --------------------------------------------------------------
        # Subject heterogeneity
        # --------------------------------------------------------------

        subject_intercept <-
            rnorm(
                1,
                mean = 0,
                sd = 0.12
            )


        subject_scale <-
            rlnorm(
                1,
                meanlog = 0,
                sdlog = 0.10
            )


        phase_shift <-
            rnorm(
                1,
                mean = 0,
                sd = 0.20
            )


        # --------------------------------------------------------------
        # Shared AR(1) temporal disturbance
        # --------------------------------------------------------------

        ar_noise <- numeric(n_visits)


        ar_noise[1] <-
            rnorm(
                1,
                mean = 0,
                sd = 0.10
            )


        if(n_visits > 1) {

            for(v in 2:n_visits) {

                ar_noise[v] <-

                    0.65 *
                    ar_noise[v - 1] +

                    rnorm(
                        1,
                        mean = 0,
                        sd = 0.08
                    )
            }
        }


        # ==============================================================
        # INFORMATIVE VARIABLES
        # ==============================================================

        for(j in seq_len(n_signal)) {

            for(v in seq_len(n_visits)) {

                tt <-
                    time[v] +
                    phase_shift


                latent <-

                    trajectory_function(
                        phenotype = phenotype,
                        t = tt
                    )


                X[
                    i,
                    v,
                    j
                ] <-

                    signal_baseline[j] +

                    subject_intercept +

                    signal_loading[j] *
                    subject_scale *
                    latent +

                    0.30 *
                    ar_noise[v] +

                    rnorm(
                        1,
                        mean = 0,
                        sd = 0.12
                    )
            }
        }


        # ==============================================================
        # NUISANCE VARIABLES
        #
        # These contain longitudinal structure but are independent of
        # phenotype.
        # ==============================================================

        if(p > n_signal) {

            for(j in (n_signal + 1):p) {

                nuisance_intercept <-
                    rnorm(
                        1,
                        mean = 0,
                        sd = 0.30
                    )


                nuisance_slope <-
                    rnorm(
                        1,
                        mean = 0,
                        sd = 0.025
                    )


                nuisance_phase <-
                    runif(
                        1,
                        min = 0,
                        max = 2 * pi
                    )


                nuisance_amplitude <-
                    runif(
                        1,
                        min = 0.10,
                        max = 0.20
                    )


                nuisance_ar <- numeric(n_visits)


                nuisance_ar[1] <-
                    rnorm(
                        1,
                        sd = 0.12
                    )


                if(n_visits > 1) {

                    for(v in 2:n_visits) {

                        nuisance_ar[v] <-

                            0.60 *
                            nuisance_ar[v - 1] +

                            rnorm(
                                1,
                                sd = 0.10
                            )
                    }
                }


                for(v in seq_len(n_visits)) {

                    X[
                        i,
                        v,
                        j
                    ] <-

                        nuisance_intercept +

                        nuisance_slope *
                        time[v] +

                        nuisance_amplitude *
                        sin(
                            0.50 *
                            time[v] +
                            nuisance_phase
                        ) +

                        nuisance_ar[v] +

                        rnorm(
                            1,
                            mean = 0,
                            sd = 0.15
                        )
                }
            }
        }
    }


    # ==================================================================
    # NAMES
    # ==================================================================

    dimnames(X) <- list(

        patient =
            paste0(
                "P",
                seq_len(n)
            ),

        visit =
            paste0(
                "V",
                seq_len(n_visits)
            ),

        variable =
            paste0(
                "Y",
                seq_len(p)
            )
    )


    list(
        X = X,
        y = y,
        time = time,
        cluster_sizes = cluster_sizes,
        n_signal = n_signal,
        n_noise = p - n_signal
    )
}


# ======================================================================
# ARRAY -> longTAPIO FORMAT
# ======================================================================
#
# longTAPIO receives:
#
# DATA:
#   one row per subject / visit
#
# user_id:
#   patient ID for every row
#
# ======================================================================

array_to_longtapio <- function(
    X
) {

    n <- dim(X)[1]

    V <- dim(X)[2]

    p <- dim(X)[3]


    DATA <- matrix(
        NA_real_,
        nrow = n * V,
        ncol = p
    )


    user_id <- rep(
        seq_len(n),
        each = V
    )


    row_counter <- 1L


    for(i in seq_len(n)) {

        for(v in seq_len(V)) {

            DATA[
                row_counter,
            ] <-
                X[
                    i,
                    v,
                ]


            row_counter <-
                row_counter +
                1L
        }
    }


    colnames(DATA) <-
        paste0(
            "Y",
            seq_len(p)
        )


    list(
        DATA = as.data.frame(DATA),
        user_id = user_id
    )
}


# ======================================================================
# EXTRACT longTAPIO TRAINING PARTITION
# ======================================================================
#
# We want the partition produced during fitting.
#
# NO prediction is performed here.
#
# Because different versions of your implementation may use slightly
# different object names, several likely names are checked.
#
# ======================================================================

extract_longtapio_training_cluster <- function(
    model,
    n_subjects
) {

    candidates <- c(
        "cluster",
        "clusters",
        "clustering",
        "partition",
        "labels",
        "train_cluster",
        "train_clusters",
        "training_cluster",
        "training_clusters",
        "final_cluster",
        "final_clusters"
    )


    # ==================================================================
    # LIST OBJECT
    # ==================================================================

    if(is.list(model)) {

        for(nm in candidates) {

            if(!is.null(model[[nm]])) {

                x <- model[[nm]]


                if(
                    is.atomic(x) &&
                    length(x) == n_subjects
                ) {

                    return(
                        as.integer(
                            factor(x)
                        )
                    )
                }
            }
        }
    }


    # ==================================================================
    # S3/S4 POSSIBILITY
    # ==================================================================

    if(isS4(model)) {

        slot_names <- slotNames(model)


        for(nm in candidates) {

            if(nm %in% slot_names) {

                x <- slot(model, nm)


                if(
                    is.atomic(x) &&
                    length(x) == n_subjects
                ) {

                    return(
                        as.integer(
                            factor(x)
                        )
                    )
                }
            }
        }
    }


    # ==================================================================
    # FAILURE MESSAGE
    # ==================================================================

    cat("\n")
    cat("Could not automatically identify longTAPIO training clusters.\n")
    cat("\n")


    if(is.list(model)) {

        cat("Available model components:\n")

        print(
            names(model)
        )
    }


    if(isS4(model)) {

        cat("Available S4 slots:\n")

        print(
            slotNames(model)
        )
    }


    stop(
        paste0(
            "Please identify the component containing the ",
            n_subjects,
            " training cluster assignments and add its name ",
            "to 'candidates' in extract_longtapio_training_cluster()."
        )
    )
}


# ======================================================================
# RUN longTAPIO ON COMPLETE DATASET
# ======================================================================

run_longtapio_clustering <- function(
    X,
    k = 4,
    n_trees = 500,
    levels = 4,
    pca_selection = "random_weighted"
) {

    n <- dim(X)[1]

    p <- dim(X)[3]


    # ==================================================================
    # LONG FORMAT
    # ==================================================================

    dat <-
        array_to_longtapio(
            X
        )


    # ==================================================================
    # RANDOM-SUBSPACE SIZE
    #
    # Fixed prespecified rule:
    #
    # mtry = ceiling(sqrt(p))
    #
    # Minimum = 2
    # ==================================================================

    n_features_tree <-

        min(
            p,
            max(
                2,
                ceiling(
                    sqrt(p)
                )
            )
        )


    # ==================================================================
    # FIT
    # ==================================================================

    model <-

        longTAPIO_inductive(

            DATA =
                dat$DATA,

            user_id =
                dat$user_id,

            k =
                k,

            n_features =
                n_features_tree,

            n_trees =
                n_trees,

            levels =
                levels,

            method =
                "ward.D2",

            scale =
                TRUE,

            replace =
                TRUE,

            pca_selection =
                pca_selection
        )


    # ==================================================================
    # EXTRACT TRAINING PARTITION
    # ==================================================================

    cluster <-

        extract_longtapio_training_cluster(

            model =
                model,

            n_subjects =
                n
        )


    # ==================================================================
    # CHECK
    # ==================================================================

    if(length(cluster) != n) {

        stop(
            "longTAPIO cluster vector has wrong length."
        )
    }


    list(
        model = model,
        cluster = cluster,
        mtry = n_features_tree
    )
}


# ======================================================================
# CREATE kml3d OBJECT
# ======================================================================

make_kml3d_object <- function(
    X,
    time
) {

    kml3d::cld3d(
        traj = X,
        time = time
    )
}


# ======================================================================
# RUN kml3d ON COMPLETE DATASET
# ======================================================================

run_kml3d_clustering <- function(
    X,
    time,
    k = 4,
    nb_redrawing = 20
) {

    n <- dim(X)[1]


    # ==================================================================
    # CREATE OBJECT
    # ==================================================================

    cld <-

        make_kml3d_object(
            X = X,
            time = time
        )


    # ==================================================================
    # FIT
    # ==================================================================

    kml3d::kml3d(

        cld,

        nbClusters =
            k,

        nbRedrawing =
            nb_redrawing,

        toPlot =
            "none"
    )


    # ==================================================================
    # EXTRACT STORED PARTITION
    # ==================================================================

    partition_list <-
        cld[
            paste0(
                "c",
                k
            )
        ]


    if(length(partition_list) == 0) {

        stop(
            paste0(
                "kml3d produced no ",
                k,
                "-cluster partition."
            )
        )
    }


    part <-
        partition_list[[1]]


    cluster <-
        as.integer(
            part[
                "clustersAsInteger"
            ]
        )


    # ==================================================================
    # CHECK
    # ==================================================================

    if(length(cluster) != n) {

        stop(
            paste0(
                "kml3d returned ",
                length(cluster),
                " assignments for ",
                n,
                " subjects."
            )
        )
    }


    list(
        object = cld,
        partition = part,
        cluster = cluster
    )
}


# ======================================================================
# METRICS
# ======================================================================

cluster_metrics <- function(
    truth,
    pred
) {

    truth <-
        as.integer(
            factor(truth)
        )


    pred <-
        as.integer(
            factor(pred)
        )


    c(

        ARI =
            aricode::ARI(
                truth,
                pred
            ),

        NMI =
            aricode::NMI(
                truth,
                pred
            )
    )
}


# ======================================================================
# RESULT STORAGE
# ======================================================================

RESULTS <- data.frame()


# ======================================================================
# HELPER: RUN ONE DATASET
# ======================================================================

run_one_dataset <- function(
    sim,
    experiment,
    structure_name,
    condition_name,
    n,
    p,
    n_signal,
    run
) {

    cat(
        sprintf(
            paste0(
                "\n%s | %s | %s | ",
                "run %d/%d\n"
            ),
            experiment,
            structure_name,
            condition_name,
            run,
            N_RUNS
        )
    )


    cat(
        sprintf(
            paste0(
                "N=%d | P=%d | signal=%d | noise=%d | ",
                "clusters=%s\n"
            ),
            n,
            p,
            n_signal,
            p - n_signal,
            paste(
                sim$cluster_sizes,
                collapse = "/"
            )
        )
    )


    # ==================================================================
    # longTAPIO
    # ==================================================================

    set.seed(
        BASE_SEED +
        100000 +
        run
    )


    tapio_time <-
        system.time({

            tapio_fit <-

                run_longtapio_clustering(

                    X =
                        sim$X,

                    k =
                        K,

                    n_trees =
                        N_TREES,

                    levels =
                        LEVELS,

                    pca_selection =
                        PCA_SELECTION
                )
        })


    tapio_metrics <-

        cluster_metrics(

            truth =
                sim$y,

            pred =
                tapio_fit$cluster
        )


    # ==================================================================
    # kml3d
    # ==================================================================

    set.seed(
        BASE_SEED +
        200000 +
        run
    )


    kml_time <-
        system.time({

            kml_fit <-

                run_kml3d_clustering(

                    X =
                        sim$X,

                    time =
                        sim$time,

                    k =
                        K,

                    nb_redrawing =
                        KML_REDRAWINGS
                )
        })


    kml_metrics <-

        cluster_metrics(

            truth =
                sim$y,

            pred =
                kml_fit$cluster
        )


    # ==================================================================
    # PRINT
    # ==================================================================

    cat(
        sprintf(
            paste0(
                "longTAPIO: ARI=%.3f | NMI=%.3f | ",
                "mtry=%d | time=%.2f sec\n"
            ),
            tapio_metrics["ARI"],
            tapio_metrics["NMI"],
            tapio_fit$mtry,
            tapio_time["elapsed"]
        )
    )


    cat(
        sprintf(
            paste0(
                "kml3d    : ARI=%.3f | NMI=%.3f | ",
                "time=%.2f sec\n"
            ),
            kml_metrics["ARI"],
            kml_metrics["NMI"],
            kml_time["elapsed"]
        )
    )


    # ==================================================================
    # RETURN
    # ==================================================================

    rbind(

        data.frame(

            Experiment =
                experiment,

            Structure =
                structure_name,

            Condition =
                condition_name,

            Run =
                run,

            N =
                n,

            P =
                p,

            Signal =
                n_signal,

            Noise =
                p - n_signal,

            SignalFraction =
                n_signal / p,

            Method =
                "longTAPIO",

            Mtry =
                tapio_fit$mtry,

            ARI =
                unname(
                    tapio_metrics["ARI"]
                ),

            NMI =
                unname(
                    tapio_metrics["NMI"]
                ),

            Time =
                unname(
                    tapio_time["elapsed"]
                )
        ),


        data.frame(

            Experiment =
                experiment,

            Structure =
                structure_name,

            Condition =
                condition_name,

            Run =
                run,

            N =
                n,

            P =
                p,

            Signal =
                n_signal,

            Noise =
                p - n_signal,

            SignalFraction =
                n_signal / p,

            Method =
                "kml3d",

            Mtry =
                NA_integer_,

            ARI =
                unname(
                    kml_metrics["ARI"]
                ),

            NMI =
                unname(
                    kml_metrics["NMI"]
                ),

            Time =
                unname(
                    kml_time["elapsed"]
                )
        )
    )
}


# ======================================================================
# EXPERIMENT 1
#
# n x p
#
# 40% informative variables
# ======================================================================

cat("\n")
cat("======================================================================\n")
cat("EXPERIMENT 1: SAMPLE SIZE x DIMENSIONALITY\n")
cat("40% informative variables\n")
cat("======================================================================\n")


exp1_condition_counter <- 0L


for(
    structure_name in
    names(CLUSTER_STRUCTURES)
) {

    proportions <-
        CLUSTER_STRUCTURES[[
                structure_name
            ]
        ]


    for(n in EXP1_N) {

        for(p in EXP1_P) {

            exp1_condition_counter <-
                exp1_condition_counter +
                1L


            n_signal <-

                max(
                    4,
                    round(
                        SIGNAL_FRACTION *
                        p
                    )
                )


            n_signal <-
                min(
                    n_signal,
                    p
                )


            condition_name <-

                paste0(
                    "N",
                    n,
                    "_P",
                    p,
                    "_S",
                    n_signal
                )


            cat("\n\n")
            cat("----------------------------------------------------------------------\n")

            cat(
                "Structure :",
                structure_name,
                "\n"
            )

            cat(
                "Condition :",
                condition_name,
                "\n"
            )

            cat(
                "Signal    :",
                n_signal,
                "/",
                p,
                "\n"
            )

            cat("----------------------------------------------------------------------\n")


            for(run in seq_len(N_RUNS)) {

                simulation_seed <-

                    BASE_SEED +
                    1000000 +
                    10000 *
                    exp1_condition_counter +
                    run


                sim <-

                    simulate_longitudinal_data(

                        n =
                            n,

                        p =
                            p,

                        n_signal =
                            n_signal,

                        proportions =
                            proportions,

                        n_visits =
                            N_VISITS,

                        seed =
                            simulation_seed
                    )


                current <-

                    run_one_dataset(

                        sim =
                            sim,

                        experiment =
                            "DIMENSIONALITY",

                        structure_name =
                            structure_name,

                        condition_name =
                            condition_name,

                        n =
                            n,

                        p =
                            p,

                        n_signal =
                            n_signal,

                        run =
                            run
                    )


                RESULTS <-
                    rbind(
                        RESULTS,
                        current
                    )
            }
        }
    }
}


# ======================================================================
# EXPERIMENT 2
#
# FIXED 8 SIGNAL VARIABLES
#
# Increasing p adds nuisance variables.
# ======================================================================

cat("\n\n")
cat("======================================================================\n")
cat("EXPERIMENT 2: NUISANCE DIMENSIONALITY\n")
cat("8 informative variables fixed\n")
cat("======================================================================\n")


exp2_condition_counter <- 0L


for(
    structure_name in
    names(CLUSTER_STRUCTURES)
) {

    proportions <-
        CLUSTER_STRUCTURES[[
                structure_name
            ]
        ]


    for(p in EXP2_P) {

        exp2_condition_counter <-
            exp2_condition_counter +
            1L


        n_signal <-
            min(
                EXP2_SIGNAL,
                p
            )


        condition_name <-

            paste0(
                "N",
                EXP2_N,
                "_P",
                p,
                "_S",
                n_signal
            )


        cat("\n\n")
        cat("----------------------------------------------------------------------\n")

        cat(
            "Structure :",
            structure_name,
            "\n"
        )

        cat(
            "Condition :",
            condition_name,
            "\n"
        )

        cat(
            "Signal    :",
            n_signal,
            "/",
            p,
            "\n"
        )

        cat("----------------------------------------------------------------------\n")


        for(run in seq_len(N_RUNS)) {

            simulation_seed <-

                BASE_SEED +
                2000000 +
                10000 *
                exp2_condition_counter +
                run


            sim <-

                simulate_longitudinal_data(

                    n =
                        EXP2_N,

                    p =
                        p,

                    n_signal =
                        n_signal,

                    proportions =
                        proportions,

                    n_visits =
                        N_VISITS,

                    seed =
                        simulation_seed
                )


            current <-

                run_one_dataset(

                    sim =
                        sim,

                    experiment =
                        "NUISANCE",

                    structure_name =
                        structure_name,

                    condition_name =
                        condition_name,

                    n =
                        EXP2_N,

                    p =
                        p,

                    n_signal =
                        n_signal,

                    run =
                        run
                )


            RESULTS <-
                rbind(
                    RESULTS,
                    current
                )
        }
    }
}


# ======================================================================
# SUMMARY
# ======================================================================

SUMMARY_MEAN <-

    aggregate(

        cbind(
            ARI,
            NMI,
            Time
        ) ~
            Experiment +
            Structure +
            Condition +
            N +
            P +
            Signal +
            Noise +
            SignalFraction +
            Method,

        data =
            RESULTS,

        FUN =
            mean,

        na.rm =
            TRUE
    )


SUMMARY_SD <-

    aggregate(

        cbind(
            ARI,
            NMI,
            Time
        ) ~
            Experiment +
            Structure +
            Condition +
            N +
            P +
            Signal +
            Noise +
            SignalFraction +
            Method,

        data =
            RESULTS,

        FUN =
            sd,

        na.rm =
            TRUE
    )


SUMMARY <-

    merge(

        SUMMARY_MEAN,

        SUMMARY_SD,

        by =
            c(
                "Experiment",
                "Structure",
                "Condition",
                "N",
                "P",
                "Signal",
                "Noise",
                "SignalFraction",
                "Method"
            ),

        suffixes =
            c(
                "_Mean",
                "_SD"
            )
    )


# ======================================================================
# PRINT SUMMARY
# ======================================================================

cat("\n\n")
cat("======================================================================\n")
cat("FINAL SUMMARY\n")
cat("======================================================================\n")


for(
    experiment_name in
    unique(
        SUMMARY$Experiment
    )
) {

    cat("\n\n")
    cat("######################################################################\n")

    cat(
        experiment_name,
        "\n"
    )

    cat("######################################################################\n")


    for(
        structure_name in
        names(CLUSTER_STRUCTURES)
    ) {

        cat("\n")
        cat("----------------------------------------------------------------------\n")

        cat(
            structure_name,
            "\n"
        )

        cat("----------------------------------------------------------------------\n")


        temp <-

            SUMMARY[
                SUMMARY$Experiment ==
                    experiment_name &
                SUMMARY$Structure ==
                    structure_name,
            ]


        temp <-
            temp[
                order(
                    temp$N,
                    temp$P,
                    temp$Method
                ),
            ]


        print(
            temp[
                ,
                c(
                    "Condition",
                    "N",
                    "P",
                    "Signal",
                    "Noise",
                    "Method",
                    "ARI_Mean",
                    "ARI_SD",
                    "NMI_Mean",
                    "NMI_SD",
                    "Time_Mean"
                )
            ],

            row.names =
                FALSE
        )
    }
}


# ======================================================================
# PAIRED COMPARISONS
# ======================================================================

cat("\n\n")
cat("======================================================================\n")
cat("PAIRED longTAPIO - kml3d COMPARISONS\n")
cat("======================================================================\n")


TEST_RESULTS <- data.frame()


conditions <-

    unique(

        RESULTS[
            ,
            c(
                "Experiment",
                "Structure",
                "Condition",
                "N",
                "P",
                "Signal",
                "Noise"
            )
        ]
    )


for(i in seq_len(nrow(conditions))) {

    cond <-
        conditions[
            i,
        ]


    tapio <-

        RESULTS[
            RESULTS$Experiment ==
                cond$Experiment &
            RESULTS$Structure ==
                cond$Structure &
            RESULTS$Condition ==
                cond$Condition &
            RESULTS$Method ==
                "longTAPIO",
        ]


    kml <-

        RESULTS[
            RESULTS$Experiment ==
                cond$Experiment &
            RESULTS$Structure ==
                cond$Structure &
            RESULTS$Condition ==
                cond$Condition &
            RESULTS$Method ==
                "kml3d",
        ]


    tapio <-
        tapio[
            order(
                tapio$Run
            ),
        ]


    kml <-
        kml[
            order(
                kml$Run
            ),
        ]


    cat("\n")
    cat(
        cond$Experiment,
        "|",
        cond$Structure,
        "|",
        cond$Condition,
        "\n"
    )


    for(metric in c("ARI", "NMI")) {

        a <-
            tapio[[
                    metric
                ]
            ]


        b <-
            kml[[
                    metric
                ]
            ]


        delta <-
            a -
            b


        wt <-

            wilcox.test(

                a,
                b,

                paired =
                    TRUE,

                exact =
                    FALSE
            )


        cat(
            sprintf(
                paste0(
                    "%-4s delta = %+.3f +/- %.3f | ",
                    "p = %.5f\n"
                ),
                metric,
                mean(delta),
                sd(delta),
                wt$p.value
            )
        )


        TEST_RESULTS <-

            rbind(

                TEST_RESULTS,

                data.frame(

                    Experiment =
                        cond$Experiment,

                    Structure =
                        cond$Structure,

                    Condition =
                        cond$Condition,

                    N =
                        cond$N,

                    P =
                        cond$P,

                    Signal =
                        cond$Signal,

                    Noise =
                        cond$Noise,

                    Metric =
                        metric,

                    DeltaMean =
                        mean(delta),

                    DeltaSD =
                        sd(delta),

                    PValue =
                        wt$p.value
                )
            )
    }
}


# ======================================================================
# EXPERIMENT 1:
# DIMENSIONALITY ROBUSTNESS TABLE
# ======================================================================

cat("\n\n")
cat("======================================================================\n")
cat("EXPERIMENT 1: ARI BY N AND P\n")
cat("======================================================================\n")


for(
    structure_name in
    names(CLUSTER_STRUCTURES)
) {

    cat("\n")
    cat(
        structure_name,
        "\n"
    )


    temp <-

        SUMMARY[
            SUMMARY$Experiment ==
                "DIMENSIONALITY" &
            SUMMARY$Structure ==
                structure_name,
        ]


    for(method_name in c(
        "longTAPIO",
        "kml3d"
    )) {

        cat(
            "\n",
            method_name,
            "\n",
            sep = ""
        )


        temp_method <-

            temp[
                temp$Method ==
                    method_name,
            ]


        ari_matrix <- matrix(

            NA_real_,

            nrow =
                length(EXP1_N),

            ncol =
                length(EXP1_P),

            dimnames =
                list(
                    paste0(
                        "N=",
                        EXP1_N
                    ),
                    paste0(
                        "P=",
                        EXP1_P
                    )
                )
        )


        for(ii in seq_along(EXP1_N)) {

            for(jj in seq_along(EXP1_P)) {

                value <-

                    temp_method$ARI_Mean[
                        temp_method$N ==
                            EXP1_N[ii] &
                        temp_method$P ==
                            EXP1_P[jj]
                    ]


                if(length(value) == 1) {

                    ari_matrix[
                        ii,
                        jj
                    ] <-
                        value
                }
            }
        }


        print(
            round(
                ari_matrix,
                3
            )
        )
    }
}


# ======================================================================
# EXPERIMENT 2:
# NUISANCE ROBUSTNESS
# ======================================================================

cat("\n\n")
cat("======================================================================\n")
cat("EXPERIMENT 2: NUISANCE FEATURE ROBUSTNESS\n")
cat("======================================================================\n")


for(
    structure_name in
    names(CLUSTER_STRUCTURES)
) {

    cat("\n")
    cat("----------------------------------------------------------------------\n")

    cat(
        structure_name,
        "\n"
    )

    cat("----------------------------------------------------------------------\n")


    temp <-

        SUMMARY[
            SUMMARY$Experiment ==
                "NUISANCE" &
            SUMMARY$Structure ==
                structure_name,
        ]


    temp <-
        temp[
            order(
                temp$P,
                temp$Method
            ),
        ]


    print(

        temp[
            ,
            c(
                "P",
                "Signal",
                "Noise",
                "Method",
                "ARI_Mean",
                "ARI_SD",
                "NMI_Mean",
                "NMI_SD"
            )
        ],

        row.names =
            FALSE
    )
}


# ======================================================================
# SIMPLE ROBUSTNESS SLOPE FOR NUISANCE EXPERIMENT
#
# Descriptive only:
#
# ARI ~ number of nuisance variables
# ======================================================================

cat("\n\n")
cat("======================================================================\n")
cat("NUISANCE ROBUSTNESS SLOPES\n")
cat("ARI ~ number of nuisance variables\n")
cat("======================================================================\n")


for(
    structure_name in
    names(CLUSTER_STRUCTURES)
) {

    cat(
        "\n",
        structure_name,
        "\n",
        sep = ""
    )


    for(
        method_name in
        c(
            "longTAPIO",
            "kml3d"
        )
    ) {

        temp <-

            SUMMARY[
                SUMMARY$Experiment ==
                    "NUISANCE" &
                SUMMARY$Structure ==
                    structure_name &
                SUMMARY$Method ==
                    method_name,
            ]


        fit <-

            lm(
                ARI_Mean ~ Noise,
                data = temp
            )


        slope <-
            coef(fit)[
                "Noise"
            ]


        cat(
            sprintf(
                "%-10s slope = %+.5f ARI per nuisance variable\n",
                method_name,
                slope
            )
        )
    }
}


# ======================================================================
# PLOTS
# ======================================================================

old_par <-
    par(
        no.readonly = TRUE
    )


# ======================================================================
# PLOT 1:
# NUISANCE EXPERIMENT
# ======================================================================

par(
    mfrow = c(
        1,
        2
    )
)


for(
    structure_name in
    names(CLUSTER_STRUCTURES)
) {

    temp <-

        SUMMARY[
            SUMMARY$Experiment ==
                "NUISANCE" &
            SUMMARY$Structure ==
                structure_name,
        ]


    tapio <-

        temp[
            temp$Method ==
                "longTAPIO",
        ]


    kml <-

        temp[
            temp$Method ==
                "kml3d",
        ]


    tapio <-
        tapio[
            order(
                tapio$Noise
            ),
        ]


    kml <-
        kml[
            order(
                kml$Noise
            ),
        ]


    ylim <-
        range(
            c(
                tapio$ARI_Mean -
                    tapio$ARI_SD,

                tapio$ARI_Mean +
                    tapio$ARI_SD,

                kml$ARI_Mean -
                    kml$ARI_SD,

                kml$ARI_Mean +
                    kml$ARI_SD
            ),
            finite = TRUE
        )


    plot(
        tapio$Noise,
        tapio$ARI_Mean,
        type = "b",
        pch = 16,
        ylim = ylim,
        xlab = "Number of nuisance variables",
        ylab = "Mean ARI",
        main = structure_name
    )


    lines(
        kml$Noise,
        kml$ARI_Mean,
        type = "b",
        pch = 17
    )


    arrows(
        tapio$Noise,
        tapio$ARI_Mean -
            tapio$ARI_SD,
        tapio$Noise,
        tapio$ARI_Mean +
            tapio$ARI_SD,
        angle = 90,
        code = 3,
        length = 0.04
    )


    arrows(
        kml$Noise,
        kml$ARI_Mean -
            kml$ARI_SD,
        kml$Noise,
        kml$ARI_Mean +
            kml$ARI_SD,
        angle = 90,
        code = 3,
        length = 0.04
    )


    legend(
        "bottomleft",
        legend = c(
            "longTAPIO",
            "kml3d"
        ),
        pch = c(
            16,
            17
        ),
        lty = 1,
        bty = "n"
    )
}


par(old_par)


# ======================================================================
# SAVE RESULTS
# ======================================================================

write.csv(
    RESULTS,
    "longTAPIO_vs_kml3d_all_results.csv",
    row.names = FALSE
)


write.csv(
    SUMMARY,
    "longTAPIO_vs_kml3d_summary.csv",
    row.names = FALSE
)


write.csv(
    TEST_RESULTS,
    "longTAPIO_vs_kml3d_paired_tests.csv",
    row.names = FALSE
)


cat("\n\n")
cat("======================================================================\n")
cat("BENCHMARK COMPLETED\n")
cat("======================================================================\n")

cat("\nFiles written:\n")

cat(
    "  longTAPIO_vs_kml3d_all_results.csv\n"
)

cat(
    "  longTAPIO_vs_kml3d_summary.csv\n"
)

cat(
    "  longTAPIO_vs_kml3d_paired_tests.csv\n"
)