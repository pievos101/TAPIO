# ======================================================================
# BENCHMARK:
# ORIGINAL longTAPIO vs INDUCTIVE / PROGRESSIVE longTAPIO
#
# ONLY RANDOM-WEIGHTED PCA VERSION
#
# 20 independent simulation runs
#
# Main questions:
#
# 1. How does full-trajectory inductive longTAPIO compare with the
#    original transductive longTAPIO?
#
# 2. How early can inductive longTAPIO assign an unseen patient
#    to the correct trajectory phenotype?
#
#
# Original:
#
#   ALL patients
#       -> longTAPIO
#       -> evaluate TEST patients
#
#
# Inductive:
#
#   TRAIN patients with complete trajectories
#       -> fit model ONCE
#
#   TEST patient:
#
#       visit 1       -> prediction
#       visits 1:2    -> prediction
#       visits 1:3    -> prediction
#       ...
#       visits 1:10   -> prediction
#
#
# Strict induction:
#
#   * PCA fitted on TRAIN only
#   * feature sampling fixed from TRAIN
#   * PCA component selection fixed from TRAIN
#   * hierarchy fitted on TRAIN only
#   * trajectory prototypes fitted on TRAIN only
#   * final clusters fitted on TRAIN only
#
# ======================================================================


# ======================================================================
# PACKAGES
# ======================================================================

library(TAPIO)
library(clusterMLD)
library(MASS)
library(aricode)
library(reshape)
library(fastcluster)
library(ggplot2)


# ======================================================================
# CONFIGURATION
# ======================================================================

N_ITER <- 20

TRAIN_FRAC <- 0.70

N_VISITS <- 10

SET_K <- 4

SET_LEVELS <- 4

SET_N_FEATURES <- 5

SET_N_TREES <- 500

METHOD <- "ward.D2"

SCALE_PCA <- TRUE

REPLACE_FEATURES <- TRUE

PCA_SELECTION <- "random_weighted"

HORIZONS <- 1:N_VISITS


set.seed(12345)


cat("\n")
cat("======================================================================\n")
cat("longTAPIO INDUCTIVE / PROGRESSIVE BENCHMARK\n")
cat("======================================================================\n")
cat("Runs            :", N_ITER, "\n")
cat("Train fraction  :", TRAIN_FRAC, "\n")
cat("Visits          :", N_VISITS, "\n")
cat("Clusters        :", SET_K, "\n")
cat("Trees           :", SET_N_TREES, "\n")
cat("Features/tree   :", SET_N_FEATURES, "\n")
cat("Levels          :", SET_LEVELS, "\n")
cat("PCA selection   : random_weighted\n")
cat("======================================================================\n\n")


# ======================================================================
# ASSOCIATION MATRIX
# ======================================================================

association_matrix <- function(cl) {

    outer(
        cl,
        cl,
        FUN = function(a, b) {
            as.numeric(a == b)
        }
    )
}


# ======================================================================
# PREPARE LONGITUDINAL DATA
# ======================================================================

prepare_long_data <- function(
    DATA,
    user_id
) {

    DATA <- as.matrix(DATA)

    patients <- sort(
        unique(user_id)
    )


    visit_counts <- table(
        user_id
    )


    if(
        length(
            unique(
                visit_counts
            )
        ) != 1
    ) {

        stop(
            paste0(
                "Current implementation requires ",
                "equal numbers of visits per patient."
            )
        )
    }


    n_visits <- as.integer(
        visit_counts[1]
    )


    # --------------------------------------------------------------
    # Explicit patient-wise ordering
    # --------------------------------------------------------------

    row_order <- unlist(

        lapply(

            patients,

            function(pid) {

                which(
                    user_id == pid
                )
            }
        )
    )


    DATA <- DATA[
        row_order,
        ,
        drop = FALSE
    ]


    user_id <- user_id[
        row_order
    ]


    return(

        list(

            DATA =
                DATA,

            user_id =
                user_id,

            patients =
                patients,

            n_visits =
                n_visits
        )
    )
}


# ======================================================================
# FIT INDUCTIVE longTAPIO
# ======================================================================

longTAPIO_inductive_fit <- function(
    DATA,
    user_id,
    k = 4,
    n_features = 5,
    n_trees = 500,
    levels = 4,
    method = "ward.D2",
    scale = TRUE,
    replace = TRUE,
    pca_selection = "random_weighted"
) {

    # ==============================================================
    # PREPARE TRAINING DATA
    # ==============================================================

    tmp <- prepare_long_data(
        DATA,
        user_id
    )


    DATA <- tmp$DATA

    user_id <- tmp$user_id

    patients <- tmp$patients

    n_visits <- tmp$n_visits


    n_patients <- length(
        patients
    )


    p <- ncol(
        DATA
    )


    # ==============================================================
    # STORAGE
    # ==============================================================

    AFF <- matrix(
        0,
        n_patients,
        n_patients
    )


    TREES <- vector(
        "list",
        n_trees
    )


    # ==============================================================
    # TREE ENSEMBLE
    # ==============================================================

    for(xx in seq_len(
        n_trees
    )) {

        # ----------------------------------------------------------
        # Random feature sampling
        # ----------------------------------------------------------

        ids <- sample(
            seq_len(p),
            n_features,
            replace = replace
        )


        DATA_s <- DATA[
            ,
            ids,
            drop = FALSE
        ]


        # ==========================================================
        # PCA -- TRAINING DATA ONLY
        # ==========================================================

        pca <- prcomp(
            DATA_s,
            center = TRUE,
            scale. = scale
        )


        # ==========================================================
        # RANDOM WEIGHTED PCA COMPONENT
        # ==========================================================

        eig_vals <- pca$sdev^2


        prob <- eig_vals /
            sum(
                eig_vals
            )


        sel <- sample(
            seq_along(
                prob
            ),
            size = 1,
            prob = prob
        )


        # ==========================================================
        # VISIT-LEVEL PCA SCORES
        # ==========================================================

        scores <- pca$x[
            ,
            sel
        ]


        # ==========================================================
        # COMPLETE TRAINING TRAJECTORIES
        #
        # Npatients x Nvisits
        # ==========================================================

        trajectories <- matrix(

            scores,

            nrow =
                n_patients,

            ncol =
                n_visits,

            byrow =
                TRUE
        )


        # ==========================================================
        # HIERARCHICAL CLUSTERING OF TRAJECTORIES
        # ==========================================================

        hc <- fastcluster::hclust(

            dist(
                trajectories
            ),

            method =
                method
        )


        LEVEL_CLUSTERS <- vector(
            "list",
            levels
        )


        LEVEL_CENTROIDS <- vector(
            "list",
            levels
        )


        # ==========================================================
        # LEVELING
        # ==========================================================

        for(yy in seq_len(
            levels
        )) {

            n_clusters_level <-
                yy + 1


            cl <- cutree(

                hc,

                k =
                    n_clusters_level
            )


            LEVEL_CLUSTERS[[yy]] <-
                cl


            # ======================================================
            # TRAJECTORY PROTOTYPES
            # ======================================================

            cluster_ids <- sort(
                unique(
                    cl
                )
            )


            centroids <- matrix(

                NA_real_,

                nrow =
                    length(
                        cluster_ids
                    ),

                ncol =
                    n_visits
            )


            rownames(
                centroids
            ) <- as.character(
                cluster_ids
            )


            for(cc in seq_along(
                cluster_ids
            )) {

                cid <-
                    cluster_ids[cc]


                members <- which(
                    cl == cid
                )


                centroids[
                    cc,
                ] <- colMeans(

                    trajectories[
                        members,
                        ,
                        drop = FALSE
                    ]
                )
            }


            LEVEL_CENTROIDS[[yy]] <-
                centroids


            # ======================================================
            # TRAINING AFFINITY
            # ======================================================

            AFF <- AFF +
                association_matrix(
                    cl
                )
        }


        # ==========================================================
        # STORE FROZEN TREE
        # ==========================================================

        TREES[[xx]] <- list(

            ids =
                ids,

            center =
                pca$center,

            scale =
                pca$scale,

            rotation =
                pca$rotation[
                    ,
                    sel
                ],

            selected_pc =
                sel,

            train_trajectories =
                trajectories,

            level_clusters =
                LEVEL_CLUSTERS,

            level_centroids =
                LEVEL_CENTROIDS
        )
    }


    # ==================================================================
    # NORMALIZED TRAINING AFFINITY
    # ==================================================================

    AFF <- AFF /
        (
            n_trees *
            levels
        )


    DIST <- 1 - AFF

    diag(
        DIST
    ) <- 0


    # ==================================================================
    # FINAL TRAINING CLUSTERING
    # ==================================================================

    hc_final <- fastcluster::hclust(

        as.dist(
            DIST
        ),

        method =
            method
    )


    train_cl <- cutree(

        hc_final,

        k =
            k
    )


    # ==================================================================
    # MODEL
    # ==================================================================

    model <- list(

        trees =
            TREES,

        train_cl =
            train_cl,

        train_patients =
            patients,

        train_affinity =
            AFF,

        train_distance =
            DIST,

        k =
            k,

        levels =
            levels,

        n_trees =
            n_trees,

        n_visits =
            n_visits,

        n_features_total =
            p,

        method =
            method,

        pca_selection =
            pca_selection
    )


    class(
        model
    ) <- "inductiveLongTAPIO"


    return(
        model
    )
}


# ======================================================================
# EXTRACT FIRST H VISITS
# ======================================================================

extract_prefix <- function(
    DATA,
    user_id,
    H
) {

    DATA <- as.matrix(
        DATA
    )


    patients <- sort(
        unique(
            user_id
        )
    )


    rows <- integer(
        0
    )


    for(pid in patients) {

        ids <- which(
            user_id == pid
        )


        if(
            length(ids) <
            H
        ) {

            stop(
                paste0(
                    "Patient ",
                    pid,
                    " has fewer than ",
                    H,
                    " visits."
                )
            )
        }


        # ----------------------------------------------------------
        # FIRST H visits
        # ----------------------------------------------------------

        rows <- c(
            rows,
            ids[
                seq_len(H)
            ]
        )
    }


    X <- DATA[
        rows,
        ,
        drop = FALSE
    ]


    uid <- rep(

        seq_along(
            patients
        ),

        each =
            H
    )


    return(

        list(

            DATA =
                X,

            user_id =
                uid,

            patients =
                patients
        )
    )
}


# ======================================================================
# PROJECT PREFIX THROUGH ONE FROZEN TREE
# ======================================================================

project_prefix_tree <- function(
    tree,
    DATA,
    user_id,
    H
) {

    DATA <- as.matrix(
        DATA
    )


    n_new <- length(
        unique(
            user_id
        )
    )


    # ==============================================================
    # SAME VARIABLES AS TRAINING TREE
    # ==============================================================

    Xs <- DATA[
        ,
        tree$ids,
        drop = FALSE
    ]


    # ==============================================================
    # FROZEN TRAINING PCA CENTER
    # ==============================================================

    Xs <- sweep(
        Xs,
        2,
        tree$center,
        "-"
    )


    # ==============================================================
    # FROZEN TRAINING PCA SCALE
    # ==============================================================

    if(
        !is.null(
            tree$scale
        ) &&
        !identical(
            tree$scale,
            FALSE
        )
    ) {

        Xs <- sweep(
            Xs,
            2,
            tree$scale,
            "/"
        )
    }


    # ==============================================================
    # FROZEN PCA PROJECTION
    # ==============================================================

    scores <- drop(

        Xs %*%
        tree$rotation
    )


    # ==============================================================
    # PREFIX TRAJECTORIES
    #
    # Nnew x H
    # ==============================================================

    trajectories <- matrix(

        scores,

        nrow =
            n_new,

        ncol =
            H,

        byrow =
            TRUE
    )


    return(
        trajectories
    )
}


# ======================================================================
# ONE TREE:
# PREFIX -> TRAINING AFFINITY
# ======================================================================

predict_one_tree_prefix <- function(
    tree,
    DATA,
    user_id,
    H
) {

    # ==============================================================
    # NEW PREFIX TRAJECTORIES
    # ==============================================================

    new_trajectories <-
        project_prefix_tree(

            tree,

            DATA,

            user_id,

            H
        )


    n_new <- nrow(
        new_trajectories
    )


    n_train <- nrow(
        tree$train_trajectories
    )


    A <- matrix(
        0,
        n_new,
        n_train
    )


    # ==============================================================
    # STORED HIERARCHY LEVELS
    # ==============================================================

    for(yy in seq_along(
        tree$level_clusters
    )) {

        train_cl <-
            tree$level_clusters[[yy]]


        full_centroids <-
            tree$level_centroids[[yy]]


        # ==========================================================
        # ONLY FIRST H VISITS OF TRAINING PROTOTYPES
        # ==========================================================

        centroids <- full_centroids[
            ,
            seq_len(H),
            drop = FALSE
        ]


        # ==========================================================
        # CLOSEST PREFIX PROTOTYPE
        # ==========================================================

        new_cl <- integer(
            n_new
        )


        for(i in seq_len(
            n_new
        )) {

            z <- new_trajectories[
                i,
                ,
                drop = TRUE
            ]


            # ------------------------------------------------------
            # Squared Euclidean distance
            #
            # sqrt() is unnecessary for nearest-centroid assignment
            # ------------------------------------------------------

            distances <- apply(

                centroids,

                1,

                function(mu) {

                    sum(
                        (z - mu)^2
                    )
                }
            )


            new_cl[i] <-
                as.integer(

                    rownames(
                        centroids
                    )[
                        which.min(
                            distances
                        )
                    ]
                )
        }


        # ==========================================================
        # NEW -> TRAIN CO-MEMBERSHIP
        # ==========================================================

        for(i in seq_len(
            n_new
        )) {

            A[
                i,
            ] <- A[
                i,
            ] +

                as.numeric(
                    train_cl ==
                    new_cl[i]
                )
        }
    }


    return(
        A
    )
}


# ======================================================================
# PREFIX AFFINITY
# ======================================================================

longTAPIO_prefix_affinity <- function(
    model,
    DATA,
    user_id,
    H
) {

    n_new <- length(
        unique(
            user_id
        )
    )


    n_train <- length(
        model$train_cl
    )


    AFF_NEW <- matrix(
        0,
        n_new,
        n_train
    )


    # ==============================================================
    # ALL TREES
    # ==============================================================

    for(xx in seq_len(
        model$n_trees
    )) {

        AFF_NEW <-
            AFF_NEW +

            predict_one_tree_prefix(

                model$trees[[xx]],

                DATA,

                user_id,

                H
            )
    }


    # ==============================================================
    # NORMALIZE
    # ==============================================================

    AFF_NEW <-
        AFF_NEW /
        (
            model$n_trees *
            model$levels
        )


    return(
        AFF_NEW
    )
}


# ======================================================================
# PREDICT USING FIRST H VISITS
# ======================================================================

predict_longTAPIO <- function(
    model,
    DATA,
    user_id,
    visits
) {

    H <- visits


    if(
        H < 1 ||
        H > model$n_visits
    ) {

        stop(
            paste0(
                "visits must be between 1 and ",
                model$n_visits,
                "."
            )
        )
    }


    # ==============================================================
    # EXTRACT AVAILABLE PREFIX
    # ==============================================================

    prefix <- extract_prefix(

        DATA,

        user_id,

        H
    )


    # ==============================================================
    # NEW -> TRAIN AFFINITY
    # ==============================================================

    Anew <-
        longTAPIO_prefix_affinity(

            model,

            prefix$DATA,

            prefix$user_id,

            H
        )


    n_new <- nrow(
        Anew
    )


    # ==============================================================
    # AFFINITY TO FINAL TRAINING CLUSTERS
    # ==============================================================

    cluster_scores <- matrix(
        NA_real_,
        n_new,
        model$k
    )


    for(kk in seq_len(
        model$k
    )) {

        members <- which(
            model$train_cl ==
            kk
        )


        cluster_scores[
            ,
            kk
        ] <- rowMeans(

            Anew[
                ,
                members,
                drop = FALSE
            ]
        )
    }


    # ==============================================================
    # PREDICT CLUSTER
    # ==============================================================

    predicted <- max.col(

        cluster_scores,

        ties.method =
            "first"
    )


    # ==============================================================
    # CONFIDENCE MARGIN
    # ==============================================================

    confidence <- apply(

        cluster_scores,

        1,

        function(x) {

            sx <- sort(
                x,
                decreasing = TRUE
            )


            if(
                length(sx) < 2
            ) {

                return(
                    NA_real_
                )
            }


            return(
                sx[1] -
                sx[2]
            )
        }
    )


    return(

        list(

            cluster =
                predicted,

            cluster_scores =
                cluster_scores,

            confidence =
                confidence,

            affinity =
                Anew
        )
    )
}


# ======================================================================
# RESULT STORAGE
# ======================================================================

ARI_PREFIX <- matrix(
    NA_real_,
    N_ITER,
    N_VISITS
)


NMI_PREFIX <- matrix(
    NA_real_,
    N_ITER,
    N_VISITS
)


CONF_PREFIX <- matrix(
    NA_real_,
    N_ITER,
    N_VISITS
)


ARI_ORIGINAL <- rep(
    NA_real_,
    N_ITER
)


colnames(
    ARI_PREFIX
) <- paste0(
    "V",
    HORIZONS
)


colnames(
    NMI_PREFIX
) <- paste0(
    "V",
    HORIZONS
)


colnames(
    CONF_PREFIX
) <- paste0(
    "V",
    HORIZONS
)


# ======================================================================
# MAIN BENCHMARK
# ======================================================================

for(ii in seq_len(
    N_ITER
)) {

    cat("\n")
    cat("======================================================================\n")
    cat("RUN", ii, "/", N_ITER, "\n")
    cat("======================================================================\n")


    # ==================================================================
    # YOUR ORIGINAL SIMULATION SETTING
    # ==================================================================

    r_eta <- 3


    r_sigma_diag <- rep(
        3,
        5
    )


    id <- sample(
        1:5,
        1
    )


    r_sigma_diag[
        id
    ] <- sample(
        3:20,
        1
    )


    cat(
        "sigma_diag:",
        paste(
            r_sigma_diag,
            collapse = " "
        ),
        "\n"
    )


    # ==================================================================
    # GENERATE DATA
    # ==================================================================

    Longdat2 <- simLongData(

        ranTimes =
            FALSE,

        n_i =
            N_VISITS,

        eta =
            r_eta,

        sigma_diag =
            r_sigma_diag
    )


    # ==================================================================
    # RESHAPE
    # ==================================================================

    Longdat2_wide <- reshape(

        Longdat2,

        idvar = c(
            "subject",
            "time",
            "cluster"
        ),

        timevar =
            "outcome",

        direction =
            "wide"
    )


    # ==================================================================
    # EXPLICIT SUBJECT + TIME ORDER
    # ==================================================================

    Longdat2_wide <-
        Longdat2_wide[
            order(
                Longdat2_wide$subject,
                Longdat2_wide$time
            ),
        ]


    rownames(
        Longdat2_wide
    ) <- NULL


    # ==================================================================
    # FEATURE MATRIX
    # ==================================================================

    DD <- as.matrix(

        Longdat2_wide[
            ,
            4:ncol(
                Longdat2_wide
            )
        ]
    )


    USER_ID <-
        Longdat2_wide$subject


    # ==================================================================
    # TRUE SUBJECT CLUSTERS
    # ==================================================================

    subject_info <- aggregate(

        Longdat2_wide$cluster,

        by = list(
            subject =
                Longdat2_wide$subject
        ),

        FUN = function(x) {
            x[1]
        }
    )


    colnames(
        subject_info
    ) <- c(
        "subject",
        "cluster"
    )


    subject_info <-
        subject_info[
            order(
                subject_info$subject
            ),
        ]


    ALL_SUBJECTS <-
        subject_info$subject


    n_subjects <-
        length(
            ALL_SUBJECTS
        )


    # ==================================================================
    # TRAIN / TEST SUBJECT SPLIT
    # ==================================================================

    n_train <- floor(
        TRAIN_FRAC *
        n_subjects
    )


    TRAIN_SUBJECTS <- sort(

        sample(
            ALL_SUBJECTS,
            n_train
        )
    )


    TEST_SUBJECTS <- sort(

        setdiff(
            ALL_SUBJECTS,
            TRAIN_SUBJECTS
        )
    )


    # ==================================================================
    # TRUE TEST CLUSTERS
    # ==================================================================

    true_test <-
        subject_info$cluster[

            match(
                TEST_SUBJECTS,
                subject_info$subject
            )
        ]


    # ==================================================================
    # TRAIN DATA
    # ==================================================================

    train_rows <-
        USER_ID %in%
        TRAIN_SUBJECTS


    DD_train <-
        DD[
            train_rows,
            ,
            drop = FALSE
        ]


    USER_train_original <-
        USER_ID[
            train_rows
        ]


    USER_train <- match(

        USER_train_original,

        TRAIN_SUBJECTS
    )


    # ==================================================================
    # TEST DATA
    # ==================================================================

    test_rows <-
        USER_ID %in%
        TEST_SUBJECTS


    DD_test <-
        DD[
            test_rows,
            ,
            drop = FALSE
        ]


    USER_test_original <-
        USER_ID[
            test_rows
        ]


    USER_test <- match(

        USER_test_original,

        TEST_SUBJECTS
    )


    # ==================================================================
    # ORIGINAL TRANSDUCTIVE longTAPIO
    #
    # Weighted PCA only
    #
    # Gets ALL subjects.
    # Evaluation below is nevertheless restricted to TEST subjects.
    # ==================================================================

    cat("\n")
    cat("Original longTAPIO (transductive)...\n")


    set.seed(
        100000 + ii
    )


    original <- longTAPIO_trajectories(

        DD,

        k =
            SET_K,

        user_id =
            USER_ID,

        levels =
            SET_LEVELS,

        verbose =
            FALSE,

        n_trees =
            SET_N_TREES,

        method =
            METHOD,

        n_features =
            SET_N_FEATURES,

        do.leveling =
            TRUE,

        scale =
            SCALE_PCA,

        replace =
            REPLACE_FEATURES,

        pca_selection =
            PCA_SELECTION
    )


    original_subjects <- sort(
        unique(
            USER_ID
        )
    )


    original_test_clusters <-
        original$cl[

            match(
                TEST_SUBJECTS,
                original_subjects
            )
        ]


    ARI_ORIGINAL[ii] <-
        ARI(
            true_test,
            original_test_clusters
        )


    cat(
        sprintf(
            "Original test ARI = %.3f\n",
            ARI_ORIGINAL[ii]
        )
    )


    # ==================================================================
    # FIT INDUCTIVE longTAPIO ONCE
    #
    # Weighted PCA only
    # ==================================================================

    cat(
        "\nFitting inductive longTAPIO...\n"
    )


    set.seed(
        200000 + ii
    )


    model <- longTAPIO_inductive_fit(

        DD_train,

        USER_train,

        k =
            SET_K,

        n_features =
            SET_N_FEATURES,

        n_trees =
            SET_N_TREES,

        levels =
            SET_LEVELS,

        method =
            METHOD,

        scale =
            SCALE_PCA,

        replace =
            REPLACE_FEATURES,

        pca_selection =
            PCA_SELECTION
    )


    # ==================================================================
    # PROGRESSIVE TEST-PATIENT PREDICTION
    # ==================================================================

    cat("\n")
    cat("Progressive prediction:\n")
    cat("------------------------------------------------------------\n")


    for(H in HORIZONS) {

        pred <- predict_longTAPIO(

            model,

            DD_test,

            USER_test,

            visits =
                H
        )


        # ==============================================================
        # ARI
        # ==============================================================

        ari_h <- ARI(

            true_test,

            pred$cluster
        )


        # ==============================================================
        # NMI
        # ==============================================================

        nmi_h <- NMI(

            true_test,

            pred$cluster
        )


        # ==============================================================
        # MEAN CONFIDENCE
        # ==============================================================

        conf_h <- mean(

            pred$confidence,

            na.rm = TRUE
        )


        ARI_PREFIX[
            ii,
            H
        ] <- ari_h


        NMI_PREFIX[
            ii,
            H
        ] <- nmi_h


        CONF_PREFIX[
            ii,
            H
        ] <- conf_h


        cat(

            sprintf(

                paste0(
                    "Visit %2d | ",
                    "ARI = %.3f | ",
                    "NMI = %.3f | ",
                    "margin = %.3f\n"
                ),

                H,

                ari_h,

                nmi_h,

                conf_h
            )
        )
    }


    # ==================================================================
    # FULL-TRAJECTORY COMPARISON
    # ==================================================================

    cat("\n")


    cat(

        sprintf(

            paste0(
                "Full trajectory: ",
                "Original ARI = %.3f | ",
                "Inductive ARI = %.3f | ",
                "difference = %+.3f\n"
            ),

            ARI_ORIGINAL[ii],

            ARI_PREFIX[
                ii,
                N_VISITS
            ],

            ARI_PREFIX[
                ii,
                N_VISITS
            ] -
                ARI_ORIGINAL[ii]
        )
    )


    # ==================================================================
    # CURRENT MEAN CURVE
    # ==================================================================

    cat(
        "\nCurrent mean progressive ARI:\n"
    )


    current <- data.frame(

        Visit =
            HORIZONS,

        Mean_ARI =
            colMeans(

                ARI_PREFIX[
                    seq_len(ii),
                    ,
                    drop = FALSE
                ],

                na.rm =
                    TRUE
            )
    )


    print(
        current,
        digits = 3,
        row.names = FALSE
    )
}


# ======================================================================
# FINAL SUMMARY
# ======================================================================

cat("\n\n")
cat("======================================================================\n")
cat("FINAL RESULTS -- 20 RUNS\n")
cat("======================================================================\n")


SUMMARY <- data.frame(

    Visit =
        HORIZONS,

    Mean_ARI =
        colMeans(
            ARI_PREFIX
        ),

    SD_ARI =
        apply(
            ARI_PREFIX,
            2,
            sd
        ),

    Mean_NMI =
        colMeans(
            NMI_PREFIX
        ),

    SD_NMI =
        apply(
            NMI_PREFIX,
            2,
            sd
        ),

    Mean_Margin =
        colMeans(
            CONF_PREFIX
        ),

    SD_Margin =
        apply(
            CONF_PREFIX,
            2,
            sd
        )
)


print(
    SUMMARY,
    digits = 3,
    row.names = FALSE
)


# ======================================================================
# ORIGINAL vs FULL-TRAJECTORY INDUCTIVE
# ======================================================================

original_mean <-
    mean(
        ARI_ORIGINAL
    )


original_sd <-
    sd(
        ARI_ORIGINAL
    )


inductive_full_mean <-
    mean(
        ARI_PREFIX[
            ,
            N_VISITS
        ]
    )


inductive_full_sd <-
    sd(
        ARI_PREFIX[
            ,
            N_VISITS
        ]
    )


difference <-
    ARI_PREFIX[
        ,
        N_VISITS
    ] -
    ARI_ORIGINAL


cat("\n")
cat("======================================================================\n")
cat("FULL TRAJECTORY COMPARISON\n")
cat("======================================================================\n")


cat(

    sprintf(

        "Original longTAPIO  : %.3f +/- %.3f\n",

        original_mean,

        original_sd
    )
)


cat(

    sprintf(

        "Inductive longTAPIO : %.3f +/- %.3f\n",

        inductive_full_mean,

        inductive_full_sd
    )
)


cat(

    sprintf(

        "Inductive - Original: %+.3f +/- %.3f\n",

        mean(
            difference
        ),

        sd(
            difference
        )
    )
)


# ======================================================================
# PAIRED TEST
# ======================================================================

cat("\nPaired Wilcoxon test:\n")


print(

    wilcox.test(

        ARI_ORIGINAL,

        ARI_PREFIX[
            ,
            N_VISITS
        ],

        paired =
            TRUE,

        exact =
            FALSE
    )
)


# ======================================================================
# FRACTION OF FULL INDUCTIVE PERFORMANCE
# ======================================================================

mean_ari_curve <-
    colMeans(
        ARI_PREFIX
    )


final_ari <-
    mean_ari_curve[
        N_VISITS
    ]


relative_performance <-
    mean_ari_curve /
    final_ari


RELATIVE <- data.frame(

    Visit =
        HORIZONS,

    Mean_ARI =
        mean_ari_curve,

    Fraction_of_full =
        relative_performance
)


cat("\n")
cat("======================================================================\n")
cat("FRACTION OF FULL-TRAJECTORY INDUCTIVE PERFORMANCE\n")
cat("======================================================================\n")


print(
    RELATIVE,
    digits = 3,
    row.names = FALSE
)


# ======================================================================
# FIRST VISIT REACHING PERFORMANCE THRESHOLD
# ======================================================================

first_reach <- function(
    x,
    threshold
) {

    ids <- which(
        x >= threshold
    )


    if(
        length(ids) == 0
    ) {

        return(
            NA_integer_
        )
    }


    return(
        min(ids)
    )
}


v80 <- first_reach(
    relative_performance,
    0.80
)


v90 <- first_reach(
    relative_performance,
    0.90
)


v95 <- first_reach(
    relative_performance,
    0.95
)


cat("\n")
cat("Early identification:\n")
cat("---------------------\n")
cat("80% of full performance: visit", v80, "\n")
cat("90% of full performance: visit", v90, "\n")
cat("95% of full performance: visit", v95, "\n")


# ======================================================================
# FIGURE:
# PROGRESSIVE ARI
# ======================================================================

PLOT_DATA <- data.frame(

    Visit =
        HORIZONS,

    Mean =
        colMeans(
            ARI_PREFIX
        ),

    SD =
        apply(
            ARI_PREFIX,
            2,
            sd
        )
)


p_progressive <- ggplot(

    PLOT_DATA,

    aes(
        x =
            Visit,

        y =
            Mean
    )
) +

    geom_ribbon(

        aes(

            ymin =
                pmax(
                    0,
                    Mean - SD
                ),

            ymax =
                pmin(
                    1,
                    Mean + SD
                )
        ),

        alpha =
            0.20
    ) +

    geom_line(
        linewidth =
            1.1
    ) +

    geom_point(
        size =
            3
    ) +

    # --------------------------------------------------------------
    # Original full-trajectory reference
    # --------------------------------------------------------------

    geom_hline(

        yintercept =
            original_mean,

        linetype =
            "dashed",

        linewidth =
            0.8
    ) +

    scale_x_continuous(

        breaks =
            HORIZONS
    ) +

    coord_cartesian(

        ylim =
            c(0, 1)
    ) +

    xlab(
        "Number of observed visits"
    ) +

    ylab(
        "Adjusted Rand Index"
    ) +

    theme_minimal() +

    theme(

        text =
            element_text(
                size = 16
            )
    )


print(
    p_progressive
)


# ======================================================================
# FIGURE:
# PROGRESSIVE NMI
# ======================================================================

PLOT_NMI <- data.frame(

    Visit =
        HORIZONS,

    Mean =
        colMeans(
            NMI_PREFIX
        ),

    SD =
        apply(
            NMI_PREFIX,
            2,
            sd
        )
)


p_nmi <- ggplot(

    PLOT_NMI,

    aes(
        x =
            Visit,

        y =
            Mean
    )
) +

    geom_ribbon(

        aes(

            ymin =
                pmax(
                    0,
                    Mean - SD
                ),

            ymax =
                pmin(
                    1,
                    Mean + SD
                )
        ),

        alpha =
            0.20
    ) +

    geom_line(
        linewidth =
            1.1
    ) +

    geom_point(
        size =
            3
    ) +

    scale_x_continuous(

        breaks =
            HORIZONS
    ) +

    coord_cartesian(

        ylim =
            c(0, 1)
    ) +

    xlab(
        "Number of observed visits"
    ) +

    ylab(
        "Normalized Mutual Information"
    ) +

    theme_minimal() +

    theme(

        text =
            element_text(
                size = 16
            )
    )


print(
    p_nmi
)


# ======================================================================
# FIGURE:
# ASSIGNMENT CONFIDENCE
# ======================================================================

PLOT_CONF <- data.frame(

    Visit =
        HORIZONS,

    Mean =
        colMeans(
            CONF_PREFIX
        ),

    SD =
        apply(
            CONF_PREFIX,
            2,
            sd
        )
)


p_confidence <- ggplot(

    PLOT_CONF,

    aes(
        x =
            Visit,

        y =
            Mean
    )
) +

    geom_line(
        linewidth =
            1.1
    ) +

    geom_point(
        size =
            3
    ) +

    scale_x_continuous(

        breaks =
            HORIZONS
    ) +

    xlab(
        "Number of observed visits"
    ) +

    ylab(
        "Mean assignment margin"
    ) +

    theme_minimal() +

    theme(

        text =
            element_text(
                size = 16
            )
    )


print(
    p_confidence
)


# ======================================================================
# SAVE RESULTS
# ======================================================================

write.csv(

    ARI_PREFIX,

    "inductive_longTAPIO_progressive_ARI.csv",

    row.names =
        FALSE
)


write.csv(

    NMI_PREFIX,

    "inductive_longTAPIO_progressive_NMI.csv",

    row.names =
        FALSE
)


write.csv(

    CONF_PREFIX,

    "inductive_longTAPIO_progressive_confidence.csv",

    row.names =
        FALSE
)


write.csv(

    SUMMARY,

    "inductive_longTAPIO_progressive_summary.csv",

    row.names =
        FALSE
)


cat("\n")
cat("======================================================================\n")
cat("BENCHMARK COMPLETE\n")
cat("======================================================================\n")