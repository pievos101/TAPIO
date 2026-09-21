# ======================================================================
# BENCHMARK:
#
# ORIGINAL TRANSDUCTIVE longTAPIO
#        vs
# INDUCTIVE / PROGRESSIVE longTAPIO
#
# PCA STRATEGY:
#   FIRST PRINCIPAL COMPONENT (PC1) FOR EVERY TREE
#
# Includes:
#
#   1. Original transductive longTAPIO clustering
#   2. Original transductive feature importance
#   3. Inductive longTAPIO fitted on training patients only
#   4. Progressive prediction of unseen test patients
#   5. Inductive global feature importance
#   6. Progressive cluster-specific feature importance
#   7. Patient-specific progressive feature importance
#   8. Hungarian cluster alignment
#   9. Comparison of transductive vs inductive importance
#  10. Importance-profile correlations
#  11. Assignment stability
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
library(clue)


# ======================================================================
# OPTIONAL SOURCE FILES
# ======================================================================

# source("R/TAPIO_inductive.R")
# source("R/longTAPIO_inductive.R")
# source("R/importance_TAPIO_inductive.R")
# source("R/importance_longTAPIO_inductive.R")


# ======================================================================
# CONFIGURATION
# ======================================================================

TRAIN_FRAC <- 0.70

N_VISITS <- 5

SET_K <- 4

SET_LEVELS <- 4

SET_N_FEATURES <- 5

SET_N_TREES <- 500

METHOD <- "ward.D2"

SCALE_PCA <- TRUE

REPLACE_FEATURES <- TRUE


# ======================================================================
# IMPORTANT:
# USE FIRST PRINCIPAL COMPONENT
# ======================================================================

PCA_SELECTION <- "random_weighted"


HORIZONS <- seq_len(
    N_VISITS
)

SEED <- 12345


set.seed(
    SEED
)


cat("\n")
cat("======================================================================\n")
cat("TRANSDUCTIVE vs INDUCTIVE longTAPIO BENCHMARK\n")
cat("======================================================================\n")
cat("Runs            : 1\n")
cat("Train fraction  :", TRAIN_FRAC, "\n")
cat("Visits          :", N_VISITS, "\n")
cat("Clusters        :", SET_K, "\n")
cat("Trees           :", SET_N_TREES, "\n")
cat("Features/tree   :", SET_N_FEATURES, "\n")
cat("Levels          :", SET_LEVELS, "\n")
cat("PCA selection   :", PCA_SELECTION, "\n")
cat("======================================================================\n\n")


# ======================================================================
# HELPER:
# HUNGARIAN CLUSTER ALIGNMENT
# ======================================================================

align_clusters <- function(
    reference,
    estimated
) {

    reference <- as.integer(
        as.factor(
            reference
        )
    )

    estimated <- as.integer(
        as.factor(
            estimated
        )
    )


    TAB <- table(
        estimated,
        reference
    )


    nr <- nrow(
        TAB
    )

    nc <- ncol(
        TAB
    )

    nn <- max(
        nr,
        nc
    )


    SIMILARITY <- matrix(
        0,
        nrow = nn,
        ncol = nn
    )


    SIMILARITY[
        seq_len(nr),
        seq_len(nc)
    ] <- TAB


    ASSIGN <- solve_LSAP(
        SIMILARITY,
        maximum = TRUE
    )


    MAP <- as.integer(
        ASSIGN[
            seq_len(nr)
        ]
    )


    aligned <- rep(
        NA_integer_,
        length(
            estimated
        )
    )


    for(i in seq_len(
        nr
    )) {

        aligned[
            estimated == i
        ] <- MAP[i]
    }


    return(
        list(

            aligned =
                aligned,

            mapping =
                MAP,

            contingency =
                TAB
        )
    )
}


# ======================================================================
# HELPER:
# ALIGN IMPORTANCE MATRIX
# ======================================================================

align_importance <- function(
    importance_matrix,
    mapping,
    K
) {

    OUT <- matrix(
        NA_real_,
        nrow = K,
        ncol = ncol(
            importance_matrix
        )
    )


    colnames(
        OUT
    ) <- colnames(
        importance_matrix
    )


    for(i in seq_along(
        mapping
    )) {

        target <-
            mapping[i]


        if(
            target >= 1 &&
            target <= K &&
            i <= nrow(
                importance_matrix
            )
        ) {

            OUT[
                target,
            ] <- importance_matrix[
                i,
            ]
        }
    }


    rownames(
        OUT
    ) <- paste0(
        "Cluster ",
        seq_len(
            K
        )
    )


    return(
        OUT
    )
}


# ======================================================================
# SIMULATION SETTING
# ======================================================================

r_eta <- 3


r_sigma_diag <- rep(
    3,
    5
)


id <- sample(
    1:5,
    1
)


# Optional:
#
# r_sigma_diag[id] <- sample(
#     3:20,
#     1
# )


cat(
    "sigma_diag:",
    paste(
        r_sigma_diag,
        collapse = " "
    ),
    "\n"
)


cat(
    "Outcome selected:",
    id,
    "\n\n"
)


# ======================================================================
# GENERATE DATA
# ======================================================================

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


# ======================================================================
# RESHAPE
# ======================================================================

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


# ======================================================================
# SORT SUBJECT + TIME
# ======================================================================

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


# ======================================================================
# FEATURE MATRIX
# ======================================================================

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


cat(
    "Features:",
    paste(
        colnames(
            DD
        ),
        collapse = ", "
    ),
    "\n"
)


# ======================================================================
# TRUE SUBJECT CLUSTERS
# ======================================================================

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


TRUE_ALL <-
    subject_info$cluster


n_subjects <-
    length(
        ALL_SUBJECTS
    )


cat(
    "Number of subjects:",
    n_subjects,
    "\n\n"
)


# ======================================================================
# ======================================================================
#
# PART I
#
# ORIGINAL TRANSDUCTIVE longTAPIO
#
# PC1 ONLY
#
# ======================================================================
# ======================================================================


cat("\n")
cat("======================================================================\n")
cat("ORIGINAL TRANSDUCTIVE longTAPIO - PC1\n")
cat("======================================================================\n")


set.seed(
    200001
)


TRANSDUCTIVE <- longTAPIO_trajectories(

    DATA =
        DD,

    user_id =
        USER_ID,

    k =
        SET_K,

    n_features =
        SET_N_FEATURES,

    n_trees =
        SET_N_TREES,

    do.pca =
        TRUE,

    do.MFA =
        FALSE,

    do.leveling =
        TRUE,

    levels =
        SET_LEVELS,

    verbose =
        FALSE,

    method =
        METHOD,

    scale =
        SCALE_PCA,

    replace =
        REPLACE_FEATURES,

    pca_selection =
        PCA_SELECTION
)


# ======================================================================
# TRANSDUCTIVE PERFORMANCE
# ======================================================================

ARI_TRANSDUCTIVE <- ARI(

    TRUE_ALL,

    TRANSDUCTIVE$cl
)


NMI_TRANSDUCTIVE <- NMI(

    TRUE_ALL,

    TRANSDUCTIVE$cl
)


cat(
    sprintf(
        "Transductive ARI : %.3f\n",
        ARI_TRANSDUCTIVE
    )
)


cat(
    sprintf(
        "Transductive NMI : %.3f\n",
        NMI_TRANSDUCTIVE
    )
)


# ======================================================================
# ALIGN TRANSDUCTIVE CLUSTERS
# ======================================================================

ALIGN_TRANS <- align_clusters(

    TRUE_ALL,

    TRANSDUCTIVE$cl
)


TRANS_CLUSTER_ALIGNED <-
    ALIGN_TRANS$aligned


cat(
    "\nTransductive cluster mapping:\n"
)


print(

    data.frame(

        Estimated =
            seq_along(
                ALIGN_TRANS$mapping
            ),

        True =
            ALIGN_TRANS$mapping
    )
)


cat(
    "\nTransductive contingency table:\n"
)


print(
    ALIGN_TRANS$contingency
)


# ======================================================================
# ORIGINAL TRANSDUCTIVE FEATURE IMPORTANCE
# ======================================================================

cat("\n")
cat(
    "Calculating original transductive feature importance...\n"
)


IMP_TRANSDUCTIVE <- importance(
    TRANSDUCTIVE
)


colnames(
    IMP_TRANSDUCTIVE
) <- colnames(
    DD
)


rownames(
    IMP_TRANSDUCTIVE
) <- paste0(

    "Estimated cluster ",

    seq_len(
        nrow(
            IMP_TRANSDUCTIVE
        )
    )
)


cat(
    "\nOriginal transductive importance:\n"
)


print(

    round(
        IMP_TRANSDUCTIVE,
        3
    )
)


# ======================================================================
# ALIGN TRANSDUCTIVE IMPORTANCE
# ======================================================================

IMP_TRANSDUCTIVE_ALIGNED <- align_importance(

    importance_matrix =
        IMP_TRANSDUCTIVE,

    mapping =
        ALIGN_TRANS$mapping,

    K =
        SET_K
)


cat(
    "\nAligned transductive importance:\n"
)


print(

    round(
        IMP_TRANSDUCTIVE_ALIGNED,
        3
    )
)


# ======================================================================
# ======================================================================
#
# PART II
#
# TRAIN / TEST SPLIT
#
# ======================================================================
# ======================================================================


n_train <- floor(
    TRAIN_FRAC *
    n_subjects
)


set.seed(
    SEED + 1
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


cat("\n")
cat("======================================================================\n")
cat("INDUCTIVE TRAIN / TEST SPLIT\n")
cat("======================================================================\n")


cat(
    "Training subjects:",
    length(
        TRAIN_SUBJECTS
    ),
    "\n"
)


cat(
    "Test subjects    :",
    length(
        TEST_SUBJECTS
    ),
    "\n\n"
)


# ======================================================================
# TRUE TRAIN / TEST LABELS
# ======================================================================

true_train <-
    subject_info$cluster[
        match(
            TRAIN_SUBJECTS,
            subject_info$subject
        )
    ]


true_test <-
    subject_info$cluster[
        match(
            TEST_SUBJECTS,
            subject_info$subject
        )
    ]


# ======================================================================
# TRAIN DATA
# ======================================================================

train_rows <-
    USER_ID %in%
    TRAIN_SUBJECTS


DD_train <- DD[
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


# ======================================================================
# TEST DATA
# ======================================================================

test_rows <-
    USER_ID %in%
    TEST_SUBJECTS


DD_test <- DD[
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


# ======================================================================
# ======================================================================
#
# PART III
#
# FIT INDUCTIVE longTAPIO
#
# PC1 ONLY
#
# ======================================================================
# ======================================================================


cat("\n")
cat("======================================================================\n")
cat("FITTING INDUCTIVE longTAPIO - PC1\n")
cat("======================================================================\n")


set.seed(
    200001
)


model <- longTAPIO_inductive(

    DATA =
        DD_train,

    user_id =
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


cat(
    "Model fitted successfully.\n"
)


cat(
    "Training patients:",
    length(
        model$train_clusters
    ),
    "\n"
)


cat(
    "Stored trees:",
    length(
        model$trees
    ),
    "\n\n"
)


cat(
    "Selected PC in first 10 trees:\n"
)


print(
    sapply(
        model$trees[
            seq_len(
                min(
                    10,
                    length(
                        model$trees
                    )
                )
            )
        ],
        function(x) {
            x$selected_pc
        }
    )
)


cat(
    "\nExample feature contribution from tree 1:\n"
)


print(
    model$trees[[1]]$feature_contribution
)


cat(
    "Sum =",
    sum(
        model$trees[[1]]$feature_contribution
    ),
    "\n"
)


# ======================================================================
# SAFETY CHECK:
# ALL TREES MUST USE PC1
# ======================================================================

#SELECTED_PCS <- sapply(
#
#    model$trees,
#
#    function(x) {
#        x$selected_pc
#    }
#)


#if(
#    any(
#        SELECTED_PCS != 1
#    )
#) {
#
#    stop(
#        paste0(
#            "ERROR: inductive model is not using PC1 in every tree. ",
#            "Modify longTAPIO_inductive() as shown below."
#        )
#    )
#}


#cat(
#    "\nVerified: all inductive trees use PC1.\n"
#)


# ======================================================================
# ALIGN INDUCTIVE REFERENCE CLUSTERS
# ======================================================================

ALIGN_IND <- align_clusters(

    true_train,

    model$train_clusters
)


cat(
    "\nInductive reference-cluster mapping:\n"
)


print(

    data.frame(

        Estimated =
            seq_along(
                ALIGN_IND$mapping
            ),

        True =
            ALIGN_IND$mapping
    )
)


cat(
    "\nInductive training contingency table:\n"
)


print(
    ALIGN_IND$contingency
)


# ======================================================================
# ======================================================================
#
# PART IV
#
# PROGRESSIVE PREDICTION
#
# ======================================================================
# ======================================================================


cat("\n")
cat("======================================================================\n")
cat("PROGRESSIVE PREDICTION OF UNSEEN PATIENTS\n")
cat("======================================================================\n")


ARI_PREFIX <- rep(
    NA_real_,
    N_VISITS
)


NMI_PREFIX <- rep(
    NA_real_,
    N_VISITS
)


MARGIN_PREFIX <- rep(
    NA_real_,
    N_VISITS
)


PREDICTIONS <- vector(
    "list",
    N_VISITS
)


for(H in HORIZONS) {

    pred <- predict(

        model,

        newdata =
            DD_test,

        user_id =
            USER_test,

        visits =
            H
    )


    PREDICTIONS[[H]] <-
        pred


    ARI_PREFIX[H] <- ARI(

        true_test,

        pred$cluster
    )


    NMI_PREFIX[H] <- NMI(

        true_test,

        pred$cluster
    )


    MARGIN_PREFIX[H] <- mean(

        pred$margin,

        na.rm =
            TRUE
    )


    cat(

        sprintf(

            paste0(
                "Visit %2d | ",
                "ARI = %.3f | ",
                "NMI = %.3f | ",
                "margin = %.3f\n"
            ),

            H,

            ARI_PREFIX[H],

            NMI_PREFIX[H],

            MARGIN_PREFIX[H]
        )
    )
}


PERFORMANCE <- data.frame(

    Visit =
        HORIZONS,

    ARI =
        ARI_PREFIX,

    NMI =
        NMI_PREFIX,

    Margin =
        MARGIN_PREFIX
)


cat("\n")


print(
    PERFORMANCE,
    digits = 3,
    row.names = FALSE
)


# ======================================================================
# PERFORMANCE COMPARISON
# ======================================================================

COMPARISON <- data.frame(

    Method = c(

        "Original longTAPIO (transductive)",

        paste0(
            "Inductive longTAPIO (visit ",
            HORIZONS,
            ")"
        )
    ),

    Visits = c(

        N_VISITS,

        HORIZONS
    ),

    ARI = c(

        ARI_TRANSDUCTIVE,

        ARI_PREFIX
    ),

    NMI = c(

        NMI_TRANSDUCTIVE,

        NMI_PREFIX
    )
)


cat("\n")
cat("======================================================================\n")
cat("TRANSDUCTIVE vs INDUCTIVE PERFORMANCE\n")
cat("======================================================================\n")


print(
    COMPARISON,
    digits = 3,
    row.names = FALSE
)


# ======================================================================
# INDUCTIVE GAP
# ======================================================================

INDUCTIVE_GAP_ARI <-

    ARI_PREFIX[
        N_VISITS
    ] -

    ARI_TRANSDUCTIVE


INDUCTIVE_GAP_NMI <-

    NMI_PREFIX[
        N_VISITS
    ] -

    NMI_TRANSDUCTIVE


cat(

    sprintf(

        "\nFull-trajectory inductive - transductive ARI : %+.3f\n",

        INDUCTIVE_GAP_ARI
    )
)


cat(

    sprintf(

        "Full-trajectory inductive - transductive NMI : %+.3f\n",

        INDUCTIVE_GAP_NMI
    )
)


# ======================================================================
# ======================================================================
#
# PART V
#
# INDUCTIVE FEATURE IMPORTANCE
#
# ======================================================================
# ======================================================================


cat("\n")
cat("======================================================================\n")
cat("CALCULATING INDUCTIVE / PROGRESSIVE FEATURE IMPORTANCE\n")
cat("======================================================================\n")


IMP <- importance_longTAPIO_inductive(

    res =
        model,

    newdata =
        DD_test,

    user_id =
        USER_test,

    normalize =
        TRUE
)


cat(
    "Importance calculation complete.\n"
)


# ======================================================================
# ALIGN GLOBAL INDUCTIVE IMPORTANCE
# ======================================================================

IMP_INDUCTIVE_ALIGNED <- align_importance(

    importance_matrix =
        IMP$global,

    mapping =
        ALIGN_IND$mapping,

    K =
        SET_K
)


colnames(
    IMP_INDUCTIVE_ALIGNED
) <- colnames(
    DD
)


cat(
    "\nAligned inductive global importance:\n"
)


print(

    round(
        IMP_INDUCTIVE_ALIGNED,
        3
    )
)


# ======================================================================
# ======================================================================
#
# PART VI
#
# TRANSDUCTIVE vs INDUCTIVE IMPORTANCE
#
# ======================================================================
# ======================================================================


cat("\n")
cat("======================================================================\n")
cat("TRANSDUCTIVE vs INDUCTIVE FEATURE IMPORTANCE\n")
cat("======================================================================\n")


IMPORTANCE_CORRELATION <- data.frame(

    Cluster =
        seq_len(
            SET_K
        ),

    Pearson =
        NA_real_,

    Spearman =
        NA_real_
)


for(k_id in seq_len(
    SET_K
)) {

    x <-
        IMP_TRANSDUCTIVE_ALIGNED[
            k_id,
        ]


    y <-
        IMP_INDUCTIVE_ALIGNED[
            k_id,
        ]


    ok <-
        is.finite(x) &
        is.finite(y)


    if(
        sum(ok) >= 2
    ) {

        IMPORTANCE_CORRELATION$Pearson[
            k_id
        ] <- cor(

            x[ok],

            y[ok],

            method =
                "pearson"
        )


        IMPORTANCE_CORRELATION$Spearman[
            k_id
        ] <- cor(

            x[ok],

            y[ok],

            method =
                "spearman"
        )
    }
}


cat(
    "\nImportance-profile correlations:\n"
)


print(
    IMPORTANCE_CORRELATION,
    digits = 3,
    row.names = FALSE
)


cat(

    sprintf(

        "\nMean Spearman correlation: %.3f\n",

        mean(
            IMPORTANCE_CORRELATION$Spearman,
            na.rm = TRUE
        )
    )
)


# ======================================================================
# LONG FORMAT:
# IMPORTANCE COMPARISON
# ======================================================================

IMPORTANCE_COMPARISON <- data.frame()


for(k_id in seq_len(
    SET_K
)) {

    for(j in seq_len(
        ncol(DD)
    )) {

        IMPORTANCE_COMPARISON <- rbind(

            IMPORTANCE_COMPARISON,

            data.frame(

                Method =
                    "Transductive",

                Cluster =
                    paste0(
                        "Cluster ",
                        k_id
                    ),

                Feature =
                    colnames(
                        DD
                    )[j],

                Importance =
                    IMP_TRANSDUCTIVE_ALIGNED[
                        k_id,
                        j
                    ]
            ),

            data.frame(

                Method =
                    "Inductive",

                Cluster =
                    paste0(
                        "Cluster ",
                        k_id
                    ),

                Feature =
                    colnames(
                        DD
                    )[j],

                Importance =
                    IMP_INDUCTIVE_ALIGNED[
                        k_id,
                        j
                    ]
            )
        )
    }
}


# ======================================================================
# FIGURE 1:
# TRANSDUCTIVE vs INDUCTIVE IMPORTANCE
# ======================================================================

p_importance_comparison <- ggplot(

    IMPORTANCE_COMPARISON,

    aes(
        x = Feature,
        y = Cluster,
        fill = Importance
    )
) +

    geom_tile() +

    geom_text(

        aes(
            label =
                sprintf(
                    "%.2f",
                    Importance
                )
        ),

        size = 3.8
    ) +

    facet_wrap(
        ~ Method,
        ncol = 1
    ) +

    scale_fill_gradient(

        low =
            "white",

        high =
            "steelblue",

        limits =
            c(
                0,
                1
            )
    ) +

    labs(

        title =
            "Feature importance: transductive vs inductive longTAPIO",

        subtitle =
            "PC1 used in every tree; clusters aligned to simulation classes",

        x =
            "Feature",

        y =
            "Trajectory cluster"
    ) +

    theme_minimal(
        base_size = 14
    )


print(
    p_importance_comparison
)


# ======================================================================
# ======================================================================
#
# PART VII
#
# PROGRESSIVE CLUSTER-SPECIFIC IMPORTANCE
#
# ======================================================================
# ======================================================================


PROGRESSIVE_ALIGNED <- array(

    NA_real_,

    dim = c(
        SET_K,
        ncol(DD),
        N_VISITS
    ),

    dimnames = list(

        paste0(
            "Cluster ",
            seq_len(
                SET_K
            )
        ),

        colnames(
            DD
        ),

        paste0(
            "Visit ",
            HORIZONS
        )
    )
)


for(old_cluster in seq_len(
    SET_K
)) {

    new_cluster <-
        ALIGN_IND$mapping[
            old_cluster
        ]


    PROGRESSIVE_ALIGNED[
        new_cluster,
        ,
    ] <-
        IMP$cluster_progressive[
            old_cluster,
            ,
        ]
}


PROGRESSIVE_DF <- data.frame()


for(k_id in seq_len(
    SET_K
)) {

    for(j in seq_len(
        ncol(DD)
    )) {

        for(H in HORIZONS) {

            PROGRESSIVE_DF <- rbind(

                PROGRESSIVE_DF,

                data.frame(

                    Cluster =
                        paste0(
                            "Cluster ",
                            k_id
                        ),

                    Feature =
                        colnames(
                            DD
                        )[j],

                    Visit =
                        H,

                    Importance =
                        PROGRESSIVE_ALIGNED[
                            k_id,
                            j,
                            H
                        ]
                )
            )
        }
    }
}


# ======================================================================
# FIGURE 2:
# PROGRESSIVE IMPORTANCE HEATMAP
# ======================================================================

p_progressive_heatmap <- ggplot(

    PROGRESSIVE_DF,

    aes(
        x = Visit,
        y = Feature,
        fill = Importance
    )
) +

    geom_tile() +

    facet_wrap(
        ~ Cluster,
        ncol = 2
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    scale_fill_gradient(

        low =
            "white",

        high =
            "steelblue",

        limits =
            c(
                0,
                1
            )
    ) +

    labs(

        title =
            "Progressive feature importance",

        subtitle =
            "PC1; unseen patients grouped by final full-trajectory assignment",

        x =
            "Number of observed visits",

        y =
            "Feature"
    ) +

    theme_minimal(
        base_size = 14
    )


print(
    p_progressive_heatmap
)


# ======================================================================
# FIGURE 3:
# IMPORTANCE TRAJECTORIES
# ======================================================================

p_progressive_lines <- ggplot(

    PROGRESSIVE_DF,

    aes(
        x = Visit,
        y = Importance,
        colour = Feature,
        group = Feature
    )
) +

    geom_line(
        linewidth = 1.1
    ) +

    geom_point(
        size = 2.2
    ) +

    facet_wrap(
        ~ Cluster,
        ncol = 2
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    coord_cartesian(
        ylim = c(
            0,
            1
        )
    ) +

    labs(

        title =
            "Evolution of feature importance",

        subtitle =
            "Unseen patients grouped by final full-trajectory assignment",

        x =
            "Number of observed visits",

        y =
            "Normalized feature importance",

        colour =
            "Feature"
    ) +

    theme_minimal(
        base_size = 14
    ) +

    theme(
        legend.position =
            "bottom"
    )


print(
    p_progressive_lines
)


# ======================================================================
# ======================================================================
#
# PART VIII
#
# INCREMENTAL IMPORTANCE
#
# ======================================================================
# ======================================================================


INCREMENTAL_ALIGNED <- array(

    NA_real_,

    dim = c(
        SET_K,
        ncol(DD),
        N_VISITS
    )
)


for(old_cluster in seq_len(
    SET_K
)) {

    new_cluster <-
        ALIGN_IND$mapping[
            old_cluster
        ]


    INCREMENTAL_ALIGNED[
        new_cluster,
        ,
    ] <-
        IMP$incremental[
            old_cluster,
            ,
        ]
}


INCREMENTAL_DF <- data.frame()


for(k_id in seq_len(
    SET_K
)) {

    for(j in seq_len(
        ncol(DD)
    )) {

        for(H in HORIZONS) {

            INCREMENTAL_DF <- rbind(

                INCREMENTAL_DF,

                data.frame(

                    Cluster =
                        paste0(
                            "Cluster ",
                            k_id
                        ),

                    Feature =
                        colnames(
                            DD
                        )[j],

                    Visit =
                        H,

                    Change =
                        INCREMENTAL_ALIGNED[
                            k_id,
                            j,
                            H
                        ]
                )
            )
        }
    }
}


# ======================================================================
# FIGURE 4:
# INCREMENTAL IMPORTANCE
# ======================================================================

p_incremental <- ggplot(

    INCREMENTAL_DF,

    aes(
        x = Visit,
        y = Feature,
        fill = Change
    )
) +

    geom_tile() +

    facet_wrap(
        ~ Cluster,
        ncol = 2
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    scale_fill_gradient2(

        low =
            "firebrick",

        mid =
            "white",

        high =
            "steelblue",

        midpoint =
            0
    ) +

    labs(

        title =
            "Incremental feature importance",

        subtitle =
            "Change after observing each additional visit",

        x =
            "Newly observed visit",

        y =
            "Feature"
    ) +

    theme_minimal(
        base_size = 14
    )


print(
    p_incremental
)


# ======================================================================
# ======================================================================
#
# PART IX
#
# ASSIGNMENT STABILITY
#
# ======================================================================
# ======================================================================


FINAL_CLUSTER <-
    IMP$final_cluster


STABILITY <- sapply(

    HORIZONS,

    function(H) {

        mean(

            IMP$predicted_cluster[
                ,
                H
            ] ==

            FINAL_CLUSTER
        )
    }
)


STABILITY_DF <- data.frame(

    Visit =
        HORIZONS,

    Stability =
        STABILITY
)


cat("\n")
cat("======================================================================\n")
cat("ASSIGNMENT STABILITY\n")
cat("======================================================================\n")


print(
    STABILITY_DF,
    digits = 3,
    row.names = FALSE
)


# ======================================================================
# FIGURE 5:
# TRANSDUCTIVE vs PROGRESSIVE INDUCTIVE ARI
# ======================================================================

PERFORMANCE_PLOT <- data.frame(

    Visit =
        HORIZONS,

    ARI =
        ARI_PREFIX
)


p_ari <- ggplot(

    PERFORMANCE_PLOT,

    aes(
        x = Visit,
        y = ARI
    )
) +

    geom_hline(

        yintercept =
            ARI_TRANSDUCTIVE,

        linetype =
            "dashed",

        linewidth =
            0.9
    ) +

    geom_line(
        linewidth = 1.1
    ) +

    geom_point(
        size = 3
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    coord_cartesian(
        ylim = c(
            0,
            1
        )
    ) +

    labs(

        title =
            "Transductive vs progressive inductive clustering",

        subtitle =
            paste0(
                "PC1; dashed line = transductive ARI = ",
                sprintf(
                    "%.3f",
                    ARI_TRANSDUCTIVE
                )
            ),

        x =
            "Number of observed visits",

        y =
            "Adjusted Rand Index"
    ) +

    theme_minimal(
        base_size = 14
    )


print(
    p_ari
)


# ======================================================================
# FIGURE 6:
# ASSIGNMENT STABILITY
# ======================================================================

p_stability <- ggplot(

    STABILITY_DF,

    aes(
        x = Visit,
        y = Stability
    )
) +

    geom_line(
        linewidth = 1.1
    ) +

    geom_point(
        size = 3
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    coord_cartesian(
        ylim = c(
            0,
            1
        )
    ) +

    labs(

        title =
            "Stabilization of inductive cluster assignment",

        x =
            "Number of observed visits",

        y =
            "Agreement with final assignment"
    ) +

    theme_minimal(
        base_size = 14
    )


print(
    p_stability
)


# ======================================================================
# ======================================================================
#
# PART X
#
# PATIENT-SPECIFIC IMPORTANCE
#
# ======================================================================
# ======================================================================


EXAMPLE_PATIENT <- 1


PATIENT_DF <- data.frame()


for(j in seq_len(
    ncol(
        DD
    )
)) {

    PATIENT_DF <- rbind(

        PATIENT_DF,

        data.frame(

            Visit =
                HORIZONS,

            Feature =
                colnames(
                    DD
                )[j],

            Importance =
                IMP$patient[
                    EXAMPLE_PATIENT,
                    j,
                ]
        )
    )
}


# ======================================================================
# FIGURE 7:
# PATIENT-SPECIFIC IMPORTANCE
# ======================================================================

p_patient <- ggplot(

    PATIENT_DF,

    aes(
        x = Visit,
        y = Importance,
        colour = Feature,
        group = Feature
    )
) +

    geom_line(
        linewidth = 1.1
    ) +

    geom_point(
        size = 2.2
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    coord_cartesian(
        ylim = c(
            0,
            1
        )
    ) +

    labs(

        title =
            paste0(
                "Patient-specific importance: unseen patient ",
                EXAMPLE_PATIENT
            ),

        subtitle =
            paste0(
                "Final assigned reference cluster = ",
                FINAL_CLUSTER[
                    EXAMPLE_PATIENT
                ]
            ),

        x =
            "Number of observed visits",

        y =
            "Normalized feature importance",

        colour =
            "Feature"
    ) +

    theme_minimal(
        base_size = 14
    ) +

    theme(
        legend.position =
            "bottom"
    )


print(
    p_patient
)


# ======================================================================
# ======================================================================
#
# PART XI
#
# IMPORTANCE DIFFERENCE
#
# ======================================================================
# ======================================================================


IMPORTANCE_DIFFERENCE <-

    IMP_INDUCTIVE_ALIGNED -

    IMP_TRANSDUCTIVE_ALIGNED


cat("\n")
cat("======================================================================\n")
cat("INDUCTIVE - TRANSDUCTIVE IMPORTANCE DIFFERENCE\n")
cat("======================================================================\n")


print(

    round(
        IMPORTANCE_DIFFERENCE,
        3
    )
)


DIFF_DF <- data.frame()


for(k_id in seq_len(
    SET_K
)) {

    for(j in seq_len(
        ncol(
            DD
        )
    )) {

        DIFF_DF <- rbind(

            DIFF_DF,

            data.frame(

                Cluster =
                    paste0(
                        "Cluster ",
                        k_id
                    ),

                Feature =
                    colnames(
                        DD
                    )[j],

                Difference =
                    IMPORTANCE_DIFFERENCE[
                        k_id,
                        j
                    ]
            )
        )
    }
}


# ======================================================================
# FIGURE 8:
# IMPORTANCE DIFFERENCE
# ======================================================================

p_importance_difference <- ggplot(

    DIFF_DF,

    aes(
        x = Feature,
        y = Cluster,
        fill = Difference
    )
) +

    geom_tile() +

    geom_text(

        aes(
            label =
                sprintf(
                    "%+.2f",
                    Difference
                )
        ),

        size = 3.8
    ) +

    scale_fill_gradient2(

        low =
            "firebrick",

        mid =
            "white",

        high =
            "steelblue",

        midpoint =
            0
    ) +

    labs(

        title =
            "Change in feature importance after inductive reformulation",

        subtitle =
            "Inductive minus original transductive importance; PC1",

        x =
            "Feature",

        y =
            "Trajectory cluster"
    ) +

    theme_minimal(
        base_size = 14
    )


print(
    p_importance_difference
)


# ======================================================================
# ======================================================================
#
# FINAL OUTPUT
#
# ======================================================================
# ======================================================================


cat("\n")
cat("======================================================================\n")
cat("FINAL RESULTS\n")
cat("======================================================================\n")


cat(
    "PCA strategy                    : PC1\n"
)


cat(

    sprintf(

        "Transductive ARI               : %.3f\n",

        ARI_TRANSDUCTIVE
    )
)


cat(

    sprintf(

        "Transductive NMI               : %.3f\n",

        NMI_TRANSDUCTIVE
    )
)


cat("\n")


for(H in HORIZONS) {

    cat(

        sprintf(

            "Inductive visit %d ARI        : %.3f\n",

            H,

            ARI_PREFIX[H]
        )
    )
}


cat("\n")


cat(

    sprintf(

        "Full inductive ARI             : %.3f\n",

        ARI_PREFIX[
            N_VISITS
        ]
    )
)


cat(

    sprintf(

        "Full inductive NMI             : %.3f\n",

        NMI_PREFIX[
            N_VISITS
        ]
    )
)


cat(

    sprintf(

        "Inductive-transductive ARI gap : %+.3f\n",

        INDUCTIVE_GAP_ARI
    )
)


cat(

    sprintf(

        "Inductive-transductive NMI gap : %+.3f\n",

        INDUCTIVE_GAP_NMI
    )
)


cat(

    sprintf(

        "Mean importance Spearman       : %.3f\n",

        mean(
            IMPORTANCE_CORRELATION$Spearman,
            na.rm = TRUE
        )
    )
)


cat(
    "\nFinal inductive predicted cluster sizes:\n"
)


print(
    table(
        IMP$final_cluster
    )
)


cat(
    "\nOriginal transductive importance:\n"
)


print(

    round(
        IMP_TRANSDUCTIVE_ALIGNED,
        3
    )
)


cat(
    "\nInductive global importance:\n"
)


print(

    round(
        IMP_INDUCTIVE_ALIGNED,
        3
    )
)


cat(
    "\nImportance correlations:\n"
)


print(
    IMPORTANCE_CORRELATION,
    digits = 3,
    row.names = FALSE
)


cat("\n")
cat("======================================================================\n")
cat("BENCHMARK COMPLETE\n")
cat("======================================================================\n")