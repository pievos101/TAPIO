# ======================================================================
# ONE-RUN INDUCTIVE longTAPIO BENCHMARK
#
# Progressive clustering + progressive feature importance
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
# IMPORTANT
#
# If these functions are not yet part of the installed TAPIO package,
# source them here.
# ======================================================================

# source("R/TAPIO_inductive.R")
# source("R/longTAPIO_inductive.R")
# source("R/importance_TAPIO_inductive.R")
# source("R/importance_longTAPIO_inductive.R")


# ======================================================================
# CONFIGURATION
# ======================================================================

TRAIN_FRAC <- 0.70

N_VISITS <- 10

SET_K <- 4

SET_LEVELS <- 4

SET_N_FEATURES <- 5

SET_N_TREES <- 500

METHOD <- "ward.D2"

SCALE_PCA <- TRUE

REPLACE_FEATURES <- TRUE

HORIZONS <- seq_len(
    N_VISITS
)

SEED <- 12345


set.seed(
    SEED
)


cat("\n")
cat("======================================================================\n")
cat("longTAPIO INDUCTIVE / PROGRESSIVE IMPORTANCE BENCHMARK\n")
cat("======================================================================\n")
cat("Runs            : 1\n")
cat("Train fraction  :", TRAIN_FRAC, "\n")
cat("Visits          :", N_VISITS, "\n")
cat("Clusters        :", SET_K, "\n")
cat("Trees           :", SET_N_TREES, "\n")
cat("Features/tree   :", SET_N_FEATURES, "\n")
cat("Levels          :", SET_LEVELS, "\n")
cat("PCA selection   : random_weighted\n")
cat("======================================================================\n\n")


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


#r_sigma_diag[id] <- sample(
#    3:20,
#    1
#)


cat(
    "sigma_diag:",
    paste(
        r_sigma_diag,
        collapse = " "
    ),
    "\n"
)


cat(
    "Outcome with modified variance:",
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
# SUBJECT + TIME ORDER
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
        colnames(DD),
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


n_subjects <-
    length(
        ALL_SUBJECTS
    )


cat(
    "Number of subjects:",
    n_subjects,
    "\n"
)


# ======================================================================
# TRAIN / TEST SPLIT
# ======================================================================

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


cat(
    "Training subjects:",
    length(TRAIN_SUBJECTS),
    "\n"
)


cat(
    "Test subjects    :",
    length(TEST_SUBJECTS),
    "\n\n"
)


# ======================================================================
# TRUE TEST LABELS
# ======================================================================

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
# FIT INDUCTIVE longTAPIO
# ======================================================================

cat("\n")
cat("======================================================================\n")
cat("FITTING INDUCTIVE longTAPIO\n")
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
        REPLACE_FEATURES
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
    "Example feature contribution from tree 1:\n"
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
# PROGRESSIVE PREDICTION
# ======================================================================

cat("\n")
cat("======================================================================\n")
cat("PROGRESSIVE PREDICTION\n")
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
        na.rm = TRUE
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
# FEATURE IMPORTANCE
# ======================================================================

cat("\n")
cat("======================================================================\n")
cat("CALCULATING PROGRESSIVE FEATURE IMPORTANCE\n")
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
# GLOBAL IMPORTANCE
# ======================================================================

cat("\n")
cat("======================================================================\n")
cat("GLOBAL CLUSTER-SPECIFIC IMPORTANCE\n")
cat("======================================================================\n")


print(
    round(
        IMP$global,
        3
    )
)


# ======================================================================
# GLOBAL IMPORTANCE DATA FRAME
# ======================================================================

GLOBAL_DF <- data.frame()


for(k_id in seq_len(
    SET_K
)) {

    for(j in seq_len(
        ncol(DD)
    )) {

        GLOBAL_DF <- rbind(

            GLOBAL_DF,

            data.frame(

                Cluster =
                    paste0(
                        "Cluster ",
                        k_id
                    ),

                Feature =
                    colnames(DD)[j],

                Importance =
                    IMP$global[
                        k_id,
                        j
                    ]
            )
        )
    }
}


# ======================================================================
# FIGURE 1:
# GLOBAL IMPORTANCE HEATMAP
# ======================================================================

p_global <- ggplot(

    GLOBAL_DF,

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

        size = 4
    ) +

    scale_fill_gradient(

        low =
            "white",

        high =
            "steelblue",

        limits =
            c(0, 1)
    ) +

    labs(

        title =
            "Global cluster-specific feature importance",

        x =
            "Feature",

        y =
            "Trajectory cluster"
    ) +

    theme_minimal(
        base_size = 14
    )


print(
    p_global
)


# ======================================================================
# PROGRESSIVE IMPORTANCE DATA FRAME
# ======================================================================

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
                        colnames(DD)[j],

                    Visit =
                        H,

                    Importance =
                        IMP$cluster_progressive[
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
            c(0, 1)
    ) +

    labs(

        title =
            "Progressive feature importance",

        subtitle =
            "Patients grouped by final full-trajectory assignment",

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
        ylim = c(0, 1)
    ) +

    labs(

        title =
            "Evolution of feature importance",

        subtitle =
            "Patients grouped by final full-trajectory assignment",

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
# INCREMENTAL IMPORTANCE DATA
# ======================================================================

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
                        colnames(DD)[j],

                    Visit =
                        H,

                    Change =
                        IMP$incremental[
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
# ASSIGNMENT STABILITY
#
# Agreement of prefix assignment with the patient's final
# full-trajectory assignment.
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
# PROGRESSIVE ARI
# ======================================================================

p_ari <- ggplot(

    PERFORMANCE,

    aes(
        x = Visit,
        y = ARI
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
        ylim = c(0, 1)
    ) +

    labs(

        title =
            "Progressive trajectory assignment",

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
        ylim = c(0, 1)
    ) +

    labs(

        title =
            "Stabilization of cluster assignment",

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
# EXAMPLE PATIENT
# ======================================================================

EXAMPLE_PATIENT <- 1


PATIENT_DF <- data.frame()


for(j in seq_len(
    ncol(DD)
)) {

    PATIENT_DF <- rbind(

        PATIENT_DF,

        data.frame(

            Visit =
                HORIZONS,

            Feature =
                colnames(DD)[j],

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
        ylim = c(0, 1)
    ) +

    labs(

        title =
            paste0(
                "Patient-specific importance: patient ",
                EXAMPLE_PATIENT
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
# FINAL OUTPUT
# ======================================================================

cat("\n")
cat("======================================================================\n")
cat("FINAL RESULTS\n")
cat("======================================================================\n")


cat(
    sprintf(
        "Visit 1 ARI         : %.3f\n",
        ARI_PREFIX[1]
    )
)


cat(
    sprintf(
        "Visit 5 ARI         : %.3f\n",
        ARI_PREFIX[5]
    )
)


cat(
    sprintf(
        "Full trajectory ARI : %.3f\n",
        ARI_PREFIX[N_VISITS]
    )
)


cat(
    sprintf(
        "Full trajectory NMI : %.3f\n",
        NMI_PREFIX[N_VISITS]
    )
)


cat("\nFinal predicted cluster sizes:\n")


print(
    table(
        IMP$final_cluster
    )
)


cat("\nGlobal feature importance:\n")


print(
    round(
        IMP$global,
        3
    )
)


cat("\n")
cat("======================================================================\n")
cat("BENCHMARK COMPLETE\n")
cat("======================================================================\n")