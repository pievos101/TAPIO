# ======================================================================
# BENCHMARK:
# ORIGINAL longTAPIO vs INDUCTIVE / PROGRESSIVE longTAPIO
#
# IMPORTANT:
#
# This benchmark uses the canonical implementation:
#
#     longTAPIO_inductive()
#     predict.inductiveLongTAPIO()
#
# These functions must already be loaded/sourced before running
# this benchmark.
#
# PCA selection can be:
#
#     "first"
#     "random_weighted"
#     "random_weighted_95"
#
# Main questions:
#
# 1. How does full-trajectory inductive longTAPIO compare with the
#    original transductive longTAPIO?
#
# 2. How early can inductive longTAPIO assign an unseen patient
#    to the correct trajectory phenotype?
#
# Original:
#
#   ALL patients
#       -> original longTAPIO
#       -> evaluate TEST patients
#
# Inductive:
#
#   TRAIN patients with complete trajectories
#       -> fit longTAPIO_inductive() ONCE
#
#   TEST patient:
#
#       visit 1       -> prediction
#       visits 1:2    -> prediction
#       visits 1:3    -> prediction
#       ...
#       visits 1:T    -> prediction
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
# CHECK THAT CANONICAL INDUCTIVE IMPLEMENTATION IS AVAILABLE
# ======================================================================

if(!exists("longTAPIO_inductive")) {

    stop(
        paste0(
            "longTAPIO_inductive() is not available. ",
            "Source the canonical inductive longTAPIO implementation ",
            "before running this benchmark."
        )
    )
}


if(!exists("predict.inductiveLongTAPIO")) {

    stop(
        paste0(
            "predict.inductiveLongTAPIO() is not available. ",
            "Source the canonical inductive longTAPIO implementation ",
            "before running this benchmark."
        )
    )
}


# ======================================================================
# CONFIGURATION
# ======================================================================

N_ITER <- 20

TRAIN_FRAC <- 0.70

N_VISITS <- 10

SET_K <- 4

SET_LEVELS <- 4

SET_N_FEATURES <- NaN

SET_N_TREES <- 500

METHOD <- "ward.D2"

SCALE_PCA <- TRUE

REPLACE_FEATURES <- TRUE


# ----------------------------------------------------------------------
# PCA selection:
#
# "first"
#     -> always PC1
#
# "random_weighted"
#     -> randomly sample one PC from all PCs with probability
#        proportional to explained variance
#
# "random_weighted_95"
#     -> retain the smallest set of leading PCs explaining at least
#        95% of variance and randomly sample one PC from this set
#        proportional to explained variance
# ----------------------------------------------------------------------

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
cat("PCA selection   :", PCA_SELECTION, "\n")
cat("======================================================================\n\n")


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


MARGIN_PREFIX <- matrix(
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
    MARGIN_PREFIX
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
    # ORIGINAL SIMULATION SETTING
    # ==================================================================

    r_eta <- 3


    r_sigma_diag <- rep(
        5,
        5
    )


    # --------------------------------------------------------------
    # Optional heterogeneous variance setting
    # --------------------------------------------------------------

    # id <- sample(
    #     1:5,
    #     1
    # )
    #
    # r_sigma_diag[
    #     id
    # ] <- sample(
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
    #
    # This is important because longTAPIO assumes that the within-patient
    # row order corresponds to longitudinal visit order.
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


    # --------------------------------------------------------------
    # Consecutive training-patient IDs
    # --------------------------------------------------------------

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


    # --------------------------------------------------------------
    # Consecutive test-patient IDs
    # --------------------------------------------------------------

    USER_test <- match(

        USER_test_original,

        TEST_SUBJECTS
    )


    # ==================================================================
    # ORIGINAL TRANSDUCTIVE longTAPIO
    #
    # PCA selection controlled by PCA_SELECTION.
    #
    # The original method receives ALL subjects.
    # Evaluation is restricted to TEST subjects.
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
    # FIT CANONICAL INDUCTIVE longTAPIO ONCE
    #
    # IMPORTANT:
    #
    # This now uses the actual longTAPIO_inductive() implementation.
    # There is no benchmark-specific duplicate fitting function.
    #
    # Only TRAIN patients are used.
    # ==================================================================

    cat(
        "\nFitting inductive longTAPIO...\n"
    )


    set.seed(
        200000 + ii
    )


    model <- longTAPIO_inductive(

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
    #
    # Uses the canonical S3 method:
    #
    #     predict.inductiveLongTAPIO()
    #
    # For H = 1, only visit 1 is used.
    # For H = 2, visits 1:2 are used.
    # ...
    # For H = N_VISITS, the complete test trajectory is used.
    #
    # The reference model is NEVER refitted.
    # ==================================================================

    cat("\n")
    cat("Progressive prediction:\n")
    cat("------------------------------------------------------------\n")


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
        # MEAN ASSIGNMENT MARGIN
        #
        # This is NOT a probability.
        #
        # margin =
        #     best cluster affinity score
        #     -
        #     second-best cluster affinity score
        # ==============================================================

        margin_h <- mean(

            pred$margin,

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


        MARGIN_PREFIX[
            ii,
            H
        ] <- margin_h


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

                margin_h
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
cat("FINAL RESULTS --", N_ITER, "RUNS\n")
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
            MARGIN_PREFIX
        ),

    SD_Margin =
        apply(
            MARGIN_PREFIX,
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
    # Original full-trajectory transductive reference
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
# ASSIGNMENT MARGIN
# ======================================================================

PLOT_MARGIN <- data.frame(

    Visit =
        HORIZONS,

    Mean =
        colMeans(
            MARGIN_PREFIX
        ),

    SD =
        apply(
            MARGIN_PREFIX,
            2,
            sd
        )
)


p_margin <- ggplot(

    PLOT_MARGIN,

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
    p_margin
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

    MARGIN_PREFIX,

    "inductive_longTAPIO_progressive_margin.csv",

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