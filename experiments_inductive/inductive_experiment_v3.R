# ======================================================================
# DIAGNOSTIC BENCHMARK:
#
# SAME TRAINING DATA
#
# longTAPIO_trajectories()
#           vs
# longTAPIO_inductive()
#
# Across:
#
#   eta   = 3, 5, 7
#   sigma = 3, 5, 7
#
# PCA selection:
#
#   random_weighted
#
# PURPOSE
# -------
#
# Both methods are fitted on EXACTLY THE SAME TRAINING SUBJECTS.
#
# We evaluate:
#
#   1. Original longTAPIO TRAIN ARI / NMI
#   2. Inductive longTAPIO TRAIN ARI / NMI
#   3. Agreement between the two TRAIN clusterings
#   4. Inductive TEST ARI / NMI
#   5. Inductive TEST assignment margin
#
# This diagnostic separates:
#
#   * differences in the learned clustering structure
#
# from
#
#   * the out-of-sample assignment problem.
#
# IMPORTANT:
#
# The original longTAPIO method is NOT used to predict the held-out
# patients here because it does not provide the inductive assignment
# mechanism that is being investigated.
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
# CHECK FUNCTIONS
# ======================================================================

if(!exists("longTAPIO_trajectories")) {

    stop(
        "longTAPIO_trajectories() is not available."
    )
}


if(!exists("longTAPIO_inductive")) {

    stop(
        paste0(
            "longTAPIO_inductive() is not available. ",
            "Source the canonical inductive implementation first."
        )
    )
}


if(!exists("predict.inductiveLongTAPIO")) {

    stop(
        paste0(
            "predict.inductiveLongTAPIO() is not available. ",
            "Source the canonical inductive implementation first."
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

PCA_SELECTION <- "random_weighted"


ETA_VALUES <- c(
    5,
    10,
    15
)

SIGMA_VALUES <- c(
    3,
    5,
    7
)


MASTER_SEED <- 12345


# ======================================================================
# PRINT CONFIGURATION
# ======================================================================

cat("\n")
cat("======================================================================\n")
cat("SAME-TRAINING-DATA longTAPIO DIAGNOSTIC\n")
cat("======================================================================\n")
cat("Runs/condition :", N_ITER, "\n")
cat("Train fraction :", TRAIN_FRAC, "\n")
cat("Visits         :", N_VISITS, "\n")
cat("Clusters       :", SET_K, "\n")
cat("Trees          :", SET_N_TREES, "\n")
cat("Features/tree  :", SET_N_FEATURES, "\n")
cat("Levels         :", SET_LEVELS, "\n")
cat("PCA selection  :", PCA_SELECTION, "\n")
cat("Eta            :", paste(ETA_VALUES, collapse = ", "), "\n")
cat("Sigma          :", paste(SIGMA_VALUES, collapse = ", "), "\n")
cat("======================================================================\n\n")


# ======================================================================
# RESULT STORAGE
# ======================================================================

RESULTS <- list()

result_counter <- 0L


N_CONDITIONS <-
    length(ETA_VALUES) *
    length(SIGMA_VALUES)


condition_counter <- 0L


# ======================================================================
# MAIN BENCHMARK
# ======================================================================

for(eta_id in seq_along(
    ETA_VALUES
)) {

    eta_value <-
        ETA_VALUES[
            eta_id
        ]


    for(sigma_id in seq_along(
        SIGMA_VALUES
    )) {

        sigma_value <-
            SIGMA_VALUES[
                sigma_id
            ]


        condition_counter <-
            condition_counter + 1L


        cat("\n\n")
        cat("######################################################################\n")

        cat(
            "CONDITION",
            condition_counter,
            "/",
            N_CONDITIONS,
            "\n"
        )

        cat("######################################################################\n")
        cat("eta   =", eta_value, "\n")
        cat("sigma =", sigma_value, "\n")
        cat("######################################################################\n")


        # ==============================================================
        # REPETITIONS
        # ==============================================================

        for(ii in seq_len(
            N_ITER
        )) {

            cat("\n")

            cat(
                sprintf(
                    "eta = %d | sigma = %d | run %d/%d\n",
                    eta_value,
                    sigma_value,
                    ii,
                    N_ITER
                )
            )

            cat(
                "------------------------------------------------------------\n"
            )


            # ==========================================================
            # DATA SEED
            # ==========================================================

            data_seed <-
                MASTER_SEED +
                eta_id * 1000000L +
                sigma_id * 10000L +
                ii


            set.seed(
                data_seed
            )


            # ==========================================================
            # SIMULATE DATA
            # ==========================================================

            r_sigma_diag <- rep(
                sigma_value,
                5
            )


            Longdat2 <- simLongData(

                ranTimes =
                    FALSE,

                n_i =
                    N_VISITS,

                eta =
                    eta_value,

                sigma_diag =
                    r_sigma_diag
            )


            # ==========================================================
            # RESHAPE LONG -> WIDE
            # ==========================================================

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


            # ==========================================================
            # EXPLICIT SUBJECT / TIME ORDER
            # ==========================================================

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


            # ==========================================================
            # FEATURE MATRIX
            # ==========================================================

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


            # ==========================================================
            # TRUE SUBJECT CLUSTERS
            # ==========================================================

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


            # ==========================================================
            # TRAIN / TEST SPLIT
            #
            # The SAME split is used for all comparisons within this run.
            # ==========================================================

            split_seed <-
                MASTER_SEED +
                50000000L +
                eta_id * 1000000L +
                sigma_id * 10000L +
                ii


            set.seed(
                split_seed
            )


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


            # ==========================================================
            # TRUE TRAIN / TEST LABELS
            # ==========================================================

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


            # ==========================================================
            # TRAIN DATA
            # ==========================================================

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


            # ----------------------------------------------------------
            # Consecutive IDs
            # ----------------------------------------------------------

            USER_train <- match(

                USER_train_original,

                TRAIN_SUBJECTS
            )


            # ==========================================================
            # TEST DATA
            # ==========================================================

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


            # ==========================================================
            # IMPORTANT:
            #
            # SAME FITTING SEED FOR BOTH METHODS
            #
            # This makes stochastic feature / PCA sampling as directly
            # comparable as possible.
            # ==========================================================

            fit_seed <-
                MASTER_SEED +
                100000000L +
                eta_id * 1000000L +
                sigma_id * 10000L +
                ii


            # ==========================================================
            # ORIGINAL longTAPIO
            #
            # FIT ONLY ON TRAINING SUBJECTS
            # ==========================================================

            cat(
                "  Original TRAIN  : "
            )


            set.seed(
                fit_seed
            )


            time_original <- system.time({

                original_train <- longTAPIO_trajectories(

                    DD_train,

                    k =
                        SET_K,

                    user_id =
                        USER_train,

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
            })


            # ==========================================================
            # ORIGINAL TRAIN CLUSTERS
            #
            # USER_train IDs are 1 ... n_train, so sorted unique IDs
            # correspond directly to TRAIN_SUBJECTS.
            # ==========================================================

            original_train_subjects <- sort(
                unique(
                    USER_train
                )
            )


            original_train_clusters <-
                original_train$cl[

                    match(
                        seq_along(
                            TRAIN_SUBJECTS
                        ),

                        original_train_subjects
                    )
                ]


            # ==========================================================
            # ORIGINAL TRAIN PERFORMANCE
            # ==========================================================

            ari_original_train <- ARI(

                true_train,

                original_train_clusters
            )


            nmi_original_train <- NMI(

                true_train,

                original_train_clusters
            )


            cat(
                sprintf(
                    "ARI = %.3f | NMI = %.3f\n",
                    ari_original_train,
                    nmi_original_train
                )
            )


            # ==========================================================
            # INDUCTIVE longTAPIO
            #
            # EXACT SAME TRAINING DATA
            # EXACT SAME FITTING SEED
            # ==========================================================

            cat(
                "  Inductive TRAIN : "
            )


            set.seed(
                fit_seed
            )


            time_inductive_fit <- system.time({

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
            })


            # ==========================================================
            # EXTRACT INDUCTIVE TRAIN CLUSTERS
            #
            # Canonical implementation stores the final reference
            # clustering in model$train_clusters.
            # ==========================================================

            if(
                !is.null(
                    model$train_clusters
                )
            ) {

                inductive_train_clusters <-
                    model$train_clusters

            } else if(
                !is.null(
                    model$cluster
                )
            ) {

                inductive_train_clusters <-
                    model$cluster

            } else {

                stop(
                    paste0(
                        "Could not find training clusters in the ",
                        "inductive model. Expected model$train_clusters ",
                        "or model$cluster."
                    )
                )
            }


            # ==========================================================
            # SAFETY CHECK
            # ==========================================================

            if(
                length(
                    inductive_train_clusters
                ) !=
                length(
                    true_train
                )
            ) {

                stop(
                    paste0(
                        "Length mismatch for inductive training clusters: ",
                        length(inductive_train_clusters),
                        " vs ",
                        length(true_train),
                        "."
                    )
                )
            }


            # ==========================================================
            # INDUCTIVE TRAIN PERFORMANCE
            # ==========================================================

            ari_inductive_train <- ARI(

                true_train,

                inductive_train_clusters
            )


            nmi_inductive_train <- NMI(

                true_train,

                inductive_train_clusters
            )


            cat(
                sprintf(
                    "ARI = %.3f | NMI = %.3f\n",
                    ari_inductive_train,
                    nmi_inductive_train
                )
            )


            # ==========================================================
            # AGREEMENT BETWEEN ORIGINAL AND INDUCTIVE TRAIN CLUSTERINGS
            #
            # ARI/NMI are label permutation invariant.
            # ==========================================================

            ari_between_methods <- ARI(

                original_train_clusters,

                inductive_train_clusters
            )


            nmi_between_methods <- NMI(

                original_train_clusters,

                inductive_train_clusters
            )


            cat(
                sprintf(
                    paste0(
                        "  Agreement       : ",
                        "ARI = %.3f | NMI = %.3f\n"
                    ),

                    ari_between_methods,
                    nmi_between_methods
                )
            )


            # ==========================================================
            # DIFFERENCE IN TRAIN PERFORMANCE
            # ==========================================================

            delta_train_ari <-
                ari_inductive_train -
                ari_original_train


            delta_train_nmi <-
                nmi_inductive_train -
                nmi_original_train


            cat(
                sprintf(
                    paste0(
                        "  Delta TRAIN     : ",
                        "ARI = %+.3f | NMI = %+.3f\n"
                    ),

                    delta_train_ari,
                    delta_train_nmi
                )
            )


            # ==========================================================
            # INDUCTIVE TEST PREDICTION
            #
            # Complete trajectories.
            # No refitting.
            # ==========================================================

            cat(
                "  Inductive TEST  : "
            )


            time_inductive_predict <- system.time({

                pred <- predict(

                    model,

                    newdata =
                        DD_test,

                    user_id =
                        USER_test,

                    visits =
                        N_VISITS
                )
            })


            # ==========================================================
            # TEST PERFORMANCE
            # ==========================================================

            ari_inductive_test <- ARI(

                true_test,

                pred$cluster
            )


            nmi_inductive_test <- NMI(

                true_test,

                pred$cluster
            )


            mean_margin <- mean(

                pred$margin,

                na.rm =
                    TRUE
            )


            cat(
                sprintf(
                    paste0(
                        "ARI = %.3f | ",
                        "NMI = %.3f | ",
                        "margin = %.3f\n"
                    ),

                    ari_inductive_test,
                    nmi_inductive_test,
                    mean_margin
                )
            )


            # ==========================================================
            # GENERALIZATION GAP
            #
            # Not a direct overfitting estimate because train/test
            # assignments are produced differently, but useful as a
            # descriptive diagnostic.
            # ==========================================================

            inductive_generalization_gap_ari <-
                ari_inductive_test -
                ari_inductive_train


            inductive_generalization_gap_nmi <-
                nmi_inductive_test -
                nmi_inductive_train


            # ==========================================================
            # STORE RESULTS
            # ==========================================================

            result_counter <-
                result_counter + 1L


            RESULTS[[result_counter]] <- data.frame(

                eta =
                    eta_value,

                sigma =
                    sigma_value,

                run =
                    ii,

                Original_Train_ARI =
                    ari_original_train,

                Original_Train_NMI =
                    nmi_original_train,

                Inductive_Train_ARI =
                    ari_inductive_train,

                Inductive_Train_NMI =
                    nmi_inductive_train,

                Between_Methods_ARI =
                    ari_between_methods,

                Between_Methods_NMI =
                    nmi_between_methods,

                Delta_Train_ARI =
                    delta_train_ari,

                Delta_Train_NMI =
                    delta_train_nmi,

                Inductive_Test_ARI =
                    ari_inductive_test,

                Inductive_Test_NMI =
                    nmi_inductive_test,

                Inductive_Test_Margin =
                    mean_margin,

                Test_Minus_Train_ARI =
                    inductive_generalization_gap_ari,

                Test_Minus_Train_NMI =
                    inductive_generalization_gap_nmi,

                Time_Original =
                    unname(
                        time_original["elapsed"]
                    ),

                Time_Inductive_Fit =
                    unname(
                        time_inductive_fit["elapsed"]
                    ),

                Time_Inductive_Predict =
                    unname(
                        time_inductive_predict["elapsed"]
                    ),

                stringsAsFactors =
                    FALSE
            )
        }
    }
}


# ======================================================================
# COMBINE RESULTS
# ======================================================================

RESULTS <- do.call(
    rbind,
    RESULTS
)


rownames(
    RESULTS
) <- NULL


# ======================================================================
# CONDITION-WISE SUMMARY
# ======================================================================

SUMMARY_LIST <- list()

summary_counter <- 0L


for(eta_value in ETA_VALUES) {

    for(sigma_value in SIGMA_VALUES) {

        tmp <-
            RESULTS[
                RESULTS$eta ==
                    eta_value &
                RESULTS$sigma ==
                    sigma_value,
                ,
                drop = FALSE
            ]


        summary_counter <-
            summary_counter + 1L


        SUMMARY_LIST[[summary_counter]] <- data.frame(

            eta =
                eta_value,

            sigma =
                sigma_value,

            # ----------------------------------------------------------
            # Original training
            # ----------------------------------------------------------

            Original_Train_ARI_Mean =
                mean(
                    tmp$Original_Train_ARI
                ),

            Original_Train_ARI_SD =
                sd(
                    tmp$Original_Train_ARI
                ),

            # ----------------------------------------------------------
            # Inductive training
            # ----------------------------------------------------------

            Inductive_Train_ARI_Mean =
                mean(
                    tmp$Inductive_Train_ARI
                ),

            Inductive_Train_ARI_SD =
                sd(
                    tmp$Inductive_Train_ARI
                ),

            # ----------------------------------------------------------
            # Difference
            # ----------------------------------------------------------

            Delta_Train_ARI_Mean =
                mean(
                    tmp$Delta_Train_ARI
                ),

            Delta_Train_ARI_SD =
                sd(
                    tmp$Delta_Train_ARI
                ),

            # ----------------------------------------------------------
            # Direct agreement between methods
            # ----------------------------------------------------------

            Between_Methods_ARI_Mean =
                mean(
                    tmp$Between_Methods_ARI
                ),

            Between_Methods_ARI_SD =
                sd(
                    tmp$Between_Methods_ARI
                ),

            # ----------------------------------------------------------
            # Test performance
            # ----------------------------------------------------------

            Inductive_Test_ARI_Mean =
                mean(
                    tmp$Inductive_Test_ARI
                ),

            Inductive_Test_ARI_SD =
                sd(
                    tmp$Inductive_Test_ARI
                ),

            # ----------------------------------------------------------
            # NMI
            # ----------------------------------------------------------

            Original_Train_NMI_Mean =
                mean(
                    tmp$Original_Train_NMI
                ),

            Original_Train_NMI_SD =
                sd(
                    tmp$Original_Train_NMI
                ),

            Inductive_Train_NMI_Mean =
                mean(
                    tmp$Inductive_Train_NMI
                ),

            Inductive_Train_NMI_SD =
                sd(
                    tmp$Inductive_Train_NMI
                ),

            Between_Methods_NMI_Mean =
                mean(
                    tmp$Between_Methods_NMI
                ),

            Between_Methods_NMI_SD =
                sd(
                    tmp$Between_Methods_NMI
                ),

            Inductive_Test_NMI_Mean =
                mean(
                    tmp$Inductive_Test_NMI
                ),

            Inductive_Test_NMI_SD =
                sd(
                    tmp$Inductive_Test_NMI
                ),

            # ----------------------------------------------------------
            # Margin
            # ----------------------------------------------------------

            Inductive_Test_Margin_Mean =
                mean(
                    tmp$Inductive_Test_Margin
                ),

            Inductive_Test_Margin_SD =
                sd(
                    tmp$Inductive_Test_Margin
                )
        )
    }
}


SUMMARY <- do.call(
    rbind,
    SUMMARY_LIST
)


rownames(
    SUMMARY
) <- NULL


# ======================================================================
# PRINT FULL SUMMARY
# ======================================================================

cat("\n\n")
cat("======================================================================\n")
cat("FINAL CONDITION-WISE SUMMARY\n")
cat("======================================================================\n\n")


print(
    SUMMARY,
    digits = 4,
    row.names = FALSE
)


# ======================================================================
# COMPACT DIAGNOSTIC TABLE
# ======================================================================

DIAGNOSTIC_TABLE <- data.frame(

    eta =
        SUMMARY$eta,

    sigma =
        SUMMARY$sigma,

    Original_Train =
        sprintf(
            "%.3f +/- %.3f",
            SUMMARY$Original_Train_ARI_Mean,
            SUMMARY$Original_Train_ARI_SD
        ),

    Inductive_Train =
        sprintf(
            "%.3f +/- %.3f",
            SUMMARY$Inductive_Train_ARI_Mean,
            SUMMARY$Inductive_Train_ARI_SD
        ),

    Delta_Train =
        sprintf(
            "%+.3f +/- %.3f",
            SUMMARY$Delta_Train_ARI_Mean,
            SUMMARY$Delta_Train_ARI_SD
        ),

    Method_Agreement =
        sprintf(
            "%.3f +/- %.3f",
            SUMMARY$Between_Methods_ARI_Mean,
            SUMMARY$Between_Methods_ARI_SD
        ),

    Inductive_Test =
        sprintf(
            "%.3f +/- %.3f",
            SUMMARY$Inductive_Test_ARI_Mean,
            SUMMARY$Inductive_Test_ARI_SD
        )
)


cat("\n")
cat("======================================================================\n")
cat("COMPACT ARI DIAGNOSTIC\n")
cat("======================================================================\n\n")


print(
    DIAGNOSTIC_TABLE,
    row.names = FALSE
)


# ======================================================================
# PAIRED WILCOXON:
#
# ORIGINAL TRAIN vs INDUCTIVE TRAIN
#
# SAME DATA / SAME RUN
# ======================================================================

TEST_LIST <- list()

test_counter <- 0L


for(eta_value in ETA_VALUES) {

    for(sigma_value in SIGMA_VALUES) {

        tmp <-
            RESULTS[
                RESULTS$eta ==
                    eta_value &
                RESULTS$sigma ==
                    sigma_value,
                ,
                drop = FALSE
            ]


        # ==============================================================
        # ARI
        # ==============================================================

        if(
            all(
                abs(
                    tmp$Delta_Train_ARI
                ) < .Machine$double.eps^0.5
            )
        ) {

            p_ari <- 1

        } else {

            p_ari <- wilcox.test(

                tmp$Inductive_Train_ARI,

                tmp$Original_Train_ARI,

                paired =
                    TRUE,

                exact =
                    FALSE
            )$p.value
        }


        # ==============================================================
        # NMI
        # ==============================================================

        if(
            all(
                abs(
                    tmp$Delta_Train_NMI
                ) < .Machine$double.eps^0.5
            )
        ) {

            p_nmi <- 1

        } else {

            p_nmi <- wilcox.test(

                tmp$Inductive_Train_NMI,

                tmp$Original_Train_NMI,

                paired =
                    TRUE,

                exact =
                    FALSE
            )$p.value
        }


        test_counter <-
            test_counter + 1L


        TEST_LIST[[test_counter]] <- data.frame(

            eta =
                eta_value,

            sigma =
                sigma_value,

            Mean_Delta_Train_ARI =
                mean(
                    tmp$Delta_Train_ARI
                ),

            P_ARI =
                p_ari,

            Mean_Delta_Train_NMI =
                mean(
                    tmp$Delta_Train_NMI
                ),

            P_NMI =
                p_nmi
        )
    }
}


TESTS <- do.call(
    rbind,
    TEST_LIST
)


# ======================================================================
# MULTIPLE-TEST CORRECTION
# ======================================================================

TESTS$P_ARI_BH <- p.adjust(

    TESTS$P_ARI,

    method =
        "BH"
)


TESTS$P_NMI_BH <- p.adjust(

    TESTS$P_NMI,

    method =
        "BH"
)


cat("\n")
cat("======================================================================\n")
cat("PAIRED ORIGINAL-TRAIN vs INDUCTIVE-TRAIN TESTS\n")
cat("======================================================================\n\n")


print(
    TESTS,
    digits = 5,
    row.names = FALSE
)


# ======================================================================
# LONG-FORM TRAINING DATA FOR PLOTS
# ======================================================================

PLOT_ORIGINAL <- data.frame(

    eta =
        RESULTS$eta,

    sigma =
        RESULTS$sigma,

    run =
        RESULTS$run,

    Method =
        "Original",

    ARI =
        RESULTS$Original_Train_ARI,

    NMI =
        RESULTS$Original_Train_NMI
)


PLOT_INDUCTIVE <- data.frame(

    eta =
        RESULTS$eta,

    sigma =
        RESULTS$sigma,

    run =
        RESULTS$run,

    Method =
        "Inductive",

    ARI =
        RESULTS$Inductive_Train_ARI,

    NMI =
        RESULTS$Inductive_Train_NMI
)


PLOT_DATA <- rbind(

    PLOT_ORIGINAL,

    PLOT_INDUCTIVE
)


PLOT_DATA$Method <- factor(

    PLOT_DATA$Method,

    levels = c(
        "Original",
        "Inductive"
    )
)


PLOT_DATA$Eta <- factor(

    PLOT_DATA$eta,

    levels =
        ETA_VALUES
)


PLOT_DATA$Sigma <- factor(

    PLOT_DATA$sigma,

    levels =
        SIGMA_VALUES
)


# ======================================================================
# FIGURE 1:
#
# TRAINING ARI
# ORIGINAL vs INDUCTIVE
#
# Both fitted to exactly the same subjects.
# ======================================================================

p_train_ari <- ggplot(

    PLOT_DATA,

    aes(
        x =
            Eta,

        y =
            ARI,

        fill =
            Method
    )
) +

    geom_boxplot(

        position =
            position_dodge(
                width = 0.8
            ),

        width =
            0.65,

        outlier.size =
            1.5
    ) +

    facet_wrap(

        ~ Sigma,

        nrow =
            1,

        labeller =
            label_both
    ) +

    coord_cartesian(

        ylim =
            c(
                0,
                1
            )
    ) +

    xlab(
        "Eta"
    ) +

    ylab(
        "Training Adjusted Rand Index"
    ) +

    labs(
        fill =
            "Method"
    ) +

    theme_minimal() +

    theme(

        text =
            element_text(
                size = 14
            ),

        legend.position =
            "bottom",

        panel.grid.minor =
            element_blank()
    )


print(
    p_train_ari
)


# ======================================================================
# FIGURE 2:
#
# TRAINING NMI
# ORIGINAL vs INDUCTIVE
# ======================================================================

p_train_nmi <- ggplot(

    PLOT_DATA,

    aes(
        x =
            Eta,

        y =
            NMI,

        fill =
            Method
    )
) +

    geom_boxplot(

        position =
            position_dodge(
                width = 0.8
            ),

        width =
            0.65,

        outlier.size =
            1.5
    ) +

    facet_wrap(

        ~ Sigma,

        nrow =
            1,

        labeller =
            label_both
    ) +

    coord_cartesian(

        ylim =
            c(
                0,
                1
            )
    ) +

    xlab(
        "Eta"
    ) +

    ylab(
        "Training Normalized Mutual Information"
    ) +

    labs(
        fill =
            "Method"
    ) +

    theme_minimal() +

    theme(

        text =
            element_text(
                size = 14
            ),

        legend.position =
            "bottom",

        panel.grid.minor =
            element_blank()
    )


print(
    p_train_nmi
)


# ======================================================================
# FIGURE 3:
#
# AGREEMENT BETWEEN ORIGINAL AND INDUCTIVE TRAIN CLUSTERINGS
#
# ARI = 1 means exactly the same partition, up to label permutation.
# ======================================================================

AGREEMENT_DATA <- data.frame(

    eta =
        factor(
            RESULTS$eta,
            levels = ETA_VALUES
        ),

    sigma =
        factor(
            RESULTS$sigma,
            levels = SIGMA_VALUES
        ),

    Agreement_ARI =
        RESULTS$Between_Methods_ARI
)


p_agreement <- ggplot(

    AGREEMENT_DATA,

    aes(
        x =
            eta,

        y =
            Agreement_ARI
    )
) +

    geom_boxplot(

        width =
            0.65,

        outlier.size =
            1.5
    ) +

    facet_wrap(

        ~ sigma,

        nrow =
            1,

        labeller =
            label_both
    ) +

    coord_cartesian(

        ylim =
            c(
                0,
                1
            )
    ) +

    xlab(
        "Eta"
    ) +

    ylab(
        "ARI between learned clusterings"
    ) +

    theme_minimal() +

    theme(

        text =
            element_text(
                size = 14
            ),

        panel.grid.minor =
            element_blank()
    )


print(
    p_agreement
)


# ======================================================================
# FIGURE 4:
#
# INDUCTIVE TRAIN vs TEST
#
# This is NOT the same comparison as Original vs Inductive.
#
# It shows how the frozen inductive model performs on unseen subjects.
# ======================================================================

TRAIN_TEST_PLOT <- rbind(

    data.frame(

        eta =
            RESULTS$eta,

        sigma =
            RESULTS$sigma,

        run =
            RESULTS$run,

        Dataset =
            "Train",

        ARI =
            RESULTS$Inductive_Train_ARI
    ),

    data.frame(

        eta =
            RESULTS$eta,

        sigma =
            RESULTS$sigma,

        run =
            RESULTS$run,

        Dataset =
            "Test",

        ARI =
            RESULTS$Inductive_Test_ARI
    )
)


TRAIN_TEST_PLOT$Dataset <- factor(

    TRAIN_TEST_PLOT$Dataset,

    levels = c(
        "Train",
        "Test"
    )
)


TRAIN_TEST_PLOT$Eta <- factor(

    TRAIN_TEST_PLOT$eta,

    levels =
        ETA_VALUES
)


TRAIN_TEST_PLOT$Sigma <- factor(

    TRAIN_TEST_PLOT$sigma,

    levels =
        SIGMA_VALUES
)


p_train_test <- ggplot(

    TRAIN_TEST_PLOT,

    aes(
        x =
            Eta,

        y =
            ARI,

        fill =
            Dataset
    )
) +

    geom_boxplot(

        position =
            position_dodge(
                width = 0.8
            ),

        width =
            0.65,

        outlier.size =
            1.5
    ) +

    facet_wrap(

        ~ Sigma,

        nrow =
            1,

        labeller =
            label_both
    ) +

    coord_cartesian(

        ylim =
            c(
                0,
                1
            )
    ) +

    xlab(
        "Eta"
    ) +

    ylab(
        "Adjusted Rand Index"
    ) +

    labs(
        fill =
            "Dataset"
    ) +

    theme_minimal() +

    theme(

        text =
            element_text(
                size = 14
            ),

        legend.position =
            "bottom",

        panel.grid.minor =
            element_blank()
    )


print(
    p_train_test
)


# ======================================================================
# FIGURE 5:
#
# DELTA TRAIN ARI:
#
# Inductive - Original
#
# If this remains around zero, both fitting procedures themselves are
# producing similarly accurate training clusterings.
# ======================================================================

DELTA_DATA <- data.frame(

    eta =
        factor(
            RESULTS$eta,
            levels = ETA_VALUES
        ),

    sigma =
        factor(
            RESULTS$sigma,
            levels = SIGMA_VALUES
        ),

    Delta =
        RESULTS$Delta_Train_ARI
)


p_delta <- ggplot(

    DELTA_DATA,

    aes(
        x =
            eta,

        y =
            Delta
    )
) +

    geom_hline(

        yintercept =
            0,

        linetype =
            "dashed"
    ) +

    geom_boxplot(

        width =
            0.65,

        outlier.size =
            1.5
    ) +

    facet_wrap(

        ~ sigma,

        nrow =
            1,

        labeller =
            label_both
    ) +

    xlab(
        "Eta"
    ) +

    ylab(
        "Training ARI: Inductive - Original"
    ) +

    theme_minimal() +

    theme(

        text =
            element_text(
                size = 14
            ),

        panel.grid.minor =
            element_blank()
    )


print(
    p_delta
)


# ======================================================================
# SAVE RESULTS
# ======================================================================

write.csv(

    RESULTS,

    "longTAPIO_same_training_diagnostic_results.csv",

    row.names =
        FALSE
)


write.csv(

    SUMMARY,

    "longTAPIO_same_training_diagnostic_summary.csv",

    row.names =
        FALSE
)


write.csv(

    DIAGNOSTIC_TABLE,

    "longTAPIO_same_training_diagnostic_table.csv",

    row.names =
        FALSE
)


write.csv(

    TESTS,

    "longTAPIO_same_training_diagnostic_tests.csv",

    row.names =
        FALSE
)


# ======================================================================
# COMPLETE
# ======================================================================

cat("\n")
cat("======================================================================\n")
cat("DIAGNOSTIC COMPLETE\n")
cat("======================================================================\n")