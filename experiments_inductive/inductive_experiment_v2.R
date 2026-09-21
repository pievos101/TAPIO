# ======================================================================
# ROBUSTNESS BENCHMARK
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
# Both methods:
#
#   PCA selection = "random_weighted"
#
# Comparison:
#
#   ORIGINAL / TRANSDUCTIVE
#       longTAPIO_trajectories() fitted on ALL subjects
#       performance evaluated on TEST subjects
#
#   INDUCTIVE
#       longTAPIO_inductive() fitted on TRAIN subjects only
#       complete TEST trajectories assigned out-of-sample
#
# IMPORTANT:
#
#   * Same simulated dataset for both methods
#   * Same train/test split
#   * Same eta/sigma condition
#   * Full trajectories used for evaluation
#   * 500 trees
#   * 20 repetitions per eta/sigma condition
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

N_ITER <- 50

TRAIN_FRAC <- 0.70

N_VISITS <- 10

SET_K <- 4

SET_LEVELS <- 4

SET_N_FEATURES <- NaN

SET_N_TREES <- 100

METHOD <- "ward.D2"

SCALE_PCA <- TRUE

REPLACE_FEATURES <- TRUE

PCA_SELECTION <- "random_weighted"


# ======================================================================
# NOISE / SIGNAL GRID
# ======================================================================

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


# ======================================================================
# MASTER SEED
# ======================================================================

MASTER_SEED <- 12345


# ======================================================================
# PRINT CONFIGURATION
# ======================================================================

cat("\n")
cat("======================================================================\n")
cat("longTAPIO ROBUSTNESS BENCHMARK\n")
cat("======================================================================\n")
cat("Original        : longTAPIO_trajectories()\n")
cat("Inductive       : longTAPIO_inductive()\n")
cat("Runs/condition  :", N_ITER, "\n")
cat("Train fraction  :", TRAIN_FRAC, "\n")
cat("Visits          :", N_VISITS, "\n")
cat("Clusters        :", SET_K, "\n")
cat("Trees           :", SET_N_TREES, "\n")
cat("Features/tree   :", SET_N_FEATURES, "\n")
cat("Levels          :", SET_LEVELS, "\n")
cat("PCA selection   :", PCA_SELECTION, "\n")
cat("Eta values      :", paste(ETA_VALUES, collapse = ", "), "\n")
cat("Sigma values    :", paste(SIGMA_VALUES, collapse = ", "), "\n")
cat("Conditions      :", length(ETA_VALUES) * length(SIGMA_VALUES), "\n")
cat("======================================================================\n\n")


# ======================================================================
# RESULT STORAGE
# ======================================================================

RESULTS <- list()

result_counter <- 0L


# ======================================================================
# CONDITION COUNTER
# ======================================================================

condition_counter <- 0L

N_CONDITIONS <-
    length(ETA_VALUES) *
    length(SIGMA_VALUES)


# ======================================================================
# MAIN LOOP
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
        cat("eta   :", eta_value, "\n")
        cat("sigma :", sigma_value, "\n")
        cat("######################################################################\n")


        # ==============================================================
        # RUNS
        # ==============================================================

        for(ii in seq_len(
            N_ITER
        )) {

            cat("\n")

            cat(
                sprintf(
                    "eta = %.2f | sigma = %.2f | run %d/%d\n",
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
            # SIGMA
            # ==========================================================

            r_sigma_diag <- rep(
                sigma_value,
                5
            )


            # ==========================================================
            # SIMULATE DATA
            # ==========================================================

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
            # RESHAPE
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
            # EXPLICIT SUBJECT + TIME ORDER
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
            # TRUE TEST LABELS
            # ==========================================================

            true_test <-
                subject_info$cluster[

                    match(
                        TEST_SUBJECTS,
                        subject_info$subject
                    )
                ]


            # ==========================================================
            # TRAIN DATA FOR INDUCTIVE MODEL
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


            USER_train <- match(

                USER_train_original,

                TRAIN_SUBJECTS
            )


            # ==========================================================
            # TEST DATA FOR INDUCTIVE MODEL
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
            # ORIGINAL / TRANSDUCTIVE longTAPIO
            #
            # ALL subjects are included in fitting.
            #
            # Evaluation is restricted to TEST subjects.
            # ==========================================================

            cat(
                "  Original longTAPIO  : "
            )


            original_seed <-
                MASTER_SEED +
                100000000L +
                eta_id * 1000000L +
                sigma_id * 10000L +
                ii


            set.seed(
                original_seed
            )


            time_original <- system.time({

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
            })


            # ==========================================================
            # ORIGINAL TEST CLUSTERS
            # ==========================================================

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


            # ==========================================================
            # ORIGINAL PERFORMANCE
            # ==========================================================

            ari_original <- ARI(

                true_test,

                original_test_clusters
            )


            nmi_original <- NMI(

                true_test,

                original_test_clusters
            )


            cat(
                sprintf(
                    "ARI = %.3f | NMI = %.3f\n",
                    ari_original,
                    nmi_original
                )
            )


            # ==========================================================
            # INDUCTIVE longTAPIO
            #
            # TRAIN SUBJECTS ONLY.
            # ==========================================================

            cat(
                "  Inductive longTAPIO : "
            )


            inductive_seed <-
                MASTER_SEED +
                200000000L +
                eta_id * 1000000L +
                sigma_id * 10000L +
                ii


            set.seed(
                inductive_seed
            )


            time_inductive <- system.time({

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


                # ======================================================
                # FULL-TRAJECTORY OUT-OF-SAMPLE ASSIGNMENT
                # ======================================================

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
            # INDUCTIVE PERFORMANCE
            # ==========================================================

            ari_inductive <- ARI(

                true_test,

                pred$cluster
            )


            nmi_inductive <- NMI(

                true_test,

                pred$cluster
            )


            margin_inductive <- mean(

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

                    ari_inductive,
                    nmi_inductive,
                    margin_inductive
                )
            )


            # ==========================================================
            # PAIRED DIFFERENCES
            # ==========================================================

            delta_ari <-
                ari_inductive -
                ari_original


            delta_nmi <-
                nmi_inductive -
                nmi_original


            cat(
                sprintf(
                    "  Delta Ind-Orig      : ARI = %+.3f | NMI = %+.3f\n",
                    delta_ari,
                    delta_nmi
                )
            )


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

                ARI_original =
                    ari_original,

                NMI_original =
                    nmi_original,

                ARI_inductive =
                    ari_inductive,

                NMI_inductive =
                    nmi_inductive,

                Margin_inductive =
                    margin_inductive,

                Delta_ARI =
                    delta_ari,

                Delta_NMI =
                    delta_nmi,

                Time_original =
                    unname(
                        time_original["elapsed"]
                    ),

                Time_inductive =
                    unname(
                        time_inductive["elapsed"]
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

            Original_ARI_Mean =
                mean(
                    tmp$ARI_original
                ),

            Original_ARI_SD =
                sd(
                    tmp$ARI_original
                ),

            Inductive_ARI_Mean =
                mean(
                    tmp$ARI_inductive
                ),

            Inductive_ARI_SD =
                sd(
                    tmp$ARI_inductive
                ),

            Delta_ARI_Mean =
                mean(
                    tmp$Delta_ARI
                ),

            Delta_ARI_SD =
                sd(
                    tmp$Delta_ARI
                ),

            Original_NMI_Mean =
                mean(
                    tmp$NMI_original
                ),

            Original_NMI_SD =
                sd(
                    tmp$NMI_original
                ),

            Inductive_NMI_Mean =
                mean(
                    tmp$NMI_inductive
                ),

            Inductive_NMI_SD =
                sd(
                    tmp$NMI_inductive
                ),

            Delta_NMI_Mean =
                mean(
                    tmp$Delta_NMI
                ),

            Delta_NMI_SD =
                sd(
                    tmp$Delta_NMI
                ),

            Mean_Margin =
                mean(
                    tmp$Margin_inductive
                ),

            Mean_Time_Original =
                mean(
                    tmp$Time_original
                ),

            Mean_Time_Inductive =
                mean(
                    tmp$Time_inductive
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
# PRINT SUMMARY
# ======================================================================

cat("\n\n")
cat("======================================================================\n")
cat("FINAL CONDITION-WISE RESULTS\n")
cat("======================================================================\n\n")


print(
    SUMMARY,
    digits = 4,
    row.names = FALSE
)


# ======================================================================
# COMPACT ARI TABLE
# ======================================================================

ARI_TABLE <- data.frame(

    eta =
        SUMMARY$eta,

    sigma =
        SUMMARY$sigma,

    Original =
        sprintf(
            "%.3f +/- %.3f",
            SUMMARY$Original_ARI_Mean,
            SUMMARY$Original_ARI_SD
        ),

    Inductive =
        sprintf(
            "%.3f +/- %.3f",
            SUMMARY$Inductive_ARI_Mean,
            SUMMARY$Inductive_ARI_SD
        ),

    Difference =
        sprintf(
            "%+.3f +/- %.3f",
            SUMMARY$Delta_ARI_Mean,
            SUMMARY$Delta_ARI_SD
        )
)


cat("\n")
cat("======================================================================\n")
cat("ARI TABLE\n")
cat("======================================================================\n\n")


print(
    ARI_TABLE,
    row.names = FALSE
)


# ======================================================================
# PAIRED WILCOXON TESTS BY CONDITION
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
                tmp$Delta_ARI == 0
            )
        ) {

            p_ari <- 1

        } else {

            p_ari <- wilcox.test(

                tmp$ARI_inductive,

                tmp$ARI_original,

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
                tmp$Delta_NMI == 0
            )
        ) {

            p_nmi <- 1

        } else {

            p_nmi <- wilcox.test(

                tmp$NMI_inductive,

                tmp$NMI_original,

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

            Mean_Delta_ARI =
                mean(
                    tmp$Delta_ARI
                ),

            P_ARI =
                p_ari,

            Mean_Delta_NMI =
                mean(
                    tmp$Delta_NMI
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
cat("PAIRED TESTS\n")
cat("======================================================================\n\n")


print(
    TESTS,
    digits = 5,
    row.names = FALSE
)


# ======================================================================
# OVERALL COMPARISON
# ======================================================================

cat("\n")
cat("======================================================================\n")
cat("OVERALL COMPARISON ACROSS ALL CONDITIONS\n")
cat("======================================================================\n")


cat(
    sprintf(
        "Original ARI  : %.3f +/- %.3f\n",
        mean(
            RESULTS$ARI_original
        ),
        sd(
            RESULTS$ARI_original
        )
    )
)


cat(
    sprintf(
        "Inductive ARI : %.3f +/- %.3f\n",
        mean(
            RESULTS$ARI_inductive
        ),
        sd(
            RESULTS$ARI_inductive
        )
    )
)


cat(
    sprintf(
        "Delta ARI     : %+.3f +/- %.3f\n",
        mean(
            RESULTS$Delta_ARI
        ),
        sd(
            RESULTS$Delta_ARI
        )
    )
)


cat(
    sprintf(
        "Original NMI  : %.3f +/- %.3f\n",
        mean(
            RESULTS$NMI_original
        ),
        sd(
            RESULTS$NMI_original
        )
    )
)


cat(
    sprintf(
        "Inductive NMI : %.3f +/- %.3f\n",
        mean(
            RESULTS$NMI_inductive
        ),
        sd(
            RESULTS$NMI_inductive
        )
    )
)


cat(
    sprintf(
        "Delta NMI     : %+.3f +/- %.3f\n",
        mean(
            RESULTS$Delta_NMI
        ),
        sd(
            RESULTS$Delta_NMI
        )
    )
)


# ======================================================================
# LONG FORMAT FOR PLOTS
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
        RESULTS$ARI_original,

    NMI =
        RESULTS$NMI_original
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
        RESULTS$ARI_inductive,

    NMI =
        RESULTS$NMI_inductive
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


PLOT_DATA$Sigma <- factor(
    PLOT_DATA$sigma
)


# ======================================================================
# FIGURE 1:
# ARI vs ETA, FACETED BY SIGMA
# ======================================================================

p_ari <- ggplot(

    PLOT_DATA,

    aes(
        x =
            eta,

        y =
            ARI,

        linetype =
            Method,

        shape =
            Method,

        group =
            Method
    )
) +

    stat_summary(
        fun =
            mean,

        geom =
            "line",

        linewidth =
            1
    ) +

    stat_summary(
        fun =
            mean,

        geom =
            "point",

        size =
            3
    ) +

    facet_wrap(
        ~ Sigma,
        labeller =
            label_both
    ) +

    scale_x_continuous(
        breaks =
            ETA_VALUES
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

    theme_minimal() +

    theme(

        text =
            element_text(
                size = 14
            ),

        legend.position =
            "bottom"
    )


print(
    p_ari
)


# ======================================================================
# FIGURE 2:
# NMI vs ETA, FACETED BY SIGMA
# ======================================================================

p_nmi <- ggplot(

    PLOT_DATA,

    aes(
        x =
            eta,

        y =
            NMI,

        linetype =
            Method,

        shape =
            Method,

        group =
            Method
    )
) +

    stat_summary(
        fun =
            mean,

        geom =
            "line",

        linewidth =
            1
    ) +

    stat_summary(
        fun =
            mean,

        geom =
            "point",

        size =
            3
    ) +

    facet_wrap(
        ~ Sigma,
        labeller =
            label_both
    ) +

    scale_x_continuous(
        breaks =
            ETA_VALUES
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
        "Normalized Mutual Information"
    ) +

    theme_minimal() +

    theme(

        text =
            element_text(
                size = 14
            ),

        legend.position =
            "bottom"
    )


print(
    p_nmi
)


# ======================================================================
# FIGURE 3:
# HEATMAP OF INDUCTIVE - ORIGINAL ARI
# ======================================================================

DELTA_PLOT <- data.frame(

    eta =
        SUMMARY$eta,

    sigma =
        SUMMARY$sigma,

    Delta =
        SUMMARY$Delta_ARI_Mean
)


p_delta <- ggplot(

    DELTA_PLOT,

    aes(
        x =
            factor(
                sigma
            ),

        y =
            factor(
                eta
            ),

        fill =
            Delta
    )
) +

    geom_tile() +

    geom_text(

        aes(
            label =
                sprintf(
                    "%+.3f",
                    Delta
                )
        ),

        size =
            5
    ) +

    xlab(
        "Sigma"
    ) +

    ylab(
        "Eta"
    ) +

    labs(

        fill =
            expression(
                Delta * " ARI"
            ),

        title =
            "Inductive - Original longTAPIO"
    ) +

    theme_minimal() +

    theme(
        text =
            element_text(
                size = 14
            )
    )


print(
    p_delta
)

# ======================================================================
# FIGURE 1:
# ARI -- NESTED BOXPLOTS
#
# Within each ETA:
#   Original vs Inductive
#
# Separate panels:
#   Sigma = 3, 5, 7
# ======================================================================

PLOT_DATA$Eta <- factor(
    PLOT_DATA$eta,
    levels = ETA_VALUES
)

PLOT_DATA$Sigma <- factor(
    PLOT_DATA$sigma,
    levels = SIGMA_VALUES
)


p_ari <- ggplot(

    PLOT_DATA,

    aes(
        x = Eta,
        y = ARI,
        fill = Method
    )

) +

    geom_boxplot(

        position = position_dodge(
            width = 0.8
        ),

        width = 0.65,

        outlier.size = 1.5
    ) +

    facet_wrap(
        ~ Sigma,
        nrow = 1,
        labeller = label_both
    ) +

    coord_cartesian(
        ylim = c(
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
        fill = "Method"
    ) +

    theme_minimal() +

    theme(

        text = element_text(
            size = 14
        ),

        legend.position = "bottom",

        panel.grid.minor = element_blank()
    )


print(
    p_ari
)


# ======================================================================
# FIGURE 2:
# NMI -- NESTED BOXPLOTS
#
# Within each ETA:
#   Original vs Inductive
#
# Separate panels:
#   Sigma = 3, 5, 7
# ======================================================================

p_nmi <- ggplot(

    PLOT_DATA,

    aes(
        x = Eta,
        y = NMI,
        fill = Method
    )

) +

    geom_boxplot(

        position = position_dodge(
            width = 0.8
        ),

        width = 0.65,

        outlier.size = 1.5
    ) +

    facet_wrap(
        ~ Sigma,
        nrow = 1,
        labeller = label_both
    ) +

    coord_cartesian(
        ylim = c(
            0,
            1
        )
    ) +

    xlab(
        "Eta"
    ) +

    ylab(
        "Normalized Mutual Information"
    ) +

    labs(
        fill = "Method"
    ) +

    theme_minimal() +

    theme(

        text = element_text(
            size = 14
        ),

        legend.position = "bottom",

        panel.grid.minor = element_blank()
    )


print(
    p_nmi
)

# ======================================================================
# SAVE RESULTS
# ======================================================================

write.csv(

    RESULTS,

    "longTAPIO_original_vs_inductive_noise_results.csv",

    row.names =
        FALSE
)


write.csv(

    SUMMARY,

    "longTAPIO_original_vs_inductive_noise_summary.csv",

    row.names =
        FALSE
)


write.csv(

    ARI_TABLE,

    "longTAPIO_original_vs_inductive_noise_ARI_table.csv",

    row.names =
        FALSE
)


write.csv(

    TESTS,

    "longTAPIO_original_vs_inductive_noise_tests.csv",

    row.names =
        FALSE
)


# ======================================================================
# COMPLETE
# ======================================================================

cat("\n")
cat("======================================================================\n")
cat("BENCHMARK COMPLETE\n")
cat("======================================================================\n")