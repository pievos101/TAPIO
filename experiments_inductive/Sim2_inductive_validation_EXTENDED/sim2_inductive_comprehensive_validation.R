# =============================================================================
# INDUCTIVE longTAPIO
#
# ROBUSTNESS + PROGRESSIVE PREDICTION + CONFIDENCE BENCHMARK
#
# PURPOSE
# -------
#
# This experiment addresses three questions:
#
# 1. PERFORMANCE PRESERVATION
#
#    Does inductive longTAPIO retain the clustering performance of the
#    original transductive longTAPIO as the simulation becomes harder?
#
#
# 2. PROGRESSIVE ASSIGNMENT
#
#    How many longitudinal visits are required before unseen patients can
#    be reliably assigned?
#
#
# 3. CONFIDENCE / RELIABILITY
#
#    Does the inductive assignment margin identify predictions that are
#    more likely to be correct and/or stable?
#
#
# DESIGN
# ------
#
# Exact TAPIO synthetic generator:
#
#       TAPIO::simLongData()
#
# Noise:
#
#       eta = 3, 5, 10
#
# Subject heterogeneity:
#
#       one outcome receives sigma = 3, 5, or 10
#
# Remaining outcomes:
#
#       sigma = 3
#
# Replications:
#
#       20 per condition
#
# Total:
#
#       3 x 3 x 20 = 180 simulated datasets
#
#
# IMPORTANT
# ---------
#
# Transductive:
#
#       all 200 subjects -> longTAPIO_trajectories()
#
# Inductive:
#
#       140 training subjects -> longTAPIO_inductive()
#        60 unseen subjects  -> predict()
#
# Both methods are evaluated on the SAME 60 test subjects.
#
# =============================================================================


# =============================================================================
# 1. PACKAGES
# =============================================================================

library(TAPIO)
library(MASS)
library(aricode)
library(reshape)
library(fastcluster)
library(ggplot2)


# =============================================================================
# 2. CHECK FUNCTIONS
# =============================================================================

if(!exists("longTAPIO_trajectories")) {
    stop("longTAPIO_trajectories() is not available.")
}


if(!exists("longTAPIO_inductive")) {
    stop(
        paste0(
            "longTAPIO_inductive() is not available. ",
            "Load/source the inductive implementation before running ",
            "this benchmark."
        )
    )
}


# =============================================================================
# 3. GLOBAL SETTINGS
# =============================================================================

N_RUNS <- 20

N_TOTAL <- 200

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


# =============================================================================
# 4. ROBUSTNESS CONDITIONS
# =============================================================================

ETA_VALUES <- c(
    3,
    5,
    10
)


SIGMA_VALUES <- c(
    3,
    5,
    10
)


HORIZONS <- seq_len(
    N_VISITS
)


# =============================================================================
# 5. EQUIVALENCE MARGIN
# =============================================================================

EQUIVALENCE_MARGIN <- 0.05

N_BOOT <- 5000


# =============================================================================
# 6. PREPARE SIMULATED DATA
# =============================================================================

prepare_simulation <- function(Longdat2) {

    Longdat2 <- Longdat2[
        order(
            Longdat2$subject,
            Longdat2$time,
            Longdat2$outcome
        ),
        ,
        drop = FALSE
    ]


    rownames(
        Longdat2
    ) <- NULL


    subject_info <- Longdat2[
        !duplicated(
            Longdat2$subject
        ),
        c(
            "subject",
            "cluster"
        ),
        drop = FALSE
    ]


    subject_info <- subject_info[
        order(
            subject_info$subject
        ),
        ,
        drop = FALSE
    ]


    rownames(
        subject_info
    ) <- NULL


    Longdat2_wide <- reshape(
        Longdat2,
        idvar = c(
            "subject",
            "time",
            "cluster"
        ),
        timevar = "outcome",
        direction = "wide"
    )


    Longdat2_wide <- Longdat2_wide[
        order(
            Longdat2_wide$subject,
            Longdat2_wide$time
        ),
        ,
        drop = FALSE
    ]


    rownames(
        Longdat2_wide
    ) <- NULL


    FEATURE_NAMES <- paste0(
        "y.",
        seq_len(
            SET_N_FEATURES
        )
    )


    if(!all(FEATURE_NAMES %in% colnames(Longdat2_wide))) {

        stop(
            paste0(
                "Expected feature columns missing. Found: ",
                paste(
                    colnames(Longdat2_wide),
                    collapse = ", "
                )
            )
        )
    }


    DD <- as.matrix(
        Longdat2_wide[
            ,
            FEATURE_NAMES,
            drop = FALSE
        ]
    )


    storage.mode(
        DD
    ) <- "double"


    USER_ID <- Longdat2_wide$subject


    if(anyNA(DD)) {
        stop("DD contains NA values.")
    }


    if(!is.numeric(DD)) {
        stop("DD is not numeric.")
    }


    list(
        DD = DD,
        USER_ID = USER_ID,
        subject_info = subject_info,
        wide = Longdat2_wide
    )
}


# =============================================================================
# 7. STRATIFIED TRAIN/TEST SPLIT
# =============================================================================

make_stratified_split <- function(
    subject_info,
    train_frac,
    seed
) {

    set.seed(
        seed
    )


    TRAIN_SUBJECTS <- integer(0)


    for(k in sort(unique(subject_info$cluster))) {

        IDs <- subject_info$subject[
            subject_info$cluster == k
        ]


        n_train_k <- floor(
            length(IDs) *
            train_frac
        )


        TRAIN_SUBJECTS <- c(
            TRAIN_SUBJECTS,
            sample(
                IDs,
                size = n_train_k,
                replace = FALSE
            )
        )
    }


    TRAIN_SUBJECTS <- sort(
        TRAIN_SUBJECTS
    )


    TEST_SUBJECTS <- sort(
        setdiff(
            subject_info$subject,
            TRAIN_SUBJECTS
        )
    )


    list(
        TRAIN = TRAIN_SUBJECTS,
        TEST = TEST_SUBJECTS
    )
}


# =============================================================================
# 8. BOOTSTRAP CI
# =============================================================================

bootstrap_mean_ci <- function(
    x,
    B = 5000,
    seed = 1
) {

    x <- x[
        is.finite(x)
    ]


    set.seed(
        seed
    )


    n <- length(
        x
    )


    BOOT <- numeric(
        B
    )


    for(b in seq_len(B)) {

        BOOT[b] <- mean(
            sample(
                x,
                size = n,
                replace = TRUE
            )
        )
    }


    as.numeric(
        quantile(
            BOOT,
            probs = c(
                0.025,
                0.975
            )
        )
    )
}


# =============================================================================
# 9. SAFE ARI
# =============================================================================

safe_ari <- function(
    x,
    y
) {

    if(
        length(x) != length(y) ||
        length(x) < 2
    ) {
        return(NA_real_)
    }


    aricode::ARI(
        x,
        y
    )
}


# =============================================================================
# 10. SAFE NMI
# =============================================================================

safe_nmi <- function(
    x,
    y
) {

    if(
        length(x) != length(y) ||
        length(x) < 2
    ) {
        return(NA_real_)
    }


    aricode::NMI(
        x,
        y
    )
}


# =============================================================================
# 11. STORAGE
# =============================================================================

FULL_RESULTS <- data.frame()


PROGRESSIVE_RESULTS <- data.frame()


PATIENT_RESULTS <- data.frame()


# =============================================================================
# 12. HEADER
# =============================================================================

cat("\n")
cat("==============================================================================================================\n")
cat("INDUCTIVE longTAPIO ROBUSTNESS BENCHMARK\n")
cat("==============================================================================================================\n")

cat(
    "Noise levels     : ",
    paste(
        ETA_VALUES,
        collapse = ", "
    ),
    "\n",
    sep = ""
)

cat(
    "Sigma levels     : ",
    paste(
        SIGMA_VALUES,
        collapse = ", "
    ),
    "\n",
    sep = ""
)

cat(
    "Runs/condition   : ",
    N_RUNS,
    "\n",
    sep = ""
)

cat(
    "Total datasets   : ",
    length(ETA_VALUES) *
    length(SIGMA_VALUES) *
    N_RUNS,
    "\n",
    sep = ""
)

cat(
    "Patients         : ",
    N_TOTAL,
    "\n",
    sep = ""
)

cat(
    "Train/Test       : ",
    round(N_TOTAL * TRAIN_FRAC),
    "/",
    N_TOTAL - round(N_TOTAL * TRAIN_FRAC),
    "\n",
    sep = ""
)

cat(
    "Visits           : ",
    N_VISITS,
    "\n",
    sep = ""
)

cat(
    "Clusters         : ",
    SET_K,
    "\n",
    sep = ""
)

cat(
    "Trees            : ",
    SET_N_TREES,
    "\n",
    sep = ""
)

cat(
    "PCA selection    : ",
    PCA_SELECTION,
    "\n",
    sep = ""
)

cat("==============================================================================================================\n")


# =============================================================================
# 13. MAIN BENCHMARK
# =============================================================================

GLOBAL_RUN <- 0


for(ETA_NOW in ETA_VALUES) {

    for(SIGMA_NOW in SIGMA_VALUES) {

        cat("\n\n")
        cat("##############################################################################################################\n")

        cat(
            "CONDITION: eta = ",
            ETA_NOW,
            " | sigma = ",
            SIGMA_NOW,
            "\n",
            sep = ""
        )

        cat("##############################################################################################################\n")


        for(ii in seq_len(N_RUNS)) {

            GLOBAL_RUN <- GLOBAL_RUN + 1


            cat("\n")
            cat("--------------------------------------------------------------------------------------------------------------\n")

            cat(
                "RUN ",
                ii,
                "/",
                N_RUNS,
                " | eta=",
                ETA_NOW,
                " | sigma=",
                SIGMA_NOW,
                "\n",
                sep = ""
            )

            cat("--------------------------------------------------------------------------------------------------------------\n")


            # =================================================================
            # 13.1 SIMULATION SEED
            # =================================================================

            SIM_SEED <-
                100000 +
                ETA_NOW * 1000 +
                SIGMA_NOW * 100 +
                ii


            set.seed(
                SIM_SEED
            )


            # =================================================================
            # 13.2 SUBJECT HETEROGENEITY
            #
            # One randomly chosen outcome receives SIGMA_NOW.
            #
            # Remaining outcomes have sigma = 3.
            #
            # For SIGMA_NOW = 3 this is simply:
            #
            #     c(3,3,3,3,3)
            # =================================================================

            r_sigma_diag <- rep(
                3,
                SET_N_FEATURES
            )


            sigma_id <- sample(
                seq_len(
                    SET_N_FEATURES
                ),
                size = 1
            )


            r_sigma_diag[sigma_id] <-
                SIGMA_NOW


            cat(
                "sigma_diag: ",
                paste(
                    r_sigma_diag,
                    collapse = " "
                ),
                "\n",
                sep = ""
            )


            # =================================================================
            # 13.3 EXACT TAPIO GENERATOR
            # =================================================================

            Longdat2 <- TAPIO::simLongData(
                n_total = N_TOTAL,
                K = SET_K,
                outcomes = SET_N_FEATURES,
                eta = ETA_NOW,
                cluster_sizes = rep(
                    N_TOTAL / SET_K,
                    SET_K
                ),
                ranTimes = FALSE,
                n_i = N_VISITS,
                sigma_diag = r_sigma_diag
            )


            # =================================================================
            # 13.4 PREPARE
            # =================================================================

            PREP <- prepare_simulation(
                Longdat2
            )


            DD <- PREP$DD

            USER_ID <- PREP$USER_ID

            subject_info <- PREP$subject_info


            # =================================================================
            # 13.5 CHECKS
            # =================================================================

            if(
                length(
                    unique(
                        USER_ID
                    )
                ) != N_TOTAL
            ) {

                stop(
                    "Unexpected number of subjects."
                )
            }


            if(
                any(
                    table(USER_ID) !=
                    N_VISITS
                )
            ) {

                stop(
                    "Unexpected number of visits."
                )
            }


            # =================================================================
            # 13.6 TRAIN/TEST SPLIT
            # =================================================================

            SPLIT <- make_stratified_split(
                subject_info = subject_info,
                train_frac = TRAIN_FRAC,
                seed = 500000 + GLOBAL_RUN
            )


            TRAIN_SUBJECTS <- SPLIT$TRAIN

            TEST_SUBJECTS <- SPLIT$TEST


            true_test <- subject_info$cluster[
                match(
                    TEST_SUBJECTS,
                    subject_info$subject
                )
            ]


            cat(
                "Train/Test: ",
                length(TRAIN_SUBJECTS),
                "/",
                length(TEST_SUBJECTS),
                "\n",
                sep = ""
            )


            # =================================================================
            # 13.7 TRANSDUCTIVE MODEL
            #
            # ALL subjects are supplied.
            # =================================================================

            cat(
                "Transductive..."
            )


            set.seed(
                600000 + GLOBAL_RUN
            )


            t0 <- proc.time()[3]


            original <- longTAPIO_trajectories(
                DD,
                k = SET_K,
                user_id = USER_ID,
                levels = SET_LEVELS,
                verbose = FALSE,
                n_trees = SET_N_TREES,
                method = METHOD,
                n_features = SET_N_FEATURES,
                do.leveling = TRUE,
                scale = SCALE_PCA,
                replace = REPLACE_FEATURES,
                pca_selection = PCA_SELECTION
            )


            TIME_TRANS <-
                proc.time()[3] -
                t0


            original_subjects <- sort(
                unique(
                    USER_ID
                )
            )


            original_test_clusters <- original$cl[
                match(
                    TEST_SUBJECTS,
                    original_subjects
                )
            ]


            ARI_TRANS <- safe_ari(
                true_test,
                original_test_clusters
            )


            NMI_TRANS <- safe_nmi(
                true_test,
                original_test_clusters
            )


            cat(
                sprintf(
                    " ARI=%.3f NMI=%.3f\n",
                    ARI_TRANS,
                    NMI_TRANS
                )
            )


            # =================================================================
            # 13.8 TRAINING DATA
            # =================================================================

            TRAIN_ROWS <-
                USER_ID %in%
                TRAIN_SUBJECTS


            DD_train <- DD[
                TRAIN_ROWS,
                ,
                drop = FALSE
            ]


            USER_train_original <- USER_ID[
                TRAIN_ROWS
            ]


            USER_train <- match(
                USER_train_original,
                TRAIN_SUBJECTS
            )


            # =================================================================
            # 13.9 TEST DATA
            # =================================================================

            TEST_ROWS <-
                USER_ID %in%
                TEST_SUBJECTS


            DD_test <- DD[
                TEST_ROWS,
                ,
                drop = FALSE
            ]


            USER_test_original <- USER_ID[
                TEST_ROWS
            ]


            USER_test <- match(
                USER_test_original,
                TEST_SUBJECTS
            )


            # =================================================================
            # 13.10 FIT INDUCTIVE MODEL
            # =================================================================

            cat(
                "Inductive training..."
            )


            set.seed(
                700000 + GLOBAL_RUN
            )


            t0 <- proc.time()[3]


            model <- longTAPIO_inductive(
                DD_train,
                USER_train,
                k = SET_K,
                n_features = SET_N_FEATURES,
                n_trees = SET_N_TREES,
                levels = SET_LEVELS,
                method = METHOD,
                scale = SCALE_PCA,
                replace = REPLACE_FEATURES,
                pca_selection = PCA_SELECTION
            )


            TIME_IND_FIT <-
                proc.time()[3] -
                t0


            cat(
                sprintf(
                    " %.2fs\n",
                    TIME_IND_FIT
                )
            )


            # =================================================================
            # 13.11 FINAL FULL-TRAJECTORY PREDICTION
            #
            # This is performed FIRST because all earlier predictions will be
            # compared with the final 10-visit assignment.
            # =================================================================

            FINAL_PRED <- predict(
                model,
                newdata = DD_test,
                user_id = USER_test,
                visits = N_VISITS
            )


            FINAL_CLUSTER <-
                FINAL_PRED$cluster


            ARI_IND_FINAL <- safe_ari(
                true_test,
                FINAL_CLUSTER
            )


            NMI_IND_FINAL <- safe_nmi(
                true_test,
                FINAL_CLUSTER
            )


            # =================================================================
            # 13.12 STORE FULL-TRAJECTORY RESULT
            # =================================================================

            FULL_RESULTS <- rbind(
                FULL_RESULTS,
                data.frame(
                    Global_run = GLOBAL_RUN,
                    Eta = ETA_NOW,
                    Sigma = SIGMA_NOW,
                    Sigma_variable = sigma_id,
                    Replicate = ii,

                    ARI_transductive = ARI_TRANS,
                    ARI_inductive = ARI_IND_FINAL,
                    Delta_ARI =
                        ARI_IND_FINAL -
                        ARI_TRANS,

                    NMI_transductive = NMI_TRANS,
                    NMI_inductive = NMI_IND_FINAL,
                    Delta_NMI =
                        NMI_IND_FINAL -
                        NMI_TRANS,

                    Time_transductive = TIME_TRANS,
                    Time_inductive_fit = TIME_IND_FIT
                )
            )


            cat(
                sprintf(
                    paste0(
                        "Full trajectory: ",
                        "Trans ARI=%.3f | ",
                        "Ind ARI=%.3f | ",
                        "Delta=%+.3f\n"
                    ),
                    ARI_TRANS,
                    ARI_IND_FINAL,
                    ARI_IND_FINAL - ARI_TRANS
                )
            )


            # =================================================================
            # 13.13 PROGRESSIVE PREDICTION
            #
            # Visits:
            #
            #     1, 2, ..., 10
            #
            # At every horizon:
            #
            #     ARI vs TRUE cluster
            #     NMI vs TRUE cluster
            #     ARI vs FINAL 10-visit assignment
            #     NMI vs FINAL 10-visit assignment
            #     exact agreement with final assignment
            #     mean assignment margin
            #
            # =================================================================

            for(H in HORIZONS) {

                PRED_H <- predict(
                    model,
                    newdata = DD_test,
                    user_id = USER_test,
                    visits = H
                )


                CLUSTER_H <-
                    PRED_H$cluster


                # -------------------------------------------------------------
                # Accuracy against known simulation truth
                # -------------------------------------------------------------

                ARI_TRUE_H <- safe_ari(
                    true_test,
                    CLUSTER_H
                )


                NMI_TRUE_H <- safe_nmi(
                    true_test,
                    CLUSTER_H
                )


                # -------------------------------------------------------------
                # Stability relative to final 10-visit assignment
                # -------------------------------------------------------------

                ARI_FINAL_H <- safe_ari(
                    FINAL_CLUSTER,
                    CLUSTER_H
                )


                NMI_FINAL_H <- safe_nmi(
                    FINAL_CLUSTER,
                    CLUSTER_H
                )


                AGREEMENT_FINAL_H <- mean(
                    CLUSTER_H ==
                    FINAL_CLUSTER
                )


                # -------------------------------------------------------------
                # Margin
                # -------------------------------------------------------------

                if(!is.null(PRED_H$margin)) {

                    MARGIN_H <-
                        PRED_H$margin

                } else {

                    MARGIN_H <- rep(
                        NA_real_,
                        length(CLUSTER_H)
                    )
                }


                MEAN_MARGIN_H <- mean(
                    MARGIN_H,
                    na.rm = TRUE
                )


                # -------------------------------------------------------------
                # Correctness against truth
                # -------------------------------------------------------------

                CORRECT_H <-
                    CLUSTER_H ==
                    true_test


                # -------------------------------------------------------------
                # Already equal to eventual full-trajectory assignment
                # -------------------------------------------------------------

                FINAL_MATCH_H <-
                    CLUSTER_H ==
                    FINAL_CLUSTER


                # -------------------------------------------------------------
                # Mean margin correct vs incorrect
                # -------------------------------------------------------------

                if(
                    any(
                        CORRECT_H,
                        na.rm = TRUE
                    )
                ) {

                    MARGIN_CORRECT <- mean(
                        MARGIN_H[
                            CORRECT_H
                        ],
                        na.rm = TRUE
                    )

                } else {

                    MARGIN_CORRECT <-
                        NA_real_
                }


                if(
                    any(
                        !CORRECT_H,
                        na.rm = TRUE
                    )
                ) {

                    MARGIN_INCORRECT <- mean(
                        MARGIN_H[
                            !CORRECT_H
                        ],
                        na.rm = TRUE
                    )

                } else {

                    MARGIN_INCORRECT <-
                        NA_real_
                }


                # -------------------------------------------------------------
                # Mean margin final-match vs later-different
                # -------------------------------------------------------------

                if(
                    any(
                        FINAL_MATCH_H,
                        na.rm = TRUE
                    )
                ) {

                    MARGIN_FINAL_MATCH <- mean(
                        MARGIN_H[
                            FINAL_MATCH_H
                        ],
                        na.rm = TRUE
                    )

                } else {

                    MARGIN_FINAL_MATCH <-
                        NA_real_
                }


                if(
                    any(
                        !FINAL_MATCH_H,
                        na.rm = TRUE
                    )
                ) {

                    MARGIN_FINAL_DIFFERENT <- mean(
                        MARGIN_H[
                            !FINAL_MATCH_H
                        ],
                        na.rm = TRUE
                    )

                } else {

                    MARGIN_FINAL_DIFFERENT <-
                        NA_real_
                }


                # -------------------------------------------------------------
                # Store horizon-level result
                # -------------------------------------------------------------

                PROGRESSIVE_RESULTS <- rbind(
                    PROGRESSIVE_RESULTS,
                    data.frame(
                        Global_run = GLOBAL_RUN,
                        Eta = ETA_NOW,
                        Sigma = SIGMA_NOW,
                        Replicate = ii,
                        Visit = H,

                        ARI_truth = ARI_TRUE_H,
                        NMI_truth = NMI_TRUE_H,

                        ARI_final = ARI_FINAL_H,
                        NMI_final = NMI_FINAL_H,

                        Agreement_final =
                            AGREEMENT_FINAL_H,

                        Mean_margin =
                            MEAN_MARGIN_H,

                        Margin_correct =
                            MARGIN_CORRECT,

                        Margin_incorrect =
                            MARGIN_INCORRECT,

                        Margin_final_match =
                            MARGIN_FINAL_MATCH,

                        Margin_final_different =
                            MARGIN_FINAL_DIFFERENT
                    )
                )


                # -------------------------------------------------------------
                # Patient-level data
                #
                # Required for proper confidence/reliability analysis.
                # -------------------------------------------------------------

                PATIENT_RESULTS <- rbind(
                    PATIENT_RESULTS,
                    data.frame(
                        Global_run = GLOBAL_RUN,
                        Eta = ETA_NOW,
                        Sigma = SIGMA_NOW,
                        Replicate = ii,
                        Visit = H,

                        Patient =
                            TEST_SUBJECTS,

                        True_cluster =
                            true_test,

                        Predicted_cluster =
                            CLUSTER_H,

                        Final_cluster =
                            FINAL_CLUSTER,

                        Correct =
                            as.integer(
                                CORRECT_H
                            ),

                        Final_match =
                            as.integer(
                                FINAL_MATCH_H
                            ),

                        Margin =
                            MARGIN_H
                    )
                )
            }


            cat(
                sprintf(
                    paste0(
                        "Progressive: ",
                        "V1 ARI=%.3f | ",
                        "V5 ARI=%.3f | ",
                        "V10 ARI=%.3f\n"
                    ),
                    PROGRESSIVE_RESULTS$ARI_truth[
                        PROGRESSIVE_RESULTS$Global_run ==
                        GLOBAL_RUN &
                        PROGRESSIVE_RESULTS$Visit ==
                        1
                    ],
                    PROGRESSIVE_RESULTS$ARI_truth[
                        PROGRESSIVE_RESULTS$Global_run ==
                        GLOBAL_RUN &
                        PROGRESSIVE_RESULTS$Visit ==
                        5
                    ],
                    PROGRESSIVE_RESULTS$ARI_truth[
                        PROGRESSIVE_RESULTS$Global_run ==
                        GLOBAL_RUN &
                        PROGRESSIVE_RESULTS$Visit ==
                        10
                    ]
                )
            )
        }
    }
}


# =============================================================================
# 14. FULL-TRAJECTORY SUMMARY
# =============================================================================

FULL_SUMMARY <- aggregate(
    cbind(
        ARI_transductive,
        ARI_inductive,
        Delta_ARI,
        NMI_transductive,
        NMI_inductive,
        Delta_NMI
    ) ~ Eta + Sigma,
    data = FULL_RESULTS,
    FUN = mean
)


# =============================================================================
# 15. FULL-TRAJECTORY SD
# =============================================================================

FULL_SD <- aggregate(
    cbind(
        ARI_transductive,
        ARI_inductive,
        Delta_ARI,
        NMI_transductive,
        NMI_inductive,
        Delta_NMI
    ) ~ Eta + Sigma,
    data = FULL_RESULTS,
    FUN = sd
)


# =============================================================================
# 16. PROGRESSIVE SUMMARY
# =============================================================================

PROGRESSIVE_SUMMARY <- aggregate(
    cbind(
        ARI_truth,
        NMI_truth,
        ARI_final,
        NMI_final,
        Agreement_final,
        Mean_margin
    ) ~ Eta + Sigma + Visit,
    data = PROGRESSIVE_RESULTS,
    FUN = mean
)


# =============================================================================
# 17. PROGRESSIVE SD
# =============================================================================

PROGRESSIVE_SD <- aggregate(
    cbind(
        ARI_truth,
        NMI_truth,
        ARI_final,
        NMI_final,
        Agreement_final,
        Mean_margin
    ) ~ Eta + Sigma + Visit,
    data = PROGRESSIVE_RESULTS,
    FUN = sd
)


# =============================================================================
# 18. CONFIDENCE SUMMARY:
#     CORRECT vs INCORRECT
# =============================================================================

CONFIDENCE_CORRECTNESS <- aggregate(
    Margin ~ Eta + Sigma + Visit + Correct,
    data = PATIENT_RESULTS,
    FUN = function(x) {
        mean(
            x,
            na.rm = TRUE
        )
    }
)


# =============================================================================
# 19. CONFIDENCE SUMMARY:
#     FINAL-MATCH vs LATER-DIFFERENT
# =============================================================================

CONFIDENCE_STABILITY <- aggregate(
    Margin ~ Eta + Sigma + Visit + Final_match,
    data = PATIENT_RESULTS,
    FUN = function(x) {
        mean(
            x,
            na.rm = TRUE
        )
    }
)


# =============================================================================
# 20. SPEARMAN CORRELATION:
#     MARGIN vs CORRECTNESS
#
# Point-biserial interpretation is also possible, but Spearman is simple and
# robust for the present descriptive analysis.
# =============================================================================

VALID_MARGIN <- is.finite(
    PATIENT_RESULTS$Margin
)


MARGIN_CORRECTNESS_COR <- cor.test(
    PATIENT_RESULTS$Margin[
        VALID_MARGIN
    ],
    PATIENT_RESULTS$Correct[
        VALID_MARGIN
    ],
    method = "spearman",
    exact = FALSE
)


MARGIN_STABILITY_COR <- cor.test(
    PATIENT_RESULTS$Margin[
        VALID_MARGIN
    ],
    PATIENT_RESULTS$Final_match[
        VALID_MARGIN
    ],
    method = "spearman",
    exact = FALSE
)


# =============================================================================
# 21. SELECTIVE PREDICTION / COVERAGE ANALYSIS
#
# For each visit:
#
# Sort predictions by margin.
#
# Retain:
#
#     top 100%
#     top 90%
#     ...
#     top 20%
#
# Then evaluate the retained patients.
#
# This asks:
#
#     If we only accept high-confidence assignments,
#     how reliable are they?
#
# =============================================================================

COVERAGE_LEVELS <- seq(
    0.20,
    1.00,
    by = 0.10
)


SELECTIVE_RESULTS <- data.frame()


for(ETA_NOW in ETA_VALUES) {

    for(SIGMA_NOW in SIGMA_VALUES) {

        for(H in HORIZONS) {

            TMP <- PATIENT_RESULTS[
                PATIENT_RESULTS$Eta == ETA_NOW &
                PATIENT_RESULTS$Sigma == SIGMA_NOW &
                PATIENT_RESULTS$Visit == H &
                is.finite(PATIENT_RESULTS$Margin),
                ,
                drop = FALSE
            ]


            if(nrow(TMP) == 0) {
                next
            }


            for(COV in COVERAGE_LEVELS) {

                # -------------------------------------------------------------
                # Margin threshold corresponding to desired retained fraction
                # -------------------------------------------------------------

                THRESHOLD <- as.numeric(
                    quantile(
                        TMP$Margin,
                        probs = 1 - COV,
                        na.rm = TRUE
                    )
                )


                KEEP <- TMP$Margin >=
                    THRESHOLD


                TMP_KEEP <- TMP[
                    KEEP,
                    ,
                    drop = FALSE
                ]


                if(nrow(TMP_KEEP) < 2) {
                    next
                }


                ACCURACY <- mean(
                    TMP_KEEP$Correct,
                    na.rm = TRUE
                )


                STABILITY <- mean(
                    TMP_KEEP$Final_match,
                    na.rm = TRUE
                )


                SELECTIVE_RESULTS <- rbind(
                    SELECTIVE_RESULTS,
                    data.frame(
                        Eta = ETA_NOW,
                        Sigma = SIGMA_NOW,
                        Visit = H,
                        Target_coverage = COV,
                        Actual_coverage =
                            nrow(TMP_KEEP) /
                            nrow(TMP),
                        Margin_threshold =
                            THRESHOLD,
                        Accuracy =
                            ACCURACY,
                        Final_stability =
                            STABILITY
                    )
                )
            }
        }
    }
}


# =============================================================================
# 22. CONDITION-SPECIFIC EQUIVALENCE ANALYSIS
# =============================================================================

EQUIVALENCE_RESULTS <- data.frame()


EQ_COUNTER <- 0


for(ETA_NOW in ETA_VALUES) {

    for(SIGMA_NOW in SIGMA_VALUES) {

        EQ_COUNTER <- EQ_COUNTER + 1


        TMP <- FULL_RESULTS[
            FULL_RESULTS$Eta == ETA_NOW &
            FULL_RESULTS$Sigma == SIGMA_NOW,
            ,
            drop = FALSE
        ]


        ARI_CI <- bootstrap_mean_ci(
            TMP$Delta_ARI,
            B = N_BOOT,
            seed = 800000 + EQ_COUNTER
        )


        NMI_CI <- bootstrap_mean_ci(
            TMP$Delta_NMI,
            B = N_BOOT,
            seed = 900000 + EQ_COUNTER
        )


        EQUIVALENCE_RESULTS <- rbind(
            EQUIVALENCE_RESULTS,
            data.frame(
                Eta = ETA_NOW,
                Sigma = SIGMA_NOW,

                Delta_ARI =
                    mean(
                        TMP$Delta_ARI
                    ),

                ARI_CI_lower =
                    ARI_CI[1],

                ARI_CI_upper =
                    ARI_CI[2],

                ARI_equivalent =
                    ARI_CI[1] >
                    -EQUIVALENCE_MARGIN &&
                    ARI_CI[2] <
                    EQUIVALENCE_MARGIN,

                Delta_NMI =
                    mean(
                        TMP$Delta_NMI
                    ),

                NMI_CI_lower =
                    NMI_CI[1],

                NMI_CI_upper =
                    NMI_CI[2],

                NMI_equivalent =
                    NMI_CI[1] >
                    -EQUIVALENCE_MARGIN &&
                    NMI_CI[2] <
                    EQUIVALENCE_MARGIN
            )
        )
    }
}


# =============================================================================
# 23. PRINT FULL-TRAJECTORY RESULTS
# =============================================================================

cat("\n\n")
cat("==============================================================================================================\n")
cat("FULL-TRAJECTORY ROBUSTNESS RESULTS\n")
cat("==============================================================================================================\n\n")


print(
    FULL_SUMMARY,
    digits = 4,
    row.names = FALSE
)


cat("\n")
cat("==============================================================================================================\n")
cat("EQUIVALENCE RESULTS\n")
cat("==============================================================================================================\n\n")


print(
    EQUIVALENCE_RESULTS,
    digits = 4,
    row.names = FALSE
)


# =============================================================================
# 24. PRINT PROGRESSIVE RESULTS
# =============================================================================

cat("\n")
cat("==============================================================================================================\n")
cat("PROGRESSIVE ASSIGNMENT RESULTS\n")
cat("==============================================================================================================\n\n")


print(
    PROGRESSIVE_SUMMARY,
    digits = 4,
    row.names = FALSE
)


# =============================================================================
# 25. PRINT CONFIDENCE RESULTS
# =============================================================================

cat("\n")
cat("==============================================================================================================\n")
cat("CONFIDENCE RESULTS\n")
cat("==============================================================================================================\n")


cat(
    sprintf(
        "\nMargin vs correctness: Spearman rho = %.3f, p = %.4g\n",
        unname(
            MARGIN_CORRECTNESS_COR$estimate
        ),
        MARGIN_CORRECTNESS_COR$p.value
    )
)


cat(
    sprintf(
        "Margin vs final-assignment stability: Spearman rho = %.3f, p = %.4g\n",
        unname(
            MARGIN_STABILITY_COR$estimate
        ),
        MARGIN_STABILITY_COR$p.value
    )
)


# =============================================================================
# 26. SAVE ALL NUMERIC RESULTS
# =============================================================================

write.csv(
    FULL_RESULTS,
    "longTAPIO_robustness_full_results.csv",
    row.names = FALSE
)


write.csv(
    FULL_SUMMARY,
    "longTAPIO_robustness_full_summary.csv",
    row.names = FALSE
)


write.csv(
    FULL_SD,
    "longTAPIO_robustness_full_sd.csv",
    row.names = FALSE
)


write.csv(
    PROGRESSIVE_RESULTS,
    "longTAPIO_progressive_results.csv",
    row.names = FALSE
)


write.csv(
    PROGRESSIVE_SUMMARY,
    "longTAPIO_progressive_summary.csv",
    row.names = FALSE
)


write.csv(
    PATIENT_RESULTS,
    "longTAPIO_patient_level_predictions.csv",
    row.names = FALSE
)


write.csv(
    SELECTIVE_RESULTS,
    "longTAPIO_selective_prediction.csv",
    row.names = FALSE
)


write.csv(
    EQUIVALENCE_RESULTS,
    "longTAPIO_condition_equivalence.csv",
    row.names = FALSE
)


# =============================================================================
# 27. FIGURE 1
#
# PERFORMANCE PRESERVATION
#
# Transductive versus inductive ARI as noise increases.
# =============================================================================

PERFORMANCE_PLOT <- rbind(
    data.frame(
        Eta = FULL_RESULTS$Eta,
        Sigma = FULL_RESULTS$Sigma,
        Method = "Transductive",
        ARI = FULL_RESULTS$ARI_transductive
    ),

    data.frame(
        Eta = FULL_RESULTS$Eta,
        Sigma = FULL_RESULTS$Sigma,
        Method = "Inductive",
        ARI = FULL_RESULTS$ARI_inductive
    )
)


PERFORMANCE_PLOT$Method <- factor(
    PERFORMANCE_PLOT$Method,
    levels = c(
        "Transductive",
        "Inductive"
    )
)


P1 <- ggplot(
    PERFORMANCE_PLOT,
    aes(
        x = factor(Eta),
        y = ARI,
        group = Method,
        linetype = Method,
        shape = Method
    )
) +
    stat_summary(
        fun = mean,
        geom = "line",
        linewidth = 1
    ) +
    stat_summary(
        fun = mean,
        geom = "point",
        size = 3
    ) +
    stat_summary(
        fun.data = mean_se,
        geom = "errorbar",
        width = 0.12
    ) +
    facet_wrap(
        ~Sigma,
        labeller = label_both
    ) +
    coord_cartesian(
        ylim = c(
            0,
            1
        )
    ) +
    labs(
        title = "Performance preservation under increasing difficulty",
        x = "Observation noise (eta)",
        y = "Adjusted Rand Index",
        linetype = NULL,
        shape = NULL
    ) +
    theme_classic(
        base_size = 14
    ) +
    theme(
        plot.title = element_text(
            face = "bold"
        ),
        strip.text = element_text(
            face = "bold"
        )
    )


print(
    P1
)


# =============================================================================
# 28. FIGURE 2
#
# PROGRESSIVE ASSIGNMENT
#
# ARI against TRUE simulated phenotype as visits accumulate.
# =============================================================================

P2 <- ggplot(
    PROGRESSIVE_SUMMARY,
    aes(
        x = Visit,
        y = ARI_truth,
        group = factor(Eta),
        linetype = factor(Eta),
        shape = factor(Eta)
    )
) +
    geom_line(
        linewidth = 1
    ) +
    geom_point(
        size = 2.5
    ) +
    facet_wrap(
        ~Sigma,
        labeller = label_both
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
        title = "Progressive assignment of unseen subjects",
        subtitle = "Clustering accuracy as longitudinal information accumulates",
        x = "Available visits",
        y = "Adjusted Rand Index",
        linetype = "eta",
        shape = "eta"
    ) +
    theme_classic(
        base_size = 14
    ) +
    theme(
        plot.title = element_text(
            face = "bold"
        ),
        strip.text = element_text(
            face = "bold"
        )
    )


print(
    P2
)


# =============================================================================
# 29. FIGURE 3
#
# ASSIGNMENT STABILITY
#
# Agreement with final 10-visit assignment.
# =============================================================================

P3 <- ggplot(
    PROGRESSIVE_SUMMARY,
    aes(
        x = Visit,
        y = Agreement_final,
        group = factor(Eta),
        linetype = factor(Eta),
        shape = factor(Eta)
    )
) +
    geom_line(
        linewidth = 1
    ) +
    geom_point(
        size = 2.5
    ) +
    facet_wrap(
        ~Sigma,
        labeller = label_both
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
        title = "Stabilization of progressive cluster assignments",
        subtitle = "Agreement with the final 10-visit inductive assignment",
        x = "Available visits",
        y = "Agreement with final assignment",
        linetype = "eta",
        shape = "eta"
    ) +
    theme_classic(
        base_size = 14
    ) +
    theme(
        plot.title = element_text(
            face = "bold"
        ),
        strip.text = element_text(
            face = "bold"
        )
    )


print(
    P3
)


# =============================================================================
# 30. FIGURE 4
#
# MARGIN OVER TIME
# =============================================================================

P4 <- ggplot(
    PROGRESSIVE_SUMMARY,
    aes(
        x = Visit,
        y = Mean_margin,
        group = factor(Eta),
        linetype = factor(Eta),
        shape = factor(Eta)
    )
) +
    geom_line(
        linewidth = 1
    ) +
    geom_point(
        size = 2.5
    ) +
    facet_wrap(
        ~Sigma,
        labeller = label_both
    ) +
    scale_x_continuous(
        breaks = HORIZONS
    ) +
    labs(
        title = "Assignment confidence as longitudinal information accumulates",
        x = "Available visits",
        y = "Mean assignment margin",
        linetype = "eta",
        shape = "eta"
    ) +
    theme_classic(
        base_size = 14
    ) +
    theme(
        plot.title = element_text(
            face = "bold"
        ),
        strip.text = element_text(
            face = "bold"
        )
    )


print(
    P4
)


# =============================================================================
# 31. FIGURE 5
#
# CONFIDENCE vs CORRECTNESS
#
# Pool all simulation conditions.
# Bin margin into deciles.
# =============================================================================

CONF_DATA <- PATIENT_RESULTS[
    is.finite(
        PATIENT_RESULTS$Margin
    ),
    ,
    drop = FALSE
]


CONF_DATA$Margin_bin <- cut(
    CONF_DATA$Margin,
    breaks = quantile(
        CONF_DATA$Margin,
        probs = seq(
            0,
            1,
            length.out = 11
        ),
        na.rm = TRUE
    ),
    include.lowest = TRUE,
    labels = FALSE
)


CONF_BIN <- aggregate(
    cbind(
        Correct,
        Final_match,
        Margin
    ) ~ Margin_bin,
    data = CONF_DATA,
    FUN = mean
)


P5 <- ggplot(
    CONF_BIN,
    aes(
        x = Margin,
        y = Correct
    )
) +
    geom_line(
        linewidth = 1
    ) +
    geom_point(
        size = 3
    ) +
    coord_cartesian(
        ylim = c(
            0,
            1
        )
    ) +
    labs(
        title = "Assignment confidence and prediction reliability",
        subtitle = "Predictions grouped by assignment-margin decile",
        x = "Mean assignment margin",
        y = "Proportion correctly assigned"
    ) +
    theme_classic(
        base_size = 14
    ) +
    theme(
        plot.title = element_text(
            face = "bold"
        )
    )


print(
    P5
)


# =============================================================================
# 32. FIGURE 6
#
# SELECTIVE PREDICTION:
# COVERAGE vs ACCURACY
#
# For readability, average over sigma and show separate eta curves.
# =============================================================================

SELECTIVE_SUMMARY <- aggregate(
    cbind(
        Actual_coverage,
        Accuracy,
        Final_stability
    ) ~ Eta + Visit + Target_coverage,
    data = SELECTIVE_RESULTS,
    FUN = mean
)


P6 <- ggplot(
    SELECTIVE_SUMMARY[
        SELECTIVE_SUMMARY$Visit %in% c(
            1,
            3,
            5,
            10
        ),
        ,
        drop = FALSE
    ],
    aes(
        x = Actual_coverage,
        y = Accuracy,
        group = factor(Eta),
        linetype = factor(Eta),
        shape = factor(Eta)
    )
) +
    geom_line(
        linewidth = 1
    ) +
    geom_point(
        size = 2.5
    ) +
    facet_wrap(
        ~Visit,
        labeller = label_both
    ) +
    coord_cartesian(
        xlim = c(
            0,
            1
        ),
        ylim = c(
            0,
            1
        )
    ) +
    labs(
        title = "Selective prediction using the assignment margin",
        subtitle = "Higher confidence thresholds reduce coverage but may improve reliability",
        x = "Coverage",
        y = "Correct assignment proportion",
        linetype = "eta",
        shape = "eta"
    ) +
    theme_classic(
        base_size = 14
    ) +
    theme(
        plot.title = element_text(
            face = "bold"
        ),
        strip.text = element_text(
            face = "bold"
        )
    )


print(
    P6
)


# =============================================================================
# 33. FIGURE 7
#
# EQUIVALENCE / PERFORMANCE DIFFERENCE
# =============================================================================

P7 <- ggplot(
    EQUIVALENCE_RESULTS,
    aes(
        x = factor(Eta),
        y = Delta_ARI,
        group = factor(Sigma),
        linetype = factor(Sigma),
        shape = factor(Sigma)
    )
) +
    annotate(
        "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = -EQUIVALENCE_MARGIN,
        ymax = EQUIVALENCE_MARGIN,
        alpha = 0.12
    ) +
    geom_hline(
        yintercept = 0,
        linewidth = 0.8
    ) +
    geom_hline(
        yintercept = c(
            -EQUIVALENCE_MARGIN,
            EQUIVALENCE_MARGIN
        ),
        linetype = "dashed"
    ) +
    geom_line(
        linewidth = 1
    ) +
    geom_point(
        size = 3
    ) +
    labs(
        title = "Inductive versus transductive longTAPIO",
        subtitle = "Mean paired ARI difference; shaded region = +/-0.05",
        x = "Observation noise (eta)",
        y = "ARI difference: inductive - transductive",
        linetype = "sigma",
        shape = "sigma"
    ) +
    theme_classic(
        base_size = 14
    ) +
    theme(
        plot.title = element_text(
            face = "bold"
        )
    )


print(
    P7
)


# =============================================================================
# 34. SAVE FIGURES
# =============================================================================

ggsave(
    "Fig1_longTAPIO_performance_preservation.pdf",
    P1,
    width = 9,
    height = 5.5
)


ggsave(
    "Fig2_longTAPIO_progressive_assignment.pdf",
    P2,
    width = 9,
    height = 5.5
)


ggsave(
    "Fig3_longTAPIO_assignment_stability.pdf",
    P3,
    width = 9,
    height = 5.5
)


ggsave(
    "Fig4_longTAPIO_margin_over_time.pdf",
    P4,
    width = 9,
    height = 5.5
)


ggsave(
    "Fig5_longTAPIO_confidence_reliability.pdf",
    P5,
    width = 7,
    height = 5.5
)


ggsave(
    "Fig6_longTAPIO_selective_prediction.pdf",
    P6,
    width = 9,
    height = 6
)


ggsave(
    "Fig7_longTAPIO_equivalence_robustness.pdf",
    P7,
    width = 8,
    height = 5.5
)


# =============================================================================
# 35. FINAL COMPACT OUTPUT
# =============================================================================

cat("\n\n")
cat("==============================================================================================================\n")
cat("COMPACT PAPER SUMMARY\n")
cat("==============================================================================================================\n\n")


for(ETA_NOW in ETA_VALUES) {

    for(SIGMA_NOW in SIGMA_VALUES) {

        TMP <- FULL_RESULTS[
            FULL_RESULTS$Eta == ETA_NOW &
            FULL_RESULTS$Sigma == SIGMA_NOW,
            ,
            drop = FALSE
        ]


        cat(
            sprintf(
                paste0(
                    "eta=%2d sigma=%2d | ",
                    "Trans ARI %.3f | ",
                    "Ind ARI %.3f | ",
                    "Delta %+.3f | ",
                    "Trans NMI %.3f | ",
                    "Ind NMI %.3f | ",
                    "Delta %+.3f\n"
                ),
                ETA_NOW,
                SIGMA_NOW,
                mean(
                    TMP$ARI_transductive
                ),
                mean(
                    TMP$ARI_inductive
                ),
                mean(
                    TMP$Delta_ARI
                ),
                mean(
                    TMP$NMI_transductive
                ),
                mean(
                    TMP$NMI_inductive
                ),
                mean(
                    TMP$Delta_NMI
                )
            )
        )
    }
}


cat("\n")
cat(
    sprintf(
        "Margin vs correctness: rho = %.3f\n",
        unname(
            MARGIN_CORRECTNESS_COR$estimate
        )
    )
)


cat(
    sprintf(
        "Margin vs final stability: rho = %.3f\n",
        unname(
            MARGIN_STABILITY_COR$estimate
        )
    )
)


cat("\n")
cat("==============================================================================================================\n")
cat("BENCHMARK COMPLETE\n")
cat("==============================================================================================================\n")