# =============================================================================
# longTAPIO BENCHMARK
#
# ORIGINAL TRANSDUCTIVE longTAPIO
#                 vs
# INDUCTIVE longTAPIO
#
# 50 PAIRED SYNTHETIC SIMULATIONS
#
# Synthetic data:
#   TAPIO::simLongData()
#
# Primary question:
#
#   Does inductive longTAPIO retain the clustering performance of the
#   original transductive implementation when assigning previously unseen
#   patients?
#
# Design:
#
#   ORIGINAL / TRANSDUCTIVE
#   -----------------------
#   All 200 patients are supplied to longTAPIO_trajectories().
#   Performance is subsequently evaluated on the 60 designated test patients.
#
#   INDUCTIVE
#   ---------
#   longTAPIO_inductive() is fitted on 140 patients only.
#   The fitted model is frozen.
#   The 60 unseen patients are assigned using predict().
#
#   Both methods are therefore scored on exactly the same 60 patients.
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
# 2. LOAD INDUCTIVE IMPLEMENTATION
#
# IMPORTANT:
#
# If longTAPIO_inductive() is already part of your installed TAPIO package,
# nothing needs to be sourced here.
#
# If it is stored in another R file, source THAT file here.
#
# DO NOT source this benchmark file from itself.
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
# 3. BENCHMARK SETTINGS
# =============================================================================

N_RUNS <- 50

N_TOTAL <- 200
TRAIN_FRAC <- 0.70

N_VISITS <- 10

SET_K <- 4

SET_LEVELS <- 4
SET_N_FEATURES <- 5
SET_N_TREES <- 500

METHOD <- "ward.D2"

SCALE_PCA <- TRUE

# Same setting as the earlier successful benchmark
REPLACE_FEATURES <- TRUE

PCA_SELECTION <- "random_weighted"


# =============================================================================
# 4. SYNTHETIC DATA SETTINGS
#
# These parameters correspond directly to your TAPIO::simLongData().
# =============================================================================

ETA <- 3

BASE_SIGMA <- 3

MIN_INFLATED_SIGMA <- 3
MAX_INFLATED_SIGMA <- 20


# =============================================================================
# 5. PRACTICAL EQUIVALENCE SETTINGS
#
# Difference is always:
#
#       inductive - transductive
#
# A +/-0.05 interval is used as a practical similarity region.
# =============================================================================

EQUIVALENCE_MARGIN <- 0.05

N_BOOT <- 10000


# =============================================================================
# 6. HELPER: PREPARE simLongData OUTPUT
#
# simLongData() returns:
#
#   subject
#   time
#   outcome
#   y
#   cluster
#
# We reshape:
#
#               outcome
#   subject,time  1 2 3 4 5
#
# into one row per subject/visit and one column per outcome.
#
# =============================================================================

prepare_simulation <- function(Longdat2) {

    # -------------------------------------------------------------------------
    # Ensure deterministic ordering
    # -------------------------------------------------------------------------

    Longdat2 <- Longdat2[
        order(
            Longdat2$subject,
            Longdat2$time,
            Longdat2$outcome
        ),
        ,
        drop = FALSE
    ]

    rownames(Longdat2) <- NULL


    # -------------------------------------------------------------------------
    # True cluster labels are subject-level
    # -------------------------------------------------------------------------

    subject_info <- Longdat2[
        !duplicated(Longdat2$subject),
        c(
            "subject",
            "cluster"
        ),
        drop = FALSE
    ]

    subject_info <- subject_info[
        order(subject_info$subject),
        ,
        drop = FALSE
    ]

    rownames(subject_info) <- NULL


    # -------------------------------------------------------------------------
    # Reshape y from long to wide
    #
    # reshape() produces:
    #
    # subject
    # time
    # cluster
    # y.1
    # y.2
    # ...
    # y.5
    # -------------------------------------------------------------------------

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


    # -------------------------------------------------------------------------
    # Sort again after reshape
    # -------------------------------------------------------------------------

    Longdat2_wide <- Longdat2_wide[
        order(
            Longdat2_wide$subject,
            Longdat2_wide$time
        ),
        ,
        drop = FALSE
    ]

    rownames(Longdat2_wide) <- NULL


    # -------------------------------------------------------------------------
    # Identify y columns explicitly
    # -------------------------------------------------------------------------

    FEATURE_NAMES <- paste0(
        "y.",
        seq_len(SET_N_FEATURES)
    )


    if(!all(FEATURE_NAMES %in% colnames(Longdat2_wide))) {

        stop(
            paste0(
                "Expected columns missing after reshape. Found: ",
                paste(
                    colnames(Longdat2_wide),
                    collapse = ", "
                )
            )
        )
    }


    # -------------------------------------------------------------------------
    # Numeric feature matrix
    # -------------------------------------------------------------------------

    DD <- as.matrix(
        Longdat2_wide[
            ,
            FEATURE_NAMES,
            drop = FALSE
        ]
    )

    storage.mode(DD) <- "double"


    USER_ID <- Longdat2_wide$subject


    # -------------------------------------------------------------------------
    # Checks
    # -------------------------------------------------------------------------

    if(!is.matrix(DD)) {
        stop("DD is not a matrix.")
    }

    if(!is.numeric(DD)) {
        stop("DD is not numeric.")
    }

    if(anyNA(DD)) {
        stop("DD contains NA values.")
    }

    if(nrow(DD) != length(USER_ID)) {
        stop("DD and USER_ID have incompatible lengths.")
    }


    list(
        DD = DD,
        USER_ID = USER_ID,
        subject_info = subject_info,
        wide = Longdat2_wide
    )
}


# =============================================================================
# 7. HELPER: STRATIFIED TRAIN/TEST SPLIT
#
# We stratify by the TRUE simulated clusters.
#
# This ensures approximately equal representation of the four simulated
# clusters in train and test.
#
# Labels are used ONLY for constructing the benchmark split and evaluation.
# They are never supplied to either clustering method.
# =============================================================================

make_stratified_split <- function(
    subject_info,
    train_frac,
    seed
) {

    set.seed(seed)

    TRAIN_SUBJECTS <- integer(0)


    for(k in sort(unique(subject_info$cluster))) {

        IDs <- subject_info$subject[
            subject_info$cluster == k
        ]

        n_train_k <- floor(
            length(IDs) * train_frac
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


    TRAIN_SUBJECTS <- sort(TRAIN_SUBJECTS)


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
# 8. HELPER: BOOTSTRAP CI FOR MEAN PAIRED DIFFERENCE
# =============================================================================

bootstrap_mean_ci <- function(
    x,
    B = 10000,
    seed = 1
) {

    x <- x[
        is.finite(x)
    ]

    set.seed(seed)

    n <- length(x)

    BOOT <- numeric(B)


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
# 9. HELPER: TOST FOR PAIRED DIFFERENCES
#
# H01:
#
#   mean difference <= -margin
#
# H02:
#
#   mean difference >= +margin
#
# Equivalence requires rejection of BOTH null hypotheses.
# =============================================================================

paired_tost <- function(
    difference,
    margin = 0.05,
    alpha = 0.05
) {

    difference <- difference[
        is.finite(difference)
    ]


    n <- length(difference)

    md <- mean(difference)

    sd_d <- sd(difference)

    se_d <- sd_d / sqrt(n)

    df <- n - 1


    if(sd_d == 0) {

        equivalent <- abs(md) < margin

        return(
            list(
                mean_difference = md,
                p_lower = ifelse(equivalent, 0, 1),
                p_upper = ifelse(equivalent, 0, 1),
                equivalent = equivalent
            )
        )
    }


    # -------------------------------------------------------------------------
    # Lower equivalence test
    #
    # H0: difference <= -margin
    # H1: difference >  -margin
    # -------------------------------------------------------------------------

    t_lower <- (
        md + margin
    ) / se_d


    p_lower <- 1 - pt(
        t_lower,
        df = df
    )


    # -------------------------------------------------------------------------
    # Upper equivalence test
    #
    # H0: difference >= +margin
    # H1: difference <  +margin
    # -------------------------------------------------------------------------

    t_upper <- (
        md - margin
    ) / se_d


    p_upper <- pt(
        t_upper,
        df = df
    )


    equivalent <- (
        p_lower < alpha &&
        p_upper < alpha
    )


    list(
        mean_difference = md,
        t_lower = t_lower,
        t_upper = t_upper,
        p_lower = p_lower,
        p_upper = p_upper,
        equivalent = equivalent
    )
}


# =============================================================================
# 10. STORAGE
# =============================================================================

RESULTS <- data.frame(
    Run = seq_len(N_RUNS),

    Sigma_variable = NA_integer_,
    Sigma_value = NA_real_,

    ARI_transductive = NA_real_,
    ARI_inductive = NA_real_,
    Delta_ARI = NA_real_,

    NMI_transductive = NA_real_,
    NMI_inductive = NA_real_,
    Delta_NMI = NA_real_,

    Inductive_margin = NA_real_,

    Time_transductive = NA_real_,
    Time_inductive_fit = NA_real_,
    Time_inductive_predict = NA_real_
)


# =============================================================================
# 11. HEADER
# =============================================================================

cat("\n")
cat("==============================================================================================================\n")
cat("PAIRED longTAPIO BENCHMARK\n")
cat("ORIGINAL TRANSDUCTIVE vs INDUCTIVE\n")
cat("==============================================================================================================\n")

cat(
    "Runs             : ",
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
    as.integer(N_TOTAL * TRAIN_FRAC),
    "/",
    as.integer(N_TOTAL * (1 - TRAIN_FRAC)),
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
    "Visits           : ",
    N_VISITS,
    "\n",
    sep = ""
)

cat(
    "Outcomes         : ",
    SET_N_FEATURES,
    "\n",
    sep = ""
)

cat(
    "eta              : ",
    ETA,
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
    "Features/tree    : ",
    SET_N_FEATURES,
    "\n",
    sep = ""
)

cat(
    "Levels           : ",
    SET_LEVELS,
    "\n",
    sep = ""
)

cat(
    "PCA selection    : ",
    PCA_SELECTION,
    "\n",
    sep = ""
)

cat(
    "Replace features : ",
    REPLACE_FEATURES,
    "\n",
    sep = ""
)

cat(
    "Equiv. margin    : +/-",
    EQUIVALENCE_MARGIN,
    "\n",
    sep = ""
)

cat("==============================================================================================================\n")


# =============================================================================
# 12. MAIN SIMULATION LOOP
# =============================================================================

for(ii in seq_len(N_RUNS)) {

    cat("\n")
    cat("--------------------------------------------------------------------------------------------------------------\n")

    cat(
        "RUN ",
        ii,
        "/",
        N_RUNS,
        "\n",
        sep = ""
    )

    cat("--------------------------------------------------------------------------------------------------------------\n")


    # =========================================================================
    # 12.1 SIMULATION PARAMETERS
    # =========================================================================

    set.seed(
        1000 + ii
    )


    r_sigma_diag <- rep(
        BASE_SIGMA,
        SET_N_FEATURES
    )


    sigma_id <- sample(
        seq_len(SET_N_FEATURES),
        size = 1
    )


    sigma_value <- sample(
        MIN_INFLATED_SIGMA:MAX_INFLATED_SIGMA,
        size = 1
    )


    r_sigma_diag[sigma_id] <- sigma_value


    RESULTS$Sigma_variable[ii] <- sigma_id

    RESULTS$Sigma_value[ii] <- sigma_value


    cat(
        "sigma_diag: ",
        paste(
            r_sigma_diag,
            collapse = " "
        ),
        "\n",
        sep = ""
    )


    # =========================================================================
    # 12.2 EXACT TAPIO SYNTHETIC GENERATOR
    #
    # This is YOUR simLongData().
    #
    # No alternative/reconstructed generator is used.
    # =========================================================================

    Longdat2 <- TAPIO::simLongData(
        n_total = N_TOTAL,
        K = SET_K,
        outcomes = SET_N_FEATURES,
        eta = ETA,
        cluster_sizes = rep(
            N_TOTAL / SET_K,
            SET_K
        ),
        ranTimes = FALSE,
        n_i = N_VISITS,
        sigma_diag = r_sigma_diag
    )


    # =========================================================================
    # 12.3 PREPARE DATA
    # =========================================================================

    PREP <- prepare_simulation(
        Longdat2
    )


    DD <- PREP$DD

    USER_ID <- PREP$USER_ID

    subject_info <- PREP$subject_info


    # =========================================================================
    # 12.4 CHECK GENERATED DATA
    # =========================================================================

    n_subjects <- length(
        unique(USER_ID)
    )


    if(n_subjects != N_TOTAL) {

        stop(
            paste0(
                "Expected ",
                N_TOTAL,
                " patients but found ",
                n_subjects,
                "."
            )
        )
    }


    visits_per_subject <- table(
        USER_ID
    )


    if(any(visits_per_subject != N_VISITS)) {

        stop(
            "Not every patient has the expected number of visits."
        )
    }


    if(ncol(DD) != SET_N_FEATURES) {

        stop(
            paste0(
                "Expected ",
                SET_N_FEATURES,
                " outcomes but DD has ",
                ncol(DD),
                "."
            )
        )
    }


    # =========================================================================
    # 12.5 TRAIN/TEST SPLIT
    # =========================================================================

    SPLIT <- make_stratified_split(
        subject_info = subject_info,
        train_frac = TRAIN_FRAC,
        seed = 5000 + ii
    )


    TRAIN_SUBJECTS <- SPLIT$TRAIN

    TEST_SUBJECTS <- SPLIT$TEST


    cat(
        "Training patients: ",
        length(TRAIN_SUBJECTS),
        " | Test patients: ",
        length(TEST_SUBJECTS),
        "\n",
        sep = ""
    )


    # =========================================================================
    # 12.6 TRUE TEST LABELS
    # =========================================================================

    true_test <- subject_info$cluster[
        match(
            TEST_SUBJECTS,
            subject_info$subject
        )
    ]


    # =========================================================================
    # 12.7 ORIGINAL TRANSDUCTIVE longTAPIO
    #
    # ALL 200 patients are supplied.
    #
    # This intentionally preserves the transductive nature of the original
    # implementation.
    # =========================================================================

    cat(
        "  Original transductive longTAPIO..."
    )


    set.seed(
        100000 + ii
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


    RESULTS$Time_transductive[ii] <-
        proc.time()[3] - t0


    # =========================================================================
    # 12.8 EXTRACT TEST PATIENT CLUSTERS FROM TRANSDUCTIVE SOLUTION
    # =========================================================================

    original_subjects <- sort(
        unique(USER_ID)
    )


    if(length(original$cl) != length(original_subjects)) {

        stop(
            paste0(
                "original$cl has length ",
                length(original$cl),
                " but there are ",
                length(original_subjects),
                " subjects."
            )
        )
    }


    original_test_clusters <- original$cl[
        match(
            TEST_SUBJECTS,
            original_subjects
        )
    ]


    # =========================================================================
    # 12.9 TRANSDUCTIVE PERFORMANCE
    # =========================================================================

    RESULTS$ARI_transductive[ii] <- aricode::ARI(
        true_test,
        original_test_clusters
    )


    RESULTS$NMI_transductive[ii] <- aricode::NMI(
        true_test,
        original_test_clusters
    )


    cat(
        sprintf(
            " ARI=%.3f NMI=%.3f\n",
            RESULTS$ARI_transductive[ii],
            RESULTS$NMI_transductive[ii]
        )
    )


    # =========================================================================
    # 12.10 BUILD TRAINING DATA
    # =========================================================================

    TRAIN_ROWS <- USER_ID %in% TRAIN_SUBJECTS


    DD_train <- DD[
        TRAIN_ROWS,
        ,
        drop = FALSE
    ]


    USER_train_original <- USER_ID[
        TRAIN_ROWS
    ]


    # -------------------------------------------------------------------------
    # Remap IDs to 1,...,Ntrain.
    #
    # This avoids assumptions inside the inductive implementation about
    # consecutive patient IDs.
    # -------------------------------------------------------------------------

    USER_train <- match(
        USER_train_original,
        TRAIN_SUBJECTS
    )


    # =========================================================================
    # 12.11 BUILD TEST DATA
    # =========================================================================

    TEST_ROWS <- USER_ID %in% TEST_SUBJECTS


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


    # =========================================================================
    # 12.12 VERIFY TRAIN/TEST SEPARATION
    # =========================================================================

    if(
        length(
            intersect(
                TRAIN_SUBJECTS,
                TEST_SUBJECTS
            )
        ) != 0
    ) {

        stop(
            "Train/test leakage detected."
        )
    }


    if(
        length(
            unique(USER_train)
        ) != length(TRAIN_SUBJECTS)
    ) {

        stop(
            "Training ID remapping failed."
        )
    }


    if(
        length(
            unique(USER_test)
        ) != length(TEST_SUBJECTS)
    ) {

        stop(
            "Test ID remapping failed."
        )
    }


    # =========================================================================
    # 12.13 FIT INDUCTIVE longTAPIO
    #
    # ONLY training patients are supplied.
    # =========================================================================

    cat(
        "  Inductive longTAPIO training..."
    )


    set.seed(
        200000 + ii
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


    RESULTS$Time_inductive_fit[ii] <-
        proc.time()[3] - t0


    cat(
        sprintf(
            " %.2fs\n",
            RESULTS$Time_inductive_fit[ii]
        )
    )


    # =========================================================================
    # 12.14 PREDICT COMPLETELY UNSEEN TEST PATIENTS
    #
    # Full 10-visit trajectories are used here because this experiment asks
    # whether the inductive extension preserves the performance of the
    # original full-trajectory method.
    # =========================================================================

    cat(
        "  Inductive test prediction..."
    )


    t0 <- proc.time()[3]


    pred <- predict(
        model,
        newdata = DD_test,
        user_id = USER_test,
        visits = N_VISITS
    )


    RESULTS$Time_inductive_predict[ii] <-
        proc.time()[3] - t0


    # =========================================================================
    # 12.15 VERIFY PREDICTIONS
    # =========================================================================

    if(length(pred$cluster) != length(TEST_SUBJECTS)) {

        stop(
            paste0(
                "predict() returned ",
                length(pred$cluster),
                " clusters for ",
                length(TEST_SUBJECTS),
                " test patients."
            )
        )
    }


    # =========================================================================
    # 12.16 INDUCTIVE PERFORMANCE
    # =========================================================================

    RESULTS$ARI_inductive[ii] <- aricode::ARI(
        true_test,
        pred$cluster
    )


    RESULTS$NMI_inductive[ii] <- aricode::NMI(
        true_test,
        pred$cluster
    )


    if(!is.null(pred$margin)) {

        RESULTS$Inductive_margin[ii] <- mean(
            pred$margin,
            na.rm = TRUE
        )
    }


    cat(
        sprintf(
            " ARI=%.3f NMI=%.3f\n",
            RESULTS$ARI_inductive[ii],
            RESULTS$NMI_inductive[ii]
        )
    )


    # =========================================================================
    # 12.17 PAIRED DIFFERENCES
    # =========================================================================

    RESULTS$Delta_ARI[ii] <-
        RESULTS$ARI_inductive[ii] -
        RESULTS$ARI_transductive[ii]


    RESULTS$Delta_NMI[ii] <-
        RESULTS$NMI_inductive[ii] -
        RESULTS$NMI_transductive[ii]


    cat(
        sprintf(
            "  Paired difference: ARI=%+.3f | NMI=%+.3f\n",
            RESULTS$Delta_ARI[ii],
            RESULTS$Delta_NMI[ii]
        )
    )


    # =========================================================================
    # 12.18 RUNNING SUMMARY
    # =========================================================================

    cat(
        sprintf(
            paste0(
                "  Running mean ARI: trans=%.3f | ",
                "ind=%.3f | delta=%+.3f\n"
            ),
            mean(
                RESULTS$ARI_transductive[seq_len(ii)],
                na.rm = TRUE
            ),
            mean(
                RESULTS$ARI_inductive[seq_len(ii)],
                na.rm = TRUE
            ),
            mean(
                RESULTS$Delta_ARI[seq_len(ii)],
                na.rm = TRUE
            )
        )
    )
}


# =============================================================================
# 13. DESCRIPTIVE STATISTICS
# =============================================================================

ARI_TRANS_MEAN <- mean(
    RESULTS$ARI_transductive
)

ARI_TRANS_SD <- sd(
    RESULTS$ARI_transductive
)


ARI_IND_MEAN <- mean(
    RESULTS$ARI_inductive
)

ARI_IND_SD <- sd(
    RESULTS$ARI_inductive
)


NMI_TRANS_MEAN <- mean(
    RESULTS$NMI_transductive
)

NMI_TRANS_SD <- sd(
    RESULTS$NMI_transductive
)


NMI_IND_MEAN <- mean(
    RESULTS$NMI_inductive
)

NMI_IND_SD <- sd(
    RESULTS$NMI_inductive
)


DELTA_ARI_MEAN <- mean(
    RESULTS$Delta_ARI
)

DELTA_ARI_SD <- sd(
    RESULTS$Delta_ARI
)


DELTA_NMI_MEAN <- mean(
    RESULTS$Delta_NMI
)

DELTA_NMI_SD <- sd(
    RESULTS$Delta_NMI
)


# =============================================================================
# 14. BOOTSTRAP CIs OF PAIRED DIFFERENCES
# =============================================================================

ARI_BOOT_CI <- bootstrap_mean_ci(
    RESULTS$Delta_ARI,
    B = N_BOOT,
    seed = 900001
)


NMI_BOOT_CI <- bootstrap_mean_ci(
    RESULTS$Delta_NMI,
    B = N_BOOT,
    seed = 900002
)


# =============================================================================
# 15. STANDARD PAIRED TESTS
#
# These test whether the mean difference is zero.
#
# IMPORTANT:
# A non-significant result does NOT by itself establish equivalence.
# =============================================================================

ARI_PAIRED_TEST <- t.test(
    RESULTS$ARI_inductive,
    RESULTS$ARI_transductive,
    paired = TRUE
)


NMI_PAIRED_TEST <- t.test(
    RESULTS$NMI_inductive,
    RESULTS$NMI_transductive,
    paired = TRUE
)


# =============================================================================
# 16. FORMAL PAIRED TOST
# =============================================================================

ARI_TOST <- paired_tost(
    RESULTS$Delta_ARI,
    margin = EQUIVALENCE_MARGIN
)


NMI_TOST <- paired_tost(
    RESULTS$Delta_NMI,
    margin = EQUIVALENCE_MARGIN
)


# =============================================================================
# 17. BOOTSTRAP CI EQUIVALENCE CHECK
# =============================================================================

ARI_BOOT_EQUIVALENT <- (
    ARI_BOOT_CI[1] > -EQUIVALENCE_MARGIN &&
    ARI_BOOT_CI[2] < EQUIVALENCE_MARGIN
)


NMI_BOOT_EQUIVALENT <- (
    NMI_BOOT_CI[1] > -EQUIVALENCE_MARGIN &&
    NMI_BOOT_CI[2] < EQUIVALENCE_MARGIN
)


# =============================================================================
# 18. PER-RUN PRACTICAL DIFFERENCE
# =============================================================================

ARI_WITHIN_MARGIN <- mean(
    abs(RESULTS$Delta_ARI) <= EQUIVALENCE_MARGIN
)


NMI_WITHIN_MARGIN <- mean(
    abs(RESULTS$Delta_NMI) <= EQUIVALENCE_MARGIN
)


# =============================================================================
# 19. BETTER / SAME / WORSE
# =============================================================================

EPS <- 1e-12


ARI_BETTER <- sum(
    RESULTS$Delta_ARI > EPS
)


ARI_EQUAL <- sum(
    abs(RESULTS$Delta_ARI) <= EPS
)


ARI_WORSE <- sum(
    RESULTS$Delta_ARI < -EPS
)


NMI_BETTER <- sum(
    RESULTS$Delta_NMI > EPS
)


NMI_EQUAL <- sum(
    abs(RESULTS$Delta_NMI) <= EPS
)


NMI_WORSE <- sum(
    RESULTS$Delta_NMI < -EPS
)


# =============================================================================
# 20. FINAL SUMMARY TABLE
# =============================================================================

SUMMARY <- data.frame(
    Metric = c(
        "ARI",
        "NMI"
    ),

    Transductive_mean = c(
        ARI_TRANS_MEAN,
        NMI_TRANS_MEAN
    ),

    Transductive_SD = c(
        ARI_TRANS_SD,
        NMI_TRANS_SD
    ),

    Inductive_mean = c(
        ARI_IND_MEAN,
        NMI_IND_MEAN
    ),

    Inductive_SD = c(
        ARI_IND_SD,
        NMI_IND_SD
    ),

    Difference = c(
        DELTA_ARI_MEAN,
        DELTA_NMI_MEAN
    ),

    Difference_SD = c(
        DELTA_ARI_SD,
        DELTA_NMI_SD
    ),

    Bootstrap_CI_lower = c(
        ARI_BOOT_CI[1],
        NMI_BOOT_CI[1]
    ),

    Bootstrap_CI_upper = c(
        ARI_BOOT_CI[2],
        NMI_BOOT_CI[2]
    ),

    Within_margin = c(
        ARI_WITHIN_MARGIN,
        NMI_WITHIN_MARGIN
    ),

    TOST_equivalent = c(
        ARI_TOST$equivalent,
        NMI_TOST$equivalent
    ),

    Bootstrap_equivalent = c(
        ARI_BOOT_EQUIVALENT,
        NMI_BOOT_EQUIVALENT
    )
)


# =============================================================================
# 21. PRINT FINAL RESULTS
# =============================================================================

cat("\n\n")
cat("==============================================================================================================\n")
cat("FINAL PAIRED BENCHMARK RESULTS\n")
cat("==============================================================================================================\n\n")


cat("ARI\n")
cat("--------------------------------------------------------------------------------------------------------------\n")

cat(
    sprintf(
        "Original transductive : %.3f +/- %.3f\n",
        ARI_TRANS_MEAN,
        ARI_TRANS_SD
    )
)

cat(
    sprintf(
        "Inductive             : %.3f +/- %.3f\n",
        ARI_IND_MEAN,
        ARI_IND_SD
    )
)

cat(
    sprintf(
        "Paired difference     : %+.4f +/- %.4f\n",
        DELTA_ARI_MEAN,
        DELTA_ARI_SD
    )
)

cat(
    sprintf(
        "Bootstrap 95%% CI      : [%+.4f, %+.4f]\n",
        ARI_BOOT_CI[1],
        ARI_BOOT_CI[2]
    )
)

cat(
    sprintf(
        "Standard paired p     : %.6g\n",
        ARI_PAIRED_TEST$p.value
    )
)

cat(
    sprintf(
        "TOST lower p          : %.6g\n",
        ARI_TOST$p_lower
    )
)

cat(
    sprintf(
        "TOST upper p          : %.6g\n",
        ARI_TOST$p_upper
    )
)

cat(
    "TOST equivalence       : ",
    ifelse(
        ARI_TOST$equivalent,
        "YES",
        "NO"
    ),
    "\n",
    sep = ""
)

cat(
    "Bootstrap CI within +/-0.05: ",
    ifelse(
        ARI_BOOT_EQUIVALENT,
        "YES",
        "NO"
    ),
    "\n",
    sep = ""
)

cat(
    sprintf(
        "Runs within +/-0.05   : %.1f%%\n",
        100 * ARI_WITHIN_MARGIN
    )
)

cat(
    sprintf(
        "Better / same / worse : %d / %d / %d\n",
        ARI_BETTER,
        ARI_EQUAL,
        ARI_WORSE
    )
)


cat("\n")
cat("NMI\n")
cat("--------------------------------------------------------------------------------------------------------------\n")

cat(
    sprintf(
        "Original transductive : %.3f +/- %.3f\n",
        NMI_TRANS_MEAN,
        NMI_TRANS_SD
    )
)

cat(
    sprintf(
        "Inductive             : %.3f +/- %.3f\n",
        NMI_IND_MEAN,
        NMI_IND_SD
    )
)

cat(
    sprintf(
        "Paired difference     : %+.4f +/- %.4f\n",
        DELTA_NMI_MEAN,
        DELTA_NMI_SD
    )
)

cat(
    sprintf(
        "Bootstrap 95%% CI      : [%+.4f, %+.4f]\n",
        NMI_BOOT_CI[1],
        NMI_BOOT_CI[2]
    )
)

cat(
    sprintf(
        "Standard paired p     : %.6g\n",
        NMI_PAIRED_TEST$p.value
    )
)

cat(
    sprintf(
        "TOST lower p          : %.6g\n",
        NMI_TOST$p_lower
    )
)

cat(
    sprintf(
        "TOST upper p          : %.6g\n",
        NMI_TOST$p_upper
    )
)

cat(
    "TOST equivalence       : ",
    ifelse(
        NMI_TOST$equivalent,
        "YES",
        "NO"
    ),
    "\n",
    sep = ""
)

cat(
    "Bootstrap CI within +/-0.05: ",
    ifelse(
        NMI_BOOT_EQUIVALENT,
        "YES",
        "NO"
    ),
    "\n",
    sep = ""
)

cat(
    sprintf(
        "Runs within +/-0.05   : %.1f%%\n",
        100 * NMI_WITHIN_MARGIN
    )
)

cat(
    sprintf(
        "Better / same / worse : %d / %d / %d\n",
        NMI_BETTER,
        NMI_EQUAL,
        NMI_WORSE
    )
)


cat("\n")
cat("--------------------------------------------------------------------------------------------------------------\n")
cat("SUMMARY TABLE\n")
cat("--------------------------------------------------------------------------------------------------------------\n\n")

print(
    SUMMARY,
    digits = 4,
    row.names = FALSE
)


# =============================================================================
# 22. SAVE NUMERIC RESULTS
# =============================================================================

write.csv(
    RESULTS,
    "longTAPIO_inductive_vs_transductive_runs.csv",
    row.names = FALSE
)


write.csv(
    SUMMARY,
    "longTAPIO_inductive_vs_transductive_summary.csv",
    row.names = FALSE
)


# =============================================================================
# 23. FIGURE 1
#
# PAIRED ARI/NMI COMPARISON
# =============================================================================

PLOT_DATA <- rbind(
    data.frame(
        Run = RESULTS$Run,
        Metric = "ARI",
        Method = "Transductive",
        Value = RESULTS$ARI_transductive
    ),

    data.frame(
        Run = RESULTS$Run,
        Metric = "ARI",
        Method = "Inductive",
        Value = RESULTS$ARI_inductive
    ),

    data.frame(
        Run = RESULTS$Run,
        Metric = "NMI",
        Method = "Transductive",
        Value = RESULTS$NMI_transductive
    ),

    data.frame(
        Run = RESULTS$Run,
        Metric = "NMI",
        Method = "Inductive",
        Value = RESULTS$NMI_inductive
    )
)


PLOT_DATA$Method <- factor(
    PLOT_DATA$Method,
    levels = c(
        "Transductive",
        "Inductive"
    )
)


P1 <- ggplot(
    PLOT_DATA,
    aes(
        x = Method,
        y = Value,
        group = Run
    )
) +
    geom_line(
        alpha = 0.20,
        linewidth = 0.5
    ) +
    geom_point(
        alpha = 0.55,
        size = 1.8
    ) +
    stat_summary(
        aes(group = 1),
        fun = mean,
        geom = "point",
        shape = 18,
        size = 5
    ) +
    facet_wrap(
        ~Metric,
        nrow = 1
    ) +
    coord_cartesian(
        ylim = c(
            0,
            1
        )
    ) +
    labs(
        title = "Original versus inductive longTAPIO",
        subtitle = paste0(
            N_RUNS,
            " paired simulations"
        ),
        x = NULL,
        y = "Clustering performance"
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


print(P1)


# =============================================================================
# 24. FIGURE 2
#
# TRANSDUCTIVE vs INDUCTIVE SCATTER
# =============================================================================

SCATTER_DATA <- rbind(
    data.frame(
        Metric = "ARI",
        Transductive = RESULTS$ARI_transductive,
        Inductive = RESULTS$ARI_inductive
    ),

    data.frame(
        Metric = "NMI",
        Transductive = RESULTS$NMI_transductive,
        Inductive = RESULTS$NMI_inductive
    )
)


P2 <- ggplot(
    SCATTER_DATA,
    aes(
        x = Transductive,
        y = Inductive
    )
) +
    geom_abline(
        intercept = 0,
        slope = 1,
        linetype = "dashed",
        linewidth = 0.8
    ) +
    geom_point(
        size = 2.8,
        alpha = 0.7
    ) +
    facet_wrap(
        ~Metric,
        nrow = 1
    ) +
    coord_equal(
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
        title = "Inductive versus transductive clustering performance",
        subtitle = "Dashed line denotes identical performance",
        x = "Original transductive",
        y = "Inductive"
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


print(P2)


# =============================================================================
# 25. FIGURE 3
#
# MOST IMPORTANT PUBLICATION FIGURE:
# MEAN PAIRED DIFFERENCE + BOOTSTRAP CI
# =============================================================================

DIFFERENCE_SUMMARY <- data.frame(
    Metric = factor(
        c(
            "ARI",
            "NMI"
        ),
        levels = c(
            "ARI",
            "NMI"
        )
    ),

    Difference = c(
        DELTA_ARI_MEAN,
        DELTA_NMI_MEAN
    ),

    Lower = c(
        ARI_BOOT_CI[1],
        NMI_BOOT_CI[1]
    ),

    Upper = c(
        ARI_BOOT_CI[2],
        NMI_BOOT_CI[2]
    )
)


P3 <- ggplot(
    DIFFERENCE_SUMMARY,
    aes(
        x = Metric,
        y = Difference
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
        linetype = "dashed",
        linewidth = 0.7
    ) +
    geom_errorbar(
        aes(
            ymin = Lower,
            ymax = Upper
        ),
        width = 0.12,
        linewidth = 1
    ) +
    geom_point(
        size = 4
    ) +
    labs(
        title = "Performance difference between inductive and transductive longTAPIO",
        subtitle = paste0(
            "Inductive - transductive; ",
            "95% bootstrap CI; practical equivalence region +/-",
            EQUIVALENCE_MARGIN
        ),
        x = NULL,
        y = "Mean paired difference"
    ) +
    theme_classic(
        base_size = 14
    ) +
    theme(
        plot.title = element_text(
            face = "bold"
        )
    )


print(P3)


# =============================================================================
# 26. SAVE FIGURES
# =============================================================================

ggsave(
    "longTAPIO_paired_performance.pdf",
    P1,
    width = 8,
    height = 5.5
)


ggsave(
    "longTAPIO_transductive_vs_inductive_scatter.pdf",
    P2,
    width = 8,
    height = 5.5
)


ggsave(
    "longTAPIO_equivalence.pdf",
    P3,
    width = 7.5,
    height = 5.5
)


# =============================================================================
# 27. PAPER-STYLE AUTOMATIC SUMMARY
# =============================================================================

cat("\n")
cat("==============================================================================================================\n")
cat("PAPER-STYLE RESULT\n")
cat("==============================================================================================================\n\n")


cat(
    sprintf(
        paste0(
            "Across %d paired simulations, original transductive longTAPIO ",
            "achieved an ARI of %.3f +/- %.3f and an NMI of %.3f +/- %.3f. "
        ),
        N_RUNS,
        ARI_TRANS_MEAN,
        ARI_TRANS_SD,
        NMI_TRANS_MEAN,
        NMI_TRANS_SD
    )
)


cat(
    sprintf(
        paste0(
            "The inductive implementation achieved an ARI of %.3f +/- %.3f ",
            "and an NMI of %.3f +/- %.3f.\n"
        ),
        ARI_IND_MEAN,
        ARI_IND_SD,
        NMI_IND_MEAN,
        NMI_IND_SD
    )
)


cat(
    sprintf(
        paste0(
            "The mean paired ARI difference (inductive - transductive) ",
            "was %+.4f (95%% bootstrap CI %+.4f to %+.4f). "
        ),
        DELTA_ARI_MEAN,
        ARI_BOOT_CI[1],
        ARI_BOOT_CI[2]
    )
)


cat(
    sprintf(
        paste0(
            "The corresponding NMI difference was %+.4f ",
            "(95%% bootstrap CI %+.4f to %+.4f).\n"
        ),
        DELTA_NMI_MEAN,
        NMI_BOOT_CI[1],
        NMI_BOOT_CI[2]
    )
)


cat(
    "ARI equivalence within +/-",
    EQUIVALENCE_MARGIN,
    ": ",
    ifelse(
        ARI_TOST$equivalent,
        "SUPPORTED",
        "NOT SUPPORTED"
    ),
    ".\n",
    sep = ""
)


cat(
    "NMI equivalence within +/-",
    EQUIVALENCE_MARGIN,
    ": ",
    ifelse(
        NMI_TOST$equivalent,
        "SUPPORTED",
        "NOT SUPPORTED"
    ),
    ".\n",
    sep = ""
)


cat("\n")
cat("==============================================================================================================\n")
cat("BENCHMARK COMPLETE\n")
cat("==============================================================================================================\n")