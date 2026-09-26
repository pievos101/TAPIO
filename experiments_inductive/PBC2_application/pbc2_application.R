# =============================================================================
# PBC2 - K = 2 INDUCTIVE longTAPIO
#
# COMPLETE ANALYSIS
#
# FIGURE 1:
#   Kaplan-Meier curves of TRAINING clusters
#
# FIGURE 2:
#   Progressive assignment stability (ARI)
#
# FIGURE 3:
#   Overall feature-importance evolution
#
# FIGURE 4:
#   Cluster-specific feature-importance evolution
#
# FIGURE 5:
#   Standardized biomarker trajectories by cluster
#
# FIGURE 6:
#   Biomarker separation heatmap
#   Higher-risk minus lower-risk trajectory
#
# FIGURE 7:
#   Time to stable cluster assignment
#
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

library(DynForest)
library(survival)
library(survminer)
library(ggplot2)
library(aricode)


# =============================================================================
# 1. CHECK longTAPIO FUNCTIONS
# =============================================================================

if(!exists("longTAPIO_inductive")) {
    stop("Please source longTAPIO_inductive() first.")
}


if(!exists("importance_longTAPIO_inductive")) {
    stop("Please source importance_longTAPIO_inductive() first.")
}


# =============================================================================
# 2. SETTINGS
# =============================================================================

SEED <- 42

K <- 2

TRAIN_FRACTION <- 0.70

MAX_VISITS <- 5

HORIZONS <- seq_len(MAX_VISITS)

N_TREES <- 500

LEVELS <- 4

PCA_SELECTION <- "random_weighted"


FEATURES <- c(
    "serBilir",
    "serChol",
    "albumin",
    "alkaline",
    "SGOT",
    "platelets",
    "prothrombin"
)


LOG_FEATURES <- c(
    "serBilir",
    "serChol",
    "alkaline",
    "SGOT",
    "platelets"
)


# =============================================================================
# 3. LOAD PBC2
# =============================================================================

data(
    pbc2,
    package = "DynForest"
)


PBC <- pbc2


cat("\n")
cat("==============================================================================================================\n")
cat("PBC2 - K=2 INDUCTIVE longTAPIO\n")
cat("==============================================================================================================\n")


cat(
    "\nPatients: ",
    length(unique(PBC$id)),
    "\n",
    sep = ""
)


cat(
    "Rows    : ",
    nrow(PBC),
    "\n",
    sep = ""
)


cat("\nOriginal event coding:\n")

print(
    table(PBC$event)
)


# =============================================================================
# 4. DEATH ENDPOINT
# =============================================================================
#
# PBC2:
#
# 0 = censored
# 1 = transplantation
# 2 = death
#
# For the present cause-specific death analysis:
#
# death = 1 only when event == 2.
#
# Transplantation is treated as censoring.
#
# =============================================================================

PBC$death_event <- ifelse(
    PBC$event == 2,
    1L,
    0L
)


# =============================================================================
# 5. PATIENT-LEVEL SURVIVAL DATA
# =============================================================================

extract_patient_survival <- function(dat) {

    ids <- sort(
        unique(dat$id)
    )


    OUT <- vector(
        "list",
        length(ids)
    )


    for(i in seq_along(ids)) {

        d_i <- dat[
            dat$id == ids[i],
            ,
            drop = FALSE
        ]


        survival_values <- unique(
            d_i$years[
                !is.na(d_i$years)
            ]
        )


        death_values <- unique(
            d_i$death_event[
                !is.na(d_i$death_event)
            ]
        )


        original_values <- unique(
            d_i$event[
                !is.na(d_i$event)
            ]
        )


        if(length(survival_values) != 1) {
            stop(
                paste0(
                    "Non-unique survival time for patient ",
                    ids[i]
                )
            )
        }


        if(length(death_values) != 1) {
            stop(
                paste0(
                    "Non-unique death endpoint for patient ",
                    ids[i]
                )
            )
        }


        if(length(original_values) != 1) {
            stop(
                paste0(
                    "Non-unique original endpoint for patient ",
                    ids[i]
                )
            )
        }


        OUT[[i]] <- data.frame(

            id =
                ids[i],

            survival_time =
                as.numeric(
                    survival_values[1]
                ),

            event =
                as.integer(
                    death_values[1]
                ),

            original_event =
                as.integer(
                    original_values[1]
                ),

            stringsAsFactors = FALSE
        )
    }


    OUT <- do.call(
        rbind,
        OUT
    )


    rownames(OUT) <- NULL


    return(
        OUT
    )
}


SURVIVAL_DATA <- extract_patient_survival(
    PBC
)


# =============================================================================
# 6. PREPARE LONGITUDINAL COHORT
# =============================================================================

prepare_pbc2 <- function(
    dat,
    survival_data,
    features,
    max_visits
) {

    dat <- dat[
        order(
            dat$id,
            dat$time
        ),
        ,
        drop = FALSE
    ]


    # -------------------------------------------------------------------------
    # Complete biomarker rows
    # -------------------------------------------------------------------------

    complete_rows <- complete.cases(
        dat[
            ,
            features,
            drop = FALSE
        ]
    )


    dat_complete <- dat[
        complete_rows,
        ,
        drop = FALSE
    ]


    # -------------------------------------------------------------------------
    # Require at least MAX_VISITS
    # -------------------------------------------------------------------------

    visit_counts <- table(
        dat_complete$id
    )


    eligible_ids <- names(
        visit_counts[
            visit_counts >= max_visits
        ]
    )


    cat(
        "\nPatients with >= ",
        max_visits,
        " complete visits: ",
        length(eligible_ids),
        "\n",
        sep = ""
    )


    dat_complete <- dat_complete[
        as.character(dat_complete$id) %in% eligible_ids,
        ,
        drop = FALSE
    ]


    # -------------------------------------------------------------------------
    # First MAX_VISITS complete observations
    # -------------------------------------------------------------------------

    patient_list <- split(
        dat_complete,
        dat_complete$id
    )


    patient_list <- lapply(
        patient_list,
        function(d) {

            d <- d[
                order(
                    d$time
                ),
                ,
                drop = FALSE
            ]


            d <- d[
                seq_len(max_visits),
                ,
                drop = FALSE
            ]


            d$visit_index <- seq_len(
                max_visits
            )


            return(
                d
            )
        }
    )


    dat_final <- do.call(
        rbind,
        patient_list
    )


    rownames(dat_final) <- NULL


    patients <- sort(
        unique(
            dat_final$id
        )
    )


    dat_final$patient_index <- match(
        dat_final$id,
        patients
    )


    dat_final <- dat_final[
        order(
            dat_final$patient_index,
            dat_final$visit_index
        ),
        ,
        drop = FALSE
    ]


    # -------------------------------------------------------------------------
    # Patient-level survival
    # -------------------------------------------------------------------------

    survival_final <- survival_data[
        match(
            patients,
            survival_data$id
        ),
        ,
        drop = FALSE
    ]


    survival_final$patient_index <- seq_len(
        nrow(survival_final)
    )


    # -------------------------------------------------------------------------
    # Fifth-visit landmark
    # -------------------------------------------------------------------------

    landmark_time <- tapply(
        dat_final$time,
        dat_final$patient_index,
        max
    )


    survival_final$landmark_time <- as.numeric(
        landmark_time[
            as.character(
                survival_final$patient_index
            )
        ]
    )


    survival_final$residual_survival <-
        survival_final$survival_time -
        survival_final$landmark_time


    valid <- (
        is.finite(
            survival_final$residual_survival
        ) &
        survival_final$residual_survival >= 0
    )


    if(any(!valid)) {

        cat(
            "\nRemoving ",
            sum(!valid),
            " patients with endpoint before landmark.\n",
            sep = ""
        )


        keep_ids <- survival_final$id[
            valid
        ]


        dat_final <- dat_final[
            dat_final$id %in% keep_ids,
            ,
            drop = FALSE
        ]


        survival_final <- survival_final[
            valid,
            ,
            drop = FALSE
        ]


        patients <- sort(
            unique(
                dat_final$id
            )
        )


        dat_final$patient_index <- match(
            dat_final$id,
            patients
        )


        dat_final <- dat_final[
            order(
                dat_final$patient_index,
                dat_final$visit_index
            ),
            ,
            drop = FALSE
        ]


        survival_final <- survival_final[
            match(
                patients,
                survival_final$id
            ),
            ,
            drop = FALSE
        ]


        survival_final$patient_index <- seq_len(
            nrow(survival_final)
        )
    }


    return(
        list(
            longitudinal = dat_final,
            survival = survival_final
        )
    )
}


PREP <- prepare_pbc2(

    dat = PBC,

    survival_data = SURVIVAL_DATA,

    features = FEATURES,

    max_visits = MAX_VISITS
)


PBC_LONG <- PREP$longitudinal

PATIENT_DATA <- PREP$survival


cat("\nAnalysis cohort:\n")

cat(
    "Patients : ",
    nrow(PATIENT_DATA),
    "\n",
    sep = ""
)


cat(
    "Deaths   : ",
    sum(PATIENT_DATA$event),
    "\n",
    sep = ""
)


# =============================================================================
# 7. PATIENT x VISIT x FEATURE ARRAY
# =============================================================================

pbc2_to_array <- function(
    dat,
    features,
    max_visits
) {

    ids <- sort(
        unique(
            dat$patient_index
        )
    )


    N <- length(ids)

    P <- length(features)


    X <- array(
        NA_real_,
        dim = c(
            N,
            max_visits,
            P
        ),
        dimnames = list(
            patient =
                as.character(ids),
            visit =
                paste0(
                    "Visit_",
                    seq_len(max_visits)
                ),
            feature =
                features
        )
    )


    for(i in seq_along(ids)) {

        d_i <- dat[
            dat$patient_index == ids[i],
            ,
            drop = FALSE
        ]


        d_i <- d_i[
            order(
                d_i$visit_index
            ),
            ,
            drop = FALSE
        ]


        X[i, , ] <- as.matrix(
            d_i[
                ,
                features,
                drop = FALSE
            ]
        )
    }


    if(any(!is.finite(X))) {
        stop("Non-finite values detected in longitudinal array.")
    }


    return(
        X
    )
}


X_RAW <- pbc2_to_array(

    dat = PBC_LONG,

    features = FEATURES,

    max_visits = MAX_VISITS
)


# =============================================================================
# 8. MODEL INPUT
# =============================================================================
#
# Preserve raw values separately.
#
# X_RAW = actual biomarker values
# X     = transformed values used by longTAPIO
#
# =============================================================================

X <- X_RAW


for(feature_name in LOG_FEATURES) {

    j <- match(
        feature_name,
        FEATURES
    )


    if(!is.na(j)) {

        X[, , j] <- log1p(
            X[, , j]
        )
    }
}


# =============================================================================
# 9. ARRAY -> longTAPIO FORMAT
# =============================================================================

array_to_longTAPIO <- function(X) {

    N <- dim(X)[1]

    V <- dim(X)[2]

    P <- dim(X)[3]


    DATA <- matrix(
        NA_real_,
        nrow = N * V,
        ncol = P
    )


    user_id <- rep(
        seq_len(N),
        each = V
    )


    for(i in seq_len(N)) {

        rows_i <- (
            (i - 1) * V + 1
        ):(
            i * V
        )


        DATA[rows_i, ] <- X[i, , ]
    }


    DATA <- as.data.frame(
        DATA
    )


    names(DATA) <- FEATURES


    return(
        list(
            DATA = DATA,
            user_id = user_id
        )
    )
}


# =============================================================================
# 10. TRAIN / TEST SPLIT
# =============================================================================

make_split <- function(
    event,
    train_fraction,
    seed
) {

    set.seed(
        seed
    )


    train_idx <- integer(0)


    for(status in sort(unique(event))) {

        ids <- which(
            event == status
        )


        n_train <- floor(
            length(ids) *
            train_fraction
        )


        selected <- sample(
            ids,
            size = n_train,
            replace = FALSE
        )


        train_idx <- c(
            train_idx,
            selected
        )
    }


    train_idx <- sort(
        train_idx
    )


    test_idx <- setdiff(
        seq_along(event),
        train_idx
    )


    return(
        list(
            train = train_idx,
            test = test_idx
        )
    )
}


SPLIT <- make_split(

    event = PATIENT_DATA$event,

    train_fraction = TRAIN_FRACTION,

    seed = SEED
)


TRAIN_IDX <- SPLIT$train

TEST_IDX <- SPLIT$test


X_TRAIN <- X[
    TRAIN_IDX,
    ,
    ,
    drop = FALSE
]


X_TEST <- X[
    TEST_IDX,
    ,
    ,
    drop = FALSE
]


X_RAW_TRAIN <- X_RAW[
    TRAIN_IDX,
    ,
    ,
    drop = FALSE
]


X_RAW_TEST <- X_RAW[
    TEST_IDX,
    ,
    ,
    drop = FALSE
]


SURV_TRAIN <- PATIENT_DATA[
    TRAIN_IDX,
    ,
    drop = FALSE
]


SURV_TEST <- PATIENT_DATA[
    TEST_IDX,
    ,
    drop = FALSE
]


TRAIN_PREP <- array_to_longTAPIO(
    X_TRAIN
)


TEST_PREP <- array_to_longTAPIO(
    X_TEST
)


cat("\n")

cat(
    "Training patients : ",
    nrow(SURV_TRAIN),
    "\n",
    sep = ""
)


cat(
    "Test patients     : ",
    nrow(SURV_TEST),
    "\n",
    sep = ""
)


cat(
    "Training deaths   : ",
    sum(SURV_TRAIN$event),
    "\n",
    sep = ""
)


cat(
    "Test deaths       : ",
    sum(SURV_TEST$event),
    "\n",
    sep = ""
)


# =============================================================================
# 11. FIT K=2 longTAPIO
# =============================================================================

N_FEATURES_TREE <- max(
    2,
    ceiling(
        sqrt(
            length(FEATURES)
        )
    )
)


N_FEATURES_TREE <- min(
    N_FEATURES_TREE,
    length(FEATURES)
)


set.seed(
    1002
)


MODEL <- longTAPIO_inductive(

    DATA = TRAIN_PREP$DATA,

    user_id = TRAIN_PREP$user_id,

    k = K,

    n_features = N_FEATURES_TREE,

    n_trees = N_TREES,

    levels = LEVELS,

    method = "ward.D2",

    scale = TRUE,

    replace = FALSE,

    pca_selection = PCA_SELECTION
)


TRAIN_CLUSTER <- as.integer(
    MODEL$train_clusters
)


cat("\nTraining cluster sizes:\n")

print(
    table(
        TRAIN_CLUSTER
    )
)


# =============================================================================
# 12. SURVIVAL CHARACTERIZATION
# =============================================================================

TRAIN_SURVIVAL <- data.frame(

    time =
        SURV_TRAIN$residual_survival,

    event =
        SURV_TRAIN$event,

    cluster =
        TRAIN_CLUSTER
)


TRAIN_EVENT_RATE <- aggregate(

    event ~ cluster,

    data = TRAIN_SURVIVAL,

    FUN = mean
)


HIGH_RISK_CLUSTER <- TRAIN_EVENT_RATE$cluster[
    which.max(
        TRAIN_EVENT_RATE$event
    )
]


LOW_RISK_CLUSTER <- TRAIN_EVENT_RATE$cluster[
    which.min(
        TRAIN_EVENT_RATE$event
    )
]


risk_label <- function(cluster) {

    ifelse(
        cluster == HIGH_RISK_CLUSTER,
        "Higher-risk trajectory",
        "Lower-risk trajectory"
    )
}


TRAIN_SURVIVAL$risk_group <- factor(

    risk_label(
        TRAIN_CLUSTER
    ),

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


cat("\nTraining survival groups:\n")

print(
    table(
        TRAIN_SURVIVAL$risk_group
    )
)


cat("\nDeaths by survival group:\n")

print(
    with(
        TRAIN_SURVIVAL,
        table(
            risk_group,
            event
        )
    )
)


# =============================================================================
# 13. KAPLAN-MEIER
# =============================================================================

KM_TRAIN <- survival::survfit(

    survival::Surv(
        time,
        event
    ) ~ risk_group,

    data = TRAIN_SURVIVAL
)


LOGRANK_TRAIN <- survival::survdiff(

    survival::Surv(
        time,
        event
    ) ~ risk_group,

    data = TRAIN_SURVIVAL
)


TRAIN_CHISQ <- as.numeric(
    LOGRANK_TRAIN$chisq
)


TRAIN_DF <- length(
    LOGRANK_TRAIN$n
) - 1L


TRAIN_P <- pchisq(

    TRAIN_CHISQ,

    df = TRAIN_DF,

    lower.tail = FALSE
)


cat("\nTRAINING SURVIVAL DIFFERENCE\n")
cat("----------------------------------------\n")


cat(
    sprintf(
        "Log-rank chi-square = %.3f\n",
        TRAIN_CHISQ
    )
)


cat(
    sprintf(
        "df                  = %d\n",
        TRAIN_DF
    )
)


cat(
    sprintf(
        "p                   = %.8g\n",
        TRAIN_P
    )
)


# =============================================================================
# FIGURE 1
# KAPLAN-MEIER
# =============================================================================

PLOT_KM <- survminer::ggsurvplot(

    fit =
        KM_TRAIN,

    data =
        TRAIN_SURVIVAL,

    pval =
        TRUE,

    pval.method =
        TRUE,

    conf.int =
        TRUE,

    risk.table =
        TRUE,

    risk.table.height =
        0.25,

    risk.table.y.text =
        FALSE,

    risk.table.col =
        "strata",

    censor =
        TRUE,

    censor.shape =
        124,

    censor.size =
        3,

    xlab =
        "Years after fifth-visit landmark",

    ylab =
        "Survival probability",

    break.time.by =
        1,

    surv.scale =
        "percent",

    legend.title =
        "Trajectory cluster",

    legend.labs =
        c(
            "Lower-risk trajectory",
            "Higher-risk trajectory"
        ),

    legend =
        "bottom",

    size =
        1.2,

    ggtheme =
        theme_classic(
            base_size = 14
        ),

    title =
        "Survival of training-derived trajectory clusters"
)


print(
    PLOT_KM
)


# =============================================================================
# 14. INDUCTIVE TEST ASSIGNMENT + IMPORTANCE
# =============================================================================

IMP <- importance_longTAPIO_inductive(

    res = MODEL,

    newdata = TEST_PREP$DATA,

    user_id = TEST_PREP$user_id,

    normalize = TRUE
)


FINAL_CLUSTER <- as.integer(
    IMP$predicted_cluster[, MAX_VISITS]
)


cat("\nFinal test clusters:\n")

print(
    table(
        FINAL_CLUSTER
    )
)


# =============================================================================
# 15. PROGRESSIVE ASSIGNMENT PERFORMANCE
# =============================================================================

ARI_VALUES <- numeric(
    MAX_VISITS
)


NMI_VALUES <- numeric(
    MAX_VISITS
)


AGREEMENT_VALUES <- numeric(
    MAX_VISITS
)


for(H in HORIZONS) {

    PRED_H <- as.integer(
        IMP$predicted_cluster[, H]
    )


    ARI_VALUES[H] <- aricode::ARI(
        FINAL_CLUSTER,
        PRED_H
    )


    NMI_VALUES[H] <- aricode::NMI(
        FINAL_CLUSTER,
        PRED_H
    )


    AGREEMENT_VALUES[H] <- mean(
        FINAL_CLUSTER ==
            PRED_H
    )
}


PERFORMANCE <- data.frame(

    Visit =
        HORIZONS,

    ARI =
        ARI_VALUES,

    NMI =
        NMI_VALUES,

    Agreement =
        AGREEMENT_VALUES
)


cat("\nProgressive assignment:\n\n")

print(
    PERFORMANCE,
    digits = 3,
    row.names = FALSE
)


# =============================================================================
# FIGURE 2
# PROGRESSIVE ARI
# =============================================================================

PLOT_ARI <- ggplot(

    PERFORMANCE,

    aes(
        x = Visit,
        y = ARI
    )
) +

    geom_line(
        linewidth = 1.3
    ) +

    geom_point(
        size = 3.5
    ) +

    geom_text(

        aes(
            label = sprintf(
                "%.2f",
                ARI
            )
        ),

        vjust = -1,

        size = 4
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    scale_y_continuous(
        limits = c(
            0,
            1.08
        ),
        breaks = seq(
            0,
            1,
            0.2
        )
    ) +

    labs(

        title =
            "Progressive trajectory-cluster assignment",

        subtitle =
            "Agreement with final five-visit assignment in unseen patients",

        x =
            "Number of observed visits",

        y =
            "Adjusted Rand Index"
    ) +

    theme_classic(
        base_size = 14
    )


print(
    PLOT_ARI
)


# =============================================================================
# 16. PATIENT-LEVEL IMPORTANCE DATA
# =============================================================================

IMPORTANCE_LIST <- list()

counter <- 1L


for(H in HORIZONS) {

    for(j in seq_along(FEATURES)) {

        IMPORTANCE_LIST[[counter]] <- data.frame(

            Patient =
                seq_along(
                    FINAL_CLUSTER
                ),

            PatientID =
                SURV_TEST$id,

            Visit =
                H,

            Feature =
                FEATURES[j],

            Importance =
                as.numeric(
                    IMP$patient[, j, H]
                ),

            Final_cluster =
                FINAL_CLUSTER,

            Risk_group =
                risk_label(
                    FINAL_CLUSTER
                ),

            stringsAsFactors = FALSE
        )


        counter <- counter + 1L
    }
}


IMPORTANCE_PATIENT <- do.call(
    rbind,
    IMPORTANCE_LIST
)


IMPORTANCE_PATIENT$Feature <- factor(

    IMPORTANCE_PATIENT$Feature,

    levels = FEATURES
)


IMPORTANCE_PATIENT$Risk_group <- factor(

    IMPORTANCE_PATIENT$Risk_group,

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


# =============================================================================
# 17. OVERALL FEATURE IMPORTANCE
# =============================================================================

OVERALL_IMPORTANCE <- aggregate(

    Importance ~ Visit + Feature,

    data = IMPORTANCE_PATIENT,

    FUN = mean
)


# =============================================================================
# FIGURE 3
# OVERALL FEATURE IMPORTANCE
# =============================================================================

PLOT_IMPORTANCE <- ggplot(

    OVERALL_IMPORTANCE,

    aes(
        x = Visit,
        y = Importance,
        colour = Feature,
        group = Feature
    )
) +

    geom_line(
        linewidth = 1.15
    ) +

    geom_point(
        size = 2.7
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    labs(

        title =
            "Evolution of biomarker importance",

        x =
            "Number of observed visits",

        y =
            "Normalized importance",

        colour =
            "Biomarker"
    ) +

    theme_classic(
        base_size = 14
    )


print(
    PLOT_IMPORTANCE
)


# =============================================================================
# 18. CLUSTER-SPECIFIC FEATURE IMPORTANCE
# =============================================================================

CLUSTER_IMPORTANCE <- aggregate(

    Importance ~ Visit + Feature + Final_cluster + Risk_group,

    data = IMPORTANCE_PATIENT,

    FUN = mean
)


# =============================================================================
# FIGURE 4
# CLUSTER-SPECIFIC FEATURE IMPORTANCE
# =============================================================================

PLOT_CLUSTER_IMPORTANCE <- ggplot(

    CLUSTER_IMPORTANCE,

    aes(
        x = Visit,
        y = Importance,
        colour = Feature,
        group = Feature
    )
) +

    geom_line(
        linewidth = 1.15
    ) +

    geom_point(
        size = 2.5
    ) +

    facet_wrap(

        ~ Risk_group,

        ncol = 1
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    labs(

        title =
            "Cluster-specific evolution of biomarker importance",

        subtitle =
            "Patients grouped by final five-visit assignment",

        x =
            "Number of observed visits",

        y =
            "Normalized feature importance",

        colour =
            "Biomarker"
    ) +

    theme_classic(
        base_size = 14
    ) +

    theme(

        strip.text =
            element_text(
                face = "bold"
            ),

        legend.position =
            "right"
    )


print(
    PLOT_CLUSTER_IMPORTANCE
)


# =============================================================================
# 19. STANDARDIZE ACTUAL BIOMARKER VALUES
#
# IMPORTANT:
#
# Standardization parameters come ONLY from training patients.
#
# z > 0 = above training population mean
# z < 0 = below training population mean
#
# =============================================================================

P <- length(
    FEATURES
)


TRAIN_MEAN <- numeric(
    P
)


TRAIN_SD <- numeric(
    P
)


names(TRAIN_MEAN) <- FEATURES

names(TRAIN_SD) <- FEATURES


for(j in seq_len(P)) {

    TRAIN_MEAN[j] <- mean(
        X_RAW_TRAIN[, , j],
        na.rm = TRUE
    )


    TRAIN_SD[j] <- sd(
        X_RAW_TRAIN[, , j],
        na.rm = TRUE
    )


    if(
        !is.finite(TRAIN_SD[j]) ||
        TRAIN_SD[j] == 0
    ) {

        TRAIN_SD[j] <- 1
    }
}


X_Z_TEST <- X_RAW_TEST


for(j in seq_len(P)) {

    X_Z_TEST[, , j] <- (
        X_RAW_TEST[, , j] -
        TRAIN_MEAN[j]
    ) /
        TRAIN_SD[j]
}


# =============================================================================
# 20. LONG-FORM BIOMARKER DATA
# =============================================================================

BIOMARKER_LONG_LIST <- list()

counter <- 1L


for(i in seq_len(dim(X_Z_TEST)[1])) {

    for(H in HORIZONS) {

        for(j in seq_along(FEATURES)) {

            BIOMARKER_LONG_LIST[[counter]] <- data.frame(

                Patient =
                    i,

                PatientID =
                    SURV_TEST$id[i],

                Visit =
                    H,

                Feature =
                    FEATURES[j],

                Raw_value =
                    X_RAW_TEST[i, H, j],

                Z_value =
                    X_Z_TEST[i, H, j],

                Final_cluster =
                    FINAL_CLUSTER[i],

                Risk_group =
                    risk_label(
                        FINAL_CLUSTER[i]
                    ),

                stringsAsFactors = FALSE
            )


            counter <- counter + 1L
        }
    }
}


BIOMARKER_LONG <- do.call(
    rbind,
    BIOMARKER_LONG_LIST
)


BIOMARKER_LONG$Feature <- factor(

    BIOMARKER_LONG$Feature,

    levels = FEATURES
)


BIOMARKER_LONG$Risk_group <- factor(

    BIOMARKER_LONG$Risk_group,

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


# =============================================================================
# 21. MEAN BIOMARKER TRAJECTORIES
# =============================================================================

BIOMARKER_CLUSTER_MEAN <- aggregate(

    Z_value ~ Visit + Feature + Final_cluster + Risk_group,

    data = BIOMARKER_LONG,

    FUN = mean
)


BIOMARKER_CLUSTER_MEAN$Feature <- factor(

    BIOMARKER_CLUSTER_MEAN$Feature,

    levels = FEATURES
)


BIOMARKER_CLUSTER_MEAN$Risk_group <- factor(

    BIOMARKER_CLUSTER_MEAN$Risk_group,

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


cat("\n")
cat("==============================================================================================================\n")
cat("STANDARDIZED BIOMARKER TRAJECTORIES\n")
cat("==============================================================================================================\n\n")


print(
    BIOMARKER_CLUSTER_MEAN,
    digits = 3,
    row.names = FALSE
)


# =============================================================================
# FIGURE 5
# BIOMARKER TRAJECTORIES BY CLUSTER
# =============================================================================

PLOT_BIOMARKER_TRAJECTORIES <- ggplot(

    BIOMARKER_CLUSTER_MEAN,

    aes(
        x = Visit,
        y = Z_value,
        colour = Risk_group,
        group = Risk_group
    )
) +

    geom_hline(

        yintercept = 0,

        linetype = "dashed",

        linewidth = 0.5
    ) +

    geom_line(
        linewidth = 1.25
    ) +

    geom_point(
        size = 2.8
    ) +

    facet_wrap(

        ~ Feature,

        ncol = 3
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    scale_y_continuous(
        limits = c(
            -1.6,
            1.6
        ),
        breaks = seq(
            -1.5,
            1.5,
            0.5
        )
    ) +

    labs(

        title =
            "Biomarker trajectories characterizing the two clusters",

        subtitle =
            "Standardized relative to the training population",

        x =
            "Visit",

        y =
            "Standardized biomarker value (z-score)",

        colour =
            "Trajectory cluster"
    ) +

    theme_classic(
        base_size = 13
    ) +

    theme(

        plot.title =
            element_text(
                face = "bold"
            ),

        strip.text =
            element_text(
                face = "bold"
            ),

        legend.position =
            "bottom"
    )


print(
    PLOT_BIOMARKER_TRAJECTORIES
)


# =============================================================================
# 22. NEW:
# BIOMARKER SEPARATION BETWEEN CLUSTERS
#
# Delta = mean z-score in higher-risk cluster
#         minus
#         mean z-score in lower-risk cluster
#
# Therefore:
#
# Delta > 0:
# biomarker is HIGHER in higher-risk patients
#
# Delta < 0:
# biomarker is LOWER in higher-risk patients
#
# =============================================================================

HIGH_PROFILE <- BIOMARKER_CLUSTER_MEAN[
    BIOMARKER_CLUSTER_MEAN$Risk_group == "Higher-risk trajectory",
    c(
        "Visit",
        "Feature",
        "Z_value"
    )
]


LOW_PROFILE <- BIOMARKER_CLUSTER_MEAN[
    BIOMARKER_CLUSTER_MEAN$Risk_group == "Lower-risk trajectory",
    c(
        "Visit",
        "Feature",
        "Z_value"
    )
]


names(HIGH_PROFILE)[3] <- "Higher_risk_Z"

names(LOW_PROFILE)[3] <- "Lower_risk_Z"


BIOMARKER_SEPARATION <- merge(

    HIGH_PROFILE,

    LOW_PROFILE,

    by = c(
        "Visit",
        "Feature"
    )
)


BIOMARKER_SEPARATION$Delta_Z <-
    BIOMARKER_SEPARATION$Higher_risk_Z -
    BIOMARKER_SEPARATION$Lower_risk_Z


BIOMARKER_SEPARATION$Feature <- factor(

    BIOMARKER_SEPARATION$Feature,

    levels = rev(
        FEATURES
    )
)


BIOMARKER_SEPARATION <- BIOMARKER_SEPARATION[
    order(
        BIOMARKER_SEPARATION$Feature,
        BIOMARKER_SEPARATION$Visit
    ),
    ,
    drop = FALSE
]


cat("\n")
cat("==============================================================================================================\n")
cat("BIOMARKER SEPARATION: HIGHER-RISK MINUS LOWER-RISK\n")
cat("==============================================================================================================\n\n")


print(
    BIOMARKER_SEPARATION,
    digits = 3,
    row.names = FALSE
)


# =============================================================================
# FIGURE 6
# BIOMARKER SEPARATION HEATMAP
# =============================================================================

PLOT_SEPARATION <- ggplot(

    BIOMARKER_SEPARATION,

    aes(
        x = factor(Visit),
        y = Feature,
        fill = Delta_Z
    )
) +

    geom_tile(
        linewidth = 0.7
    ) +

    geom_text(

        aes(
            label = sprintf(
                "%.2f",
                Delta_Z
            )
        ),

        size = 3.8
    ) +

    scale_fill_gradient2(

        midpoint = 0,

        name =
            "Difference\nin z-score"
    ) +

    labs(

        title =
            "Emergence of biomarker differences between trajectory clusters",

        subtitle =
            "Higher-risk minus lower-risk trajectory",

        x =
            "Visit",

        y =
            NULL
    ) +

    theme_classic(
        base_size = 13
    ) +

    theme(

        plot.title =
            element_text(
                face = "bold"
            ),

        axis.text.y =
            element_text(
                face = "bold"
            ),

        legend.title =
            element_text(
                face = "bold"
            )
    )


print(
    PLOT_SEPARATION
)


# =============================================================================
# 23. NEW:
# TIME TO STABLE CLUSTER ASSIGNMENT
#
# Definition:
#
# Earliest visit H such that the patient's assignment at H and ALL
# subsequent visits equals the final visit-5 assignment.
#
# Example:
#
# 2 2 2 2 2 -> stable at visit 1
#
# 1 2 2 2 2 -> stable at visit 2
#
# 2 2 1 1 1 -> stable at visit 3
#
# 2 2 2 2 1 -> stable at visit 5
#
# =============================================================================

PREDICTION_MATRIX <- as.matrix(
    IMP$predicted_cluster[
        ,
        HORIZONS,
        drop = FALSE
    ]
)


STABLE_VISIT <- integer(
    nrow(
        PREDICTION_MATRIX
    )
)


for(i in seq_len(nrow(PREDICTION_MATRIX))) {

    final_i <- PREDICTION_MATRIX[
        i,
        MAX_VISITS
    ]


    stable_i <- MAX_VISITS


    for(H in HORIZONS) {

        subsequent_assignments <- PREDICTION_MATRIX[
            i,
            H:MAX_VISITS
        ]


        if(
            all(
                subsequent_assignments ==
                    final_i
            )
        ) {

            stable_i <- H

            break
        }
    }


    STABLE_VISIT[i] <- stable_i
}


STABILITY_PATIENT <- data.frame(

    PatientID =
        SURV_TEST$id,

    Final_cluster =
        FINAL_CLUSTER,

    Risk_group =
        risk_label(
            FINAL_CLUSTER
        ),

    Stable_from_visit =
        STABLE_VISIT,

    stringsAsFactors = FALSE
)


STABILITY_PATIENT$Risk_group <- factor(

    STABILITY_PATIENT$Risk_group,

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


cat("\n")
cat("==============================================================================================================\n")
cat("TIME TO STABLE ASSIGNMENT\n")
cat("==============================================================================================================\n\n")


print(
    table(
        STABILITY_PATIENT$Stable_from_visit
    )
)


# =============================================================================
# 24. CUMULATIVE STABILIZATION
# =============================================================================

STABILITY_SUMMARY <- data.frame(

    Visit =
        HORIZONS,

    N_stable =
        NA_integer_,

    Proportion_stable =
        NA_real_
)


for(H in HORIZONS) {

    STABILITY_SUMMARY$N_stable[
        STABILITY_SUMMARY$Visit == H
    ] <- sum(
        STABLE_VISIT <= H
    )


    STABILITY_SUMMARY$Proportion_stable[
        STABILITY_SUMMARY$Visit == H
    ] <- mean(
        STABLE_VISIT <= H
    )
}


cat("\nCumulative stabilization:\n\n")


print(
    STABILITY_SUMMARY,
    digits = 3,
    row.names = FALSE
)


# =============================================================================
# FIGURE 7
# CUMULATIVE PROPORTION WITH STABLE ASSIGNMENT
# =============================================================================

PLOT_STABILITY <- ggplot(

    STABILITY_SUMMARY,

    aes(
        x = Visit,
        y = Proportion_stable
    )
) +

    geom_line(
        linewidth = 1.3
    ) +

    geom_point(
        size = 3.5
    ) +

    geom_text(

        aes(
            label = paste0(
                round(
                    100 *
                        Proportion_stable,
                    1
                ),
                "%"
            )
        ),

        vjust = -1,

        size = 4
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    scale_y_continuous(

        limits = c(
            0,
            1.08
        ),

        breaks = seq(
            0,
            1,
            0.2
        ),

        labels = function(x) {
            paste0(
                round(
                    x * 100
                ),
                "%"
            )
        }
    ) +

    labs(

        title =
            "Time to stable trajectory-cluster assignment",

        subtitle =
            "Earliest visit after which assignment remains unchanged",

        x =
            "Visit",

        y =
            "Patients with stable assignment"
    ) +

    theme_classic(
        base_size = 14
    ) +

    theme(

        plot.title =
            element_text(
                face = "bold"
            )
    )


print(
    PLOT_STABILITY
)


# =============================================================================
# 25. STABILITY BY FINAL CLUSTER
# =============================================================================

STABILITY_CLUSTER <- aggregate(

    Stable_from_visit ~ Risk_group,

    data = STABILITY_PATIENT,

    FUN = function(x) {

        c(
            Mean = mean(x),
            Median = median(x)
        )
    }
)


cat("\nStability by final cluster:\n\n")

print(
    STABILITY_CLUSTER
)


# =============================================================================
# 26. FINAL VISIT BIOMARKER PROFILE
# =============================================================================

FINAL_PROFILE <- BIOMARKER_CLUSTER_MEAN[
    BIOMARKER_CLUSTER_MEAN$Visit == MAX_VISITS,
    ,
    drop = FALSE
]


PLOT_FINAL_PROFILE <- ggplot(

    FINAL_PROFILE,

    aes(
        x = Feature,
        y = Z_value,
        fill = Risk_group
    )
) +

    geom_col(

        position =
            position_dodge(
                width = 0.8
            ),

        width =
            0.7
    ) +

    geom_hline(

        yintercept = 0,

        linetype = "dashed"
    ) +

    coord_flip() +

    labs(

        title =
            "Biomarker profile after five visits",

        subtitle =
            "Standardized biomarker levels in the two trajectory clusters",

        x =
            NULL,

        y =
            "Standardized biomarker value (z-score)",

        fill =
            "Trajectory cluster"
    ) +

    theme_classic(
        base_size = 13
    ) +

    theme(

        plot.title =
            element_text(
                face = "bold"
            ),

        legend.position =
            "bottom"
    )


print(
    PLOT_FINAL_PROFILE
)


# =============================================================================
# 27. TEST SURVIVAL ANALYSIS
#
# This provides independent descriptive validation of the survival
# association in inductively assigned patients.
#
# Risk labels were determined from TRAINING survival only.
# =============================================================================

TEST_SURVIVAL <- data.frame(

    time =
        SURV_TEST$residual_survival,

    event =
        SURV_TEST$event,

    cluster =
        FINAL_CLUSTER,

    risk_group =
        factor(
            risk_label(
                FINAL_CLUSTER
            ),
            levels = c(
                "Lower-risk trajectory",
                "Higher-risk trajectory"
            )
        )
)


KM_TEST <- survival::survfit(

    survival::Surv(
        time,
        event
    ) ~ risk_group,

    data = TEST_SURVIVAL
)


LOGRANK_TEST <- survival::survdiff(

    survival::Surv(
        time,
        event
    ) ~ risk_group,

    data = TEST_SURVIVAL
)


TEST_CHISQ <- as.numeric(
    LOGRANK_TEST$chisq
)


TEST_DF <- length(
    LOGRANK_TEST$n
) - 1L


TEST_P <- pchisq(

    TEST_CHISQ,

    df = TEST_DF,

    lower.tail = FALSE
)


cat("\n")
cat("==============================================================================================================\n")
cat("TEST SURVIVAL VALIDATION\n")
cat("==============================================================================================================\n\n")


cat(
    sprintf(
        "Log-rank chi-square = %.3f\n",
        TEST_CHISQ
    )
)


cat(
    sprintf(
        "p                   = %.8g\n",
        TEST_P
    )
)


cat("\nDeaths by test cluster:\n")


print(
    with(
        TEST_SURVIVAL,
        table(
            risk_group,
            event
        )
    )
)


# =============================================================================
# OPTIONAL TEST KM FIGURE
# =============================================================================

PLOT_KM_TEST <- survminer::ggsurvplot(

    fit =
        KM_TEST,

    data =
        TEST_SURVIVAL,

    pval =
        TRUE,

    pval.method =
        TRUE,

    conf.int =
        TRUE,

    risk.table =
        TRUE,

    risk.table.height =
        0.25,

    risk.table.y.text =
        FALSE,

    risk.table.col =
        "strata",

    censor =
        TRUE,

    censor.shape =
        124,

    censor.size =
        3,

    xlab =
        "Years after fifth-visit landmark",

    ylab =
        "Survival probability",

    break.time.by =
        1,

    surv.scale =
        "percent",

    legend.title =
        "Trajectory cluster",

    legend.labs =
        c(
            "Lower-risk trajectory",
            "Higher-risk trajectory"
        ),

    legend =
        "bottom",

    size =
        1.2,

    ggtheme =
        theme_classic(
            base_size = 14
        ),

    title =
        "Survival of inductively assigned test patients"
)


print(
    PLOT_KM_TEST
)


# =============================================================================
# 28. SAVE TABLES
# =============================================================================

write.csv(
    PERFORMANCE,
    "PBC2_K2_progressive_assignment.csv",
    row.names = FALSE
)


write.csv(
    OVERALL_IMPORTANCE,
    "PBC2_K2_overall_importance.csv",
    row.names = FALSE
)


write.csv(
    CLUSTER_IMPORTANCE,
    "PBC2_K2_cluster_specific_importance.csv",
    row.names = FALSE
)


write.csv(
    BIOMARKER_CLUSTER_MEAN,
    "PBC2_K2_biomarker_trajectories.csv",
    row.names = FALSE
)


write.csv(
    BIOMARKER_SEPARATION,
    "PBC2_K2_biomarker_separation.csv",
    row.names = FALSE
)


write.csv(
    STABILITY_PATIENT,
    "PBC2_K2_patient_stability.csv",
    row.names = FALSE
)


write.csv(
    STABILITY_SUMMARY,
    "PBC2_K2_stability_summary.csv",
    row.names = FALSE
)


# =============================================================================
# 29. SAVE FIGURES
# =============================================================================

# -----------------------------------------------------------------------------
# Training KM including risk table
# -----------------------------------------------------------------------------

pdf(
    "PBC2_Figure1_Training_KM.pdf",
    width = 7.5,
    height = 7
)

print(
    PLOT_KM
)

dev.off()


# -----------------------------------------------------------------------------
# Progressive ARI
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Figure2_Progressive_ARI.pdf",

    PLOT_ARI,

    width = 6.5,

    height = 5
)


# -----------------------------------------------------------------------------
# Overall importance
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Figure3_Overall_Importance.pdf",

    PLOT_IMPORTANCE,

    width = 8,

    height = 5.5
)


# -----------------------------------------------------------------------------
# Cluster-specific importance
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Figure4_ClusterSpecific_Importance.pdf",

    PLOT_CLUSTER_IMPORTANCE,

    width = 8,

    height = 7
)


# -----------------------------------------------------------------------------
# Biomarker trajectories
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Figure5_Biomarker_Trajectories.pdf",

    PLOT_BIOMARKER_TRAJECTORIES,

    width = 10,

    height = 8
)


# -----------------------------------------------------------------------------
# Biomarker separation heatmap
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Figure6_Biomarker_Separation_Heatmap.pdf",

    PLOT_SEPARATION,

    width = 7.5,

    height = 5.5
)


# -----------------------------------------------------------------------------
# Stable assignment
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Figure7_Assignment_Stability.pdf",

    PLOT_STABILITY,

    width = 6.5,

    height = 5
)


# -----------------------------------------------------------------------------
# Final profile - supplementary
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Supplement_Final_Biomarker_Profile.pdf",

    PLOT_FINAL_PROFILE,

    width = 8,

    height = 5
)


# -----------------------------------------------------------------------------
# Test KM - validation
# -----------------------------------------------------------------------------

pdf(
    "PBC2_Supplement_Test_KM.pdf",
    width = 7.5,
    height = 7
)

print(
    PLOT_KM_TEST
)

dev.off()


# =============================================================================
# 30. FINAL SUMMARY
# =============================================================================

cat("\n\n")
cat("==============================================================================================================\n")
cat("FINAL PBC2 RESULTS\n")
cat("==============================================================================================================\n\n")


cat("TRAINING CLUSTERS\n\n")

print(
    table(
        TRAIN_CLUSTER
    )
)


cat("\nTRAINING SURVIVAL\n")

cat(
    sprintf(
        "Log-rank chi-square = %.3f\n",
        TRAIN_CHISQ
    )
)


cat(
    sprintf(
        "p = %.8g\n",
        TRAIN_P
    )
)


cat("\nTEST SURVIVAL\n")

cat(
    sprintf(
        "Log-rank chi-square = %.3f\n",
        TEST_CHISQ
    )
)


cat(
    sprintf(
        "p = %.8g\n",
        TEST_P
    )
)


cat("\nPROGRESSIVE ASSIGNMENT\n\n")

print(
    PERFORMANCE,
    digits = 3,
    row.names = FALSE
)


cat("\nTIME TO STABLE ASSIGNMENT\n\n")

print(
    STABILITY_SUMMARY,
    digits = 3,
    row.names = FALSE
)


cat("\nBIOMARKER SEPARATION\n\n")

print(
    BIOMARKER_SEPARATION,
    digits = 3,
    row.names = FALSE
)


cat("\n")
cat("==============================================================================================================\n")
cat("DONE\n")
cat("==============================================================================================================\n")